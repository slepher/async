# 审核原则与架构边界 (Review Guidelines)

## 一、核心设计目标与分层职责

1. **`sf-do`（跨热更无缝续跑）**
   - 保持原 Monad 类型（`async_t` / `async_m`）与组合律不变，不引入 `{await, ...}` / `{done, ...}` 或独立解释器 VM。
   - 通过 `monad_frame:bind_frame/4` 与 `then_frame/4`，将 `<-` / 无绑定步骤的后续计算提升为模块导出函数 `Module:'$sf_<block>__<step>'/2` + 最小词法环境 `Env`，使挂起请求在经历模块热更新与 `code:purge/1` 后，仍能通过最新模块代码的 `$async_sf_manifest/0` 校验并继续执行。
2. **`sf-cc`（完成回调解耦与旧写法 `do` 错误收口）**
   - 将最外层完成回调（outer completion）从内部 CPS 闭包链中剥离为独立的 ` outer_node`（`sf_cc` 或 `ordinary_outer`），中间节点与叶子动作通过单向 `parent_ref` 指向直接父节点。
   - **旧写法 `do` 的热更回错保障（需求 1）**：当业务流程包含普通 `do`（匿名 continuation）但在热更后被 `code:purge/1` 清除时，只要外层最终回调仍可调用（如 `run_with_sf_cc`、`async_gen_server:default_callback(From)` 或未 purge 模块的回调），runtime 必须精确识别该中间匿名 continuation 已失效，清理当前请求链资源，并将 `{error, {sf_runtime, unavailable_continuation}}` 沿 `parent_ref` 送达外层完成回调，使调用方收到明确的错误返回而非进程崩溃或永久超时。
3. **`legacy_outcome`（连最终回调也失效时的兜底保护，需求 2）**
   - 当业务代码使用旧写法 `run/4`、`exec/4`、`run_with_cc/4` 传入本地匿名最终回调（或存在无父节点的遗留回调），且该最终回调自身也在模块热更后被 `code:purge/1` 清除时，由于调用方上下文（如 `From`）被困在已销毁的旧闭包内无法提取，runtime 必须在完成 `CleanState` 清理后，将本应送达最终回调的 `Outcome` 包装为 `{legacy_outcome, Outcome, CleanState}` 返回给上层（`handle_info` / `route_info`），保证承载进程（如 `gen_server`）不因 `badfun` 崩溃且不丢失状态清理。
4. **`sf-migrate`（声明式跨版本帧迁移）**
   - 仅在 `FrameOwner` 模块定义 `sf_do_code_change/2` 与 `sf_migrate/3`，编译期直接生成桥接函数与静态迁移计划；在 reply 到达时按实际存储的 `StoredVsn` 与当前模块 `CurrentVsn` 按需（lazy）解析，不在 `StateHolder:code_change/3` 中全表扫描或提前改写 frames。

---

## 二、Continuation Runtime 与多模块混用边界

1. **主路径保持轻量生命周期操作**
   - 正常 dispatch 是生命周期代码，不是 store 审计代码。主路径保持 `lookup -> take -> cleanup -> invoke/forward`；不得在每次 message、reply 或 failure 中递归检查 ancestors、descendants、cycle、depth 或对整个 `Callbacks` 做全表扫描。
   - `cleanup_accumulator` 必须先做 `O(1)` 的 key 存在性检查（`async_util:find(AccRef, Callbacks)`）；仅当 `AccRef` 确实存在于 store 中时，才检查是否仍有其他活跃 action 引用该 `AccRef`。
2. **只在最早责任边界检查必要不变量**
   - 注册节点或动作时一次性检查 key shape、node/action kind、duplicate key 和 direct `parent_ref` 合法性，并维护反向 ownership 索引（`?CHILDREN(ParentRef)`）。
3. **多模块 `do` 与 `sf-do` 混用的父链完整性（关键不变量）**
   - 在多模块调用链（如 `Mod0(sf_do) -> ModA(do) -> ModB(sf_do)` 或 `ModA(sf_do) -> ModB(do)`）及并行分支（`sf_do` 内调用 `par/2`、`map_promises/2,3`、`catch_error/3`）中，`SfContext` 中的未物化帧 `current = {frame, Frame}` **必须在改变 continuation `K` 或分叉执行之前先物化为唯一的 `ContinuationRef` 节点**：
     - 普通 `>>=`（`do`）、`par/2`、`par_acc_1/2`、`catch_error/3` 在构造新的 `K` 或启动多个子分支前，若当前 `SfContext` 持有未物化的 `{frame, Frame}`（或未物化的外层 continuation），必须先将其注册为 `Ref_Frame` 节点，并令新构造的 `K` 尾部以及传给子表达式的 `SfContext.parent_ref` 指向同一个 `Ref_Frame`。
     - **严禁**：
       1. 让 `cont_t:'>>='` 先把未物化的 `FrameRunner` 捕获进 `K_ModA` 闭包，随后在内层跳过 `Frame` 直接将 `parent_ref` 指向 `Frame` 的父节点（导致父链丢失 `Frame`）；
       2. 在 `K_ModA` 已经捕获旧 `FrameRunner` 之后才对 `Frame` 物化出新的 `Ref_Frame`（导致正常完成路径绕过 `Ref_Frame` 造成节点内存泄漏）；
       3. 在 `par/2` 或 `map_promises` 的每个子分支内对同一个未物化 `FrameContext` 重复调用 `materialize_parent`（导致同一个外层 `Frame` 被复制成多个孤立的父节点）。
4. **中间节点与最终节点在 `failure` 模式下的严格区分**
   - `dispatch_failure`（`Mode = failure`，用于传递 `{error, {sf_runtime, ...}}` 和 `{error, {sf_aborted, ...}}`）是结构性/管理性失败通道，不是普通业务返回值通道。
   - 所有中间节点——**无论是 `#sf_callback`（`sf_do` 中间帧）还是 `ordinary_continuation`（物化的普通 `do` 中间步骤）**——在 `Mode = failure` 时都必须直接调用 `forward_failure(ParentRef, Outcome, MR)` 向上透传，**不得**在 `Mode = failure` 时调用中间 `ordinary_continuation` 的 `Callback(Outcome)`（否则会被中间层的 `lift_reply` / `catch_error` 误吞，或把 `failure` 模式降格为对更外层 `sf_do` 帧的 `success` 调用）。
   - 只有最外层的 `outer_node`（`sf_cc` 与 `ordinary_outer`）才在 `Mode = failure` 时执行最终完成回调。
5. **Entry 缺失分类**
   - External `ActionMRef` 缺失属于迟到、重复或取消后的 `missing_callback`；
   - Internal `ContinuationRef` 缺失违反已认领消息的 continuation lifecycle，必须立即抛出 `{invalid_continuation, {missing_node, Ref}}`；
   - 只有不属于 async 协议/地址范围的消息才是 `not_async`（兼容层映射为 `unhandled`）。

---

## 三、可用性失效（`badfun`）、Error 与 Exception 语义

1. **在真实的用户函数边界（而非 CPS 库包装层）识别 `badfun`**
   - 在 `async_t` / `async_m` 中，`#callback.cc`、`ordinary_continuation` 和 `ordinary_outer` 顶层保存的 `Fun` 往往是 `reply_t`、`cont_t`、`async_t` 或 `async_m:callback_to_cc` 生成的库内包装函数，而真正随业务模块热更被 `code:purge/1` 清除的用户匿名函数位于包装层内部：
     - 普通 `do` 步骤的用户函数位于 `async_t:'>>='/3` 的 `KATB`（即 `run_k(A, KATB)` 调用点）；
     - `run/4`、`exec/4`、`wait_t/3` 的用户最终回调位于 `async_m:callback_to_cc/1` 和 `async_t:callback_to_cc/2` 内部的 `Callback` 调用点（其中 2 参数 `Callback(A, State)` 位于返回的 `async_r_t` 执行体内）；
     - 直接传入 `run_with_cc/4`、`run_cc/2`、`run_sf_cc/3`、`run_with_sf_cc/5` 的回调位于节点自身的 `Callback` 调用点。
   - **规则**：必须在上述**真实调用用户 `TargetFun` 的唯一边界**检查可用性并捕获 `error:{badfun, BadFun} when BadFun =:= TargetFun`（以及 external fun 的模块/导出缺失）：
     - 若 `BadFun =:= TargetFun`，判定为 `unavailable_continuation`；
     - 若 `TargetFun` 已经成功进入执行，其函数体内部调用其他函数触发的 `badfun`、`undef` 或业务异常（`BadFun =/= TargetFun`），必须原样抛出传播，不得误判为 continuation 不可用。
2. **中间 continuation 不可用 vs. 最终 outer callback 不可用**
   - **中间 continuation 不可用**（`#callback.cc`、`#sf_callback`、`ordinary_continuation`，或存在 `parent_ref =/= none` 的节点）：
     转化为 `{error, {sf_runtime, unavailable_continuation}}`，并沿 `parent_ref` 执行 `dispatch_failure`。
   - **最终 outer callback 不可用**（`ordinary_outer` / `callback_to_cc` 包装的最终回调，或 `parent_ref = none` 的顶层回调发生 `unavailable`）：
     此时当前请求链的 `ActionMRef`、`OuterRef`、timer、monitor、accumulator 均已完成清理得到 `CleanState`。由于没有更外层的 `parent_ref`，runtime 不得丢弃状态或崩溃，而必须保留到达最终回调时的 `Outcome`（若前序步骤成功则为正常结果，若前序步骤已失败则为 `{error, {sf_runtime, ...}}`），向上层返回 `{legacy_outcome, Outcome, CleanState}`。
3. **校验错误与业务 Outcome 分离**
   - 不得仅为了让上层再转换成异常而返回 `{error, Reason}`；校验错误和协议错误应在检测边界直接抛出。
   - timeout、DOWN、manual abort 和业务 `{error, Reason}` 仍是 continuation input，不能仅凭 tuple shape 将其分类为 runtime exception。
4. **Take-before-invoke 与 Exactly-once**
   - Node 一旦 take 消费，后续 callback crash 或非法返回都不得恢复或回滚。
   - Batch/active-failure（`fail_sf_callbacks`）路径不得把 callback 协议错误改写为 status data，应与正常执行路径一样直接异常退出。

---

## 四、Message 与 Info Routing 契约

1. `handle_message` 只消费已成功路由并恢复 continuation 后产生的 progress `{message, Payload}`，接口与 message propagation 语义保持不变。
2. 精确路由接口 `route_info` 必须区分：
   - `{handled, Outcome, State}`：正常完成或通过存活的 outer callback 完成错误收口；
   - `{legacy_outcome, Outcome, CleanState}`：到达最终完成边界时，因旧写法最终回调本身不可用（被 purge）而无法执行最终回调，但已完成资源清理；
   - `{not_async, State}`：非异步协议消息；
   - `{missing_callback, Metadata, State}`：迟到、重复或命中非 action 节点的引用。
3. `async_m:handle_info/3` 对 `{handled, _Outcome, NewState}` 返回 `NewState`，对 `{legacy_outcome, Outcome, CleanState}` 原样返回 `{legacy_outcome, Outcome, CleanState}`，对 `{not_async, _}` 和 `{missing_callback, _, _}` 返回 `unhandled`。
4. callback store 中合法的 continuation node、accumulator 或 reverse-index entry 不是 action callback，外部消息命中时返回 `missing_callback`；action kind 无法解码或 value shape 为 `other` 才是 registry/invariant 错误，应直接抛出 `{invalid_callback_entry, Key}`。
5. Batch snapshot 中被先前 sibling cleanup 删除的 action 使用 `already_removed`，不得复用面向外部消息的 `unhandled`。

---

## 五、Callback 契约

1. Ordinary continuation callback `/1` 和 `async_r_t` 模式的 `sf_cc` callback `/2` 必须返回 `async_r_t:async_r_t/3`（或经 `callback_to_cc` 归一化为 `async_r_t`）。
2. Direct-state `sf_cc` callback `/2` 的返回值被忽略，最终状态保持 `CleanState`；Direct-state `sf_cc` callback `/3` 必须返回与输入 `CleanState` 具有相同 tuple tag 和 arity 的 `NewState`。
3. 初始化只检查 fun identity、arity 和 mode；唯一 invocation boundary 检查实际返回值，非法时立即抛出。不得使用伪造输入提前执行用户 callback 进行校验。
4. Public API 与 constructor 的 `-spec` 必须明确展示 callback 返回契约。

---

## 六、复杂度控制与 Review 拒绝信号

没有新的明确需求时，以下实现应被拒绝或简化：

- 在 dispatch、reply、message、take 或 fail 中执行递归 preflight 或无前置存在性检查的全 store 扫描（如未检查 `AccRef` 是否存在就遍历整个 `Callbacks`）；
- 为普通路径和 `fail_sf_callbacks` 维护两套重复的节点执行逻辑（如同时维护 `execute_node` 与 `execute_node_state`）；
- 在代码中保留未构造的死分支（如 `SfContext` 中未使用的 `{ordinary, Callback}` 状态）或完全等价的包装函数（如与 `async_r_t:run/5` 完全重复的 `run_scope/5`）；
- 同一不变量在注册时检查后，又在每次执行时重复检查；
- `{error, Reason}` 只为稍后抛出而穿过多个 private layer；
- 将 `StateHolder` 热升级全表扫描混入 runtime 生命周期；
- node 消费后的 rollback 或 retry。

---

## 七、验证门槛与必测场景

遵循本原则的实现必须通过 compile、EUnit、完整 Common Test、xref、Dialyzer 和 `git diff --check`。除基础契约外，测试套件必须显式覆盖以下核心场景：

1. **旧写法 `do` 热更回错（需求 1）**：业务模块使用普通 `do([async_m || X <- promise(Ref), ...])`，配合存活的外层回调（`run_with_sf_cc` 或跨模块未 purge 回调），在该业务模块经历两次热更并 `code:purge/1` 后收到 reply，验证不会因 `cont_t`/`reply_t` 包装漏捕 `badfun`，且外层回调精确收到 `{error, {sf_runtime, unavailable_continuation}}`。
2. **旧写法 `run/4` 最终匿名回调热更降级（需求 2）**：业务模块使用 `async_m:run(Monad, fun(Reply, State) -> ... end, Offset, State)`，在业务模块经历两次热更并 `code:purge/1` 后收到 reply（覆盖“中间步骤也失效”与“中间步骤为 `sf_do` 成功但最终匿名回调失效”两种子场景），验证进程不崩溃、资源清理干净，且 `handle_info`/`route_info` 返回 `{legacy_outcome, Outcome, CleanState}`。
3. **多模块 `do` 与 `sf-do` 混用热更矩阵**：
   - `ModA(sf_do) -> ModB(do)`：仅 `ModA` 热更 purge（验证无缝续跑）、仅 `ModB` 热更 purge（验证沿 `Ref_FrameA` 正确回错且不漏节点）；
   - `ModA(do) -> ModB(sf_do)`：仅 `ModB` 热更 purge（验证无缝续跑并回调 `ModA`）、仅 `ModA` 热更 purge（验证 `ModB` 完成后在 `Ref_ModA` 处触发 `unavailable_continuation` 并收口）；
   - `Mod0(sf_do) -> ModA(do) -> ModB(sf_do)`：验证三层夹心下父链 `MRefB -> Ref_ModA -> Ref_Frame0 -> OuterRef` 完整，且 `ModB` 发生 `sf_runtime`/`sf_aborted` 失败时能以 `failure` 模式穿透 `Ref_ModA` 和 `Ref_Frame0` 直达 `OuterRef`；
   - `sf_do` 步骤内嵌套 `par([ModB:sf_flow(Ref1), ModB:sf_flow(Ref2)])`：验证共享同一个物化父节点 `Ref_FrameA`，且单次失败/取消满足 exactly-once。
