# Changelog

<!-- NEXT_CHANGE: 在此标记下方、分隔线上方追加后续变更，最新内容置顶。 -->

## 2026-08-28 — ordinary outer availability representation（未提交）

- ordinary runner的最外层completion改为直接保存真实`Cc`的`ordinary_outer` node，不再把`Cc`藏在通用
  `sf_cc` descriptor context和永久可用wrapper后。
- old-code purge使该current stored fun精确产生`error:{badfun,Cc}`时，沿既有availability合同转换为
  `{error,{sf_runtime,unavailable_continuation}}`；callback内部/downstream异常仍原样传播。

## 2026-08-28 — linked continuation contract（未实现）

- named frame identity收敛为`{Protocol, Block, StepName}`；BlockName在module内全局唯一，lexical parent、
  函数提取、调用位置和Promise传递不参与identity。持久化frame字段由`block_path`改为`block`。
- 删除persisted `AsyncId`及SHA/hex命名；named generated function固定为
  `'$sf_<block>__<step>'/2`，拼接碰撞、用户函数冲突和名称过长均在编译期拒绝。
- continuation edge统一命名为`parent_ref`，表示current `cc`/frame的direct dynamic continuation parent，
  而不是下一source qualifier、root address或resource owner。
- ordinary action callback改为`#callback{cc,acc_ref,parent_ref,...}`。classic `do` bind只透传显式
  `SfContext`；真正挂起的Promise writer保存current anonymous `cc`与direct parent Ref，不拆包Promise。
- ordinary Outcome仍调用`cc`；只有精确归属current `cc`本身不可调用的availability failure才跳过它并
  dispatch `parent_ref`，从而让classic `do`直接由sf-cc runner运行时仍可逐层fallback。
- stable continuation 从 callback 内递归 `sf_next(..., Downstream)` 值改为 callbacks store 中的单向
  `ContinuationRef` 链。完整 stable entry 是
  `#sf_callback{frame,acc_ref,parent_ref,timer_ref}`；其 continuation 投影只含当前
  `sf_frame` 和直接 `parent_ref`，不得复制 outer frame、完整 downstream chain 或最终/root reference。
- `ContinuationRef` 不区分 outer 与 final 引用类型；同一种 reference 可以指向 frame node、ordinary
  callback node 或最外层 completion descriptor，节点内容和 `parent_ref = none` 决定行为。
- `bind_frame` 在 nested composition 时自动物化直接父层节点，并让 inner suspension 只保存
  `InnerFrame + OuterRef`。`run_cc`/sf-cc runner 只把外层 callback 作为同一链上的节点，不创建
  `sf_cc_ref` terminal map。
- normal Outcome 每次恢复一层；frame invocation、resolution、migration 或可归属的 `badfun` 失败时，
  普通 success frame 不执行，failure 被转发给直接 `parent_ref`。可处理 failure 的显式 handler 可以消费它；
  无 handler 时逐层到达最外层 callback。
- 删除 stable callback 对 `sf_cc_mref` root/final address 的重复保存。主动取消和 parallel cleanup 所需
  的反向关系属于 sf-cc 的独立索引/生命周期设计，不得重新塞入每个 continuation node。
- 本条 supersede 2026-08-27/26 中关于递归 `sf_next`、`sf_cc_ref` terminal、root `sf_cc_mref` 和
  structural failure 直接 terminal-complete 的合同；transform、frame identity、Env、generated Monad
  remainder 与逐 frame lazy resolution 保持不变。

## 2026-08-27 — transform diagnostic ownership 收敛

- compiler 统一负责 file、line、column、source excerpt 和位置标记；`async_sf_transform` reason/message
  只携带规则、允许形式及必要动态身份，不复制 AST/source 或完整内部 state。
- 领域 `format_error/1` 收敛为严格精确 clauses，不再自带 catch-all；正常 compiler 未匹配兜底归
  `astranaut_lib`，测试直接调用领域 formatter 以暴露 reason 拼写或 clause 遗漏。
- duplicate StepName/Block 不再级联 generated-function conflict；trailing marker 不再同时派生
  invalid-last qualifier。不同 source occurrence 的独立 diagnostics 仍按 source order 累计。
- ordering mismatch 只报告 occurrence identity 或有界 rewrite 摘要；missing target capture 明确归属
  sf-migrate/runtime resolver；generated-name collision只报告一次。
- empty comprehension 仅作为 earlier-transform malformed AST 的防御性 diagnostic；function 外
  application 不是合法 compiler form，删除不可达的领域 reason。

## 2026-08-27 — 已实现合同修正

- `async_t` 以显式 `(K, SfContext)` 传播 stable execution context，不使用进程字典。
- ordinary bind 继续保存 anonymous continuation；只有 `bind_frame` 创建 `sf_next`。
- Promise 只在实际 continuation 等于 stable runner 时保存 structured chain。frame 位于 ordinary bind
  下方时，以独立 CcRef descriptor 承接 anonymous downstream，不引入 legacy frame node。
- callback reader 只接受当前四元 record；删除旧三元 tuple、`legacy_outcome` 和 unavailable-code
  compatibility classifier。
- ordinary action continuation 的 `badfun`、`undef` 和内部异常原样传播，不由 sf-do/sf-cc 转换为
  structural failure。stable-frame protocol/manifest/migration failure 仍通过 CcRef terminal completion。
- ordinary runners/wait 继续使用统一 CcRef lifecycle，但 ordinary completion adapter 编码为普通 `sf_cc`
  descriptor，不再存在 legacy mode/descriptor。
- 2026-08-26 “未实现（合同已更新）”及其实现核实保留为当日历史快照；本条 supersede 其中与当前实现冲突的
  compatibility 和状态描述。

## 2026-08-26 — 未实现（合同已更新）

- `sf-do` 不再拥有或定义完整 `sf_cc` descriptor、public final runner、callback arity/return
  contract 或 final invoker；这些能力统一由 `sf-cc` change 提供。
- stable continuation chain 的 terminal 从内嵌完整 `sf_cc` 改为
  `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`。完整 external callback、mode 和预存
  Context 只保存在 CcRef entry 中。
- stable action callback 继续在 `#callback.cc` 保存原 frame continuation，同时通过新增尾字段
  `#callback.sf_cc_mref` 保存最终 CcRef；旧三元 callback tuple 按
  `sf_cc_mref = undefined` 兼容读取。
- compatibility boundary 改为 callback 创建时间，而不是 runner 名称：升级后由 `wait`、ordinary
  anonymous callback runner 或 sf-cc runner 新启动的 Promise 都创建 CcRef；只有升级前已经 pending
  的旧三元 tuple 保持 unprotected。
- `wait/1`、`wait_t/2` 保持原 API 和正常返回，由库内固定 external completion 无感迁移到 CcRef
  engine。旧 anonymous callback API 继续支持原 source/return contract，并通过 legacy adapter 使用
  统一 lifecycle；只有显式 external callback 获得 final callback 本身的跨升级保证。
- old unprotected tuple 的 anonymous continuation 若因旧代码 purge 不可调用，runtime MUST 清理该
  request 且不得 crash/retry。因为没有 CcRef，不调用 final CC；`async_m:handle_info` 返回
  `{legacy_outcome, Outcome, CleanState}`，其中 Outcome 是原本要交给旧 continuation 的 action Outcome，
  由 receive-loop 调用方决定如何处理。
- 同一 Promise 的 nested/parallel internal callbacks 可以继承同一 CcRef，但保持独立 RequestMRef；
  共享 CcRef 不是多 Promise operation group，也不产生 per-entry final callback。
- structural frame failure 不再递归遍历可能损坏的 continuation 寻找 CC；runtime 必须通过
  `#callback.sf_cc_mref` 进入 `sf-cc` 的统一 completion primitive，由它先消费并清理 CcRef
  所属 lifecycle，再调用 final callback。
- `sf_frame` 和 current manifest 增加 owner Module 与 module `-vsn` literal term（无可用显式
  Vsn 时为 `undefined`）。Vsn 不进入boundary identity，只供 inspection 和 migration endpoint selection。
- frame resume 增加逐节点 owner-module resolution 边界。外部 owner module 独立升级、持有进程未执行
  `code_change/3` 时，reply-time 仍须在调用 frame MFA 前进行 lazy resolve；不同模块的 nested frames
  分别处理。
- `sf-do` 只提供 source identity、stored Env、frame owner/version 和 current target manifest。
  historical block、Env/Input migrator、migration catalog、version-chain composition 与 strict downgrade
  policy 由 `sf-migrate` 定义，不在本 change 重复冻结。
- ordinary timeout、DOWN、message、Monad failure、source syntax、Block/StepName identity、capture
  analysis 和 nested Monad composition 保持既有合同。

### 实现核实

截至 2026-08-26，仓库实际情况如下：

**已实现的旧基线**

- `async_sf_transform` 已生成 `bind_frame`/`then_frame`、external generated frame MFA、minimal Env、
  Block/StepName identity 和 `'$async_sf_manifest'/0`。
- `async_sf_runtime` 已实现 `run_sf_frame/4`、内嵌完整 `sf_cc` terminal、current-manifest validation、
  frame resume 和 structural failure 后调用旧 terminal `sf_cc`。
- `async_t` 已实现 frame-aware bind/then 和在 `#callback.cc` 中保存 stable continuation data。

**尚未实现的新合同**

- 尚无 `#callback.sf_cc_mref`、CcRef entry、`sf_cc_ref` terminal 或旧/新 callback tuple 统一 decoder。
- 尚无 `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5` external-fun macro/runtime 和统一
  cleanup-before-invoke completion primitive。
- 现有 `wait/1`、`wait_t/2` 和 ordinary runner 尚未迁移到 CcRef engine；`handle_info/3` 也尚无
  `{legacy_outcome, Outcome, CleanState}` 分支或 unavailable legacy fun 的受控清理。
- frame/manifest 尚未保存 module Vsn，inspection matcher 也尚未暴露 Module/Vsn/frame summaries。
- frame resume 仍直接校验/调用 current manifest MFA，尚无按 owner module/version 的 lazy migration
  resolution、optional Input conversion 或无 historical block 的 upgrade path。
- 因此本节上方 2026-08-26 的条目是待实现目标，不表示当前 runtime 已满足新合同。

---

## 2026-08-24 — 已实现

- `sf_do` public source 收敛为 `sf_do(Comprehension)` 与
  `sf_do(BlockName, Comprehension)`；StepName 改为紧邻非最终 action 的 inline literal atom
  marker，不再使用额外 list/record 参数。
- standalone literal atom qualifier 只允许作为 StepName marker；duplicate、dangling、连续 marker、
  修饰 pure let/final action 或其他位置均为 source error。unnamed boundary 产生 warning，允许普通
  执行和 same-version resume，但 appup/appdown migration 必须失败。
- migration identity 只使用 Protocol、Block 和 StepName；删除 derived identity、Action
  canonicalization、AsyncId/hash、AsyncForm digest 及 AST/index fuzzy matching；generated function直接使用
  `'$sf_<block>__<step>'/2`。
- Pass 1 使用 Astranaut return channel 累积 errors/warnings；有 error 时直接
  `astranaut_return:to_compiler/1` 且不运行 Pass 2，只有 warning 时继续 rewrite。Pass 2 只消费冻结
  plan，不重复 source validation。
- scope-aware capture 明确区分 `case`/`receive`/`try` outer equality reference 与
  `fun`/named-fun fresh binding，并要求共享 lexical binding classifier。
