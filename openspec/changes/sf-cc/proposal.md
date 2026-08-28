# sf-cc

## Why

async callback store 可以持有两种 continuation：

- ordinary bind 生成的 anonymous fun；
- `sf_do` 生成的 stable frame data。

它们的 resume 形式不同，但一个顶层 promise 都只有一个最终 Outcome 和一次
completion。frame 无法 migration、用户主动终止或 continuation data 损坏时，runtime 不能依赖
frame chain 中嵌入完整 callback 来完成 Promise。

`sf-cc` 把 continuation 表示为 callbacks store 中的一次性单向 node chain。stable callback的
continuation portion只保存current frame与direct `parent_ref`，record另外保存accumulator和timer等
action lifecycle metadata；outer frame和最终callback各自是独立node。outer/final
reference使用同一种`ContinuationRef`，不在每个pending callback中复制root address。

## What Changes

- 新增 stable completion runners：

  ```erlang
  run_sf_cc(Promise, fun Module:Function/2, Context) -> async_r_t().
  run_with_sf_cc(Promise, fun Module:Function/2, Context, Offset, State) -> StateResult.
  run_with_sf_cc(Promise, fun Module:Function/3, Context, Offset, State) -> StateResult.
  exec_sf_cc(Promise, fun Module:Function/2 | fun Module:Function/3,
             Context, Offset, State) -> term().
  ```

- `run_sf_cc/3` callback `/2` MUST 返回 `async_r_t`；State 从 effect 内获取。
- `run_with_sf_cc/5`/`exec_sf_cc/5` callback `/2` 的返回值 MUST 忽略；callback `/3`
  MUST 接收 CleanState 并返回 NewState。
- 新 runner 名称使用 `sf_` 边界。旧 `run_cc/2`、`run_with_cc/4`、`exec_cc/4`、
  `run/4` 和 `callback_to_cc/1` 保持 ordinary callback 参数、normalization 与正常返回合同，但在
  升级后内部委托统一 ContinuationRef lifecycle。
- `run_sf_cc`/`run_with_sf_cc`/`exec_sf_cc` 是 Astranaut macros。macro MUST 检查第二个
  source argument 是 external fun，并拒绝 local fun、anonymous fun、runtime fun variable 和错误 arity。
- macro MUST 将原 external-fun expression 传给普通 runtime function，不拆成 MFA tuple。
- runtime MUST 持久化 `sf_cc` descriptor：external fun、预存 Context 和 runner mode。
- ordinary suspension MUST 使用 `#callback{cc, acc_ref, parent_ref, ...}`，其中 `cc` 是当前 anonymous
  continuation，`parent_ref` 是该 `cc` 的 direct continuation parent。stable path MUST 使用
  `#sf_callback{frame, acc_ref, parent_ref, timer_ref}`；两种 action entry 都显式保存 direct parent edge，但 ordinary
  callback 不因此获得 frame identity 或 migration guarantee。
- callbacks store MUST 以同一种 `ContinuationRef` 保存 frame、ordinary continuation 或 outer completion
  node。任何 node 都不得重复保存 root/final Ref。
- node dispatch MUST 先 take 当前 node、清理它直接拥有的 ActionMRef/timer/monitor/accumulator 等资源，
  然后执行或转发该 node；只有 outer sf_cc node调用 final callback，`/3` MUST 看到 CleanState。
- 每个 ContinuationRef MUST exactly-once。node crash、迟到 reply、重复 fail 不得恢复或再次调用。
- 新增 inspect 和 immediate-fail API，核心 selector 为用户 matcher fun。matcher 可按
  Module、module `-vsn`、Protocol、Block、StepName、continuation kind 和 request
  MRef 筛选，但不可见 Env、State、timer 或 CC Context。
- structural failure MUST 跳过当前 ordinary success frame并向 direct `parent_ref` 转发；不得跳到 root。
  `parent_ref = none` 的 outer callback node 才执行最终 completion。

## Compatibility

- callback store 明确区分 stable `#sf_callback{}` 与 ordinary `#callback{}`；ordinary callback
  不被猜测为 stable frame。当前 change 不保留缺少 `parent_ref` 的旧 callback tuple compatibility。
- 所有 public runners 都注册 outer continuation node：`wait/1`、`wait_t/2` 使用 library-owned completion；
  ordinary callback APIs 使用 ordinary completion descriptor；sf-prefixed APIs 使用用户 external-fun descriptor。
- ordinary API 不受新 macro/external-fun source 约束，且其 normal callback/result contract 不变；
  anonymous final callback 本身不获得 external fun 的跨 purge 稳定保证。
- ContinuationRef 不把 ordinary anonymous continuation 转换为 frame。ordinary outcome 始终先调用
  `#callback.cc`；仅当 `badfun`/`undef` 可精确归属到当前 `cc` 本身不可调用时，runtime 才跳过该 `cc`
  并向 `parent_ref` 转发一层。callback-internal exception 保持原合同。

## Non-Goals

- 不将 ContinuationRef 定义为多个业务 promises 的 operation-group identity。
- 不从 ordinary anonymous fun 推断 Module/Vsn/Protocol/Block/StepName。
- 不定义 Env/Input migration DSL、version transition catalog 或 appup/appdown ordering；这些属于
  `sf-migrate`。
- 不迁移用户的 external final callback 或 Context。
- 不为 CC 外部副作用提供 rollback 或自动 retry。

## Capabilities

### New Capabilities

- `sf-cc`: unified continuation-node registration、逐节点 routing、所有 runners 的 outer-node lifecycle、
  exactly-once invocation 和 active inspection/failure。

## Dependencies

- 使用 Astranaut macro provider 和项目公共 include。
- 依赖 `async_t`/`async_r_t` callbacks store、State offset lens、timer/monitor/accumulator lifecycle。
- `sf-do` MUST 改用 `sf-cc` 作为 ContinuationRef store/dispatcher；`sf-migrate` 使用同一逐节点
  dispatcher 传播 migration failure。

## Impact

- `async_m`：新 macro facade/runtime functions、现有 runner/wait facade 和 inspect/fail facade。
- `async_t`：`#callback.cc/parent_ref`、`#sf_callback.frame/parent_ref`、scope propagation、ContinuationRef node routing。
- `async_r_t`：传播与 accumulator ref 独立的 current continuation context。
- `async_sf_runtime`：逐节点 frame execution 与 failure forwarding。
- `async_util`：current callback decoding、callback existence 与 callbacks dictionary 分类。
- tests：macro diagnostics、ordinary/frame composition、immediate/suspended completion、active fail、late reply
  和 CC failure boundaries。
