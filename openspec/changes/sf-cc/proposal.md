# sf-cc

## Why

async callback store 可以持有两种 continuation：

- ordinary bind 生成的 anonymous fun；
- `sf_do` 生成的 stable frame data。

它们的 resume 形式不同，但一个顶层 promise 都只有一个最终 Outcome 和一次
completion。frame 无法 migration、用户主动终止或 continuation data 损坏时，runtime 不能依赖
frame chain 中嵌入完整 callback 来完成 Promise。

`sf-cc` 把最终 completion 从 structured continuation 中抽离为独立、一次性 CcRef。每个 pending
action callback 另存当前 CcRef；正常完成和 stable-frame structural failure 通过同一条路径消费它。
ordinary anonymous continuation 仍按普通 bind 语义执行，其失效不是 sf-cc 的恢复责任。

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
  升级后内部委托统一 CcRef lifecycle。
- `run_sf_cc`/`run_with_sf_cc`/`exec_sf_cc` 是 Astranaut macros。macro MUST 检查第二个
  source argument 是 external fun，并拒绝 local fun、anonymous fun、runtime fun variable 和错误 arity。
- macro MUST 将原 external-fun expression 传给普通 runtime function，不拆成 MFA tuple。
- runtime MUST 持久化 `sf_cc` descriptor：external fun、预存 Context 和 runner mode。
- `#callback{}` MUST 包含 `sf_cc_mref`。ordinary fun 和 frame data 继续原样保存在
  `#callback.cc`。
- callbacks store MUST 以 CcRef 保存一次性 final entry。升级后由任意 public runner 创建的每个新
  action callback MUST 继承当前 `sf_cc_mref`。
- final completion MUST 先 take CcRef、清理 ActionMRef/timer/monitor/accumulator 等失效资源，
  然后调用 final callback。`/3` MUST 看到 CleanState。
- CcRef MUST exactly-once。CC crash、迟到 reply、重复 fail 不得恢复或再次调用。
- 新增 inspect 和 immediate-fail API，核心 selector 为用户 matcher fun。matcher 可按
  Module、module `-vsn`、BlockPath、StepName、protocol、AsyncId、continuation kind 和 request
  MRef 筛选，但不可见 Env、State、timer 或 CC Context。
- frame continuation 最终终点 MUST 只持有 CcRef，不再嵌入另一份完整 final CC
  descriptor。structural failure MUST 优先使用 `#callback.sf_cc_mref`。

## Compatibility

- callback store 只接受当前 `#callback{cc, acc_ref, sf_cc_mref}` encoding；本 change 不提供旧三元
  callback tuple 的 runtime compatibility。
- 所有 public runners 都创建 CcRef：`wait/1`、`wait_t/2` 使用 library-owned completion；
  ordinary callback APIs 使用 ordinary completion descriptor；sf-prefixed APIs 使用用户 external-fun descriptor。
- ordinary API 不受新 macro/external-fun source 约束，且其 normal callback/result contract 不变；
  anonymous final callback 本身不获得 external fun 的跨 purge 稳定保证。
- CcRef 只增加 final ownership，不把 ordinary anonymous continuation 转换为 frame。ordinary continuation
  失效时错误原样传播，不转换为 sf runtime Outcome。

## Non-Goals

- 不将 CcRef 定义为多个业务 promises 的 operation-group identity。
- 不从 ordinary anonymous fun 推断 Module/Vsn/BlockPath/StepName。
- 不定义 Env/Input migration DSL、version transition catalog 或 appup/appdown ordering；这些属于
  `sf-migrate`。
- 不迁移用户的 external final callback 或 Context。
- 不为 CC 外部副作用提供 rollback 或自动 retry。

## Capabilities

### New Capabilities

- `sf-cc`: stable final completion registration、所有 runners 的 CcRef lifecycle、frame terminal routing、
  exactly-once invocation 和 active inspection/failure。

## Dependencies

- 使用 Astranaut macro provider 和项目公共 include。
- 依赖 `async_t`/`async_r_t` callbacks store、State offset lens、timer/monitor/accumulator lifecycle。
- `sf-do` MUST 改用 `sf-cc` 作为 final completion owner；`sf-migrate` 后续使用同一
  CcRef 进入 terminal failure。

## Impact

- `async_m`：新 macro facade/runtime functions、现有 runner/wait facade 和 inspect/fail facade。
- `async_t`：`#callback.sf_cc_mref`、scope propagation、CcRef entry routing、completion primitive。
- `async_r_t`：传播与 accumulator ref 独立的 optional sf-cc scope。
- `async_sf_runtime`：frame terminal 改为 CcRef，structural failure 改走统一 completion primitive。
- `async_util`：current callback decoding、callback existence 与 callbacks dictionary 分类。
- tests：macro diagnostics、ordinary/frame composition、immediate/suspended completion、active fail、late reply
  和 CC failure boundaries。
