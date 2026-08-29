# sf-cc Design

## Context

ordinary `async_t` action callback entry 是：

```erlang
-record(callback, {
    cc,
    acc_ref,
    parent_ref
}).
```

stable path 使用独立 entry：

```erlang
#sf_callback{
    frame = CurrentFrame,
    acc_ref = AccRef,
    parent_ref = ParentRef,
    timer_ref = TimerRef
}.
```

`#callback.cc` 保存 ordinary anonymous continuation fun，`#callback.parent_ref` 保存该 fun 的 direct
continuation parent。stable callback 的continuation portion保存current frame和同义的direct `parent_ref`，
同时保留`acc_ref`和timer等action lifecycle metadata；outer frame与final callback
是callbacks store中的其他nodes。

resume 由统一 ContinuationRef dispatcher 负责；sf-cc 拥有 node registration、take、direct cleanup、
failure forwarding 和 outer callback invocation。

## Goals

- 每个 public runner invocation 注册一个 outer continuation node。
- ordinary runner 与 stable frame runner 使用相同 ContinuationRef store、dispatch 和主动失败路径。
- ordinary `#callback{}` 保存 current anonymous continuation + direct `parent_ref`；完整 stable
  `#sf_callback{}` 的 continuation 投影是 current frame + direct `parent_ref`。
- sf-prefixed API 的用户 final callback 必须是源码中显式出现的 external fun；ordinary API 保持原合同。
- outer callback 在 current ActionMRef/node 清理后调用；`/3` 看到 CleanState。
- 正常完成、结构失败、迁移失败和人工失败均逐 node exactly-once。
- structured frame 与 ordinary anonymous downstream 相遇时，把 downstream 物化为普通 node。

## Non-Goals

- ContinuationRef 不是多个独立业务 promises 的 operation-group id。
- 不从 anonymous fun 恢复 source stack、Protocol、Block、StepName 或 source Vsn。
- 不定义 migration mapping、Env/Input converter 或 appup/appdown transition ordering。
- 不迁移 final external fun 或 Context。
- 不保证 final callback 外部副作用可回滚。

## Terminology

- `ActionMRef`：外部 request/reply、timeout、DOWN 或 message 使用的 correlation key。
- `AccRef`：现有 `async_r_t` local accumulator reference；并行/聚合可以局部重绑定。
- `ContinuationRef`：callbacks store 中任一 continuation node 的一次性 reference；不区分 outer/final Ref 类型。
- `sf_cc`：outer node 可持有的 stable callback descriptor。
- `CleanState`：已 take 当前 node并清理其直接 lifecycle resources 后的 State。
- stable callback：明确编码的
  `#sf_callback{frame,acc_ref,parent_ref,timer_ref}`；`frame`本身是完整
  self-describing `sf_frame` data。
- ordinary callback：`#callback{cc,acc_ref,parent_ref,...}` action entry，不自动获得 frame migration。

`AccRef` 与 `ContinuationRef` 不得合并。内部 parallel/local accumulator 可重绑定 AccRef；当前 node
只持有 direct `parent_ref`，不复制顶层 address。

## Public API

### Stable async_r_t runner

```erlang
run_sf_cc(Promise, fun Module:Function/2, Context) -> async_r_t().
```

callback contract：

```erlang
Module:Function(Outcome, Context) -> async_r_t().
```

该 mode 不接受 callback `/3`。callback 需要 State 时使用 `async_r_t:get_state`/`put_state`。callback
返回的 `async_r_t` 是 outer completion effect；它在已消费 current node 的 scope 中执行。若该 effect
需要启动另一个受保护 promise，必须显式再次调用 `run_sf_cc`。

### Direct-State runner

```erlang
run_with_sf_cc(Promise, fun Module:Function/2, Context, Offset, State) -> StateResult.
run_with_sf_cc(Promise, fun Module:Function/3, Context, Offset, State) -> StateResult.
```

callback contracts：

```erlang
Module:Function(Outcome, Context) -> Ignored.
Module:Function(Outcome, Context, CleanState) -> NewState.
```

- `/2` return value is ignored；final State 保持 CleanState。
- `/3` return value MUST 通过现有 State record tag/arity compatibility check，然后成为 NewState。
- `/3` 返回 `async_r_t` 是 invalid return。

### Exec wrapper

```erlang
exec_sf_cc(Promise, ExternalFun, Context, Offset, State) -> term().
```

它只包装 `run_with_sf_cc/5` 的执行/result 投影，不定义第三套 callback result semantics。

### Existing APIs

以下 API 保持原参数、callback normalization 和正常返回 semantics：

```erlang
run_cc/2
run_with_cc/4
exec_cc/4
run/4
callback_to_cc/1
```

它们不受 external-fun macro 约束，但内部 MUST 通过 ordinary completion descriptor 委托统一
ContinuationRef lifecycle。`wait/1`、`wait_t/2` 同样 MUST 注册 outer node，并使用 library-owned
exported completion；其 public API 和正常结果不变。该兼容迁移只保证新挂起 action 可逐 node 清理和
exactly-once 完成，
不把用户 anonymous final callback 变成可跨 old-code purge 调用的 external fun。

## Macro Boundary

三个 `sf_cc` source APIs 使用独立 Astranaut macro provider，不属于 `async_sf_transform`。provider
由公共 include 启用。

第二个 source argument MUST 是 remote/external fun AST：

```erlang
{'fun', Pos, {function, ModuleExpr, FunctionExpr, {integer, _, Arity}}}
```

accepted forms：

- `run_sf_cc/3`：arity 2；
- `run_with_sf_cc/5`、`exec_sf_cc/5`：arity 2 或 3。

rejected forms：

- `fun Local/2`、`fun Local/3`；
- anonymous fun clauses；
- variable/expression containing a fun；
- unsupported arity。

macro 成功后把原 external-fun AST 原样放进普通 remote runtime call。它不得构造
`{Module, Function, Arity}`，runtime 也不得用 `fun_info` 重新执行 source-origin validation。
runtime 用 `is_function(Callback, 2 | 3)` 分派协议；实际 apply 前 MAY 用 `fun_info` 取得 external MFA，
仅用于 `ensure_loaded`/`function_exported` availability check。

## Persistent Encoding

### sf_cc descriptor

```erlang
-type sf_cc() :: #{
    kind := sf_cc,
    protocol := 1,
    mode := async_r_t | direct_state,
    callback := fun(),
    context := term()
}.
```

sf-prefixed descriptor 的 `callback` MUST 是 macro 原样传入的 external-fun term；ordinary descriptor
可以保存 library-generated adapter。`context` 对 sf-prefixed API 是用户预存 opaque term，对 ordinary
adapter 是原 callback。两者都不是 `sf_do` lexical Env，不参与 Env/Input migration，也不得出现在
selector summary。

### Continuation nodes

callbacks store 使用统一 reference key：

```erlang
-type continuation_ref() :: reference().

ContinuationRef =>
    #sf_callback{frame = Frame, acc_ref = AccRef,
                 parent_ref = ParentRef, timer_ref = TimerRef}
  | #{kind => ordinary_continuation, protocol => 1, callback => Fun,
      parent_ref => ParentRef | none}
  | #{kind => sf_cc, protocol => 1, mode => Mode, callback => Fun,
      context => Context, parent_ref => none}.
```

Ref 的类型不表达 outer/final；value shape 决定 executor，`parent_ref = none` 决定链结束。entry absence
是 consumed marker，不另存 tombstone。一个 node 只保存一个 direct parent edge，不得复制 root/final Ref；
stable action node仍保留完整 lifecycle fields。

### Stable and ordinary action callbacks

ordinary suspension MUST 写 `#callback{cc,acc_ref,parent_ref,...}`；stable suspension MUST 写完整
`#sf_callback{frame,acc_ref,parent_ref,timer_ref}`。ActionMRef 本身就是 current action node 的路由 key。ordinary `bind`
把当前 anonymous `K` 放进 `cc`，并把执行该 `K` 之后的 direct outer continuation address 放进
`parent_ref`；`bind_frame` 对 frame 执行相同规则，若 direct parent 尚不可寻址则先物化一个
ContinuationRef node。runtime 不从 ordinary fun 推断 stable frame metadata。

record/tag 定义 MUST 位于 shared internal header，所有 router、inspection 和 migration readers 使用同一
classifier。

## sf-cc Scope Propagation

initial execution 和 resumed continuation 创建下一 ActionMRef 时必须知道 direct parent node。该信息
不得藏进 AccRef。

`async_r_t` 使用彼此独立的 locals 传播 AccRef 与 current continuation context：

- `get_local_ref` 只返回 AccRef；
- continuation context 保存 current continuation data或 addressable ParentRef；
- `local_ref(NewAccRef, Action)` 只替换 AccRef；
- public runners 安装 outer callback node；
- reply executor 从 ordinary/stable callback 的 `acc_ref` 与 `parent_ref` 重建 parent scope；stable entry
  还从 `frame` 重建 current frame scope。

### Explicit async continuation context

`async_t` internal action MUST 显式接收 `(K, SfContext)`。`SfContext` 保存 current structured continuation
和对应 stable runner，不得通过进程字典或其他隐藏 mutable channel 传播。

- ordinary `>>=` 继续构造 anonymous `K`，并显式透传 `SfContext.parent_ref`；
- Promise 在实际 `K` 与 `SfContext.runner` 完全相同时写 `#sf_callback{}`，否则写保存实际 `K` 与同一
  direct parent edge 的 `#callback{}`；
- `bind_frame` 把尚未可寻址的 current parent 物化为 node，并把 Ref 保存为新 frame 的 `parent_ref`；
- ordinary anonymous downstream 被物化为 ordinary node，不嵌入 stable frame data。

## Frame Integration

frame continuation MUST 是单层 `CurrentFrame + ParentRef`。normal success invoke current frame并把返回 Monad
运行于 ParentRef；structural/manifest/MFA/migration/invocation availability failure MUST 跳过当前普通 success
frame并 dispatch ParentRef。dispatcher 不寻找 root，也不区分 FinalCcRef 与 OuterRef。

## Completion State Machine

每个 ContinuationRef 有两个可观察状态：

```text
pending  : callbacks store contains node entry
consumed : entry absent
```

统一 dispatch primitive：

```erlang
dispatch_continuation(Ref, Outcome, State) -> DispatchResult.
```

commit order：

1. atomic-with-respect-to-process `take` current node；
2. cancel/demonitor/remove current node直接拥有的 timer、monitor 和 accumulator resources；
3. obtain CleanState；
4. success 时执行 current continuation；failure 时普通 success frame直接 dispatch `parent_ref`；
5. current invocation 产生可归属 failure 时 dispatch `parent_ref`；
6. outer callback node 应用 mode-specific result semantics。

parallel cancellation 如需从 outer node 查找 active children，MUST 使用独立反向索引/ownership metadata；
不得通过在每个 node 复制 root Ref 实现。

Action callback成功路由后若ContinuationRef已缺失，直接抛出`invalid_continuation`。如果 take 后 user CC
crash、返回非法 shape 或产生外部副作用，Ref 保持 consumed，不 rollback、不 retry。callback/effect
内部异常原样传播。batch fail 不得把 callback协议错误、invalid return或invalid descriptor改写为status data。

reply/timeout/DOWN path 在执行 continuation 前已知 ActionMRef 和 decoded callback。ordinary business
Outcome 必须交给 `#callback.cc`，不得仅凭 Outcome shape 绕过它。frame protocol/shape/manifest/MFA failure
dispatch direct `parent_ref`；ordinary `cc` 或 frame 的 `badfun`/`undef` 只有可精确归属到 current
continuation 本身不可调用时才转换为 failure并 dispatch direct `parent_ref`。callback-internal exception
原样传播。message path MUST 保留 ActionMRef。

## Outcome Contract

normal completion 使用 promise 产生的原 Outcome。

runtime structural failure：

```erlang
{error, {sf_runtime, Reason}}
```

manual failure：

```erlang
{error, {sf_aborted, Reason}}
```

ordinary reply、timeout、DOWN 和业务 `{error, Reason}` 默认仍是 continuation Input；只有 ordinary `cc`
本身或 structured frame infrastructure 明确不能调用 current continuation 时，才把 runtime failure 向
direct `parent_ref` 转发。

## Active Inspection and Failure

### Summary shape

matcher 每个 action callback 调用一次，并接收：

```erlang
#{
    request_mref := term(),
    parent_ref := reference() | none,
    stable := boolean(),
    continuation_kind := ordinary_fun | sf_frame | unknown,
    frame := #{
        module := module(),
        vsn := term() | undefined,
        protocol := integer(),
        block := atom() | undefined,
        step_name := atom() | undefined
    } | undefined
}.
```

summary 默认只暴露 current frame。ordinary fun 使用 `frame = undefined`；runtime 不从 fun 推断 metadata。
frame `vsn` 沿用 producer 保存的 module `-vsn` literal term；未提供时为 `undefined`，不得假定 atom。
需要父链 inspection 时必须显式沿 Ref lookup，并受 cycle/depth/shape validation 限制。

summary MUST NOT 包含 continuation value、Env、State、timer、monitor、accumulator、sf_cc callback 或 Context。

### APIs

```erlang
inspect_sf_callbacks(Matcher, Offset, State) ->
    {ok, [Summary]}.

fail_sf_callbacks(Matcher, Reason, Offset, State) ->
    {ok, [FailResult], NewState}.
```

`Matcher :: fun((Summary) -> boolean())`。它是同步 ephemeral fun，不持久化。
非 fun Matcher 在 facade boundary 返回 `invalid_matcher` error tuple；一旦开始调用有效 Matcher，异常原样传播。

preflight rules：

- callbacks 按 RequestMRef term order 规范化后调用 matcher；
- matcher exception 原样传播；非-boolean result 抛出 `{invalid_matcher_return, Value}`；
- 所有 matcher 必须在任意 mutation 前完成；
- inspect 永不修改 State；
- fail 对 selected stable entries 按 RequestMRef term order commit；ordinary entries 返回
  `ordinary`且不删除；
- commit 按 selected Ref term order执行，并向其 direct `parent_ref` 注入 manual failure；
- node 结果记录 `forwarded | handled | already_removed`；ordinary callback记录`ordinary`；
- callback协议错误、invalid return和invalid descriptor直接抛出；
- callback/effect 内部异常原样传播并停止本次 commit；此前已经消费的 nodes 不 rollback。

manual Outcome 固定为 `{error, {sf_aborted, Reason}}`。

## callback_exists and Routing

callbacks dictionary 同时包含 stable/ordinary action callbacks、continuation nodes 和 accumulator data。分类必须基于 value
shape：

- `callback_exists/1` 只统计可接收 action reply 的 stable/ordinary action entries；
- intermediate/outer node 不单独表示 external pending action；
- ActionMRef router 只执行 decoded action entry；
- internal dispatcher 才能按 ContinuationRef 执行 intermediate/outer node；
- external key缺失或命中合法的non-action node/accumulator/reverse-index entry返回`missing_callback`；
  action kind无法解码或非法`other` value直接抛出`invalid_callback_entry`；
- dictionary map/list/dict backends 保持支持。

## Failure and Transaction Boundary

outer callback 总是在 current node cleanup 后调用。`/3` 不得看到已消费的 current Ref 或其直接 resources。

take 后没有 rollback：

- direct `/2` exception：原样传播，State 保持 CleanState；
- direct `/3` exception：原样传播；invalid NewState 抛 `invalid_sf_cc_return`；State 保持 CleanState；
- async_r_t `/2` 返回非-`async_r_t`：`invalid_sf_cc_return`；
- async_r_t effect 自身失败：按底层 monad failure 传播；原 node 不恢复。

该边界避免 final callback 已发送 reply 后因 retry 重复通知。

## Compatibility and Rollout

- ordinary、wait 和 sf-prefixed runners 都注册 outer continuation node，同时保留 public callback/result contract。
- stable path 使用 `#sf_callback{}`；ordinary path 使用带 direct `parent_ref` 的 `#callback{}`，但仍与
  frame identity/migration 隔离。缺少 `parent_ref` 的旧 callback tuple 不属于本 change 的 compatibility boundary。
- ordinary bind 保持 anonymous continuation，且不获得 stable-frame migration guarantee。
- recursive `sf_next`、`sf_cc_ref` terminal 和 per-callback root `sf_cc_mref` 是被 supersede 的未发布合同，
  不保留双 representation。

## Alternatives Rejected

### Store final callback only inside continuation

完整 final descriptor 不嵌入 stable frame；它是外层 node。损坏 current frame 时只需读取其 direct
`parent_ref`，不需要 root address。

### Wrap every ordinary callback in another continuation node

ordinary action entry 本身已经能同时保存 current `cc` 与 direct `parent_ref`；为每个 ordinary suspension
再创建只包装同一个 fun 的 continuation node 会重复 current continuation identity。只有 stable frame 需要把
尚未可寻址的 ordinary parent 作为独立 node materialize。

### Root sidecar as continuation semantics

ordinary normal resume直接调用current `cc`；stable returned Monad与current-continuation availability
failure只需要direct `parent_ref`，不需要root sidecar。主动 cancellation/parallel reverse lookup MAY 使用
独立索引，但该索引不参与 continuation 语义。

### Reuse AccRef as ContinuationRef

parallel/local accumulator 会重绑定 AccRef，不能代表 continuation edge。

### Store `{Module, Function, Arity}`

用户 source 已显式提供 external fun；保留 external-fun term 可让 macro 静态区分 local/anonymous，同时避免
公开低层 MFA tuple API。

### Runtime fun_info validation

runtime 已看不到 source distinction。external/local source contract 必须由 macro AST validation 保证；
runtime 的 `fun_info` 使用仅限 apply 边界的 availability check，不得把 runtime fun term 重新解释为源码证明。
