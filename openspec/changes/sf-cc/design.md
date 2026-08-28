# sf-cc Design

## Context

legacy `async_t` callback entry 是：

```erlang
-record(callback, {
    cc,
    acc_ref,
}).
```

stable path 使用独立 entry：

```erlang
#sf_callback{frame = CurrentFrame, next_ref = ParentRef, acc_ref = AccRef, ...}.
```

legacy `#callback.cc` 继续保存 ordinary anonymous continuation fun。stable callback 只保存 current frame
和 direct `next_ref`；outer frame 与 final callback 是 callbacks store 中其他 nodes。

resume 由统一 ContinuationRef dispatcher 负责；sf-cc 拥有 node registration、take、direct cleanup、
failure forwarding 和 outer callback invocation。

## Goals

- 每个 public runner invocation 注册一个 outer continuation node。
- ordinary runner 与 stable frame runner 使用相同 ContinuationRef store、dispatch 和主动失败路径。
- legacy `#callback.cc` 保持原 continuation；stable `#sf_callback{}` 保存 current frame + direct `next_ref`。
- sf-prefixed API 的用户 final callback 必须是源码中显式出现的 external fun；ordinary API 保持原合同。
- outer callback 在 current ActionMRef/node 清理后调用；`/3` 看到 CleanState。
- 正常完成、结构失败、迁移失败和人工失败均逐 node exactly-once。
- structured frame 与 ordinary anonymous downstream 相遇时，把 downstream 物化为普通 node。

## Non-Goals

- ContinuationRef 不是多个独立业务 promises 的 operation-group id。
- 不从 anonymous fun 恢复 source stack、BlockPath、StepName 或 source Vsn。
- 不定义 migration mapping、Env/Input converter 或 appup/appdown transition ordering。
- 不迁移 final external fun 或 Context。
- 不保证 final callback 外部副作用可回滚。

## Terminology

- `ActionMRef`：外部 request/reply、timeout、DOWN 或 message 使用的 correlation key。
- `AccRef`：现有 `async_r_t` local accumulator reference；并行/聚合可以局部重绑定。
- `ContinuationRef`：callbacks store 中任一 continuation node 的一次性 reference；不区分 outer/final Ref 类型。
- `sf_cc`：outer node 可持有的 stable callback descriptor。
- `CleanState`：已 take 当前 node并清理其直接 lifecycle resources 后的 State。
- stable callback：明确编码的 `#sf_callback{frame,next_ref,...}`。
- legacy callback：既有 `#callback{cc,acc_ref,...}` ordinary entry，不自动获得 frame migration。

`AccRef` 与 `ContinuationRef` 不得合并。内部 parallel/local accumulator 可重绑定 AccRef；当前 node
只持有 direct `next_ref`，不复制顶层 address。

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
    #sf_callback{frame = Frame, next_ref = ParentRef, acc_ref = AccRef, ...}
  | #{kind => ordinary_continuation, protocol => 1, callback => Fun,
      next_ref => ParentRef | none}
  | #{kind => sf_cc, protocol => 1, mode => Mode, callback => Fun,
      context => Context, next_ref => none}.
```

Ref 的类型不表达 outer/final；value shape 决定 executor，`next_ref = none` 决定链结束。entry absence
是 consumed marker，不另存 tombstone。一个 node 只保存 direct `next_ref`，不得复制 root/final Ref。

### Stable and legacy action callbacks

stable suspension MUST 写 `#sf_callback{frame,next_ref,...}`。ActionMRef 本身就是该 current node 的路由
key；如果父层尚未可寻址，`bind_frame` 先用新 ContinuationRef 把父层物化。legacy `#callback{cc,acc_ref}`
继续按 ordinary contract 解码，不从 fun/map 推断 stable frame或补造 parent/root Ref。

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
- reply executor 从 stable callback 的 `acc_ref`、`frame` 与 `next_ref` 重建 context。

### Explicit async continuation context

`async_t` internal action MUST 显式接收 `(K, SfContext)`。`SfContext` 保存 current structured continuation
和对应 stable runner，不得通过进程字典或其他隐藏 mutable channel 传播。

- ordinary `>>=` 继续构造 anonymous `K`，只透传 `SfContext`；
- Promise 仅在实际 `K` 与 `SfContext.runner` 完全相同时写 `#sf_callback{}`，否则保持 ordinary path；
- `bind_frame` 把尚未可寻址的 current parent 物化为 node，并把 Ref 保存为新 frame 的 `next_ref`；
- ordinary anonymous downstream 被物化为 ordinary node，不嵌入 stable frame data。

## Frame Integration

frame continuation MUST 是单层 `CurrentFrame + NextRef`。normal success invoke current frame并把返回 Monad
运行于 NextRef；structural/manifest/MFA/migration/invocation availability failure MUST 跳过当前普通 success
frame并 dispatch NextRef。dispatcher 不寻找 root，也不区分 FinalCcRef 与 OuterRef。

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

1. preflight current node shape、executor mode 和 direct cleanup targets；
2. atomic-with-respect-to-process `take` current node；
3. cancel/demonitor/remove current node直接拥有的 timer、monitor 和 accumulator resources；
4. obtain CleanState；
5. success 时执行 current continuation；failure 时普通 success frame直接 dispatch `next_ref`；
6. current invocation 产生可归属 failure 时 dispatch `next_ref`；
7. outer callback node 应用 mode-specific result semantics。

parallel cancellation 如需从 outer node 查找 active children，MUST 使用独立反向索引/ownership metadata；
不得通过在每个 node 复制 root Ref 实现。

如果 preflight 失败，State 不变且不调用 node。如果 Ref 已缺失，返回 unhandled。如果 take 后 user CC
crash、返回非法 shape 或产生外部副作用，Ref 保持 consumed，不 rollback、不 retry。callback/effect
内部异常原样传播。batch fail 只把预期的 unavailable callback 或 invalid return 记录为 `failed`。

reply/timeout/DOWN path 在执行 continuation 前已知 ActionMRef 和 decoded callback。frame
protocol/shape/manifest/MFA failure dispatch direct `next_ref`。仅可归属当前 invocation availability 的
`badfun`/`undef` 被转换为 failure；callback-internal exception 原样传播。message path MUST 保留 ActionMRef。

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

ordinary reply、timeout、DOWN 和业务 `{error, Reason}` 默认仍是 continuation Input；只有 structured
frame infrastructure 明确不能调用当前 success continuation 时，才把 runtime failure 向 direct
`next_ref` 转发。

## Active Inspection and Failure

### Summary shape

matcher 每个 action callback 调用一次，并接收：

```erlang
#{
    request_mref := term(),
    next_ref := reference() | none,
    stable := boolean(),
    continuation_kind := ordinary_fun | sf_frame | unknown,
    frame := #{
        module := module(),
        vsn := term() | undefined,
        protocol := integer(),
        block_path := [atom()],
        step_name := atom() | undefined,
        async_id := binary()
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
- fail 对 selected stable entries 按 RequestMRef/ContinuationRef preflight；legacy entries 返回 `legacy`，不删除；
- commit 按 selected Ref term order执行，并向其 direct `next_ref` 注入 manual failure；
- node 结果记录 `forwarded | handled | failed | unhandled`；legacy callback 记录 `legacy`；
- `failed` 不携带 Class、Reason 或 Stacktrace，只表示 invalid descriptor 在 preflight 失败，或预期的
  unavailable callback/invalid return 在 current node 消费后失败；
- callback/effect 内部异常原样传播并停止本次 commit；此前已经消费的 nodes 不 rollback。

manual Outcome 固定为 `{error, {sf_aborted, Reason}}`。

## callback_exists and Routing

callbacks dictionary 同时包含 stable/legacy action callbacks、continuation nodes 和 accumulator data。分类必须基于 value
shape：

- `callback_exists/1` 只统计可接收 action reply 的 stable/legacy action entries；
- intermediate/outer node 不单独表示 external pending action；
- ActionMRef router 只执行 decoded action entry；
- internal dispatcher 才能按 ContinuationRef 执行 intermediate/outer node；
- 命中 accumulator value 返回 unhandled；
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
- stable path 使用 `#sf_callback{}`；legacy `#callback{}` 保持 ordinary runtime compatibility并与 migration 隔离。
- ordinary bind 保持 anonymous continuation，且不获得 stable-frame migration guarantee。
- recursive `sf_next`、`sf_cc_ref` terminal 和 per-callback root `sf_cc_mref` 是被 supersede 的未发布合同，
  不保留双 representation。

## Alternatives Rejected

### Store final callback only inside continuation

完整 final descriptor 不嵌入 stable frame；它是外层 node。损坏 current frame 时只需读取其 direct
`next_ref`，不需要 root address。

### Wrap every callback in a new envelope

legacy ordinary callback 不改变；stable path 使用独立 `#sf_callback{}`，避免让一个 record 同时承载
ordinary fun、recursive frame chain 和 root ownership。

### Root sidecar as continuation semantics

normal resume 不需要 sidecar，只沿 direct `next_ref`。主动 cancellation/parallel reverse lookup MAY 使用
独立索引，但该索引不参与 continuation 语义。

### Reuse AccRef as ContinuationRef

parallel/local accumulator 会重绑定 AccRef，不能代表 continuation edge。

### Store `{Module, Function, Arity}`

用户 source 已显式提供 external fun；保留 external-fun term 可让 macro 静态区分 local/anonymous，同时避免
公开低层 MFA tuple API。

### Runtime fun_info validation

runtime 已看不到 source distinction。external/local source contract 必须由 macro AST validation 保证；
runtime 的 `fun_info` 使用仅限 apply 边界的 availability check，不得把 runtime fun term 重新解释为源码证明。
