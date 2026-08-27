# sf-cc Design

## Context

`async_t` callback entry 是：

```erlang
-record(callback, {
    cc,
    acc_ref,
    sf_cc_mref = undefined
}).
```

`cc` 可以是 ordinary anonymous continuation fun，也可以是 `sf_do` stable continuation data。
`sf_cc_mref` 是独立 final completion address。stable chain 只在 terminal 保存 CcRef，不嵌入完整
callback descriptor。

resume 仍由对应 continuation executor 负责；sf-cc 只拥有 CcRef registration、cleanup 和 final
callback invocation。ordinary anonymous continuation 失效不是 sf-cc 的恢复责任。

## Goals

- 每个 public runner invocation 创建一个顶层 CcRef 和一个最终 Outcome address。
- ordinary runner 与 stable frame runner 使用相同 CcRef store、完成原语和主动失败路径。
- `#callback.cc` 保持原 continuation，不包装、不迁移、不解析 anonymous fun。
- sf-prefixed API 的用户 final callback 必须是源码中显式出现的 external fun；ordinary API 保持原合同。
- final callback 在 ActionMRef/CcRef 清理后调用；`/3` 看到 CleanState。
- 正常完成、结构失败、迁移失败和人工失败均 exactly-once。
- structured frame 与 ordinary anonymous downstream 相遇时，以独立 CcRef descriptor 桥接表示边界。

## Non-Goals

- CcRef 不是多个独立业务 promises 的 operation-group id。
- 不从 anonymous fun 恢复 source stack、BlockPath、StepName 或 source Vsn。
- 不定义 migration mapping、Env/Input converter 或 appup/appdown transition ordering。
- 不迁移 final external fun 或 Context。
- 不保证 final callback 外部副作用可回滚。

## Terminology

- `ActionMRef`：外部 request/reply、timeout、DOWN 或 message 使用的 correlation key。
- `AccRef`：现有 `async_r_t` local accumulator reference；并行/聚合可以局部重绑定。
- `CcRef`：CC MRef；一个 runner 或 ordinary/frame bridge segment 的一次性 completion address。每个 runner
  有顶层 CcRef，表示边界可按需建立内部 CcRef。
- `sf_cc`：CcRef 指向的 stable final callback descriptor。
- `CleanState`：已删除 CcRef、属于该 CcRef lifecycle 的 action callbacks 及其失效 resources 后的 State。
- protected callback：`sf_cc_mref` 为 reference 的 callback。
- unprotected callback：当前四元 callback 中 `sf_cc_mref = undefined` 的 callback。

`AccRef` 与 `CcRef` 不得合并。内部 parallel/local accumulator 会重绑定 AccRef，但不能改变顶层 final
completion address。

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
返回的 `async_r_t` 是 final completion effect；它在已消费原 CcRef 的 scope 中执行。若该 effect
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

它们不受 external-fun macro 约束，但内部 MUST 通过 ordinary completion descriptor 委托统一 CcRef
lifecycle。`wait/1`、`wait_t/2` 同样 MUST 创建 CcRef，并使用 library-owned exported completion；其
public API 和正常结果不变。该兼容迁移只保证新挂起 action 可由 CcRef 统一清理和 exactly-once 完成，
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

### Ordinary completion descriptor

CcRef entry 只持有一种 `sf_cc` descriptor encoding。sf-prefixed API 直接保存用户 external callback
和 Context；ordinary API 以 `mode = async_r_t` 保存 library adapter，并把原 callback 作为 adapter
Context。wait 同样复用 ordinary runner。external-fun source restriction 只适用于用户显式调用
sf-prefixed macro，不追溯应用到 library adapter。

### CcRef entry

```erlang
CcRef => #{
    kind => sf_cc_ref,
    protocol => 1,
    cc => SfCc
}.
```

entry absence is the consumed marker；不另存 `done` tombstone。CcRef 是内部 reference，不作为
ActionMRef 路由。reply router 命中 `sf_cc_ref` value 时 MUST 返回 unhandled，而不是把它作为
`#callback{}` 执行。

### Action callback

```erlang
-record(callback, {
    cc,
    acc_ref,
    sf_cc_mref = undefined
}).
```

```erlang
ActionMRef => #callback{
    cc = OrdinaryFunOrStableContinuation,
    acc_ref = AccRef,
    sf_cc_mref = CcRef
}.
```

`cc` 和 `acc_ref` 的现有语义不变。`sf_cc_mref` 只表示 final address，不决定 continuation executor。

### Callback decoding

所有 callback readers MUST 通过 shared decode helper 接受当前四元 `#callback{}`，并拒绝其他 tuple
shape。runtime 不提供旧三元 callback compatibility，也不得从 `cc` fun/map 猜测或补造 CcRef。
record 定义 MUST 位于 internal shared header，避免 reader 与 writer 漂移。

## sf-cc Scope Propagation

仅给 `#callback{}` 增加字段还不够：initial execution 和 resumed continuation 创建下一 ActionMRef 时
必须知道应写入哪个 CcRef。该信息不得藏进 AccRef，因为 `local_ref/3` 和 parallel accumulator 会改变
AccRef。

`async_r_t` 使用彼此独立的 reader locals 传播 AccRef 与 optional CcRef：

- `get_local_ref` 只返回 AccRef；
- `get_sf_cc_ref` 返回 optional CcRef；
- `local_ref(NewAccRef, Action)` 只替换 AccRef；
- `local_sf_cc_ref(CcRef, Action)` 只替换 CcRef；
- public runners 安装新的顶层 CcRef；
- reply executor 从 `#callback.acc_ref` 与 `#callback.sf_cc_mref` 调用 `run_scope` 重建两个 locals。

promise suspension writer 从 reader local 读取 CcRef 并写入 callback field。ordinary continuation 本身无需
变化，resume 后产生的新 action 仍继承当前 final address。

### Explicit async continuation context

`async_t` internal action MUST 显式接收 `(K, SfContext)`。`SfContext` 保存 current structured continuation
和对应 stable runner，不得通过进程字典或其他隐藏 mutable channel 传播。

- ordinary `>>=` 继续构造 anonymous `K`，只透传 `SfContext`；
- 只有 `bind_frame/4` 创建 `sf_next`；
- Promise 仅在实际 `K` 与 `SfContext.runner` 完全相同时保存 `SfContext.continuation`，否则保存 `K`；
- `bind_frame` 的 downstream 是 ordinary anonymous fun 时，runtime 创建独立 CcRef descriptor 保存该 fun，
  并让 structured chain 终止于该 CcRef；不得把 fun 嵌入 `sf_next` chain。

## Frame Integration

frame continuation 可以保留 nested `sf_next` structure，但 terminal node MUST 变为只含 reference 的数据：

```erlang
#{kind => sf_cc_ref, protocol => 1, mref => CcRef}
```

它不得再嵌入第二份 `sf_cc` descriptor。callback record 中的 `sf_cc_mref` 与 terminal node.mref MUST
相等。structural failure、manifest/MFA failure 和主动失败 MUST 直接使用 record field；不得先递归遍历
损坏的 continuation 来寻找 final callback。

normal frame completion 到达 terminal node 时调用统一 `complete_sf_cc/3`。ordinary completion wrapper
可以捕获 CcRef 以完成正常路径，但 fallback correctness MUST 只依赖 record field 和 CcRef entry。

## Completion State Machine

一个 CcRef 有两个可观察状态：

```text
pending  : callbacks store contains sf_cc_ref entry
consumed : entry absent
```

唯一 completion primitive：

```erlang
complete_sf_cc(CcRef, Outcome, State) -> CompletionResult.
```

commit order：

1. preflight CcRef entry shape、callback mode 和 cleanup targets；
2. atomic-with-respect-to-process `take` CcRef entry；
3. remove every action callback whose `sf_cc_mref` equals CcRef；
4. cancel/demonitor/remove those callbacks' timer、monitor 和 accumulator resources；
5. obtain CleanState；
6. invoke the stored final descriptor；
7. apply mode-specific result semantics。

同一 CcRef 下可能存在多个内部 action entries；清理它们是一个 promise 的 lifecycle cleanup，不把 CcRef
定义为多个独立业务 promises 的 result group。

如果 preflight 失败，State 不变且不调用 CC。如果 CcRef 已缺失，返回 unhandled。如果 take 后 user CC
crash、返回非法 shape 或产生外部副作用，CcRef 保持 consumed，不 rollback、不 retry。callback/effect
内部异常原样传播。batch fail 只把预期的 unavailable callback 或 invalid return 记录为 `cc_failed`。

reply/timeout/DOWN path 在执行 continuation 前已知 ActionMRef 和 decoded callback。frame
protocol/shape/manifest/MFA failure 可以使用 `sf_cc_mref` terminal-complete；ordinary anonymous fun 直接
执行，其 `badfun` 或内部异常原样传播。message path MUST 保留 ActionMRef，不消费 CcRef。

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
frame infrastructure 明确不能继续时才绕过 continuation 进入 final CC。

## Active Inspection and Failure

### Summary shape

matcher 每个 action callback 调用一次，并接收：

```erlang
#{
    request_mref := term(),
    sf_cc_mref := reference() | undefined,
    protected := boolean(),
    continuation_kind := ordinary_fun | sf_frame | unknown,
    frames := [#{
        module := module(),
        vsn := term() | undefined,
        protocol := integer(),
        block_path := [atom()],
        step_name := atom() | undefined,
        async_id := binary()
    }]
}.
```

`frames` 按 current-to-downstream order。ordinary fun 使用空 list；runtime 不从 fun 推断 frame metadata。
frame `vsn` 沿用 producer 保存的 module `-vsn` literal term；未提供时为 `undefined`，不得假定 atom。

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
- fail 对 selected protected entries 按 CcRef 去重；selected unprotected entries 返回 `unprotected`，不删除；
- commit 按 CcRef term order 执行；
- protected CcRef 结果记录 `completed | cc_failed | unhandled`；unprotected callback 记录 `unprotected`；
- `cc_failed` 不携带 Class、Reason 或 Stacktrace，只表示 invalid descriptor 在 preflight 失败，或预期的
  unavailable callback/invalid return 在 CcRef 消费后失败；
- callback/effect 内部异常原样传播并停止本次 commit；此前已经消费的 CcRef 不 rollback。

manual Outcome 固定为 `{error, {sf_aborted, Reason}}`。

## callback_exists and Routing

callbacks dictionary 同时包含 action callback、accumulator data 和 `sf_cc_ref` entries。分类必须基于 value
shape：

- `callback_exists/1` 只统计当前四元 callback tuples；
- `sf_cc_ref` 不单独表示一个可接收 reply 的 pending action；
- ActionMRef router 只执行 decoded callback tuple；
- 命中 accumulator 或 `sf_cc_ref` value 返回 unhandled；
- dictionary map/list/dict backends 保持支持。

## Failure and Transaction Boundary

final callback 总是在 cleanup 后调用。`/3` 不得看到已完成的 CcRef 或 action callbacks。

take 后没有 rollback：

- direct `/2` exception：原样传播，State 保持 CleanState；
- direct `/3` exception：原样传播；invalid NewState 抛 `invalid_sf_cc_return`；State 保持 CleanState；
- async_r_t `/2` 返回非-`async_r_t`：`invalid_sf_cc_return`；
- async_r_t effect 自身失败：按底层 monad failure 传播；原 CcRef 不恢复。

该边界避免 final callback 已发送 reply 后因 retry 重复通知。

## Compatibility and Rollout

- ordinary、wait 和 sf-prefixed runners 全部写 protected callback；它们共享 CcRef lifecycle，但各自保留
  public callback/result contract。
- callback store 只支持当前四元 encoding；旧三元 runtime compatibility 和 automatic downgrade 不属于
  本 change。
- ordinary bind 保持 anonymous continuation，且不获得 stable-frame migration guarantee。
- `sf-do` 当前嵌入 full `sf_cc` 的 chain encoding 是未发布开发合同；本 change 直接升级为 CcRef terminal，
  不保留双 final-owner encoding。

## Alternatives Rejected

### Store final callback only inside continuation

损坏 frame 无法安全解析，因此完整 final descriptor 不能只嵌在 structured continuation 内。ordinary
anonymous continuation 的调用错误仍按普通 bind 语义传播。

### Wrap every callback in a new envelope

会大范围改变 ordinary callback representation。追加一个 record field 已足够建立关系。

### Sidecar ActionMRef-to-CcRef map

增加额外索引和一致性维护；record field 可以 O(1) 取得 CcRef。

### Reuse AccRef as CcRef

parallel/local accumulator 会重绑定 AccRef，不能代表不变 final address。

### Store `{Module, Function, Arity}`

用户 source 已显式提供 external fun；保留 external-fun term 可让 macro 静态区分 local/anonymous，同时避免
公开低层 MFA tuple API。

### Runtime fun_info validation

runtime 已看不到 source distinction。external/local source contract 必须由 macro AST validation 保证；
runtime 的 `fun_info` 使用仅限 apply 边界的 availability check，不得把 runtime fun term 重新解释为源码证明。
