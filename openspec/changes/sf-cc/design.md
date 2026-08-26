# sf-cc Design

## Context

当前 `async_t` callback entry 是：

```erlang
-record(callback, {
    cc,
    acc_ref
}).
```

`cc` 可以是 legacy anonymous continuation fun，也可以是 `sf_do` stable continuation data。
现有 `run_cc/2` 使用的内部 CC 固定为 `fun(Outcome) -> async_r_t()`；ordinary callback 先经
`callback_to_cc/1` 生成该匿名 adapter。当前 `sf_do` 则另有嵌入 continuation chain 尾部的
`sf_cc` descriptor。两条 final completion 路径无法在 continuation 本身已经失效时统一到达。

本 change 把 final completion 提升为独立 CcRef。resume 仍由原 continuation executor 负责；final
completion 不再依赖 executor 能解释 continuation。

## Goals

- 一个由升级后任意 public runner 启动的 promise 恰好拥有一个 CcRef 和一个最终 Outcome。
- legacy anonymous promise 与 stable frame promise 使用相同 CcRef store、完成原语和主动失败路径。
- `#callback.cc` 保持原 continuation，不包装、不迁移、不解析 anonymous fun。
- 用户 final callback 必须是源码中显式出现的 external fun，并以稳定 descriptor 保存。
- final callback 在 ActionMRef/CcRef 清理后调用；`/3` 看到 CleanState。
- 正常完成、结构失败、迁移失败和人工失败均 exactly-once。
- 升级前旧三元 callback tuple 可继续读取，并在 continuation 可调用时执行。

## Non-Goals

- CcRef 不是多个独立业务 promises 的 operation-group id。
- 不从 anonymous fun 恢复 source stack、BlockPath、StepName 或 source Vsn。
- 不定义 migration mapping、Env/Input converter 或 appup/appdown transition ordering。
- 不迁移 final external fun 或 Context。
- 不保证 final callback 外部副作用可回滚。

## Terminology

- `ActionMRef`：外部 request/reply、timeout、DOWN 或 message 使用的 correlation key。
- `AccRef`：现有 `async_r_t` local accumulator reference；并行/聚合可以局部重绑定。
- `CcRef`：CC MRef；一个 promise 的一次性 final completion address。
- `sf_cc`：CcRef 指向的 stable final callback descriptor。
- `CleanState`：已删除 CcRef、属于该 promise 的 action callbacks 及其失效 lifecycle resources 后的 State。
- protected callback：`sf_cc_mref` 为 reference 的新 callback。
- unprotected callback：旧三元 tuple 或 `sf_cc_mref = undefined` 的 callback。

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

它们不受 external-fun macro 约束，但升级后内部 MUST 通过 legacy final descriptor 委托统一 CcRef
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
`{Module, Function, Arity}`，不得在 runtime 使用 `fun_info` 判断 external/local source origin。
runtime 只用 `is_function(Callback, 2 | 3)` 分派调用协议。

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

`callback` MUST 是 macro 原样传入的 external-fun term。`context` 是用户预存 opaque term，不是
`sf_do` lexical Env，不参与 Env/Input migration，也不得出现在 selector summary。

### final descriptor union

CcRef 可以持有显式 stable descriptor，也可以持有 ordinary compatibility descriptor：

```erlang
-type legacy_cc() :: #{
    kind := legacy_cc,
    protocol := 1,
    callback := fun()
}.

-type final_cc() :: sf_cc() | legacy_cc().
```

`legacy_cc.callback` 是现有 ordinary API 已经生成的 final adapter；invoker MUST 保留其旧
normalization/result projection。wait 使用 library-owned external completion，可编码为受库控制的
`sf_cc` 或等价 stable descriptor。external-fun source restriction 只适用于用户显式调用
sf-prefixed macro 的 `sf_cc` descriptor，不追溯应用到 compatibility descriptor。

### CcRef entry

```erlang
CcRef => #{
    kind => sf_cc_ref,
    protocol => 1,
    cc => FinalCc
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
    cc = LegacyFunOrStableContinuation,
    acc_ref = AccRef,
    sf_cc_mref = CcRef
}.
```

`cc` 和 `acc_ref` 的现有语义不变。`sf_cc_mref` 只表示 final address，不决定 continuation executor。

### Old tuple decoding

所有 callback readers MUST 通过共享 decode helper 接受：

```erlang
{callback, Cc, AccRef}
{callback, Cc, AccRef, SfCcMRef}
```

三元 tuple decode 为 `sf_cc_mref = undefined`。runtime MUST NOT 从 `Cc` fun 或 map 猜测/补齐 CcRef。
旧 tuple 在 continuation 可调用时继续正常 resume。若其 anonymous continuation 因 old-code purge
已确认不可调用，则走下文的 unprotected legacy outcome 路径；structural classifier 不得从 ordinary
callback 内部抛出的异常反推代码失效。

新 record 定义 SHOULD 移入 internal shared header，避免 `async_t` 与 `async_util` 的重复声明再次漂移。

## sf-cc Scope Propagation

仅给 `#callback{}` 增加字段还不够：initial execution 和 resumed continuation 创建下一 ActionMRef 时
必须知道应写入哪个 CcRef。该信息不得藏进 AccRef，因为 `local_ref/3` 和 parallel accumulator 会改变
AccRef。

`async_r_t` reader-local execution context SHOULD 规范化为内部 scope：

```erlang
#{
    acc_ref := AccRef,
    sf_cc_mref := CcRef | undefined
}.
```

兼容规则：

- 旧 raw reference reader value decode 为 `#{acc_ref => Ref, sf_cc_mref => undefined}`；
- `get_local_ref` 继续只返回 AccRef；
- 新 internal `get_sf_cc_mref` 返回 optional CcRef；
- `local_ref(NewAccRef, Action)` 只替换 scope.acc_ref，保留 scope.sf_cc_mref；
- 所有 post-upgrade public runners 安装新 CcRef；
- reply executor 从 `#callback.acc_ref` 与 `#callback.sf_cc_mref` 重建 scope；
- 只有从升级前旧三元 tuple 恢复的 scope 使用 `sf_cc_mref = undefined`。

promise suspension writer 从 scope 读取 CcRef 并写入新 callback field。这样 legacy continuation 本身无需
变化，resume 后产生的新 action 仍继承同一 final address。

## Frame Integration

frame continuation 可以保留 nested `sf_next` structure，但 terminal node MUST 变为只含 reference 的数据：

```erlang
#{kind => sf_cc_ref, protocol => 1, mref => CcRef}
```

它不得再嵌入第二份 `sf_cc` descriptor。callback record 中的 `sf_cc_mref` 与 terminal node.mref MUST
相等。structural failure、manifest/MFA failure 和主动失败 MUST 直接使用 record field；不得先递归遍历
损坏的 continuation 来寻找 final callback。

normal frame completion 到达 terminal node 时调用统一 `complete_sf_cc/3`。legacy completion wrapper
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
7. apply sf mode-specific or legacy compatibility result semantics。

同一 CcRef 下可能存在多个内部 action entries；清理它们是一个 promise 的 lifecycle cleanup，不把 CcRef
定义为多个独立业务 promises 的 result group。

如果 preflight 失败，State 不变且不调用 CC。如果 CcRef 已缺失，返回 unhandled。如果 take 后 user CC
crash、返回非法 shape 或产生外部副作用，CcRef 保持 consumed，不 rollback、不 retry。普通 runner 可以
传播原 exception；batch fail API 捕获并记录 per-CcRef failure。

reply/timeout/DOWN path 在执行 continuation 前已知 ActionMRef 和 decoded callback，因此可以在 fun/frame
调用失败时使用 `sf_cc_mref` terminal-complete。message path MUST 保留 ActionMRef，不消费 CcRef。

### Unprotected pre-upgrade callback

旧三元 tuple 没有 CcRef，因此无法调用或恢复最终 CC。当 runtime 在进入 anonymous continuation 前，
以窄分类确认该 fun 因 old-code purge 已不可调用时，它 MUST：

1. 清理该 request 仍可识别且由其拥有的 callback、timer、monitor 和 accumulator lifecycle；
2. 不 retry、不伪造 CcRef、不调用 final CC；
3. 让 `async_m:handle_info/3` 返回 `{legacy_outcome, Outcome, CleanState}`。

这里的 `Outcome` 是 reply、timeout、DOWN 或 message 原本要交给旧 continuation 的 action input，不是
合成的 orphan/final failure。receive-loop 调用方决定后续策略。只有能确认“调用目标 old code 已不存在”
的失败可进入该分支；成功进入 callback 后产生的普通 exception 或 nested `undef` MUST 保持原异常语义。

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

ordinary reply、timeout、DOWN 和业务 `{error, Reason}` 默认仍是 continuation Input；只有 protected
continuation infrastructure 不能继续时才绕过 continuation 进入 final CC。unprotected old tuple 无 final
owner，按上节返回原 action Outcome。

## Active Inspection and Failure

### Summary shape

matcher 每个 action callback 调用一次，并接收：

```erlang
#{
    request_mref := term(),
    sf_cc_mref := reference() | undefined,
    protected := boolean(),
    continuation_kind := legacy_fun | sf_frame | unknown,
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

`frames` 按 current-to-downstream order。legacy fun 使用空 list；runtime 不从 fun 推断 frame metadata。
frame `vsn` 沿用 producer 保存的 module `-vsn` literal term；未提供时为 `undefined`，不得假定 atom。

summary MUST NOT 包含 continuation value、Env、State、timer、monitor、accumulator、sf_cc callback 或 Context。

### APIs

```erlang
inspect_sf_callbacks(Matcher, Offset, State) ->
    {ok, [Summary]}
  | {error, MatcherError}.

fail_sf_callbacks(Matcher, Reason, Offset, State) ->
    {ok, [FailResult], NewState}
  | {error, MatcherError, State}.
```

`Matcher :: fun((Summary) -> boolean())`。它是同步 ephemeral fun，不持久化。

preflight rules：

- callbacks 按 RequestMRef term order 规范化后调用 matcher；
- matcher exception 或非-boolean result 立即返回 error；
- 所有 matcher 必须在任意 mutation 前完成；
- inspect 永不修改 State；
- fail 对 selected protected entries 按 CcRef 去重；selected unprotected entries 返回 `unprotected`，不删除；
- commit 按 CcRef term order 执行；一个 CC failure 不阻止后续 preflight-selected CcRefs；
- 每个 CcRef 结果记录 `completed | cc_failed | already_consumed | unprotected`；
- `cc_failed` 记录 `{Class, Reason, Stacktrace}`，但 CcRef 保持 consumed。

manual Outcome 固定为 `{error, {sf_aborted, Reason}}`。

## callback_exists and Routing

callbacks dictionary 同时包含 action callback、accumulator data 和 `sf_cc_ref` entries。分类必须基于 value
shape：

- `callback_exists/1` 只统计 old/new callback tuples；
- `sf_cc_ref` 不单独表示一个可接收 reply 的 pending action；
- ActionMRef router 只执行 decoded callback tuple；
- 命中 accumulator 或 `sf_cc_ref` value 返回 unhandled；
- dictionary map/list/dict backends 保持支持。

## Failure and Transaction Boundary

final callback 总是在 cleanup 后调用。`/3` 不得看到已完成的 CcRef 或 action callbacks。

take 后没有 rollback：

- direct `/2` exception：传播/记录 exception，State 保持 CleanState；
- direct `/3` exception 或 invalid NewState：传播/记录 error，State 保持 CleanState；
- async_r_t `/2` 返回非-`async_r_t`：`invalid_sf_cc_return`；
- async_r_t effect 自身失败：按底层 monad failure 传播；原 CcRef 不恢复。

该边界避免 final callback 已发送 reply 后因 retry 重复通知。

## Compatibility and Rollout

- forward upgrade reader 同时接受 old/new tuple。
- 升级后 new ordinary、wait 和 sf-prefixed runners 全部写 protected callback；它们共享 CcRef lifecycle，
  但各自保留 public callback/result contract。
- 已 pending old tuple 不会获得 CcRef。可调用时继续旧路径；purged/unavailable 时通过
  `handle_info/3` 返回 tagged legacy outcome，不调用 final CC。release documentation MUST 明示。
- rollback 到完全不了解四元 tuple 的旧 runtime 前，应用必须排空、主动失败或在 `code_change` 中降为安全
  old tuple；通用自动 downgrade 不属于本 change。
- `sf-do` 当前嵌入 full `sf_cc` 的 chain encoding 是未发布开发合同；本 change 直接升级为 CcRef terminal，
  不保留双 final-owner encoding。

## Alternatives Rejected

### Store final callback only inside continuation

anonymous fun/损坏 frame 无法解析，正是本 change 要解决的问题。

### Wrap every callback in a new envelope

会大范围改变 legacy callback representation。追加一个 record field 已足够建立关系。

### Sidecar ActionMRef-to-CcRef map

增加额外索引和一致性维护；record field 可以 O(1) 取得 CcRef。

### Reuse AccRef as CcRef

parallel/local accumulator 会重绑定 AccRef，不能代表不变 final address。

### Store `{Module, Function, Arity}`

用户 source 已显式提供 external fun；保留 external-fun term 可让 macro 静态区分 local/anonymous，同时避免
公开低层 MFA tuple API。

### Runtime fun_info validation

runtime 已看不到 source distinction。external/local 必须由 macro AST validation 保证；runtime 只分派 arity。
