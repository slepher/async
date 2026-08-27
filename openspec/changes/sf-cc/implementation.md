# sf-cc Implementation Contract

## 1. Module Ownership

### `async_sf_cc_macro`

新 Astranaut provider，负责：

- `run_sf_cc/3` 第二参数 external `/2` validation；
- `run_with_sf_cc/5`、`exec_sf_cc/5` 第二参数 external `/2|3` validation；
- source-position diagnostics；
- 保留原 external-fun AST，生成普通 `async_m` remote call。

它 MUST NOT 依赖 `async_sf_transform`，也不得分析 Promise/Context/State runtime expression。

### `async_sf_cc`

新 runtime owner，负责：

- descriptor 与 CcRef entry constructors/validators；
- `complete/3` exactly-once primitive；
- mode/arity invocation；
- callback summary construction；
- inspect/fail preflight 与 commit；
- frame metadata flattening。

### `async_t`

负责：

- action callback write/read lifecycle；
- scope propagation；
- reply/message/timeout/DOWN routing；
- ordinary-fun/frame executor dispatch；
- explicit `(K, SfContext)` propagation and ordinary/frame representation boundary；
- 调用 `async_sf_cc:complete/3`。

### `async_m`

负责 public facade/macro targets，并把现有 ordinary runner、wait 和 receive-loop adapter 委托给统一
CcRef core：

```erlang
run_sf_cc/3
run_with_sf_cc/5
exec_sf_cc/5
inspect_sf_callbacks/3
fail_sf_callbacks/4
```

### `async_r_t`

负责相互独立的 reader-local AccRef 与 optional CcRef，不拥有 completion policy。

### `async_util`

负责 callbacks dictionary backend 和 shared callback decode/classification，不执行 user callback。

### `async_sf_runtime`

负责 frame execution/validation，并把 terminal/structural failure 委托给 `async_sf_cc`。

## 2. Shared Callback Encoding

创建 internal header，例如 `include/async_internal.hrl`：

```erlang
-record(callback, {
    cc,
    acc_ref,
    sf_cc_mref = undefined
}).
```

不要让 `async_t` 与 `async_util` 各自声明 record。所有 readers 通过 shared helper 统一 decode：

```erlang
decode_callback(#callback{cc = Cc, acc_ref = AccRef, sf_cc_mref = CcRef}) ->
    {ok, #{cc => Cc, acc_ref => AccRef, sf_cc_mref => CcRef}};
decode_callback(_) ->
    error.
```

旧三元 `{callback, Cc, AccRef}` 不属于当前合同，不得兼容解码。所有以下入口必须审计：

- `handle_reference`；
- callback executor；
- timeout wrapper；
- `callback_exists`；
- migration callback enumeration；
- inspect/fail enumeration；
- tests。

## 3. Internal sf-cc Types

```erlang
-type sf_cc_mode() :: async_r_t | direct_state.

-type sf_cc() :: #{
    kind := sf_cc,
    protocol := 1,
    mode := sf_cc_mode(),
    callback := fun(),
    context := term()
}.

-type sf_cc_ref_entry() :: #{
    kind := sf_cc_ref,
    protocol := 1,
    cc := sf_cc()
}.
```

constructors MUST validate mode/arity relation：

```erlang
new(async_r_t, Callback, Context) when is_function(Callback, 2).
new(direct_state, Callback, Context)
  when is_function(Callback, 2); is_function(Callback, 3).
```

该 guard 是 runtime protocol validation，不是 external/local validation。

## 4. Macro Expansion

logical expansion：

```erlang
run_sf_cc(Promise, ExternalFun, Context)
%% ->
async_m:run_sf_cc(Promise, ExternalFun, Context).

run_with_sf_cc(Promise, ExternalFun, Context, Offset, State)
%% ->
async_m:run_with_sf_cc(Promise, ExternalFun, Context, Offset, State).
```

实际 provider target MAY 使用不会再次触发 alias 的 internal exported name，但 public documentation 与 stack
errors MUST 保持上述 API。diagnostics MUST 使用 Astranaut return channel；expected source error 不得 throw。

建议 diagnostics：

```erlang
{sf_cc_external_fun_required, Runner}
{sf_cc_unsupported_arity, Runner, Arity, Allowed}
```

formatter MUST 展示 source location 与 accepted form。

## 5. Runner Construction

所有 public runner MUST 委托一个 common runner core。该 core 在 effect 真正执行时 allocate/register
CcRef、安装 reader-local CcRef、执行 Promise，并把 immediate/suspended completion 都交给同一 completion primitive。
各 facade 只负责构造 final descriptor 和保留自己的 observable normalization/result projection。

### `run_sf_cc/3`

返回一个 `async_r_t` program。在该 program 真正执行时：

1. allocate CcRef；
2. build `sf_cc(mode = async_r_t)`；
3. write CcRef entry through current callbacks lens；
4. install CcRef reader local while preserving AccRef；
5. run Promise with a final internal continuation that calls `async_sf_cc:complete/3`；
6. immediate completion consumes CcRef in the same evaluation；suspension leaves entry and action callback。

final internal continuation MAY be anonymous because fallback lookup does not depend on it；every persisted callback MUST
also contain `sf_cc_mref = CcRef`。

### `run_with_sf_cc/5`

1. derive callbacks getter/setter from Offset；
2. allocate AccRef and CcRef；
3. install direct-state descriptor；
4. execute the same core program with provided State；
5. return normal direct runner result/NewState。

`exec_sf_cc/5` MUST delegate rather than duplicate steps。

### Existing ordinary runners and wait

- `run_cc/2`、`run_with_cc/4`、`exec_cc/4`、`run/4` 使用 ordinary adapter 构造普通
  `sf_cc(mode = async_r_t)` descriptor，保持原 callback arity、normalization 和正常返回合同；
- `wait/1`、`wait_t/2` 使用 library-owned exported completion，保持原阻塞/超时和返回合同；
- 这些 facade 不做 external-fun source validation；
- 它们创建的每个 suspended callback 与 sf-prefixed runner 一样写入 CcRef；
- anonymous ordinary final callback 不获得 sf-prefixed external-fun guarantee；其内部错误原样传播。

## 6. Reader-Local Scope

AccRef 与 CcRef MUST 使用两个独立 reader layers：

- `get_local_ref/1` 读取 AccRef；
- `get_sf_cc_ref/1` 读取 `reference() | undefined`；
- `local_ref/3` 只局部替换 AccRef；
- `local_sf_cc_ref/3` 只局部替换 CcRef；
- `run/5` 默认以 `undefined` CcRef 执行；`run_scope/6` 显式接受 AccRef 与 CcRef。

When executing a decoded action callback：

```erlang
async_r_t:run_scope(CallbackAction, CallbacksGS,
    Callback#callback.acc_ref, Callback#callback.sf_cc_mref, CleanedState, MR).
```

promise suspension writer calls `get_sf_cc_ref` and copies it into record。所有 public runner execution 都携带
顶层 CcRef；只在显式没有 sf-cc scope 的内部执行中产生 `sf_cc_mref = undefined`。

### Explicit `SfContext`

`async_t` internal representation MUST 是接收 `(K, SfContext)` 的 function。`SfContext` 是
`undefined | #{continuation := term(), runner := fun()}`，通过参数和 Monad composition 显式传递，不得写入
进程字典。

- ordinary `>>=` 创建 anonymous continuation 并透传同一个 `SfContext`；
- `bind_frame/4` 是唯一创建 `sf_next` 的入口；
- Promise 仅在 `K =:= SfContext.runner` 时保存 structured continuation，否则保存实际 anonymous `K`；
- `bind_frame` 收到 ordinary anonymous downstream 时，注册独立 CcRef descriptor 保存该 downstream，
  并构造以该 CcRef 终止的 frame chain。

## 7. Callback Dictionary Classification

Introduce a classifier：

```erlang
classify_callback_value(Value) ->
    {action_callback, DecodedCallback}
  | sf_cc_ref
  | other.
```

`handle_reference` MUST only execute `action_callback`。It MUST return error/unhandled for `sf_cc_ref` and `other`。
`callback_exists` MUST use classifier and report true only for action callbacks。`sf_cc_ref` entry remains discoverable by
exact CcRef lookup and sf-specific enumeration。

## 8. Completion Algorithm

Suggested internal contract：

```erlang
complete(CcRef, Outcome, OffsetOrLens, State) ->
    {ok, CompletionValue, NewState}
  | {unhandled, State}
  | {error, Error, ConsumedState}.
```

If project layering requires `/3`, bind lens before call；observable semantics remain identical。

Pseudo algorithm：

```erlang
complete(CcRef, Outcome, Lens, State0) ->
    Callbacks0 = get_callbacks(Lens, State0),
    case take_valid_sf_cc(CcRef, Callbacks0) of
        error ->
            {unhandled, State0};
        {ok, FinalCc, Callbacks1} ->
            {OwnedCallbacks, Callbacks2} = take_owned_callbacks(CcRef, Callbacks1),
            State1 = put_callbacks(Lens, Callbacks2, State0),
            CleanState = cleanup_lifecycle(OwnedCallbacks, State1),
            invoke_and_normalize(FinalCc, Outcome, CleanState)
    end.
```

Preflight MUST validate CcRef entry and compute cleanup targets without side effects。If target shape is invalid, return error with
State0。Once entry is removed, all later failures return/propagate with consumed CleanState。

Cleanup details：

- demonitor with flush when monitor exists；
- cancel timer and ensure timeout message cannot re-complete；
- remove owned ActionMRefs；
- remove accumulator refs only when they are no longer referenced by a surviving callback；
- do not remove callback entries with a different CcRef；
- invoke user code only after cleanup。

## 9. Invocation and Result Normalization

```erlang
invoke(#{mode := async_r_t, callback := Callback, context := Context},
       Outcome, CleanState)
  when is_function(Callback, 2) ->
    case Callback(Outcome, Context) of
        {async_r_t, _} = Effect -> run_completion_effect(Effect, CleanState);
        Other -> error({invalid_sf_cc_return, async_r_t, Other})
    end;

invoke(#{mode := direct_state, callback := Callback, context := Context},
       Outcome, CleanState)
  when is_function(Callback, 2) ->
    _ = Callback(Outcome, Context),
    {ok, Outcome, CleanState};

invoke(#{mode := direct_state, callback := Callback, context := Context},
       Outcome, CleanState)
  when is_function(Callback, 3) ->
    NewState = Callback(Outcome, Context, CleanState),
    case async_util:same_type_state(NewState, CleanState) of
        true -> {ok, Outcome, NewState};
        false -> error({invalid_sf_cc_state, NewState})
    end.
```

ordinary runner 通过 `sf_cc(mode = async_r_t)` adapter 复用相同 invocation path。adapter callback 接收
`(Outcome, OriginalCc)` 并执行 `OriginalCc(Outcome)`；该 library-generated callback 不做 external-fun source
检查。wait 复用 ordinary runner normalization。

`run_completion_effect` MUST execute without reusing the consumed CcRef。Its produced value follows the existing
`run_sf_cc/3` async_r_t result contract。若 completion effect 显式调用任一 public runner，该新 invocation
按统一规则创建自己的 CcRef；runtime 不为裸 effect 隐式复用旧 CcRef。

## 10. Frame Terminal Upgrade

Replace full terminal descriptor in `sf_continuation` with：

```erlang
#{kind => sf_cc_ref, protocol => 1, mref => CcRef}.
```

Update：

- `validate_continuation`；
- `strip_runtime_metadata`；
- `terminal_cc`/completion dispatch；
- nested `sf_next` construction；
- structural/directional failure；
- continuation fixtures and kind walkers。

`terminal_cc` SHOULD become `terminal_cc_ref` and return only CcRef。Structural failure has decoded callback available and
SHOULD use callback field directly；terminal traversal is for normal chain execution/consistency validation only。

Frame summary MUST preserve each frame's owner module and optional module Vsn。Vsn is literal term or `undefined`。

### Ordinary action continuation failure

ordinary action callback is invoked directly as `Callback(Input)`。`badfun`、`undef` and callback-internal exceptions
MUST propagate unchanged。action executor MUST NOT classify them as sf runtime failure, synthesize an Outcome, invoke
CcRef completion or return a compatibility wrapper。

This boundary differs from final descriptor invocation：external final callbacks can be checked with
`code:ensure_loaded/1` and `erlang:function_exported/3` immediately before apply；the apply boundary MAY catch only
`error:{badfun, Callback}` for the exact callback being invoked。It MUST NOT catch nested `badfun` or `undef`。

## 11. Inspection

```erlang
inspect_sf_callbacks(Matcher, Offset, State) -> Result.
```

Algorithm：

1. snapshot callbacks dictionary；
2. enumerate only decoded action callback values；
3. sort by RequestMRef term order；
4. build sanitized summary；
5. call Matcher once per summary；
6. collect summaries for which result is exactly `true`；
7. matcher exception propagates unchanged；non-boolean raises `{invalid_matcher_return, Value}`；State is never written。

Frame flattening MUST be total over valid stable continuation and return `continuation_kind = unknown` plus empty/safely parsed
frames for damaged shapes；inspection itself must not invoke continuation or final CC。

## 12. Active Failure

```erlang
fail_sf_callbacks(Matcher, Reason, Offset, State0) -> Result.
```

Preflight is the same snapshot/sort/summary/match pass as inspection。No mutation before it succeeds。

Partition selected entries：

- `sf_cc_mref = undefined` -> unprotected result；
- reference CcRef -> group/dedupe by CcRef。

Commit sorted unique CcRefs：

```erlang
Outcome = {error, {sf_aborted, Reason}},
complete(CcRef, Outcome, Lens, CurrentState).
```

Result shapes：

```erlang
% protected CcRef
#{
    sf_cc_mref => CcRef,
    status => completed | cc_failed | unhandled
}

% unprotected callback
#{
    request_mref => MRef,
    sf_cc_mref => undefined,
    status => unprotected
}
```

`cc_failed` classifies invalid descriptor before consumption, or expected unavailable callback/invalid return after
consumption；it carries no exception payload。callback/effect internal exceptions propagate unchanged and stop commit。
Already-consumed CcRefs remain consumed；do not attempt rollback。

## 13. Metadata and Vsn

Summary builder for frame data returns：

```erlang
#{
    module => Module,
    vsn => VsnOrUndefined,
    protocol => Protocol,
    block_path => BlockPath,
    step_name => StepName,
    async_id => AsyncId
}.
```

Do not expose MFA function name unless separately approved；Module is enough for specified selector surface。Do not expose Env。

`-vsn(Vsn)` accepts a literal Erlang term。When reading OTP attributes, normalize the attribute container so summary receives
the literal Vsn term, not `[Vsn]`。Absent/ambiguous Vsn yields `undefined` for this capability；sf-migrate may impose stronger
version requirements。

## 14. Error Shapes

Compiler：

```erlang
{sf_cc_external_fun_required, Runner}
{sf_cc_unsupported_arity, Runner, Actual, Allowed}
```

Runtime structural：

```erlang
{sf_runtime, Reason}
{invalid_sf_cc, Value}
{invalid_sf_cc_return, Value}
```

Matcher：

```erlang
{invalid_matcher, Matcher}
{invalid_matcher_return, Value}
```

Matcher execution exceptions are not converted to matcher-specific tuples。

Manual final Outcome：

```erlang
{error, {sf_aborted, UserReason}}
```

## 15. Test Matrix

Compiler tests：external `/2|3` acceptance matrix；local/anonymous/variable/wrong arity rejection；multiple diagnostics。

Runtime tests：

- `wait/1`、`wait_t/2`、ordinary runners 和 sf-prefixed runners 均创建/传播 CcRef，同时保持各自正常结果；
- immediate/suspended ordinary Promise；
- immediate/suspended/nested frame Promise；
- sequential ActionMRef CcRef inheritance；
- AccRef rebinding while CcRef remains；
- async_r_t `/2` State access；
- direct `/2` ignored return；
- direct `/3` CleanState/NewState；
- invalid returns and callback crash after consume；
- reply/message/timeout/DOWN；
- late reply and duplicate fail；
- same-CcRef internal callbacks cleaned once；
- unrelated callbacks preserved；
- current four-tuple decoding and unprotected inspection；
- ordinary action callback `badfun`/`undef` propagates without sf-cc reclassification；
- exact final-callback availability handling does not catch nested `badfun`/`undef`；
- matcher metadata, privacy, propagation atomicity, dedupe and per-CcRef results without exception payload；
- map/list/dict callbacks backends；
- `callback_exists` ignores standalone CcRef entries。

Integration：`rebar3 compile`、EUnit、Common Test、xref、dialyzer and supported OTP matrix。
