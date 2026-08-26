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
- legacy/frame executor dispatch；
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

`handle_info/3` 还负责把无法恢复的 pre-upgrade old tuple 返回为 tagged legacy outcome。

### `async_r_t`

负责 reader-local `{AccRef, SfCcMRef}` scope compatibility，不拥有 completion policy。

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

不要让 `async_t` 与 `async_util` 继续各自声明 record。reader 不能仅写：

```erlang
{ok, #callback{} = Callback}
```

而必须先统一 decode：

```erlang
decode_callback({callback, Cc, AccRef}) ->
    {ok, #callback{cc = Cc, acc_ref = AccRef, sf_cc_mref = undefined}};
decode_callback(#callback{} = Callback) ->
    {ok, Callback};
decode_callback(_) ->
    error.
```

注意：用新 record 构造旧 tuple 会得到四元 tuple；old shape 必须用显式 tuple clause 读取。所有以下入口必须
审计：

- `handle_reference`；
- callback executor；
- timeout wrapper；
- `callback_exists`；
- migration callback enumeration；
- inspect/fail enumeration；
- test/version compatibility helpers。

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

-type legacy_cc() :: #{
    kind := legacy_cc,
    protocol := 1,
    callback := fun()
}.

-type final_cc() :: sf_cc() | legacy_cc().

-type sf_cc_ref_entry() :: #{
    kind := sf_cc_ref,
    protocol := 1,
    cc := final_cc()
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

所有 post-upgrade public runner MUST 委托一个 common runner core。该 core 在 effect 真正执行时 allocate/register
CcRef、安装 scope、执行 Promise，并把 immediate/suspended completion 都交给同一 completion primitive。
各 facade 只负责构造 final descriptor 和保留自己的 observable normalization/result projection。

### `run_sf_cc/3`

返回一个 `async_r_t` program。在该 program 真正执行时：

1. allocate CcRef；
2. build `sf_cc(mode = async_r_t)`；
3. write CcRef entry through current callbacks lens；
4. install scope `{AccRef, CcRef}`；
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

- `run_cc/2`、`run_with_cc/4`、`exec_cc/4`、`run/4` 使用 `legacy_cc` descriptor 保存现有 final
  adapter，保持原 callback arity、normalization 和正常返回合同；
- `wait/1`、`wait_t/2` 使用 library-owned exported completion，保持原阻塞/超时和返回合同；
- 这些 facade 不做 external-fun source validation；
- 它们创建的每个 suspended callback 与 sf-prefixed runner 一样写入 CcRef；
- anonymous legacy final adapter 自身仍可能因 purge 不可调用，不获得 sf-prefixed external-fun guarantee。

## 6. Reader-Local Scope

Represent internal reader local as:

```erlang
-type async_scope() :: #{
    acc_ref := reference(),
    sf_cc_mref := reference() | undefined
}.
```

compatibility helpers：

```erlang
decode_scope(Ref) when is_reference(Ref) ->
    #{acc_ref => Ref, sf_cc_mref => undefined};
decode_scope(#{acc_ref := Ref, sf_cc_mref := SfCcMRef} = Scope)
        when is_reference(Ref),
             (is_reference(SfCcMRef) orelse SfCcMRef =:= undefined) ->
    Scope.
```

`get_local_ref/1` MUST ask then extract `acc_ref`。新增 internal `get_sf_cc_mref/1` extracts second field。
`local_ref/3` MUST map only `acc_ref`。`async_r_t:run` old callers may continue passing raw Ref；sf runners pass Scope。

When executing a decoded action callback：

```erlang
Scope = #{
    acc_ref => Callback#callback.acc_ref,
    sf_cc_mref => Callback#callback.sf_cc_mref
},
async_r_t:run(CallbackAction, CallbacksGS, Scope, CleanedState).
```

promise suspension writer calls `get_sf_cc_mref` and copies it into record。所有新 runner scope 都携带 CcRef；
只有解码升级前旧三元 tuple 后恢复的 scope yields `undefined`。

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

`invoke_and_normalize/3` 还 MUST dispatch `legacy_cc`，调用其中现有 `CC/1` adapter，并复用原 ordinary
runner 的 result normalization/projection。该分支不重新解释为 sf `/2|3` contract，也不做 external-fun
source 检查。wait 的 library-owned completion 由库控制，可安全以 stable descriptor 或专用等价 tag 分派。

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

### Pre-upgrade old tuple unavailable path

Decoded `{callback, Cc, AccRef}` has `sf_cc_mref = undefined` and MUST NOT enter `complete/3`。If a narrow
pre-invocation classifier confirms its anonymous `Cc` targets purged old code, the action executor MUST remove only the
request lifecycle it can identify and own, producing `CleanState` without fabricating a final result。The facade contract is：

```erlang
async_m:handle_info(Message, Offset, State0) ->
    {legacy_outcome, Outcome, CleanState} | ExistingHandleInfoResult.
```

`Outcome` is the exact action input that would have been supplied to `Cc` for reply、timeout、DOWN or message。
Do not create an orphan error, retry `Cc`, allocate a retroactive CcRef or invoke a final callback。The classifier MUST
distinguish failure to enter purged code from an exception after callback entry；ordinary callback exceptions and nested
`undef` keep existing behavior。

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
7. exception/non-boolean returns error；State is never written。

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

Catch callback exception per CcRef after consumption：

```erlang
#{
    sf_cc_mref => CcRef,
    request_mrefs => [MRef],
    status => completed | cc_failed | already_consumed | unprotected,
    error => undefined | {Class, Reason, Stacktrace}
}.
```

Continue subsequent CcRefs。Return final threaded State。Do not attempt rollback。

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
{invalid_sf_cc_return, Mode, Value}
{invalid_sf_cc_state, Value}
```

Matcher：

```erlang
{sf_cc_matcher_failed, RequestMRef, Class, Reason, Stacktrace}
{sf_cc_matcher_non_boolean, RequestMRef, Value}
```

Manual final Outcome：

```erlang
{error, {sf_aborted, UserReason}}
```

## 15. Test Matrix

Compiler tests：external `/2|3` acceptance matrix；local/anonymous/variable/wrong arity rejection；multiple diagnostics。

Runtime tests：

- `wait/1`、`wait_t/2`、ordinary runners 和 sf-prefixed runners 均创建/传播 CcRef，同时保持各自正常结果；
- immediate/suspended legacy Promise；
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
- old three-tuple resume and unprotected inspection；
- purged old three-tuple cleanup and exact `{legacy_outcome, Outcome, CleanState}` receive-loop return；
- ordinary callback exception/nested `undef` is not misclassified as unavailable old code；
- matcher metadata, privacy, exception atomicity, dedupe and per-CcRef continuation；
- map/list/dict callbacks backends；
- `callback_exists` ignores standalone CcRef entries。

Integration：`rebar3 compile`、EUnit、Common Test、xref、dialyzer and supported OTP matrix。
