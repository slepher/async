%%%-------------------------------------------------------------------
%%% Stable sf-do continuation validation and execution.
%%%-------------------------------------------------------------------
-module(async_sf_runtime).

-export([run_sf_frame/4, invoke_sf_cc/2]).
-export([
    continue/3,
    continue/4,
    new_frame/2,
    new_next/2,
    validate_continuation/1,
    validate_frame/2,
    strip_runtime_metadata/1
]).

-type sf_frame() :: #{
    kind := sf_frame,
    protocol := 1,
    block_path := [atom()],
    step_name := atom() | undefined,
    async_id := binary(),
    mfa := {module(), atom(), 2},
    env := #{atom() => term()}
}.
-type sf_cc() :: #{kind := sf_cc, protocol := 1, mfa := {module(), atom(), 2}, context := term()}.
-type sf_continuation() ::
    #{kind := sf_next, frame := sf_frame(), downstream := sf_continuation()}
    | sf_cc().

-spec run_sf_frame(monad:m(identity, A), sf_cc(), integer(), S) -> {A, S}.
run_sf_frame(Action, SfCc, Offset, State) ->
    case validate_continuation(SfCc) of
        ok ->
            identity:run(async_t:run_sf_action(Action, SfCc, Offset, State));
        {error, Reason} ->
            erlang:error({invalid_sf_cc, Reason})
    end.

-spec invoke_sf_cc(sf_cc(), term()) -> term().
invoke_sf_cc(
    #{kind := sf_cc, protocol := 1, mfa := {Module, Function, 2}, context := Context},
    Outcome
) when is_atom(Module), is_atom(Function) ->
    apply(Module, Function, [Outcome, Context]);
invoke_sf_cc(SfCc, _Outcome) ->
    erlang:error({invalid_sf_cc, SfCc}).

-spec new_frame(monad_frame:frame_mfa(), monad_frame:frame_context()) -> sf_frame().
new_frame(
    {Module, Function, 2} = MFA,
    #{
        protocol := 1,
        block_path := BlockPath,
        step_name := StepName,
        async_id := AsyncId,
        env := Env
    } = Context
) when
    is_atom(Module),
    is_atom(Function),
    map_size(Context) =:= 5,
    is_list(BlockPath),
    (is_atom(StepName) orelse StepName =:= undefined),
    is_binary(AsyncId),
    is_map(Env)
->
    #{
        kind => sf_frame,
        protocol => 1,
        block_path => BlockPath,
        step_name => StepName,
        async_id => AsyncId,
        mfa => MFA,
        env => Env
    };
new_frame(MFA, Context) ->
    erlang:error({invalid_frame_context, MFA, Context}).

-spec new_next(sf_frame(), sf_continuation()) -> sf_continuation().
new_next(Frame, Downstream) ->
    case {validate_frame_shape(Frame), validate_continuation(Downstream)} of
        {ok, ok} -> #{kind => sf_next, frame => Frame, downstream => Downstream};
        {Error, _} when Error =/= ok -> erlang:error(Error);
        {_, Error} -> erlang:error(Error)
    end.

-spec continue(sf_continuation(), term(), monad:class()) -> term().
continue(Continuation0, Input, {async_t, _Inner} = Monad) ->
    continue(Continuation0, Input, Monad, resume).

-spec continue(sf_continuation(), term(), monad:class(), resume | appup | appdown) -> term().
continue(Continuation0, Input, {async_t, Inner} = Monad, Direction) ->
    Continuation = strip_runtime_metadata(Continuation0),
    case validate_continuation(Continuation) of
        ok -> continue_valid(Continuation, Input, Monad, Direction);
        {error, Reason} -> structural_failure(Continuation, Reason, Inner)
    end.

continue_valid(Continuation, Input, {async_t, Inner} = Monad, Direction) ->
    case Continuation of
        #{kind := sf_next, frame := Frame, downstream := Downstream} ->
            case validate_frame(Frame, Direction) of
                ok ->
                    #{mfa := {Module, Function, 2}, env := Env} = Frame,
                    Action = apply(Module, Function, [Input, Env]),
                    async_t:run_sf_action(Action, Downstream, Monad);
                {error, {appup, incompatible_frame} = Outcome} ->
                    directional_failure(Downstream, Outcome, Inner);
                {error, {appdown, incompatible_frame} = Outcome} ->
                    directional_failure(Downstream, Outcome, Inner);
                {error, Reason} ->
                    structural_failure(Downstream, Reason, Inner)
            end;
        #{kind := sf_cc} = SfCc ->
            complete(SfCc, Input, Inner)
    end.

-spec validate_continuation(term()) -> ok | {error, term()}.
validate_continuation(#{kind := sf_cc, protocol := 1, mfa := {M, F, 2}}) when
    is_atom(M), is_atom(F)
->
    ok;
validate_continuation(
    #{kind := sf_next, frame := Frame, downstream := Downstream}
) ->
    case validate_frame_shape(Frame) of
        ok -> validate_continuation(Downstream);
        Error -> Error
    end;
validate_continuation(#{protocol := Protocol}) when Protocol =/= 1 ->
    {error, {unsupported_protocol, Protocol}};
validate_continuation(Other) ->
    {error, {invalid_continuation, Other}}.

-spec validate_frame(sf_frame(), resume | appup | appdown) -> ok | {error, term()}.
validate_frame(Frame, Direction) ->
    case validate_frame_shape(Frame) of
        ok ->
            case validate_migration_name(Frame, Direction) of
                ok -> validate_current_manifest(Frame, Direction);
                Error -> Error
            end;
        Error -> Error
    end.

%% This is the sole unnamed migration gate. It intentionally runs before
%% manifest lookup and before the stored/current generated MFA can be invoked.
validate_migration_name(#{step_name := undefined}, appup) ->
    incompatible(appup);
validate_migration_name(#{step_name := undefined}, appdown) ->
    incompatible(appdown);
validate_migration_name(_Frame, _Direction) ->
    ok.

validate_frame_shape(#{
    kind := sf_frame,
    protocol := 1,
    block_path := BlockPath,
    step_name := StepName,
    async_id := AsyncId,
    mfa := {Module, Function, 2},
    env := Env
}) when
    is_list(BlockPath),
    (is_atom(StepName) orelse StepName =:= undefined),
    is_binary(AsyncId),
    is_atom(Module),
    is_atom(Function),
    is_map(Env)
->
    case lists:all(fun is_atom/1, BlockPath) andalso lists:all(fun is_atom/1, maps:keys(Env)) of
        true -> ok;
        false -> {error, invalid_frame_fields}
    end;
validate_frame_shape(#{protocol := Protocol}) when Protocol =/= 1 ->
    {error, {unsupported_protocol, Protocol}};
validate_frame_shape(Other) ->
    {error, {invalid_frame, Other}}.

validate_current_manifest(
    #{mfa := MFA} = Frame,
    Direction
) ->
    case current_manifest(MFA) of
        {ok, Manifest} -> validate_manifest_frame(Frame, Manifest, Direction);
        {error, Reason} -> target_failure(Direction, Reason)
    end.

current_manifest({Module, Function, 2} = MFA) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case {
                erlang:function_exported(Module, '$async_sf_manifest', 0),
                erlang:function_exported(Module, Function, 2)
            } of
                {false, _} -> {error, {missing_manifest, MFA}};
                {true, false} -> {error, {missing_generated_mfa, MFA}};
                {true, true} -> invoke_manifest(Module, MFA)
            end;
        _ ->
            {error, {missing_manifest_or_mfa, MFA}}
    end.

invoke_manifest(Module, MFA) ->
    try {ok, Module:'$async_sf_manifest'()}
    catch
        _:_ -> {error, {missing_manifest_or_mfa, MFA}}
    end.

validate_manifest_frame(
    #{
        block_path := BlockPath,
        step_name := StepName,
        async_id := AsyncId,
        mfa := MFA,
        env := Env
    },
    #{format := 1, protocol := 1, blocks := Blocks},
    Direction
) when is_map(Blocks) ->
    case maps:find(BlockPath, Blocks) of
        {ok, #{asyncs := Asyncs}} when is_map(Asyncs) ->
            validate_manifest_entry(
                maps:find(AsyncId, Asyncs),
                BlockPath,
                StepName,
                AsyncId,
                MFA,
                Env,
                Direction
            );
        _ ->
            target_failure(Direction, {missing_manifest_or_mfa, MFA})
    end;
validate_manifest_frame(#{mfa := MFA}, _Manifest, Direction) ->
    target_failure(Direction, {missing_manifest_or_mfa, MFA}).

validate_manifest_entry(
    {ok, #{mfa := MFA, step_name := StepName, required_captures := Required}},
    BlockPath,
    StepName,
    AsyncId,
    MFA,
    Env,
    Direction
) when is_list(Required) ->
    case validate_named_identity(BlockPath, StepName, AsyncId) of
        ok ->
            case lists:all(fun(Name) -> maps:is_key(Name, Env) end, Required) of
                true -> ok;
                false -> incompatible(Direction)
            end;
        error ->
            target_failure(Direction, {missing_manifest_or_mfa, MFA})
    end;
validate_manifest_entry(_Entry, _BlockPath, _StepName, _AsyncId, MFA, _Env, Direction) ->
    target_failure(Direction, {missing_manifest_or_mfa, MFA}).

validate_named_identity(_BlockPath, undefined, _AsyncId) ->
    ok;
validate_named_identity(BlockPath, StepName, AsyncId) ->
    Expected = crypto:hash(
        sha256, term_to_binary({sf_named_async, 1, BlockPath, StepName})
    ),
    case AsyncId =:= Expected of
        true -> ok;
        false -> error
    end.

target_failure(appup, _Reason) -> incompatible(appup);
target_failure(appdown, _Reason) -> incompatible(appdown);
target_failure(resume, Reason) -> {error, Reason}.

incompatible(appdown) -> {error, {appdown, incompatible_frame}};
incompatible(appup) -> {error, {appup, incompatible_frame}};
incompatible(resume) -> {error, incompatible_frame}.

-spec strip_runtime_metadata(sf_continuation()) -> sf_continuation().
strip_runtime_metadata(#{kind := sf_next} = Continuation) ->
    maps:remove(timer, Continuation);
strip_runtime_metadata(#{kind := sf_cc} = Continuation) ->
    maps:remove(timer, Continuation).

structural_failure(Continuation, Reason, Inner) ->
    case terminal_cc(Continuation) of
        {ok, SfCc} -> complete(SfCc, {error, {sf_runtime, Reason}}, Inner);
        error -> async_r_t:return({error, {sf_runtime, Reason}}, async_r_t:new(Inner))
    end.

directional_failure(Continuation, Outcome, Inner) ->
    Error = {error, Outcome},
    case terminal_cc(Continuation) of
        {ok, SfCc} -> complete(SfCc, Error, Inner);
        error -> async_r_t:return(Error, async_r_t:new(Inner))
    end.

terminal_cc(#{kind := sf_cc} = SfCc) ->
    {ok, strip_runtime_metadata(SfCc)};
terminal_cc(#{kind := sf_next, downstream := Downstream}) ->
    terminal_cc(Downstream);
terminal_cc(_) ->
    error.

complete(SfCc, Outcome, Inner) ->
    MR = async_r_t:new(Inner),
    Result = invoke_sf_cc(SfCc, Outcome),
    monad:'>>='(
        async_r_t:get_local_ref(MR),
        fun(AccRef) ->
            monad:'>>='(
                normalize_completion(Result, Outcome, MR),
                fun(Value) ->
                    monad:'>>'(
                        async_r_t:remove_ref(AccRef, MR),
                        async_r_t:return(Value, MR),
                        MR
                    )
                end,
                MR
            )
        end,
        MR
    ).

normalize_completion({async_r_t, _} = Result, _Outcome, _MR) ->
    Result;
normalize_completion(Result, Outcome, MR) ->
    monad:'>>='(
        async_r_t:get_state(MR),
        fun(State) ->
            case async_util:same_type_state(Result, State) of
                true ->
                    monad:'>>'(
                        async_r_t:put_state(Result, MR),
                        async_r_t:return(Outcome, MR),
                        MR
                    );
                false ->
                    async_r_t:return(Result, MR)
            end
        end,
        MR
    ).
