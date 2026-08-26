%%%-------------------------------------------------------------------
%%% Stable sf-do continuation validation and execution.
%%%-------------------------------------------------------------------
-module(async_sf_runtime).

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
    module := module(),
    vsn := term(),
    mfa := {module(), atom(), 2},
    env := #{atom() => term()}
}.
-type sf_cc_ref() :: #{kind := sf_cc_ref, protocol := 1, mref := reference()}.
-type sf_continuation() ::
    #{kind := sf_next, frame := sf_frame(), downstream := sf_continuation()}
    | #{kind := sf_legacy_next, protocol := 1, callback := fun((term()) -> term()),
        downstream := sf_continuation()}
    | sf_cc_ref().

-spec new_frame(monad_frame:frame_mfa(), monad_frame:frame_context()) -> sf_frame().
new_frame(
    {Module, Function, 2} = MFA,
    #{
        protocol := 1,
        block_path := BlockPath,
        step_name := StepName,
        async_id := AsyncId,
        module := OwnerModule,
        vsn := Vsn,
        env := Env
    } = Context
) when
    is_atom(Module),
    is_atom(Function),
    map_size(Context) =:= 7,
    is_list(BlockPath),
    (is_atom(StepName) orelse StepName =:= undefined),
    is_binary(AsyncId),
    is_atom(OwnerModule),
    OwnerModule =:= Module,
    is_map(Env)
->
    #{
        kind => sf_frame,
        protocol => 1,
        block_path => BlockPath,
        step_name => StepName,
        async_id => AsyncId,
        module => OwnerModule,
        vsn => Vsn,
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
        {error, Reason} -> structural_failure(Reason, Inner)
    end.

continue_valid(Continuation, Input, {async_t, Inner} = Monad, Direction) ->
    case Continuation of
        #{kind := sf_next, frame := Frame, downstream := Downstream} ->
            case resolve_frame(Frame, Direction) of
                {ok, ResolvedFrame} ->
                    #{mfa := {Module, Function, 2}, env := Env} = ResolvedFrame,
                    Action = apply(Module, Function, [Input, Env]),
                    async_t:run_sf_action(Action, Downstream, Monad);
                {error, {appup, incompatible_frame} = Outcome} ->
                    directional_failure(Outcome, Inner);
                {error, {appdown, incompatible_frame} = Outcome} ->
                    directional_failure(Outcome, Inner);
                {error, Reason} ->
                    structural_failure(Reason, Inner)
            end;
        #{kind := sf_legacy_next, callback := Callback} ->
            try Callback(Input)
            catch
                error:{badfun, Callback} ->
                    complete_current(
                        {error, {sf_runtime, unavailable_continuation}}, Inner)
            end;
        #{kind := sf_cc_ref, mref := CcRef} ->
            async_sf_cc:dispatch(CcRef, Input, async_r_t:new(Inner))
    end.

-spec validate_continuation(term()) -> ok | {error, term()}.
validate_continuation(#{kind := sf_cc_ref} = Terminal) ->
    async_sf_cc:validate_terminal(Terminal);
validate_continuation(
    #{kind := sf_next, frame := Frame, downstream := Downstream}
) ->
    case validate_frame_shape(Frame) of
        ok -> validate_continuation(Downstream);
        Error -> Error
    end;
validate_continuation(#{kind := sf_legacy_next, protocol := 1,
        callback := Callback, downstream := Downstream}) when is_function(Callback, 1) ->
    validate_continuation(Downstream);
validate_continuation(#{protocol := Protocol}) when Protocol =/= 1 ->
    {error, {unsupported_protocol, Protocol}};
validate_continuation(Other) ->
    {error, {invalid_continuation, Other}}.

-spec validate_frame(sf_frame(), resume | appup | appdown) -> ok | {error, term()}.
validate_frame(Frame, Direction) ->
    case resolve_frame(Frame, Direction) of
        {ok, _} -> ok;
        Error -> Error
    end.

resolve_frame(Frame, Direction) ->
    case validate_frame_shape(Frame) of
        ok ->
            case validate_migration_name(Frame, Direction) of
                ok -> resolve_current_manifest(Frame, Direction);
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
    module := OwnerModule,
    vsn := _Vsn,
    mfa := {Module, Function, 2},
    env := Env
}) when
    is_list(BlockPath),
    (is_atom(StepName) orelse StepName =:= undefined),
    is_binary(AsyncId),
    is_atom(OwnerModule),
    is_atom(Module),
    is_atom(Function),
    OwnerModule =:= Module,
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

resolve_current_manifest(
    #{module := Module, mfa := MFA} = Frame,
    Direction
) ->
    case current_manifest(Module, MFA) of
        {ok, Manifest} -> resolve_manifest_frame(Frame, Manifest, Direction);
        {error, Reason} -> target_failure(Direction, Reason)
    end.

current_manifest(Module, {_StoredModule, _Function, 2} = MFA) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case erlang:function_exported(Module, '$async_sf_manifest', 0) of
                false -> {error, {missing_manifest, MFA}};
                true -> invoke_manifest(Module, MFA)
            end;
        _ ->
            {error, {missing_manifest_or_mfa, MFA}}
    end.

invoke_manifest(Module, MFA) ->
    try {ok, Module:'$async_sf_manifest'()}
    catch
        _:_ -> {error, {missing_manifest_or_mfa, MFA}}
    end.

resolve_manifest_frame(
    #{
        block_path := BlockPath,
        step_name := StepName,
        async_id := AsyncId,
        mfa := MFA,
        env := Env
    } = Frame,
    #{format := 1, protocol := 1, module := Module, vsn := CurrentVsn,
        blocks := Blocks},
    Direction
) when is_atom(Module), is_map(Blocks) ->
    case maps:find(BlockPath, Blocks) of
        {ok, #{asyncs := Asyncs}} when is_map(Asyncs) ->
            resolve_manifest_entry(
                maps:find(AsyncId, Asyncs),
                BlockPath,
                StepName,
                AsyncId,
                MFA,
                Env,
                Direction,
                Frame,
                Module,
                CurrentVsn
            );
        _ ->
            target_failure(Direction, {missing_manifest_or_mfa, MFA})
    end;
resolve_manifest_frame(#{mfa := MFA}, _Manifest, Direction) ->
    target_failure(Direction, {missing_manifest_or_mfa, MFA}).

resolve_manifest_entry(
    {ok, #{mfa := CurrentMFA, step_name := StepName, required_captures := Required}},
    BlockPath,
    StepName,
    AsyncId,
    _StoredMFA,
    Env,
    Direction,
    Frame,
    Module,
    CurrentVsn
) when is_list(Required) ->
    case validate_named_identity(BlockPath, StepName, AsyncId) of
        ok ->
            case lists:all(fun(Name) -> maps:is_key(Name, Env) end, Required) of
                true ->
                    case current_mfa_callable(Module, CurrentMFA) of
                        true -> {ok, Frame#{module := Module, vsn := CurrentVsn,
                            mfa := CurrentMFA}};
                        false -> target_failure(Direction,
                            {missing_generated_mfa, CurrentMFA})
                    end;
                false -> incompatible(Direction)
            end;
        error ->
            target_failure(Direction, {missing_manifest_or_mfa, CurrentMFA})
    end;
resolve_manifest_entry(_Entry, _BlockPath, _StepName, _AsyncId, MFA, _Env,
        Direction, _Frame, _Module, _CurrentVsn) ->
    target_failure(Direction, {missing_manifest_or_mfa, MFA}).

current_mfa_callable(Module, {Module, Function, 2}) when is_atom(Function) ->
    erlang:function_exported(Module, Function, 2);
current_mfa_callable(_Module, _MFA) -> false.

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
strip_runtime_metadata(#{kind := sf_legacy_next} = Continuation) ->
    maps:remove(timer, Continuation);
strip_runtime_metadata(#{kind := sf_cc_ref} = Continuation) ->
    maps:remove(timer, Continuation).

structural_failure(Reason, Inner) ->
    complete_current({error, {sf_runtime, Reason}}, Inner).

directional_failure(Outcome, Inner) ->
    Error = {error, Outcome},
    complete_current(Error, Inner).

complete_current(Outcome, Inner) ->
    MR = async_r_t:new(Inner),
    monad:'>>='(
        async_r_t:get_sf_cc_ref(MR),
        fun
            (CcRef) when is_reference(CcRef) -> async_sf_cc:complete(CcRef, Outcome, MR);
            (undefined) -> async_r_t:return(Outcome, MR)
        end,
        MR
    ).
