%%% Current sf-do frame validation, resolution and invocation.
-module(async_sf_runtime).

-export([new_frame/2, continue/5]).

new_frame({Module, Function, 2} = MFA,
        #{protocol := 1, block := Block, step_name := StepName,
            identity := Identity, module := Module, vsn := Vsn, env := Env} = Context)
        when is_atom(Module), is_atom(Function), is_atom(Block),
            (is_atom(StepName) orelse StepName =:= undefined), is_map(Env) ->
    Frame = #{kind => sf_frame, protocol => 1, block => Block,
        step_name => StepName, identity => Identity, module => Module,
        vsn => Vsn, mfa => MFA, env => Env},
    case frame_shape(Frame) of
        ok when map_size(Context) =:= 7 -> Frame;
        ok -> erlang:error({invalid_frame_context, MFA, Context});
        {error, Reason} -> erlang:error({invalid_frame_context, Reason})
    end;
new_frame(MFA, Context) -> erlang:error({invalid_frame_context, MFA, Context}).

continue(Frame, ParentRef, Input, {async_r_t, _} = MR, Direction) ->
    case resolve_frame(Frame, Direction) of
        {ok, #{mfa := {Module, Function, 2}, env := Env} = Resolved} ->
            case invoke_frame(Module, Function, Input, Env) of
                {ok, {async_t, _} = Action} -> run_remainder(Action, ParentRef, MR);
                {ok, {sf_migration_failure, Reason}}
                        when map_get(resolution, Resolved) =:= migration ->
                    structural_failure(Reason, ParentRef, MR);
                {ok, _Other} when map_get(resolution, Resolved) =:= migration ->
                    structural_failure(invalid_migration_bridge, ParentRef, MR);
                {ok, Other} -> structural_failure({invalid_frame_return, Other}, ParentRef, MR);
                unavailable when map_get(resolution, Resolved) =:= migration ->
                    structural_failure(invalid_migration_bridge, ParentRef, MR);
                unavailable -> structural_failure(unavailable_continuation, ParentRef, MR)
            end;
        {error, Reason} -> structural_failure(Reason, ParentRef, MR)
    end.

run_remainder(Action, ParentRef, MR) ->
    Runner = parent_runner(ParentRef, MR),
    Context = #{current => {ref, ParentRef}, parent_ref => ParentRef, runner => Runner},
    (async_t:run_async_t(Action))(Runner, Context).

parent_runner(none, MR) -> fun(Input) -> async_r_t:return(Input, MR) end;
parent_runner(ParentRef, MR) ->
    fun
        ({message, _} = Input) -> async_sf_cc:dispatch_message(ParentRef, Input, MR);
        (Input) -> async_sf_cc:dispatch(ParentRef, Input, MR)
    end.

structural_failure(Reason, none, MR) ->
    async_r_t:return({error, {sf_runtime, Reason}}, MR);
structural_failure(Reason, ParentRef, MR) ->
    async_sf_cc:dispatch_failure(ParentRef, {error, {sf_runtime, Reason}}, MR).

invoke_frame(Module, Function, Input, Env) ->
    case erlang:function_exported(Module, Function, 2) of
        true -> {ok, erlang:apply(Module, Function, [Input, Env])};
        false -> unavailable
    end.

frame_shape(#{kind := sf_frame, protocol := 1, block := Block,
        step_name := StepName, identity := Identity, module := Module,
        vsn := _Vsn, mfa := {Module, Function, 2}, env := Env})
        when is_atom(Block), (is_atom(StepName) orelse StepName =:= undefined),
            is_atom(Module), is_atom(Function), is_map(Env) ->
    case valid_identity(Identity, Block, StepName)
            andalso lists:all(fun is_atom/1, maps:keys(Env)) of
        true -> ok;
        false -> {error, invalid_frame_fields}
    end;
frame_shape(#{protocol := Protocol}) when Protocol =/= 1 ->
    {error, {unsupported_protocol, Protocol}};
frame_shape(Other) -> {error, {invalid_frame, Other}}.

valid_identity(undefined, _Block, undefined) -> true;
valid_identity({1, Block, StepName}, Block, StepName) when is_atom(StepName) -> true;
valid_identity(_, _, _) -> false.

resolve_frame(Frame, Direction) ->
    case frame_shape(Frame) of
        ok -> resolve_current_manifest(Frame, Direction);
        Error -> Error
    end.

resolve_current_manifest(#{module := Module, mfa := StoredMFA} = Frame, Direction) ->
    case current_manifest(Module, StoredMFA) of
        {ok, Manifest} -> resolve_manifest_frame(Frame, Manifest, Direction);
        {error, Reason} -> target_failure(Direction, Reason)
    end.

current_manifest(Module, MFA) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case erlang:function_exported(Module, '$async_sf_manifest', 0) of
                true -> {ok, Module:'$async_sf_manifest'()};
                false -> {error, {missing_manifest, MFA}}
            end;
        {error, nofile} -> {error, {missing_manifest_or_mfa, MFA}};
        {error, Reason} -> {error, Reason}
    end.

resolve_manifest_frame(#{protocol := 1, block := Block, step_name := StepName,
        identity := Identity, module := Module, vsn := StoredVsn,
        mfa := _StoredMFA, env := _Env} = Frame,
        #{format := 1, protocol := 1, owner := Module, vsn := CurrentVsn,
            frames := Entries} = Manifest, Direction) when is_list(Entries) ->
    case StoredVsn =:= CurrentVsn of
        true -> resolve_automatic(Frame, Entries, CurrentVsn, Direction);
        false when is_atom(Block), is_atom(StepName),
                Identity =:= {1, Block, StepName} ->
            resolve_migration(Frame, Manifest, Direction);
        false -> resolve_automatic(Frame, Entries, CurrentVsn, Direction)
    end;
resolve_manifest_frame(#{mfa := MFA}, _Manifest, Direction) ->
    target_failure(Direction, {missing_manifest_or_mfa, MFA}).

resolve_automatic(#{protocol := 1, block := Block, step_name := StepName,
        identity := Identity, module := Module, vsn := StoredVsn,
        mfa := StoredMFA, env := Env} = Frame, Entries, CurrentVsn, Direction) ->
    case find_entry(Identity, StoredMFA, StepName, StoredVsn, CurrentVsn, Entries,
            Direction) of
        {ok, #{identity := Identity, mfa := CurrentMFA,
                required_captures := Required}} when is_list(Required) ->
            case valid_required_captures(Required, Env)
                    andalso current_mfa_callable(Module, CurrentMFA) of
                true -> {ok, Frame#{vsn := CurrentVsn, mfa := CurrentMFA,
                    block := Block, resolution => automatic}};
                false -> target_failure(Direction,
                    {incompatible_frame, {missing_capture_or_mfa, CurrentMFA}})
            end;
        error -> target_failure(Direction, {missing_manifest_or_mfa, StoredMFA})
    end.

resolve_migration(#{module := Module, vsn := StoredVsn, block := StoredBlock} = Frame,
        #{vsn := CurrentVsn, frames := Entries}, Direction) ->
    case erlang:function_exported(Module, sf_do_code_change, 2) of
        false -> resolve_automatic(Frame, Entries, CurrentVsn, Direction);
        true ->
            case Module:sf_do_code_change(StoredVsn, StoredBlock) of
                unhandled -> resolve_automatic(Frame, Entries, CurrentVsn, Direction);
                Plan when is_map(Plan) ->
                    resolve_migration_plan(Frame, Plan, CurrentVsn, Entries, Direction);
                _Other -> target_failure(Direction,
                    {invalid_migration_plan, invalid_return})
            end
    end.

resolve_migration_plan(#{module := Module, vsn := StoredVsn,
        block := StoredBlock, step_name := StoredStep} = Frame,
        Plan, CurrentVsn, Entries, Direction) ->
    case valid_migration_plan(
            Plan, Module, StoredVsn, StoredBlock, CurrentVsn, Entries) of
        {ok, Steps} ->
            case maps:find(StoredStep, Steps) of
                error -> resolve_automatic(Frame, Entries, CurrentVsn, Direction);
                {ok, StepPlan} ->
                    resolve_mapped_step(Frame, StepPlan, CurrentVsn, Entries,
                        Direction)
            end;
        {error, Reason} -> target_failure(Direction,
            {invalid_migration_plan, Reason})
    end.

valid_migration_plan(Plan, Module, StoredVsn, StoredBlock, CurrentVsn, Entries) ->
    case Plan of
        #{kind := sf_migration_plan, format := 1, owner := Module,
            source_vsn := StoredVsn, source_block := StoredBlock,
            target_vsn := CurrentVsn, target_block := TargetBlock,
            steps := Steps} when map_size(Plan) =:= 8, is_atom(TargetBlock),
                is_map(Steps) ->
            case lists:all(fun({Step, StepPlan}) ->
                    valid_step_plan(Step, StepPlan, Module, StoredBlock,
                        TargetBlock, Entries)
                end, maps:to_list(Steps)) of
                true -> {ok, Steps};
                false -> {error, invalid_steps}
            end;
        _ -> {error, metadata_mismatch}
    end.

valid_step_plan(Step, StepPlan, Module, StoredBlock, TargetBlock, Entries)
        when is_atom(Step), is_map(StepPlan), map_size(StepPlan) =:= 5 ->
    case StepPlan of
        #{source_identity := {1, StoredBlock, Step},
            source_mfa := {Module, SourceFunction, 2},
            source_required_captures := SourceRequired,
            target_identity := {1, TargetBlock, TargetStep} = TargetIdentity,
            bridge_mfa := {Module, BridgeFunction, 2}}
                when is_atom(SourceFunction), is_atom(TargetStep),
                    is_atom(BridgeFunction) ->
            valid_capture_names(SourceRequired)
                andalso valid_target_identity(TargetIdentity, Module, Entries);
        _ -> false
    end;
valid_step_plan(_Step, _StepPlan, _Module, _StoredBlock, _TargetBlock, _Entries) ->
    false.

valid_target_identity(Identity, Module, Entries) ->
    case find_unique(fun
            (#{identity := Identity0, mfa := MFA, required_captures := Required})
                    when Identity0 =:= Identity, is_list(Required) ->
                valid_capture_names(Required) andalso current_mfa_callable(Module, MFA);
            (_) -> false
        end, Entries) of
        {ok, _} -> true;
        error -> false
    end.

resolve_mapped_step(#{identity := Identity, mfa := StoredMFA,
        env := Env} = Frame,
        #{source_identity := Identity, source_mfa := StoredMFA,
            source_required_captures := Required,
            bridge_mfa := {Module, BridgeFunction, 2}}, CurrentVsn, _Entries,
        Direction) ->
    case valid_required_captures(Required, Env)
            andalso current_mfa_callable(Module, {Module, BridgeFunction, 2}) of
        true -> {ok, Frame#{vsn := CurrentVsn,
            mfa := {Module, BridgeFunction, 2}, resolution => migration}};
        false -> target_failure(Direction,
            {invalid_migration_plan, missing_capture_or_bridge})
    end;
resolve_mapped_step(_Frame, _StepPlan, _CurrentVsn, _Entries, Direction) ->
    target_failure(Direction, {invalid_migration_plan, source_mismatch}).

find_entry(undefined, StoredMFA, undefined, StoredVsn, CurrentVsn, Entries, resume)
        when StoredVsn =:= CurrentVsn ->
    find_unique(fun(#{identity := undefined, mfa := MFA}) -> MFA =:= StoredMFA;
        (_) -> false end, Entries);
find_entry(undefined, _StoredMFA, undefined, _StoredVsn, _CurrentVsn, _Entries,
        _Direction) -> error;
find_entry(Identity, _StoredMFA, _StepName, _StoredVsn, _CurrentVsn, Entries,
        _Direction) ->
    find_unique(fun(#{identity := EntryIdentity}) -> EntryIdentity =:= Identity;
        (_) -> false end, Entries).

find_unique(Predicate, Entries) ->
    case [Entry || Entry <- Entries, Predicate(Entry)] of
        [Entry] -> {ok, Entry};
        _ -> error
    end.

valid_required_captures(Required, Env) ->
    valid_capture_names(Required)
        andalso lists:all(fun(Name) -> maps:is_key(Name, Env) end, Required).

valid_capture_names(Required) ->
    is_list(Required) andalso Required =:= lists:usort(Required)
        andalso lists:all(fun is_atom/1, Required).

current_mfa_callable(Module, {Module, Function, 2}) when is_atom(Function) ->
    erlang:function_exported(Module, Function, 2);
current_mfa_callable(_Module, _MFA) -> false.

target_failure(appup, _Reason) -> {error, {appup, incompatible_frame}};
target_failure(appdown, _Reason) -> {error, {appdown, incompatible_frame}};
target_failure(resume, Reason) -> {error, Reason}.
