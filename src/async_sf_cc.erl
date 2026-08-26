%%%-------------------------------------------------------------------
%%% Single owner for sf completion references and final invocation.
%%%-------------------------------------------------------------------
-module(async_sf_cc).

-include_lib("erlando/include/do.hrl").
-include("async_internal.hrl").

-export([
    new/3,
    new_ref/2,
    terminal/1,
    validate_terminal/1,
    dispatch/3,
    complete/3,
    complete_state/4,
    inspect/3,
    fail/4
]).

new(async_r_t, Callback, Context) when is_function(Callback, 2) ->
    #{kind => sf_cc, protocol => 1, mode => async_r_t,
        callback => Callback, context => Context};
new(direct_state, Callback, Context) when is_function(Callback, 2);
        is_function(Callback, 3) ->
    #{kind => sf_cc, protocol => 1, mode => direct_state,
        callback => Callback, context => Context};
new(Mode, Callback, _Context) ->
    erlang:error({invalid_sf_cc, {Mode, Callback}}).

new_ref(CcRef, SfCc) when is_reference(CcRef) ->
    case validate_cc(SfCc) of
        ok -> #{kind => sf_cc_ref, protocol => 1, cc => SfCc};
        {error, Reason} -> erlang:error({invalid_sf_cc, Reason})
    end.

terminal(CcRef) when is_reference(CcRef) ->
    #{kind => sf_cc_ref, protocol => 1, mref => CcRef}.

validate_terminal(#{kind := sf_cc_ref, protocol := 1, mref := CcRef})
        when is_reference(CcRef) ->
    ok;
validate_terminal(#{protocol := Protocol}) when Protocol =/= 1 ->
    {error, {unsupported_protocol, Protocol}};
validate_terminal(Other) ->
    {error, {invalid_sf_cc_ref, Other}}.

dispatch(CcRef, {message, _} = Message, {async_r_t, _} = MR) ->
    bind(async_r_t:find_ref(CcRef, MR), fun
        ({ok, #{kind := sf_cc_ref, protocol := 1, cc := SfCc}}) ->
            case validate_cc(SfCc) of
                ok -> invoke(SfCc, Message, MR);
                {error, Reason} -> erlang:error({invalid_sf_cc, Reason})
            end;
        ({ok, Other}) -> erlang:error({invalid_sf_cc_ref, Other});
        (error) -> async_r_t:return(unhandled, MR)
    end, MR);
dispatch(CcRef, Outcome, {async_r_t, _} = MR) ->
    complete(CcRef, Outcome, MR).

complete(CcRef, Outcome, {async_r_t, _} = MR) ->
    do([
        MR
     || ActiveAccRef <- async_r_t:get_local_ref(MR),
        {CallbacksG, CallbacksS} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        case prepare(CcRef, [ActiveAccRef], State, CallbacksG, CallbacksS) of
            unhandled ->
                async_r_t:return(unhandled, MR);
            {error, Reason} ->
                erlang:error({invalid_sf_cc, Reason});
            {ok, SfCc, CleanState} ->
                invoke_prepared(SfCc, Outcome, propagate, CleanState, MR)
        end
    ]).

complete_state(CcRef, Outcome, Offset, State) ->
    case complete_state_result(CcRef, Outcome, Offset, State) of
        {ok, Pair} -> Pair;
        {failed, Reason, _CleanState} -> erlang:error(Reason)
    end.

inspect(MatchFun, Offset, State) when is_function(MatchFun, 1) ->
    {Get, _Set} = async_t:state_callbacks_gs(Offset),
    {ok, select_matches(MatchFun, summaries(Get(State)))};
inspect(MatchFun, _Offset, _State) ->
    {error, {invalid_matcher, MatchFun}}.

fail(MatchFun, Reason, Offset, State) when is_function(MatchFun, 1) ->
    {Get, _Set} = async_t:state_callbacks_gs(Offset),
    Selected = select_matches(MatchFun, summaries(Get(State))),
    commit_failures(Selected, Reason, Offset, State);
fail(MatchFun, _Reason, _Offset, State) ->
    {error, {invalid_matcher, MatchFun}, State}.

validate_cc(#{kind := sf_cc, protocol := 1, mode := async_r_t,
        callback := Callback, context := _}) when is_function(Callback, 2) -> ok;
validate_cc(#{kind := sf_cc, protocol := 1, mode := direct_state,
        callback := Callback, context := _}) when is_function(Callback, 2);
        is_function(Callback, 3) -> ok;
validate_cc(Other) -> {error, Other}.

prepare(CcRef, ActiveAccRefs, State, CallbacksG, CallbacksS) ->
    Callbacks = CallbacksG(State),
    case async_util:find(CcRef, Callbacks) of
        {ok, #{kind := sf_cc_ref, protocol := 1, cc := SfCc}} ->
            case validate_cc(SfCc) of
                ok ->
                    Taken = async_util:remove(CcRef, Callbacks),
                    ResourcesClean = remove_owned_resources(CcRef, Taken),
                    {CleanCallbacks, RemovedAccRefs} = remove_owned(CcRef, ResourcesClean),
                    FinalCallbacks = remove_orphan_accumulators(
                        ActiveAccRefs ++ RemovedAccRefs,
                        CleanCallbacks),
                    {ok, SfCc, CallbacksS(FinalCallbacks, State)};
                Error -> Error
            end;
        {ok, Other} -> {error, {invalid_sf_cc_ref, Other}};
        error -> unhandled
    end.

remove_owned(CcRef, Callbacks) ->
    lists:foldl(fun({MRef, Value}, {AccCallbacks, AccRefs}) ->
        case async_util:decode_callback(Value) of
            {ok, #{sf_cc_mref := CcRef, acc_ref := AccRef, cc := Callback}} ->
                cleanup_request(MRef, Callback),
                {async_util:remove(MRef, AccCallbacks), [AccRef | AccRefs]};
            _ -> {AccCallbacks, AccRefs}
        end
    end, {Callbacks, []}, async_util:to_list(Callbacks)).

remove_owned_resources(CcRef, Callbacks) ->
    lists:foldl(fun
        ({Key, #{kind := sf_cc_resource, protocol := 1, owner := Owner,
                resource := {timer, Timer}}}, Acc) when Owner =:= CcRef ->
            _ = erlang:cancel_timer(Timer),
            async_util:remove(Key, Acc);
        ({_Key, _Value}, Acc) -> Acc
    end, Callbacks, async_util:to_list(Callbacks)).

remove_orphan_accumulators(AccRefs, Callbacks) ->
    Surviving = [AccRef || {_MRef, Value} <- async_util:to_list(Callbacks),
        {ok, #{acc_ref := AccRef}} <- [async_util:decode_callback(Value)]],
    lists:foldl(fun(AccRef, Acc) ->
        case lists:member(AccRef, Surviving) of
            true -> Acc;
            false -> async_util:remove(AccRef, Acc)
        end
    end, Callbacks, lists:usort(AccRefs)).

cleanup_request(MRef, Callback) ->
    cleanup_timer(Callback),
    case is_reference(MRef) of
        true -> _ = erlang:demonitor(MRef, [flush]);
        false -> ok
    end.

cleanup_timer(#{timer := Timer}) ->
    _ = erlang:cancel_timer(Timer),
    ok;
cleanup_timer(_) -> ok.

invoke(SfCc, Outcome, MR) ->
    invoke(SfCc, Outcome, propagate, MR).

invoke(#{mode := async_r_t, callback := Callback, context := Context},
        Outcome, FailureMode, MR) ->
    Validate = fun
        ({async_r_t, _} = Effect) -> {ok, Effect};
        (Other) -> {invalid, {invalid_sf_cc_return, Other}}
    end,
    case apply_callback(FailureMode, Callback, [Outcome, Context], Validate) of
        {ok, Effect} -> async_r_t:local_sf_cc_ref(undefined, Effect, MR);
        Failure -> callback_failure(FailureMode, Failure, MR)
    end;
invoke(#{mode := direct_state, callback := Callback, context := Context},
        Outcome, FailureMode, MR) ->
    do([
        MR
     || CleanState <- async_r_t:do_get_state(MR),
        case erlang:fun_info(Callback, arity) of
            {arity, 2} ->
                Validate = fun(_) -> {ok, Outcome} end,
                case apply_callback(FailureMode, Callback,
                        [Outcome, Context], Validate) of
                    {ok, Result} -> async_r_t:return(Result, MR);
                    Failure -> callback_failure(FailureMode, Failure, MR)
                end;
            {arity, 3} ->
                Validate = fun(NewState) ->
                    case async_util:same_type_state(NewState, CleanState) of
                        true -> {ok, NewState};
                        false -> {invalid, {invalid_sf_cc_return, NewState}}
                    end
                end,
                case apply_callback(FailureMode, Callback,
                        [Outcome, Context, CleanState], Validate) of
                    {ok, NewState} ->
                        do([
                            MR
                         || async_r_t:do_put_state(NewState, MR),
                            async_r_t:return(Outcome, MR)
                        ]);
                    Failure -> callback_failure(FailureMode, Failure, MR)
                end
        end
    ]).

invoke_prepared(SfCc, Outcome, FailureMode, CleanState, MR) ->
    do([
        MR
     || async_r_t:do_put_state(CleanState, MR),
        invoke(SfCc, Outcome, FailureMode, MR)
    ]).

apply_callback(_FailureMode, Callback, Args, Validate) ->
    apply_available_callback(Callback, Args, Validate).

apply_available_callback(Callback, Args, Validate) ->
    case async_util:fun_availability(Callback) of
        ok ->
            try erlang:apply(Callback, Args) of
                Return -> Validate(Return)
            catch
                error:{badfun, Callback} ->
                    {unavailable, {sf_runtime, unavailable_continuation}}
            end;
        {unavailable, _MFA} ->
            {unavailable, {sf_runtime, unavailable_continuation}};
        {error, Reason} ->
            erlang:error(Reason)
    end.

callback_failure(propagate, {invalid, Reason}, _MR) ->
    erlang:error(Reason);
callback_failure({capture, FailureRef}, {invalid, Reason}, MR) ->
    async_r_t:return({FailureRef, {failed, Reason}}, MR);
callback_failure(propagate, {unavailable, Reason}, _MR) ->
    erlang:error(Reason);
callback_failure({capture, FailureRef}, {unavailable, Reason}, MR) ->
    async_r_t:return({FailureRef, {failed, Reason}}, MR).

summaries(Callbacks) ->
    lists:sort(fun(A, B) -> maps:get(request_mref, A) =< maps:get(request_mref, B) end,
        [summary(MRef, Decoded) || {MRef, Value} <- async_util:to_list(Callbacks),
            {ok, Decoded} <- [async_util:decode_callback(Value)]]).

summary(MRef, #{cc := Callback, sf_cc_mref := CcRef}) ->
    {Kind, Frames} = continuation_summary(Callback),
    #{request_mref => MRef, sf_cc_mref => CcRef,
        protected => is_reference(CcRef), continuation_kind => Kind,
        frames => Frames}.

continuation_summary(Callback) when is_function(Callback) -> {ordinary_fun, []};
continuation_summary(#{kind := sf_next} = Continuation) ->
    {sf_frame, frame_summaries(Continuation)};
continuation_summary(_) -> {unknown, []}.

frame_summaries(#{kind := sf_next, frame := Frame, downstream := Downstream}) ->
    [frame_summary(Frame) | frame_summaries(Downstream)];
frame_summaries(#{kind := sf_cc_ref}) -> [];
frame_summaries(_) -> [].

frame_summary(Frame) ->
    Module = case maps:find(module, Frame) of
        {ok, OwnerModule} -> OwnerModule;
        error ->
            case maps:get(mfa, Frame, undefined) of
                {MfaModule, _Function, 2} -> MfaModule;
                _ -> undefined
            end
    end,
    #{module => Module, vsn => maps:get(vsn, Frame, undefined),
        protocol => maps:get(protocol, Frame, undefined),
        block_path => maps:get(block_path, Frame, undefined),
        step_name => maps:get(step_name, Frame, undefined),
        async_id => maps:get(async_id, Frame, undefined)}.

select_matches(MatchFun, Summaries) ->
    [Summary || Summary <- Summaries,
        matcher_result(MatchFun(Summary), Summary)].

matcher_result(true, _Summary) -> true;
matcher_result(false, _Summary) -> false;
matcher_result(Other, _Summary) -> erlang:error({invalid_matcher_return, Other}).

commit_failures(Selected, Reason, Offset, State) ->
    Unprotected = [#{request_mref => maps:get(request_mref, Summary),
        sf_cc_mref => undefined, status => unprotected}
        || Summary <- Selected, maps:get(protected, Summary) =:= false],
    CcRefs = lists:usort([maps:get(sf_cc_mref, Summary) || Summary <- Selected,
        maps:get(protected, Summary) =:= true]),
    {Results, NewState} = lists:foldl(fun(CcRef, {Acc, StateAcc}) ->
        case complete_state_result(CcRef, {error, {sf_aborted, Reason}}, Offset,
                StateAcc) of
            {ok, {unhandled, State1}} ->
                {[#{sf_cc_mref => CcRef, status => unhandled} | Acc], State1};
            {ok, {_Outcome, State1}} ->
                {[#{sf_cc_mref => CcRef, status => completed} | Acc], State1};
            {failed, _Error, CleanState} ->
                Result = #{sf_cc_mref => CcRef, status => cc_failed},
                {[Result | Acc], CleanState}
        end
    end, {[], State}, lists:sort(CcRefs)),
    {ok, Unprotected ++ lists:reverse(Results), NewState}.

complete_state_result(CcRef, Outcome, Offset, State) ->
    {Get, Set} = async_t:state_callbacks_gs(Offset),
    case prepare(CcRef, [], State, Get, Set) of
        unhandled -> {ok, {unhandled, State}};
        {error, Error} -> {failed, {invalid_sf_cc, Error}, State};
        {ok, SfCc, CleanState} ->
            run_prepared(SfCc, Outcome, State, CleanState, {Get, Set})
    end.

run_prepared(SfCc, Outcome, State, CleanState, CallbacksGS) ->
    MR = async_r_t:new(identity),
    AccRef = make_ref(),
    FailureRef = make_ref(),
    Action = invoke_prepared(SfCc, Outcome, {capture, FailureRef}, CleanState, MR),
    case identity:run(async_r_t:run(Action, CallbacksGS, AccRef, State, MR)) of
        {{FailureRef, {failed, Reason}}, _State} ->
            {failed, Reason, CleanState};
        Pair ->
            {ok, Pair}
    end.

bind(Action, Fun, MR) -> monad:'>>='(Action, Fun, MR).
