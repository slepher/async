%%% Single owner for linked continuation nodes and their lifecycle.
-module(async_sf_cc).

-include_lib("erlando/include/do.hrl").
-include("async_internal.hrl").

-export([new/3, ordinary_node/2, frame_node/2, register_node/3,
    register_action/3, route_action/3, dispatch/3, dispatch_failure/3,
    dispatch_continuation/4, inspect/3, fail/4]).

-define(CHILDREN(Ref), {sf_continuation_children, Ref}).
-define(MAX_DEPTH, 1024).

new(async_r_t, Callback, Context) when is_function(Callback, 2) ->
    #{kind => sf_cc, protocol => 1, mode => async_r_t,
        callback => Callback, context => Context, parent_ref => none};
new(direct_state, Callback, Context) when is_function(Callback, 2);
        is_function(Callback, 3) ->
    #{kind => sf_cc, protocol => 1, mode => direct_state,
        callback => Callback, context => Context, parent_ref => none};
new(Mode, Callback, _Context) ->
    erlang:error({invalid_sf_cc, {Mode, Callback}}).

ordinary_node(Callback, ParentRef) when is_function(Callback, 1) ->
    checked_parent(#{kind => ordinary_continuation, protocol => 1,
        callback => Callback, parent_ref => ParentRef}, ParentRef).

frame_node(Frame, ParentRef) ->
    checked_parent(#sf_callback{frame = Frame, acc_ref = undefined,
        parent_ref = ParentRef}, ParentRef).

checked_parent(Node, ParentRef) when is_reference(ParentRef); ParentRef =:= none -> Node;
checked_parent(_Node, ParentRef) ->
    erlang:error({invalid_continuation_parent, ParentRef}).

register_node(Ref, Node, {async_r_t, _} = MR) -> register(Ref, Node, node, MR).
register_action(Key, Action, {async_r_t, _} = MR) -> register(Key, Action, action, MR).

route_action(Key, message, Callbacks) ->
    decode_action(Key, Callbacks, false);
route_action(Key, reply, Callbacks) ->
    decode_action(Key, Callbacks, true).

decode_action(Key, Callbacks, Consume) ->
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            case action_preflight(Key, Value, Callbacks) of
                {ok, Decoded} when Consume =:= false -> {ok, Decoded, Callbacks};
                {ok, Decoded} ->
                    cleanup_timer(Value),
                    Removed = remove_child(parent_ref(Value), Key,
                        async_util:remove(Key, Callbacks)),
                    {ok, Decoded, remove_orphan_accumulator(Value, Removed)};
                error -> error
            end;
        error -> error
    end.

action_preflight(Key, #callback{cc = Callback} = Value, Callbacks)
        when is_function(Callback, 1) ->
    decode_valid_action(Key, Value, Callbacks);
action_preflight(Key, #sf_callback{} = Value, Callbacks) ->
    decode_valid_action(Key, Value, Callbacks);
action_preflight(_Key, _Value, _Callbacks) -> error.

decode_valid_action(Key, Value, Callbacks) ->
    case validate_parent(parent_ref(Value), Key, Callbacks) of
        ok -> async_util:decode_callback(Value);
        {error, _} -> error
    end.

register(Key, Value, ExpectedKind, {async_r_t, _} = MR) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        case register_value(Key, Value, ExpectedKind, Get(State)) of
            {ok, Callbacks} -> async_r_t:do_put_state(Set(Callbacks, State), MR);
            {error, Reason} -> erlang:error({invalid_continuation, Reason})
        end
    ]).

register_value(Key, Value, ExpectedKind, Callbacks) ->
    Kind = async_util:callback_value_kind(Value),
    case {valid_key(Key, ExpectedKind), expected_kind(ExpectedKind, Kind),
            async_util:find(Key, Callbacks)} of
        {true, true, error} ->
            ParentRef = parent_ref(Value),
            case validate_parent(ParentRef, Key, Callbacks) of
                ok -> {ok, add_child(ParentRef, Key,
                    async_util:store(Key, Value, Callbacks))};
                Error -> Error
            end;
        {false, _, _} -> {error, {invalid_key, Key}};
        {_, false, _} -> {error, {invalid_value, Value}};
        {_, _, {ok, _}} -> {error, {duplicate_key, Key}}
    end.

expected_kind(node, intermediate_node) -> true;
expected_kind(node, outer_node) -> true;
expected_kind(action, stable_action) -> true;
expected_kind(action, ordinary_action) -> true;
expected_kind(_, _) -> false.

valid_key(Key, node) -> is_reference(Key);
valid_key(Key, action) -> is_reference(Key) orelse is_integer(Key) orelse is_binary(Key).

validate_parent(none, _Key, _Callbacks) -> ok;
validate_parent(ParentRef, Key, Callbacks) when is_reference(ParentRef) ->
    case ParentRef =:= Key of
        true -> {error, self_parent};
        false -> validate_parent_chain(ParentRef, Key, Callbacks, #{}, 0)
    end;
validate_parent(ParentRef, _Key, _Callbacks) -> {error, {invalid_parent, ParentRef}}.

validate_parent_chain(_Ref, _Key, _Callbacks, _Seen, Depth) when Depth > ?MAX_DEPTH ->
    {error, continuation_depth};
validate_parent_chain(Ref, Key, Callbacks, Seen, Depth) ->
    case maps:is_key(Ref, Seen) orelse Ref =:= Key of
        true -> {error, continuation_cycle};
        false ->
            case async_util:find(Ref, Callbacks) of
                {ok, Parent} ->
                    case node_preflight(Parent) of
                        ok ->
                            case async_util:callback_value_kind(Parent) of
                                intermediate_node -> continue_parent_validation(
                                    Parent, Key, Callbacks,
                                    maps:put(Ref, true, Seen), Depth + 1);
                                outer_node -> ok
                            end;
                        {error, Reason} -> {error, {invalid_parent_node, Ref, Reason}}
                    end;
                error -> {error, {orphan_parent, Ref}}
            end
    end.

continue_parent_validation(Node, Key, Callbacks, Seen, Depth) ->
    case parent_ref(Node) of
        none -> ok;
        ParentRef -> validate_parent_chain(ParentRef, Key, Callbacks, Seen, Depth)
    end.

dispatch(Ref, Outcome, {async_r_t, _} = MR) ->
    dispatch_mode(Ref, Outcome, success, MR).

dispatch_failure(Ref, Outcome, {async_r_t, _} = MR) ->
    dispatch_mode(Ref, Outcome, failure, MR).

dispatch_mode(Ref, Outcome, Mode, {async_r_t, _} = MR) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        case take_node(Ref, State, Get, Set) of
            {unhandled, _} -> async_r_t:return(unhandled, MR);
            {error, Reason, _} -> erlang:error({invalid_continuation, Reason});
            {ok, Node, CleanState} ->
                do([
                    MR
                 || async_r_t:do_put_state(CleanState, MR),
                    execute_node(Node, Outcome, Mode, MR)
                ])
        end
    ]).

dispatch_continuation(Ref, Outcome, Offset, State) ->
    dispatch_state(Ref, Outcome, success, Offset, State).

dispatch_failure_continuation(Ref, Outcome, Offset, State) ->
    dispatch_state(Ref, Outcome, failure, Offset, State).

dispatch_state(Ref, Outcome, Mode, Offset, State) ->
    MR = async_r_t:new(identity),
    Action = case Mode of
        success -> dispatch(Ref, Outcome, MR);
        failure -> dispatch_failure(Ref, Outcome, MR)
    end,
    case identity:run(async_r_t:run(Action, async_t:state_callbacks_gs(Offset),
            make_ref(), State, MR)) of
        {unhandled, NewState} -> {unhandled, NewState};
        {Value, NewState} -> {ok, Value, NewState}
    end.

take_node(Ref, State, Get, Set) when is_reference(Ref) ->
    Callbacks = Get(State),
    case async_util:find(Ref, Callbacks) of
        error -> {unhandled, State};
        {ok, Node} ->
            case preflight_node_in_store(Ref, Node, Callbacks) of
                ok ->
                    Taken = remove_child(parent_ref(Node), Ref,
                        async_util:remove(Ref, Callbacks)),
                    {ok, Node, Set(cleanup_owned(Ref, Node, Taken), State)};
                {error, Reason} -> {error, Reason, State}
            end
    end;
take_node(Ref, State, _Get, _Set) -> {error, {invalid_ref, Ref}, State}.

preflight_node_in_store(Ref, Node, Callbacks) ->
    case node_preflight(Node) of
        ok -> validate_parent(parent_ref(Node), Ref, Callbacks);
        Error -> Error
    end.

node_preflight(#{kind := ordinary_continuation, protocol := 1,
        callback := Callback, parent_ref := ParentRef}) when is_function(Callback, 1) ->
    parent_preflight(ParentRef);
node_preflight(#{kind := sf_cc, protocol := 1, mode := async_r_t,
        callback := Callback, parent_ref := none}) when is_function(Callback, 2) -> ok;
node_preflight(#{kind := sf_cc, protocol := 1, mode := direct_state,
        callback := Callback, parent_ref := none}) when is_function(Callback, 2);
        is_function(Callback, 3) -> ok;
node_preflight(#sf_callback{acc_ref = undefined,
        parent_ref = ParentRef}) ->
    parent_preflight(ParentRef);
node_preflight(Other) -> {error, {invalid_node, Other}}.

parent_preflight(none) -> ok;
parent_preflight(Ref) when is_reference(Ref) -> ok;
parent_preflight(Other) -> {error, {invalid_parent, Other}}.

cleanup_owned(Ref, Node, Callbacks) ->
    cleanup_timer(Node),
    Children = children(Ref, Callbacks),
    WithoutIndex = async_util:remove(?CHILDREN(Ref), Callbacks),
    lists:foldl(fun cleanup_child/2, WithoutIndex, Children).

cleanup_child(Key, Callbacks) ->
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            cleanup_timer(Value),
            cleanup_request(Key),
            Removed = remove_child(parent_ref(Value), Key,
                async_util:remove(Key, Callbacks)),
            case async_util:callback_value_kind(Value) of
                intermediate_node -> cleanup_owned(Key, Value, Removed);
                outer_node -> cleanup_owned(Key, Value, Removed);
                _ -> remove_orphan_accumulator(Value, Removed)
            end;
        error -> Callbacks
    end.

cleanup_request(Key) when is_reference(Key) ->
    _ = erlang:demonitor(Key, [flush]), ok;
cleanup_request(_Key) -> ok.

cleanup_timer(#sf_callback{timer_ref = Timer}) when is_reference(Timer) ->
    _ = erlang:cancel_timer(Timer), ok;
cleanup_timer(#callback{timer_ref = Timer}) when is_reference(Timer) ->
    _ = erlang:cancel_timer(Timer), ok;
cleanup_timer(_) -> ok.

remove_orphan_accumulator(Value, Callbacks) ->
    case async_util:decode_callback(Value) of
        {ok, #{acc_ref := AccRef}} when is_reference(AccRef) ->
            case accumulator_in_use(AccRef, Callbacks) of
                true -> Callbacks;
                false -> async_util:remove(AccRef, Callbacks)
            end;
        _ -> Callbacks
    end.

accumulator_in_use(AccRef, Callbacks) ->
    lists:any(fun({_Key, Value}) ->
        case async_util:decode_callback(Value) of
            {ok, #{acc_ref := AccRef0}} -> AccRef0 =:= AccRef;
            _ -> false
        end
    end, async_util:to_list(Callbacks)).

execute_node(#sf_callback{parent_ref = ParentRef}, Outcome, failure, MR) ->
    forward_failure(ParentRef, Outcome, MR);
execute_node(#{kind := ordinary_continuation, callback := Callback,
        parent_ref := ParentRef}, Outcome, _Mode, MR) ->
    try Callback(Outcome) of
        {async_r_t, _} = Effect -> Effect;
        Other -> erlang:error({invalid_continuation_return, Other})
    catch
        error:{badfun, Callback} ->
            forward_failure(ParentRef,
                {error, {sf_runtime, unavailable_continuation}}, MR)
    end;
execute_node(#sf_callback{frame = Frame, parent_ref = ParentRef}, Outcome, success, MR) ->
    async_sf_runtime:continue(Frame, ParentRef, Outcome, MR, resume);
execute_node(#{kind := sf_cc} = SfCc, Outcome, _Mode, MR) ->
    invoke_outer(SfCc, Outcome, MR).

forward_failure(none, Outcome, MR) -> async_r_t:return(Outcome, MR);
forward_failure(ParentRef, Outcome, MR) -> dispatch_failure(ParentRef, Outcome, MR).

invoke_outer(#{mode := async_r_t, callback := Callback, context := Context},
        Outcome, MR) ->
    case callback_availability(Callback) of
        ok ->
            case apply_exact(Callback, [Outcome, Context]) of
                {ok, {async_r_t, _} = Effect} ->
                    async_r_t:local_continuation_context(undefined, Effect, MR);
                {ok, Other} -> erlang:error({invalid_sf_cc_return, Other});
                unavailable -> erlang:error({sf_runtime, unavailable_continuation})
            end;
        unavailable -> erlang:error({sf_runtime, unavailable_continuation});
        {error, Reason} -> erlang:error(Reason)
    end;
invoke_outer(#{mode := direct_state, callback := Callback, context := Context},
        Outcome, MR) ->
    do([
        MR
     || CleanState <- async_r_t:do_get_state(MR),
        case callback_availability(Callback) of
            ok -> invoke_direct(Callback, Outcome, Context, CleanState, MR);
            unavailable -> erlang:error({sf_runtime, unavailable_continuation});
            {error, Reason} -> erlang:error(Reason)
        end
    ]).

invoke_direct(Callback, Outcome, Context, CleanState, MR) ->
    case erlang:fun_info(Callback, arity) of
        {arity, 2} ->
            case apply_exact(Callback, [Outcome, Context]) of
                {ok, _Ignored} -> async_r_t:return(Outcome, MR);
                unavailable -> erlang:error({sf_runtime, unavailable_continuation})
            end;
        {arity, 3} ->
            case apply_exact(Callback, [Outcome, Context, CleanState]) of
                {ok, NewState} ->
                    case async_util:same_type_state(NewState, CleanState) of
                        true -> monad:'>>'(async_r_t:do_put_state(NewState, MR),
                            async_r_t:return(Outcome, MR), MR);
                        false -> erlang:error({invalid_sf_cc_return, NewState})
                    end;
                unavailable -> erlang:error({sf_runtime, unavailable_continuation})
            end
    end.

callback_availability(Callback) ->
    case async_util:fun_availability(Callback) of
        ok -> ok;
        {unavailable, _MFA} -> unavailable;
        {error, Reason} -> {error, Reason}
    end.

apply_exact(Callback, Args) ->
    try erlang:apply(Callback, Args) of Return -> {ok, Return}
    catch error:{badfun, Callback} -> unavailable end.

inspect(MatchFun, Offset, State) when is_function(MatchFun, 1) ->
    {Get, _Set} = async_t:state_callbacks_gs(Offset),
    {ok, select_matches(MatchFun, summaries(Get(State)))};
inspect(MatchFun, _Offset, _State) -> {error, {invalid_matcher, MatchFun}}.

fail(MatchFun, Reason, Offset, State) when is_function(MatchFun, 1) ->
    {Get, _Set} = async_t:state_callbacks_gs(Offset),
    Selected = select_matches(MatchFun, summaries(Get(State))),
    commit_failures(Selected, Reason, Offset, State);
fail(MatchFun, _Reason, _Offset, State) -> {error, {invalid_matcher, MatchFun}, State}.

summaries(Callbacks) ->
    lists:sort(fun(A, B) -> maps:get(request_mref, A) =< maps:get(request_mref, B) end,
        [summary(Key, Decoded) || {Key, Value} <- async_util:to_list(Callbacks),
            {ok, Decoded} <- [async_util:decode_callback(Value)]]).

summary(Key, #{kind := ordinary_action, parent_ref := ParentRef}) ->
    #{request_mref => Key, stable => false, parent_ref => ParentRef,
        continuation_kind => ordinary_fun, frame => undefined};
summary(Key, #{kind := stable_action, frame := Frame, parent_ref := ParentRef}) ->
    #{request_mref => Key, stable => true, parent_ref => ParentRef,
        continuation_kind => sf_frame, frame => frame_summary(Frame)}.

frame_summary(Frame) ->
    #{module => maps:get(module, Frame, undefined), vsn => maps:get(vsn, Frame, undefined),
        protocol => maps:get(protocol, Frame, undefined),
        block => maps:get(block, Frame, undefined),
        step_name => maps:get(step_name, Frame, undefined)}.

select_matches(MatchFun, Summaries) ->
    [Summary || Summary <- Summaries, matcher_result(MatchFun(Summary))].

matcher_result(true) -> true;
matcher_result(false) -> false;
matcher_result(Other) -> erlang:error({invalid_matcher_return, Other}).

commit_failures(Selected, Reason, Offset, State) ->
    {ok, Results, NewState} = lists:foldl(fun(Summary, {ok, Acc, StateAcc}) ->
        case maps:get(stable, Summary) of
            false -> {ok, [#{request_mref => maps:get(request_mref, Summary),
                status => ordinary} | Acc], StateAcc};
            true -> fail_stable(Summary, Reason, Offset, Acc, StateAcc)
        end
    end, {ok, [], State}, Selected),
    {ok, lists:reverse(Results), NewState}.

fail_stable(Summary, Reason, Offset, Results, State) ->
    Key = maps:get(request_mref, Summary),
    ParentRef = maps:get(parent_ref, Summary),
    {Get, Set} = async_t:state_callbacks_gs(Offset),
    case take_action(Key, State, Get, Set) of
        {unhandled, State1} ->
            {ok, [#{request_mref => Key, status => unhandled} | Results], State1};
        {ok, State1} ->
            Outcome = {error, {sf_aborted, Reason}},
            case ParentRef of
                none -> {ok, [#{request_mref => Key, status => handled} | Results], State1};
                _ ->
                    case dispatch_failure_continuation(
                            ParentRef, Outcome, Offset, State1) of
                        {ok, _Value, State2} ->
                            {ok, [#{request_mref => Key, status => forwarded} | Results], State2};
                        {unhandled, State2} ->
                            {ok, [#{request_mref => Key, status => unhandled} | Results], State2};
                        {error, Error, State2} ->
                            {ok, [#{request_mref => Key, status => failed,
                                error => Error} | Results], State2}
                    end
            end
    end.

take_action(Key, State, Get, Set) ->
    Callbacks = Get(State),
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            case async_util:callback_value_kind(Value) of
                stable_action ->
                    cleanup_timer(Value), cleanup_request(Key),
                    Removed = remove_child(parent_ref(Value), Key,
                        async_util:remove(Key, Callbacks)),
                    {ok, Set(remove_orphan_accumulator(Value, Removed), State)};
                _ -> {unhandled, State}
            end;
        error -> {unhandled, State}
    end.

parent_ref(#callback{parent_ref = ParentRef}) -> ParentRef;
parent_ref(#sf_callback{parent_ref = ParentRef}) -> ParentRef;
parent_ref(#{parent_ref := ParentRef}) -> ParentRef;
parent_ref(_) -> none.

add_child(none, _Child, Callbacks) -> Callbacks;
add_child(ParentRef, Child, Callbacks) ->
    Key = ?CHILDREN(ParentRef),
    Index = #{kind => sf_reverse_index, protocol => 1,
        children => lists:usort([Child | children(ParentRef, Callbacks)])},
    async_util:store(Key, Index, Callbacks).

remove_child(none, _Child, Callbacks) -> Callbacks;
remove_child(ParentRef, Child, Callbacks) ->
    Key = ?CHILDREN(ParentRef),
    case async_util:find(Key, Callbacks) of
        {ok, #{kind := sf_reverse_index, protocol := 1, children := Children}} ->
            case lists:delete(Child, Children) of
                [] -> async_util:remove(Key, Callbacks);
                Remaining -> async_util:store(Key,
                    #{kind => sf_reverse_index, protocol => 1,
                        children => Remaining}, Callbacks)
            end;
        _ -> Callbacks
    end.

children(Ref, Callbacks) ->
    case async_util:find(?CHILDREN(Ref), Callbacks) of
        {ok, #{kind := sf_reverse_index, protocol := 1, children := Children}}
                when is_list(Children) -> Children;
        _ -> []
    end.
