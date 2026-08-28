%%% Single owner for linked continuation nodes and their lifecycle.
-module(async_sf_cc).

-include_lib("erlando/include/do.hrl").
-include("async_internal.hrl").

-export([new/3, ordinary_node/2, frame_node/2, register_node/3,
    register_action/3, register_timer/3, cleanup_accumulator/2,
    route_action/3, dispatch/3, dispatch_message/3, dispatch_failure/3,
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

register_timer(_Key, infinity, {async_r_t, _} = MR) ->
    async_r_t:return(ok, MR);
register_timer(Key, Timeout, {async_r_t, _} = MR) when is_integer(Timeout) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        Callbacks = Get(State),
        case async_util:find(Key, Callbacks) of
            {ok, Value} ->
                case preflight_action_in_store(Key, Value, Callbacks) of
                    {ok, _} ->
                        case timer_reference(Value, Key, Callbacks) of
                            undefined ->
                                Timer = erlang:send_after(Timeout,
                                    self(), {Key, {error, timeout}}),
                                async_r_t:do_put_state(
                                    Set(put_timer(Key, Timer, Value, Callbacks), State), MR);
                            {ok, _} -> erlang:error({duplicate_timer, Key});
                            {error, _} -> erlang:error({orphan_timer, Key})
                        end;
                    {error, _} -> erlang:error({orphan_timer, Key})
                end;
            error -> erlang:error({orphan_timer, Key})
        end
    ]).

cleanup_accumulator(AccRef, {async_r_t, _} = MR) when is_reference(AccRef) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        async_r_t:do_put_state(
            Set(remove_orphan_accumulator_ref(AccRef, Get(State)), State), MR)
    ]);
cleanup_accumulator(_AccRef, {async_r_t, _} = MR) ->
    async_r_t:return(ok, MR).

route_action(Key, message, Callbacks) ->
    decode_action(Key, Callbacks, false);
route_action(Key, reply, Callbacks) ->
    decode_action(Key, Callbacks, true).

decode_action(Key, Callbacks, Consume) ->
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            case preflight_action_in_store(Key, Value, Callbacks) of
                {ok, Decoded} when Consume =:= false -> {ok, Decoded, Callbacks};
                {ok, Decoded} ->
                    Removed = take_action_value(Key, Value, Callbacks),
                    {ok, Decoded, Removed};
                {error, _} -> error
            end;
        error -> error
    end.

action_preflight(Key, #callback{cc = Callback} = Value, Callbacks)
        when is_function(Callback, 1) ->
    case async_util:callback_value_kind(Value) of
        ordinary_action -> decode_valid_action(Key, Value, Callbacks);
        _ -> error
    end;
action_preflight(Key, #sf_callback{} = Value, Callbacks) ->
    case async_util:callback_value_kind(Value) of
        stable_action -> decode_valid_action(Key, Value, Callbacks);
        _ -> error
    end;
action_preflight(_Key, _Value, _Callbacks) -> error.

decode_valid_action(Key, Value, Callbacks) ->
    case validate_parent(parent_ref(Value), Key, Callbacks) of
        ok ->
            case action_resources_preflight(Key, Value, Callbacks) of
                ok -> async_util:decode_callback(Value);
                error -> error
            end;
        {error, _} -> error
    end.

action_resources_preflight(Key, Value, Callbacks) ->
    case async_util:decode_callback(Value) of
        {ok, #{acc_ref := AccRef}} when is_reference(AccRef) ->
            case valid_optional_accumulator(AccRef, Callbacks) of
                ok -> valid_timer_target(Key, Value, Callbacks);
                error -> error
            end;
        _ -> error
    end.

valid_optional_accumulator(AccRef, Callbacks) ->
    case async_util:find(AccRef, Callbacks) of
        error -> ok;
        {ok, Value} ->
            case async_util:callback_value_kind(Value) of
                accumulator -> ok;
                _ -> error
            end
    end.

valid_timer_target(Key, #sf_callback{timer_ref = Timer}, Callbacks)
        when is_reference(Timer) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        error -> ok;
        {ok, _} -> error
    end;
valid_timer_target(Key, #sf_callback{timer_ref = undefined}, Callbacks) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        error -> ok;
        {ok, _} -> error
    end;
valid_timer_target(Key, #callback{}, Callbacks) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        error -> ok;
        {ok, Timer} when is_reference(Timer) -> ok;
        _ -> error
    end;
valid_timer_target(_Key, _Value, _Callbacks) -> error.

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
                ok ->
                    case preflight_ancestors(ParentRef, Key, Callbacks, #{}, 0) of
                        ok -> {ok, add_child(ParentRef, Key,
                            async_util:store(Key, Value, Callbacks))};
                        Error -> Error
                    end;
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

dispatch_message(Ref, Outcome, {async_r_t, _} = MR) ->
    do([
        MR
     || {Get, _Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        case async_util:find(Ref, Get(State)) of
            {ok, Node} ->
                case async_util:callback_value_kind(Node) of
                    outer_node ->
                        case node_preflight(Node) of
                            ok ->
                                case execute_node(Node, Outcome, success, MR) of
                                    {ok, Action} -> Action;
                                    {error, Reason} ->
                                        erlang:error({invalid_continuation, Reason})
                                end;
                            {error, Reason} ->
                                erlang:error({invalid_continuation, Reason})
                        end;
                    _ -> async_r_t:return(unhandled, MR)
                end;
            error -> async_r_t:return(unhandled, MR)
        end
    ]).

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
                    case execute_node(Node, Outcome, Mode, MR) of
                        {ok, Action} -> Action;
                        {error, Reason} ->
                            erlang:error({invalid_continuation, Reason})
                    end
                ])
        end
    ]).

dispatch_continuation(Ref, Outcome, Offset, State) ->
    dispatch_state(Ref, Outcome, success, Offset, State).

dispatch_failure_continuation(Ref, Outcome, Offset, State) ->
    dispatch_state(Ref, Outcome, failure, Offset, State).

dispatch_state(Ref, Outcome, Mode, Offset, State) ->
    MR = async_r_t:new(identity),
    {Get, Set} = async_t:state_callbacks_gs(Offset),
    case take_node(Ref, State, Get, Set) of
        {unhandled, _} -> {unhandled, State};
        {error, Reason, ConsumedState} -> {error, Reason, ConsumedState};
        {ok, Node, ConsumedState} ->
            case execute_node_state(Node, Outcome, Mode, ConsumedState, MR) of
                {error, Error} -> {error, Error, ConsumedState};
                {ok, Action, State0} ->
                    case identity:run(async_r_t:run(Action,
                            {Get, Set}, make_ref(), State0, MR)) of
                        {unhandled, NewState} -> {unhandled, NewState};
                        {Value, NewState} -> {ok, Value, NewState}
                    end
            end
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
        ok ->
            case preflight_ancestors(parent_ref(Node), Ref, Callbacks, #{}, 0) of
                ok -> preflight_owned(Ref, Node, Callbacks, #{}, 0);
                Error -> Error
            end;
        Error -> Error
    end.

preflight_action_in_store(Key, Value, Callbacks) ->
    case action_preflight(Key, Value, Callbacks) of
        {ok, Decoded} ->
            case async_util:find(?CHILDREN(Key), Callbacks) of
                error ->
                    case preflight_ancestors(parent_ref(Value), Key, Callbacks, #{}, 0) of
                        ok -> {ok, Decoded};
                        Error -> Error
                    end;
                {ok, _} -> {error, {action_has_children, Key}}
            end;
        error -> {error, {invalid_action, Key}}
    end.

preflight_ancestors(none, _Child, _Callbacks, _Seen, _Depth) -> ok;
preflight_ancestors(Ref, Child, Callbacks, Seen, Depth)
        when is_reference(Ref), Depth =< ?MAX_DEPTH ->
    case maps:is_key(Ref, Seen) orelse Ref =:= Child of
        true -> {error, continuation_cycle};
        false ->
            case async_util:find(Ref, Callbacks) of
                {ok, Parent} ->
                    case node_preflight(Parent) of
                        ok ->
                            case preflight_children_direct(Ref, Parent, Callbacks) of
                                {ok, _} -> preflight_ancestors(parent_ref(Parent),
                                    Child, Callbacks, maps:put(Ref, true, Seen),
                                    Depth + 1);
                                Error -> Error
                            end;
                        {error, Reason} -> {error, {invalid_parent_node, Ref, Reason}}
                    end;
                error -> {error, {orphan_parent, Ref}}
            end
    end;
preflight_ancestors(_Ref, _Child, _Callbacks, _Seen, _Depth) ->
    {error, continuation_depth}.

preflight_owned(Ref, Node, Callbacks, Seen, Depth)
        when is_reference(Ref), Depth =< ?MAX_DEPTH ->
    case maps:is_key(Ref, Seen) of
        true -> {error, continuation_cycle};
        false ->
            case node_preflight(Node) of
                ok ->
                    case preflight_children_direct(Ref, Node, Callbacks) of
                        {ok, Children} ->
                            lists:foldl(fun
                                (_Child, {error, _} = Error) -> Error;
                                (Child, ok) ->
                                    case async_util:find(Child, Callbacks) of
                                        {ok, ChildValue} ->
                                            case async_util:callback_value_kind(ChildValue) of
                                                intermediate_node ->
                                                    preflight_owned(Child, ChildValue,
                                                        Callbacks,
                                                        maps:put(Ref, true, Seen),
                                                        Depth + 1);
                                                outer_node -> ok;
                                                _ -> ok
                                            end;
                                        error -> {error, {missing_child, Ref, Child}}
                                    end
                            end, ok, Children);
                        Error -> Error
                    end;
                Error -> Error
            end
    end;
preflight_owned(_Ref, _Node, _Callbacks, _Seen, _Depth) ->
    {error, continuation_depth}.

preflight_children_direct(Ref, Node, Callbacks) ->
    case async_util:find(?CHILDREN(Ref), Callbacks) of
        error -> {ok, []};
        {ok, #{kind := sf_reverse_index, protocol := 1, children := Children}}
                when is_list(Children) ->
            case lists:usort(Children) =:= Children of
                false -> {error, {invalid_reverse_index, Ref}};
                true ->
                    case {async_util:callback_value_kind(Node), Children} of
                        {stable_action, [_ | _]} ->
                            {error, {action_has_children, Ref}};
                        {ordinary_action, [_ | _]} ->
                            {error, {action_has_children, Ref}};
                        _ ->
                            case lists:foldl(fun(Child, ok) ->
                                    preflight_direct_child(Ref, Child, Callbacks);
                                (_Child, Error) -> Error
                            end, ok, Children) of
                                ok -> {ok, Children};
                                Error -> Error
                            end
                    end
            end;
        {ok, _} -> {error, {invalid_reverse_index, Ref}}
    end.

preflight_direct_child(ParentRef, Child, Callbacks) ->
    case async_util:find(Child, Callbacks) of
        {ok, Value} ->
            case parent_ref(Value) of
                ParentRef ->
                    case async_util:callback_value_kind(Value) of
                        stable_action ->
                            case action_preflight(Child, Value, Callbacks) of
                                {ok, _} -> ok;
                                error -> {error, {invalid_child, Child}}
                            end;
                        ordinary_action ->
                            case action_preflight(Child, Value, Callbacks) of
                                {ok, _} -> ok;
                                error -> {error, {invalid_child, Child}}
                            end;
                        intermediate_node ->
                            case node_preflight(Value) of
                                ok -> ok;
                                Error -> Error
                            end;
                        outer_node ->
                            case node_preflight(Value) of
                                ok -> ok;
                                Error -> Error
                            end;
                        _ -> {error, {invalid_child, Child}}
                    end;
                _ -> {error, {invalid_child_parent, Child}}
            end;
        error -> {error, {missing_child, ParentRef, Child}}
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

cleanup_owned(Ref, _Node, Callbacks) ->
    Children = children(Ref, Callbacks),
    WithoutIndex = async_util:remove(?CHILDREN(Ref), Callbacks),
    lists:foldl(fun cleanup_child/2, WithoutIndex, Children).

cleanup_child(Key, Callbacks) ->
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            Removed = remove_child(parent_ref(Value), Key,
                remove_action_key(Key, Value, async_util:remove(Key, Callbacks))),
            cleanup_direct_resources(Key, Value, Callbacks),
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

cleanup_direct_resources(Key, Value, Callbacks) ->
    _ = cleanup_timer(Key, Value, Callbacks),
    cleanup_request(Key).

cleanup_timer(_Key, #sf_callback{timer_ref = Timer}, _Callbacks)
        when is_reference(Timer) ->
    _ = erlang:cancel_timer(Timer), ok;
cleanup_timer(Key, #callback{}, Callbacks) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        {ok, Timer} when is_reference(Timer) ->
            _ = erlang:cancel_timer(Timer), ok;
        _ -> ok
    end;
cleanup_timer(_Key, _Value, _Callbacks) -> ok.

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

remove_orphan_accumulator_ref(AccRef, Callbacks) ->
    case accumulator_in_use(AccRef, Callbacks) of
        true -> Callbacks;
        false -> async_util:remove(AccRef, Callbacks)
    end.

remove_action_key(_Key, #sf_callback{}, Callbacks) ->
    Callbacks;
remove_action_key(Key, #callback{}, Callbacks) ->
    async_util:remove(ordinary_timer_key(Key), Callbacks);
remove_action_key(_Key, _Value, Callbacks) ->
    Callbacks.

take_action_value(Key, Value, Callbacks) ->
    Removed = remove_child(parent_ref(Value), Key,
        remove_action_key(Key, Value, async_util:remove(Key, Callbacks))),
    cleanup_direct_resources(Key, Value, Callbacks),
    Removed.

ordinary_timer_key(Key) -> {sf_cc_timer, Key}.

put_timer(Key, Timer, #sf_callback{} = Value, Callbacks) ->
    async_util:store(Key, Value#sf_callback{timer_ref = Timer}, Callbacks);
put_timer(Key, Timer, #callback{}, Callbacks) ->
    async_util:store(ordinary_timer_key(Key), Timer, Callbacks).

timer_reference(#sf_callback{timer_ref = Timer}, _Key, _Callbacks)
        when is_reference(Timer) ->
    {ok, Timer};
timer_reference(#sf_callback{timer_ref = undefined}, Key, Callbacks) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        error -> undefined;
        {ok, Timer} when is_reference(Timer) -> {ok, Timer};
        {ok, _} -> {error, malformed_timer}
    end;
timer_reference(#callback{}, Key, Callbacks) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        error -> undefined;
        {ok, Timer} when is_reference(Timer) -> {ok, Timer};
        {ok, _} -> {error, malformed_timer}
    end.

execute_node(#sf_callback{parent_ref = ParentRef}, Outcome, failure, MR) ->
    {ok, forward_failure(ParentRef, Outcome, MR)};
execute_node(#{kind := ordinary_continuation, callback := Callback,
        parent_ref := ParentRef}, Outcome, _Mode, MR) ->
    case async_util:fun_availability(Callback) of
        ok ->
            case Callback(Outcome) of
                {async_r_t, _} = Effect -> {ok, Effect};
                Other -> {error, {invalid_continuation_return, Other}}
            end;
        {unavailable, _} ->
            {ok, forward_failure(ParentRef,
                {error, {sf_runtime, unavailable_continuation}}, MR)};
        {error, Reason} -> erlang:error(Reason)
    end;
execute_node(#sf_callback{frame = Frame, parent_ref = ParentRef}, Outcome, success, MR) ->
    {ok, async_sf_runtime:continue(Frame, ParentRef, Outcome, MR, resume)};
execute_node(#{kind := sf_cc} = SfCc, Outcome, _Mode, MR) ->
    {ok, invoke_outer(SfCc, Outcome, MR)}.

execute_node_state(#sf_callback{parent_ref = ParentRef}, Outcome, failure,
        _State, MR) ->
    {ok, forward_failure(ParentRef, Outcome, MR), _State};
execute_node_state(#{kind := ordinary_continuation, callback := Callback,
        parent_ref := ParentRef}, Outcome, _Mode, _State, MR) ->
    case async_util:fun_availability(Callback) of
        ok ->
            case Callback(Outcome) of
                {async_r_t, _} = Effect -> {ok, Effect, _State};
                Other -> {error, {invalid_continuation_return, Other}}
            end;
        {unavailable, _} ->
            {ok, forward_failure(ParentRef,
                {error, {sf_runtime, unavailable_continuation}}, MR), _State};
        {error, Reason} -> erlang:error(Reason)
    end;
execute_node_state(#sf_callback{frame = Frame, parent_ref = ParentRef},
        Outcome, success, State, MR) ->
    {ok, async_sf_runtime:continue(Frame, ParentRef, Outcome, MR, resume), State};
execute_node_state(#{kind := sf_cc, mode := async_r_t,
        callback := Callback, context := Context}, Outcome, _Mode, State, _MR) ->
    case callback_availability(Callback) of
        ok ->
            case erlang:apply(Callback, [Outcome, Context]) of
                {async_r_t, _} = Effect -> {ok, Effect, State};
                Other -> {error, {invalid_sf_cc_return, Other}}
            end;
        unavailable -> {error, unavailable_continuation};
        {error, Reason} -> {error, Reason}
    end;
execute_node_state(#{kind := sf_cc, mode := direct_state,
        callback := Callback, context := Context}, Outcome, _Mode, State, MR) ->
    invoke_direct_state(Callback, Outcome, Context, State, MR).

invoke_direct_state(Callback, Outcome, Context, CleanState, MR) ->
    case callback_availability(Callback) of
        unavailable -> {error, unavailable_continuation};
        {error, Reason} -> {error, Reason};
        ok ->
            case erlang:fun_info(Callback, arity) of
                {arity, 2} ->
                    _ = erlang:apply(Callback, [Outcome, Context]),
                    {ok, async_r_t:return(Outcome, MR), CleanState};
                {arity, 3} ->
                    NewState = erlang:apply(Callback,
                        [Outcome, Context, CleanState]),
                    case async_util:same_type_state(NewState, CleanState) of
                        true -> {ok, async_r_t:return(Outcome, MR),
                            NewState};
                        false -> {error, {invalid_sf_cc_return, NewState}}
                    end
            end
    end.

forward_failure(none, Outcome, MR) -> async_r_t:return(Outcome, MR);
forward_failure(ParentRef, Outcome, MR) -> dispatch_failure(ParentRef, Outcome, MR).

invoke_outer(#{mode := async_r_t, callback := Callback, context := Context},
        Outcome, _MR) ->
    case callback_availability(Callback) of
        ok ->
            case erlang:apply(Callback, [Outcome, Context]) of
                {async_r_t, _} = Effect -> Effect;
                Other -> erlang:error({invalid_sf_cc_return, Other})
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
            case erlang:apply(Callback, [Outcome, Context]) of
                _Ignored -> async_r_t:return(Outcome, MR)
            end;
        {arity, 3} ->
            case erlang:apply(Callback, [Outcome, Context, CleanState]) of
                NewState ->
                    case async_util:same_type_state(NewState, CleanState) of
                        true -> monad:'>>'(async_r_t:do_put_state(NewState, MR),
                            async_r_t:return(Outcome, MR), MR);
                        false -> erlang:error({invalid_sf_cc_return, NewState})
                    end
            end
    end.

callback_availability(Callback) ->
    case async_util:fun_availability(Callback) of
        ok -> ok;
        {unavailable, _MFA} -> unavailable;
        {error, Reason} -> {error, Reason}
    end.

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
    case lists:foldl(fun(Summary, {ok, Acc, StateAcc}) ->
        case maps:get(stable, Summary) of
            false -> {ok, [#{request_mref => maps:get(request_mref, Summary),
                status => ordinary} | Acc], StateAcc};
            true -> fail_stable(Summary, Reason, Offset, Acc, StateAcc)
        end
    end, {ok, [], State}, Selected) of
        {ok, Results, NewState} -> {ok, lists:reverse(Results), NewState};
        {error, Error, ErrorState} -> {error, Error, ErrorState}
    end.

fail_stable(Summary, Reason, Offset, Results, State) ->
    Key = maps:get(request_mref, Summary),
    ParentRef = maps:get(parent_ref, Summary),
    {Get, Set} = async_t:state_callbacks_gs(Offset),
    case take_action(Key, State, Get, Set) of
        {unhandled, State1} ->
            {ok, [#{request_mref => Key, status => unhandled} | Results], State1};
        {error, Error, State1} ->
            {error, Error, State1};
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
                    case preflight_action_in_store(Key, Value, Callbacks) of
                        {ok, _} ->
                            Removed = take_action_value(Key, Value, Callbacks),
                            {ok, Set(remove_orphan_accumulator(Value, Removed), State)};
                        {error, Error} -> {error, Error, State}
                    end;
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
