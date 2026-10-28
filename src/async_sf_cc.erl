%%% Single owner for linked continuation nodes and their lifecycle.
-module(async_sf_cc).

-include_lib("erlando/include/do.hrl").
-include("async_internal.hrl").

-export([new/3, ordinary_node/2, ordinary_outer_node/1, frame_node/2, register_node/3,
    register_action/3, register_timer/3, register_resources/3, cleanup_accumulator/2,
    route_action/3, dispatch/3, dispatch_message/3, dispatch_failure/3,
    dispatch_continuation/4, inspect/3, fail/4]).

-define(CHILDREN(Ref), {sf_continuation_children, Ref}).
-define(RESOURCES(Ref), {sf_continuation_resources, Ref}).
-define(INVALID_CONTINUATION(Reason),
    erlang:error({invalid_continuation, Reason})).

-type parent_ref() :: reference() | none.
-type monad_runtime() :: {async_r_t, term()}.
-type async_effect() :: async_r_t:async_r_t(term(), term(), term()).
-type ordinary_callback(S, M, Outcome, A) ::
    fun((Outcome) -> async_r_t:async_r_t(S, M, A)).
-type ordinary_node(S, M, Outcome, A) :: #{
    kind := ordinary_continuation,
    protocol := 1,
    callback := ordinary_callback(S, M, Outcome, A),
    parent_ref := parent_ref()
}.
-type ordinary_outer_node(S, M, Outcome, A) :: #{
    kind := ordinary_outer,
    protocol := 1,
    callback := ordinary_callback(S, M, Outcome, A),
    parent_ref := none
}.
-type async_sf_callback(S, M, Outcome, Context, A) ::
    fun((Outcome, Context) -> async_r_t:async_r_t(S, M, A)).
-type async_sf_cc_node(S, M, Outcome, Context, A) :: #{
    kind := sf_cc,
    protocol := 1,
    mode := async_r_t,
    callback := async_sf_callback(S, M, Outcome, Context, A),
    context := Context,
    parent_ref := none
}.
-type direct_sf_callback(S, Outcome, Context) ::
    fun((Outcome, Context) -> term())
    | fun((Outcome, Context, S) -> S).
-type direct_sf_cc_node(S, Outcome, Context) :: #{
    kind := sf_cc,
    protocol := 1,
    mode := direct_state,
    callback := direct_sf_callback(S, Outcome, Context),
    context := Context,
    parent_ref := none
}.
-type sf_cc_node() ::
    async_sf_cc_node(term(), term(), term(), term(), term())
    | direct_sf_cc_node(term(), term(), term()).
-type continuation_node() ::
    ordinary_node(term(), term(), term(), term())
    | ordinary_outer_node(term(), term(), term(), term())
    | sf_cc_node()
    | #sf_callback{}.

-spec new(async_r_t,
    fun((Outcome, Context) -> async_r_t:async_r_t(S, M, A)), Context) ->
        async_sf_cc_node(S, M, Outcome, Context, A);
    (direct_state,
        fun((Outcome, Context) -> term())
        | fun((Outcome, Context, S) -> S), Context) ->
        direct_sf_cc_node(S, Outcome, Context).
new(async_r_t, Callback, Context) when is_function(Callback, 2) ->
    #{kind => sf_cc, protocol => 1, mode => async_r_t,
        callback => Callback, context => Context, parent_ref => none};
new(direct_state, Callback, Context) when is_function(Callback, 2);
        is_function(Callback, 3) ->
    #{kind => sf_cc, protocol => 1, mode => direct_state,
        callback => Callback, context => Context, parent_ref => none};
new(Mode, Callback, _Context) ->
    erlang:error({invalid_sf_cc, {Mode, Callback}}).

-spec ordinary_node(
    fun((Outcome) -> async_r_t:async_r_t(S, M, A)), parent_ref()) ->
    ordinary_node(S, M, Outcome, A).
ordinary_node(Callback, ParentRef) when is_function(Callback, 1) ->
    checked_parent(#{kind => ordinary_continuation, protocol => 1,
        callback => Callback, parent_ref => ParentRef}, ParentRef).

-spec ordinary_outer_node(
    fun((Outcome) -> async_r_t:async_r_t(S, M, A))) ->
    ordinary_outer_node(S, M, Outcome, A).
ordinary_outer_node(Callback) when is_function(Callback, 1) ->
    #{kind => ordinary_outer, protocol => 1,
        callback => Callback, parent_ref => none}.

frame_node(Frame, ParentRef) ->
    checked_parent(#sf_callback{frame = Frame, acc_ref = undefined,
        parent_ref = ParentRef}, ParentRef).

-spec checked_parent(Node, parent_ref()) -> Node.
checked_parent(Node, ParentRef) when is_reference(ParentRef); ParentRef =:= none -> Node;
checked_parent(_Node, ParentRef) ->
    erlang:error({invalid_continuation_parent, ParentRef}).

register_node(Ref, Node, {async_r_t, _} = MR) -> register(Ref, Node, node, MR).
register_action(Key, Action, {async_r_t, _} = MR) -> register(Key, Action, action, MR).

%% Combinator scratch keys belong to the same continuation as its actions.
register_resources(none, _Keys, MR) -> async_r_t:return(ok, MR);
register_resources(ParentRef, Keys, MR) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        Callbacks = Get(State),
        begin
            validate_registration_parent(ParentRef, none, Callbacks),
            IndexKey = ?RESOURCES(ParentRef),
            Existing = async_util:get(IndexKey, Callbacks,
                #{kind => sf_reverse_index, protocol => 1, resources => []}),
            Index = Existing#{resources := lists:usort(
                Keys ++ maps:get(resources, Existing))},
            async_r_t:do_put_state(Set(async_util:store(IndexKey, Index, Callbacks), State), MR)
        end
    ]).

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
                case async_util:callback_value_kind(Value) of
                    Kind when Kind =:= stable_action; Kind =:= ordinary_action ->
                        case timer_reference(Value, Key, Callbacks) of
                            undefined ->
                                Timer = erlang:send_after(Timeout,
                                    self(), {Key, {error, timeout}}),
                                async_r_t:do_put_state(
                                    Set(put_timer(Key, Timer, Value, Callbacks), State), MR);
                            {ok, _} -> erlang:error({duplicate_timer, Key})
                        end;
                    _ -> erlang:error({orphan_timer, Key})
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

-spec route_action(term(), message | reply, term()) ->
    {ok, map(), term()} | missing_callback.
route_action(Key, message, Callbacks) ->
    decode_action(Key, Callbacks, false);
route_action(Key, reply, Callbacks) ->
    decode_action(Key, Callbacks, true).

decode_action(Key, Callbacks, Consume) ->
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            case async_util:callback_value_kind(Value) of
                Kind when Kind =:= stable_action; Kind =:= ordinary_action ->
                    decode_routable_action(Key, Value, Callbacks, Consume);
                Kind when Kind =:= intermediate_node; Kind =:= outer_node;
                        Kind =:= accumulator; Kind =:= reverse_index ->
                    missing_callback;
                other -> erlang:error({invalid_callback_entry, Key})
            end;
        error -> missing_callback
    end.

decode_routable_action(Key, Value, Callbacks, Consume) ->
    case async_util:decode_callback(Value) of
        {ok, Decoded} when Consume =:= false -> {ok, Decoded, Callbacks};
        {ok, Decoded} ->
            Removed = take_action_value(Key, Value, Callbacks),
            {ok, Decoded, Removed};
        error -> erlang:error({invalid_callback_entry, Key})
    end.

register(Key, Value, ExpectedKind, {async_r_t, _} = MR) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        Callbacks = register_value(Key, Value, ExpectedKind, Get(State)),
        async_r_t:do_put_state(Set(Callbacks, State), MR)
    ]).

register_value(Key, Value, ExpectedKind, Callbacks) ->
    Kind = async_util:callback_value_kind(Value),
    case {valid_key(Key, ExpectedKind), expected_kind(ExpectedKind, Kind),
            async_util:find(Key, Callbacks)} of
        {true, true, error} ->
            ParentRef = parent_ref(Value),
            validate_registration_parent(ParentRef, Key, Callbacks),
            add_child(ParentRef, Key, async_util:store(Key, Value, Callbacks));
        {false, _, _} -> ?INVALID_CONTINUATION({invalid_key, Key});
        {_, false, _} -> ?INVALID_CONTINUATION({invalid_value, Value});
        {_, _, {ok, _}} -> ?INVALID_CONTINUATION({duplicate_key, Key})
    end.

expected_kind(node, intermediate_node) -> true;
expected_kind(node, outer_node) -> true;
expected_kind(action, stable_action) -> true;
expected_kind(action, ordinary_action) -> true;
expected_kind(_, _) -> false.

valid_key(Key, node) -> is_reference(Key);
valid_key(Key, action) -> is_reference(Key) orelse is_integer(Key) orelse is_binary(Key).

validate_registration_parent(none, _Key, _Callbacks) -> ok;
validate_registration_parent(ParentRef, Key, _Callbacks) when ParentRef =:= Key ->
    ?INVALID_CONTINUATION(self_parent);
validate_registration_parent(ParentRef, _Key, Callbacks) when is_reference(ParentRef) ->
    case async_util:find(ParentRef, Callbacks) of
        {ok, Parent} ->
            case async_util:callback_value_kind(Parent) of
                intermediate_node -> ok;
                outer_node -> ok;
                _ -> ?INVALID_CONTINUATION({invalid_parent_node, ParentRef})
            end;
        error -> ?INVALID_CONTINUATION({orphan_parent, ParentRef})
    end;
validate_registration_parent(ParentRef, _Key, _Callbacks) ->
    ?INVALID_CONTINUATION({invalid_parent, ParentRef}).

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
                case Node of
                    #sf_callback{} -> async_r_t:return(Outcome, MR);
                    _ ->
                        case async_util:callback_value_kind(Node) of
                            outer_node -> execute_node(Node, Outcome, success, MR);
                            _ -> ?INVALID_CONTINUATION({invalid_node, Ref})
                        end
                end;
            error -> ?INVALID_CONTINUATION({missing_node, Ref})
        end
    ]).

dispatch_mode(Ref, Outcome, Mode, {async_r_t, _} = MR) ->
    do([
        MR
     || {Get, Set} <- async_r_t:ask(MR),
        State <- async_r_t:do_get_state(MR),
        case async_util:find(Ref, Get(State)) of
            {ok, Node} ->
                case preserve_node_with_children(Ref, Node, Mode, Get(State)) of
                    true -> dispatch_pending_parent(Node, Outcome, Mode, MR);
                    false ->
                        {ok, TakenNode, CleanState} = take_node(
                            Ref, State, Get, Set),
                        dispatch_taken_node(TakenNode, Outcome, Mode,
                            CleanState, MR)
                end;
            error -> ?INVALID_CONTINUATION({missing_node, Ref})
        end
    ]).

dispatch_pending_parent(Node, Outcome, Mode, MR) ->
    case Node of
        #{mode := direct_state} -> async_r_t:return(Outcome, MR);
        _ -> execute_node(Node, Outcome, Mode, MR)
    end.

dispatch_taken_node(Node, Outcome, Mode, CleanState, MR) ->
    do([
        MR
     || async_r_t:do_put_state(CleanState, MR),
        execute_node(Node, Outcome, Mode, MR)
    ]).

preserve_node_with_children(Ref, Node, success, Callbacks) ->
    case async_util:callback_value_kind(Node) of
        Kind when Kind =:= outer_node; Kind =:= intermediate_node ->
            case async_util:find(?CHILDREN(Ref), Callbacks) of
                {ok, #{kind := sf_reverse_index, protocol := 1,
                    children := [_ | _]}} -> true;
                _ -> false
            end;
        _ -> false
    end;
preserve_node_with_children(_Ref, _Node, _Mode, _Callbacks) ->
    false.

dispatch_continuation(Ref, Outcome, Offset, State) ->
    dispatch_state(Ref, Outcome, success, Offset, State).

dispatch_failure_continuation(Ref, Outcome, Offset, State) ->
    dispatch_state(Ref, Outcome, failure, Offset, State).

dispatch_state(Ref, Outcome, Mode, Offset, State) ->
    MR = async_r_t:new(identity),
    {Get, Set} = async_t:state_callbacks_gs(Offset),
    {ok, Node, ConsumedState} = take_node(Ref, State, Get, Set),
    Action = execute_node(Node, Outcome, Mode, MR),
    {Value, NewState} = identity:run(async_r_t:run(Action,
        {Get, Set}, make_ref(), ConsumedState, MR)),
    {ok, Value, NewState}.

take_node(Ref, State, Get, Set) when is_reference(Ref) ->
    Callbacks = Get(State),
    case async_util:find(Ref, Callbacks) of
        error -> ?INVALID_CONTINUATION({missing_node, Ref});
        {ok, Node} ->
            Taken = remove_child(parent_ref(Node), Ref,
                async_util:remove(Ref, Callbacks)),
            {ok, Node, Set(cleanup_owned(Ref, Node, Taken), State)}
    end;
take_node(Ref, _State, _Get, _Set) ->
    ?INVALID_CONTINUATION({invalid_ref, Ref}).

cleanup_owned(Ref, _Node, Callbacks) ->
    Children = children(Ref, Callbacks),
    Resources = async_util:get(?RESOURCES(Ref), Callbacks, #{resources => []}),
    WithoutResources = lists:foldl(fun async_util:remove/2, Callbacks,
        [?RESOURCES(Ref) | maps:get(resources, Resources)]),
    WithoutIndex = async_util:remove(?CHILDREN(Ref), WithoutResources),
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
            remove_orphan_accumulator_ref(AccRef, Callbacks);
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
    case async_util:find(AccRef, Callbacks) of
        error -> Callbacks;
        {ok, _} ->
            case accumulator_in_use(AccRef, Callbacks) of
                true -> Callbacks;
                false -> async_util:remove(AccRef, Callbacks)
            end
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
        {ok, _} -> erlang:error({orphan_timer, Key})
    end;
timer_reference(#callback{}, Key, Callbacks) ->
    case async_util:find(ordinary_timer_key(Key), Callbacks) of
        error -> undefined;
        {ok, Timer} when is_reference(Timer) -> {ok, Timer};
        {ok, _} -> erlang:error({orphan_timer, Key})
    end.

-spec execute_node(continuation_node(), term(), success | failure,
    monad_runtime()) -> async_effect().
execute_node(#sf_callback{parent_ref = ParentRef}, Outcome, failure, MR) ->
    forward_failure(ParentRef, Outcome, MR);
execute_node(#{kind := ordinary_continuation, parent_ref := ParentRef},
        Outcome, failure, MR) ->
    forward_failure(ParentRef, Outcome, MR);
execute_node(#{kind := Kind, callback := Callback,
        parent_ref := ParentRef}, Outcome, _Mode, MR)
        when Kind =:= ordinary_continuation; Kind =:= ordinary_outer ->
    case async_util:apply_available_fun(Callback, [Outcome]) of
        {ok, {async_r_t, _} = Effect} -> Effect;
        {ok, Other} -> ?INVALID_CONTINUATION({invalid_continuation_return, Other});
        unavailable when ParentRef =:= none ->
            async_r_t:return(#async_legacy_delivery{outcome = Outcome}, MR);
        unavailable ->
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
    case async_util:apply_available_fun(Callback, [Outcome, Context]) of
        {ok, {async_r_t, _} = Effect} -> Effect;
        {ok, Other} -> erlang:error({invalid_sf_cc_return, Other});
        unavailable -> async_r_t:return(#async_legacy_delivery{outcome = Outcome}, MR)
    end;
invoke_outer(#{mode := direct_state, callback := Callback, context := Context},
        Outcome, MR) ->
    do([
        MR
     || CleanState <- async_r_t:do_get_state(MR),
        invoke_direct(Callback, Outcome, Context, CleanState, MR)
    ]).

invoke_direct(Callback, Outcome, Context, CleanState, MR) ->
    {arity, Arity} = erlang:fun_info(Callback, arity),
    Args = case Arity of
        2 -> [Outcome, Context];
        3 -> [Outcome, Context, CleanState]
    end,
    case async_util:apply_available_fun(Callback, Args) of
        unavailable -> async_r_t:return(#async_legacy_delivery{outcome = Outcome}, MR);
        {ok, _Ignored} when Arity =:= 2 ->
            async_r_t:return(Outcome, MR);
        {ok, NewState} ->
            case async_util:same_type_state(NewState, CleanState) of
                true -> monad:'>>'(async_r_t:do_put_state(NewState, MR),
                    async_r_t:return(Outcome, MR), MR);
                false -> erlang:error({invalid_sf_cc_return, NewState})
            end
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
        {already_removed, State1} ->
            {ok, [#{request_mref => Key, status => already_removed} | Results], State1};
        {ok, State1} ->
            Outcome = {error, {sf_aborted, Reason}},
            case ParentRef of
                none -> {ok, [#{request_mref => Key, status => handled} | Results], State1};
                _ ->
                    case dispatch_failure_continuation(
                            ParentRef, Outcome, Offset, State1) of
                        {ok, _Value, State2} ->
                            {ok, [#{request_mref => Key, status => forwarded} | Results], State2}
                    end
            end
    end.

take_action(Key, State, Get, Set) ->
    Callbacks = Get(State),
    case async_util:find(Key, Callbacks) of
        {ok, Value} ->
            case async_util:callback_value_kind(Value) of
                stable_action ->
                    Removed = take_action_value(Key, Value, Callbacks),
                    {ok, Set(remove_orphan_accumulator(Value, Removed), State)};
                _ -> erlang:error({invalid_callback_entry, Key})
            end;
        error -> {already_removed, State}
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
