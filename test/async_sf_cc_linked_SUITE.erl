-module(async_sf_cc_linked_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 45}}].
all() -> [all_runners_suspended, shared_parent_exactly_once,
    nested_frame_bridge, final_callback_boundaries].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_Config) -> ok.

%% R1/R2: all three sf runners register a suspended public action and consume
%% it through the same external callback lifecycle.
all_runners_suspended(_Config) ->
    Ref1 = make_ref(), Token1 = make_ref(),
    Effect = async_m:run_sf_cc(async_m:promise(Ref1),
        fun async_sf_cc_linked_fixture:async_callback/2, {self(), Token1}),
    {ok, State1} = identity:run(async_r_t:run(Effect,
        async_t:state_callbacks_gs(#state.callbacks), make_ref(), #state{})),
    [#{stable := false, parent_ref := Parent1}] = summaries(State1),
    ?assert(is_reference(Parent1)),
    {one, #state{callbacks = #{}}} = info({Ref1, one}, State1),
    assert_received({linked_callback, Token1, one}),

    Ref2 = make_ref(), Token2 = make_ref(),
    {ok, State2} = async_m:run_with_sf_cc(async_m:promise(Ref2),
        fun async_sf_cc_linked_fixture:state_callback/3, {self(), Token2},
        #state.callbacks, #state{}),
    [#{stable := false, parent_ref := Parent2}] = summaries(State2),
    ?assert(is_reference(Parent2)),
    {two, #state{callbacks = #{}}} = info({Ref2, two}, State2),
    assert_received({linked_callback, Token2, two, #{}}),

    Ref3 = make_ref(), Token3 = make_ref(),
    State3 = async_m:exec_sf_cc(async_m:promise(Ref3),
        fun async_sf_cc_linked_fixture:state_callback/3, {self(), Token3},
        #state.callbacks, #state{}),
    [#{stable := false}] = summaries(State3),
    {three, #state{callbacks = #{}}} = info({Ref3, three}, State3),
    assert_received({linked_callback, Token3, three, #{}}),

    WaitRef = make_ref(),
    erlang:send_after(0, self(), {WaitRef, waited}),
    ?assertEqual(waited, async_m:wait(async_m:promise(WaitRef))),
    ?assertEqual(immediate, async_m:wait_t(async_m:pure_return(immediate), #{})).

%% R11/R19: sibling stable actions share one direct parent. The first selected
%% failure forwards once; cleanup makes the sibling unhandled in the same commit.
shared_parent_exactly_once(_Config) ->
    A = make_ref(), B = make_ref(), Token = make_ref(),
    {[ok, ok], State0} = async_m:run_with_sf_cc(async_m:par([
        async_sf_do_linked_fixture:suspended(A),
        async_sf_do_linked_fixture:suspended(B)]),
        fun async_sf_cc_linked_fixture:state_callback/3, {self(), Token},
        #state.callbacks, #state{}),
    [First, Second] = Summaries = summaries(State0),
    ?assertEqual(2, length(Summaries)),
    Parent = maps:get(parent_ref, First),
    ?assertEqual(Parent, maps:get(parent_ref, Second)),
    {ok, Results, State1} = async_m:fail_sf_callbacks(fun(_) -> true end,
        shared_abort, #state.callbacks, State0),
    ?assertEqual([maps:get(request_mref, First), maps:get(request_mref, Second)],
        [maps:get(request_mref, R) || R <- Results]),
    ?assertEqual([forwarded, unhandled], [maps:get(status, R) || R <- Results]),
    ?assertEqual([], summaries(State1)),
    assert_received({linked_callback, Token,
        {error, {sf_aborted, shared_abort}}, #{}}),
    {ok, unhandled} = async_m:run_info({A, late}, #state.callbacks, State1),
    {ok, unhandled} = async_m:run_info({B, late}, #state.callbacks, State1),
    assert_not_received(Token).

%% R6/R8/R13/R14: nested generated frames and a frame-to-ordinary bridge
%% restore one node at a time while exposing only the current frame summary.
nested_frame_bridge(_Config) ->
    Ref = make_ref(), Token = make_ref(),
    {ok, State0} = run(async_sf_do_linked_fixture:nested(Ref), Token),
    [#{stable := true, frame := #{module := async_sf_do_linked_fixture,
        block := linked_inner, step_name := wait}}] = summaries(State0),
    {{ok, {outer, {ok, {inner, reply}}}}, State1} = info({Ref, reply}, State0),
    ?assertEqual([], summaries(State1)),
    assert_received({linked_callback, Token,
        {ok, {outer, {ok, {inner, reply}}}}, #{}}),

    BridgeRef = make_ref(), BridgeToken = make_ref(),
    {ok, Bridge0} = run(async_sf_do_linked_fixture:ordinary_bridge(BridgeRef),
        BridgeToken),
    {{ordinary, bridged}, Bridge1} = info({BridgeRef, bridged}, Bridge0),
    ?assertEqual([], summaries(Bridge1)),
    assert_received({linked_callback, BridgeToken, {ordinary, bridged}, #{}}).

%% R5/R10/R11/R16: direct callback sees cleaned State, callback exceptions keep
%% their class/reason, and no second callback is observable.
final_callback_boundaries(_Config) ->
    Ref = make_ref(), Token = make_ref(),
    {ok, State0} = async_m:run_with_sf_cc(
        async_sf_do_linked_fixture:suspended(Ref),
        fun async_sf_cc_linked_fixture:crash_callback/3, {self(), Token},
        #state.callbacks, #state{}),
    ?assertError(linked_callback_crash, info({Ref, reply}, State0)),
    assert_received({linked_callback, Token, {ok, {linked, reply}}, #{}}),
    assert_not_received(Token),

    Ref2 = make_ref(), Token2 = make_ref(),
    {ok, State2} = async_m:run_with_sf_cc(
        async_sf_do_linked_fixture:suspended(Ref2),
        fun async_sf_cc_linked_fixture:nested_undef_callback/3, {self(), Token2},
        #state.callbacks, #state{}),
    ?assertError(undef, info({Ref2, reply}, State2)),
    assert_received({linked_callback, Token2, {ok, {linked, reply}}, #{}}),
    assert_not_received(Token2).

run(Action, Token) -> async_m:run_with_sf_cc(Action,
    fun async_sf_cc_linked_fixture:state_callback/3, {self(), Token},
    #state.callbacks, #state{}).
info(Message, State) -> async_m:run_info(Message, #state.callbacks, State).
summaries(State) ->
    {ok, Result} = async_m:inspect_sf_callbacks(fun(_) -> true end,
        #state.callbacks, State),
    Result.
assert_received(Expected) ->
    receive Expected -> ok after 1000 -> ct:fail({missing_message, Expected}) end.
assert_not_received(Token) ->
    receive {linked_callback, Token, _} = Message -> ct:fail({duplicate, Message});
            {linked_callback, Token, _, _} = Message -> ct:fail({duplicate, Message})
    after 0 -> ok end.
