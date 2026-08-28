-module(async_sf_do_linked_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 45}}].
all() -> [monad_remainders, two_boundary_lifecycle, nested_and_bridge,
    capture_identity_manifest].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_Config) -> ok.

%% sf-do 1/2: generated bind/then frames return the original async_m Monad;
%% final action remains arbitrary and creates no manifest frame.
monad_remainders(_Config) ->
    ?assertEqual({ok, {4, 5}}, run_complete(
        async_sf_do_linked_fixture:immediate(4), immediate)),
    Bound = async_m:'>>='(async_sf_do_linked_fixture:immediate(5),
        fun(Value) -> async_m:pure_return({bound, Value}) end),
    ?assertEqual({bound, {5, 6}}, run_complete(Bound, bound)),
    ?assertEqual(final_value, run_complete(
        async_sf_do_linked_fixture:final_action(final_value), final)),
    ?assertEqual([], entries(linked_final)),
    ?assertEqual({outside_return, outside},
        async_sf_do_linked_fixture:return(outside)),
    ?assertEqual({remote_return, remote},
        async_sf_do_linked_fixture:remote_return(remote)).

%% sf-do 1/3/9: two generated boundaries retain one outer lifecycle; message
%% does not consume the first action and each reply advances exactly one node.
two_boundary_lifecycle(_Config) ->
    First = make_ref(), Second = make_ref(), Token = make_ref(),
    {ok, State0} = start(async_sf_do_linked_fixture:two_boundaries(First, Second), Token),
    [#{frame := #{block := linked_two, step_name := first}}] = summaries(State0),
    {{message, notice}, StateM} = info({message, First, notice}, State0),
    ?assertEqual(summaries(State0), summaries(StateM)),
    assert_no_completion(Token),
    {ok, State1} = info({First, one}, StateM),
    [#{frame := #{block := linked_two, step_name := second}}] = summaries(State1),
    {{ok, {one, two}}, State2} = info({Second, two}, State1),
    ?assertEqual([], summaries(State2)),
    assert_completion(Token, {ok, {one, two}}),
    {ok, unhandled} = async_m:run_info({First, late}, #state.callbacks, State2),
    {ok, unhandled} = async_m:run_info({Second, duplicate}, #state.callbacks, State2).

%% sf-do 3: nested sf_do and ordinary downstream composition restore through
%% direct parent links with no orphaned public action.
nested_and_bridge(_Config) ->
    Ref = make_ref(), Token = make_ref(),
    {ok, State0} = start(async_sf_do_linked_fixture:nested(Ref), Token),
    [#{frame := #{block := linked_inner}}] = summaries(State0),
    {{ok, {outer, {ok, {inner, value}}}}, State1} = info({Ref, value}, State0),
    ?assertEqual([], summaries(State1)),
    assert_completion(Token, {ok, {outer, {ok, {inner, value}}}}),
    Bridge = make_ref(), BridgeToken = make_ref(),
    {ok, Bridge0} = start(async_sf_do_linked_fixture:ordinary_bridge(Bridge), BridgeToken),
    {{ordinary, value}, Bridge1} = info({Bridge, value}, Bridge0),
    ?assertEqual([], summaries(Bridge1)),
    assert_completion(BridgeToken, {ordinary, value}).

%% sf-do 4-6: minimal captures, direct identity/MFA, literal Vsn and the single
%% documented public manifest shape.
capture_identity_manifest(_Config) ->
    Manifest = async_sf_do_linked_fixture:'$async_sf_manifest'(),
    ?assertEqual([format, frames, owner, protocol, vsn],
        lists:sort(maps:keys(Manifest))),
    ?assertEqual(async_sf_do_linked_fixture, maps:get(owner, Manifest)),
    ?assertEqual({2, 4, 0}, maps:get(vsn, Manifest)),
    ?assertEqual(1, maps:get(format, Manifest)),
    ?assertEqual(1, maps:get(protocol, Manifest)),
    lists:foreach(fun(Frame) ->
        ?assertEqual([identity, mfa, required_captures],
            lists:sort(maps:keys(Frame)))
    end, maps:get(frames, Manifest)),
    Capture = entry(linked_capture, wait),
    ?assertEqual({1, linked_capture, wait}, maps:get(identity, Capture)),
    ?assertEqual({async_sf_do_linked_fixture, '$sf_linked_capture__wait', 2},
        maps:get(mfa, Capture)),
    ?assertEqual(['Used'], maps:get(required_captures, Capture)),
    ?assertEqual(false, contains_any(Manifest,
        [async_id, hash, digest, ast, env, context, index, parent,
         final_descriptor, history])),
    Ref = make_ref(), Token = make_ref(),
    {ok, State0} = start(async_sf_do_linked_fixture:capture(used, unused, Ref), Token),
    [Summary] = summaries(State0),
    ?assertEqual(false, contains_term(Summary, unused)),
    {{ok, {used, reply}}, State1} = info({Ref, reply}, State0),
    ?assertEqual([], summaries(State1)),
    assert_completion(Token, {ok, {used, reply}}).

sf_cc(Token) -> #{kind => sf_cc, protocol => 1,
    mfa => {async_sf_do_linked_fixture, complete, 2}, context => {self(), Token}}.
start(Action, Token) -> async_m:run_sf_frame(
    Action, sf_cc(Token), #state.callbacks, #state{}).
run_complete(Action, Token) ->
    {Outcome, #state{callbacks = #{}}} = start(Action, Token),
    assert_completion(Token, Outcome), Outcome.
info(Message, State) -> async_m:run_info(Message, #state.callbacks, State).
summaries(State) ->
    {ok, Result} = async_m:inspect_sf_callbacks(fun(_) -> true end,
        #state.callbacks, State), Result.
assert_completion(Token, Outcome) ->
    receive {sf_do_linked_complete, Token, Outcome} -> ok
    after 1000 -> ct:fail({missing_completion, Token, Outcome}) end.
assert_no_completion(Token) ->
    receive {sf_do_linked_complete, Token, Outcome} ->
        ct:fail({unexpected_completion, Token, Outcome})
    after 0 -> ok end.
entries(Block) ->
    [E || E = #{identity := {1, B, _}} <- maps:get(frames,
        async_sf_do_linked_fixture:'$async_sf_manifest'()), B =:= Block].
entry(Block, Step) ->
    [Entry] = [E || E = #{identity := {1, B, S}} <- maps:get(frames,
        async_sf_do_linked_fixture:'$async_sf_manifest'()),
        B =:= Block, S =:= Step], Entry.

contains_term(Term, Needle) when Term =:= Needle -> true;
contains_term(Term, Needle) when is_map(Term) ->
    lists:any(fun({K,V}) -> contains_term(K, Needle) orelse contains_term(V, Needle) end,
        maps:to_list(Term));
contains_term(Term, Needle) when is_tuple(Term) -> contains_term(tuple_to_list(Term), Needle);
contains_term(Term, Needle) when is_list(Term) ->
    lists:any(fun(E) -> contains_term(E, Needle) end, Term);
contains_term(_, _) -> false.
contains_any(Term, Tags) when is_map(Term) ->
    lists:any(fun(Tag) -> maps:is_key(Tag, Term) end, Tags) orelse
        lists:any(fun({K,V}) -> contains_any(K, Tags) orelse contains_any(V, Tags) end,
            maps:to_list(Term));
contains_any(Term, Tags) when is_tuple(Term) -> contains_any(tuple_to_list(Term), Tags);
contains_any(Term, Tags) when is_list(Term) ->
    lists:any(fun(E) -> contains_any(E, Tags) end, Term);
contains_any(Term, Tags) -> lists:member(Term, Tags).
