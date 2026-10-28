%%% Public black-box acceptance for the sf continuation facade.
-module(async_sf_cc_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 45}}].
all() -> [runner_modes_and_context, public_outer_lifecycle,
    message_reply_timeout_down, inspection_privacy_and_backends,
    matcher_atomicity, active_failure_statuses, info_routing_boundaries,
    upgrade_badfun_forwarding, exception_boundaries].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_Case, Config) -> flush_mailbox(), Config.
end_per_testcase(_Case, _Config) -> flush_mailbox(), ok.

%% R1/R3-R5: sf-prefixed runner modes retain opaque Context and preserve return mode.
runner_modes_and_context(_Config) ->
    Token = make_ref(),
    Effect = async_m:run_sf_cc(async_m:pure_return(effect),
        fun ?MODULE:async2/2, {self(), immediate, {opaque, Token}}),
    {effect, #state{callbacks = #{}}} = identity:run(async_r_t:run(
        Effect, async_t:state_callbacks_gs(#state.callbacks), make_ref(), #state{})),
    assert_received({immediate, effect, {opaque, Token}}),
    State0 = #state{events = [ordinary]},
    {direct2, State0} = async_m:run_with_sf_cc(async_m:pure_return(direct2),
        fun ?MODULE:direct2/2, {self(), direct2, Token},
        #state.callbacks, State0),
    assert_received({direct2, direct2, Token}),
    {direct3, State1} = async_m:run_with_sf_cc(async_m:pure_return(direct3),
        fun ?MODULE:direct3/3, {self(), direct3, Token},
        #state.callbacks, State0),
    ?assertEqual([direct3, ordinary], State1#state.events),
    assert_received({direct3, direct3, Token, empty}),
    State2 = async_m:exec_sf_cc(async_m:pure_return(exec),
        fun ?MODULE:direct3/3, {self(), exec, Token},
        #state.callbacks, State0),
    ?assertEqual([exec, ordinary], State2#state.events),
    assert_received({exec, exec, Token, empty}).

%% R2/R6-R10: immediate has no pending action; suspended actions have a direct
%% parent, clean before callback, and complete once.
public_outer_lifecycle(_Config) ->
    {done, #state{callbacks = #{}}} = run(async_m:pure_return(done), immediate),
    assert_received({immediate, done, immediate, empty}),
    OrdinaryRef = make_ref(),
    {ok, Ordinary0} = run(async_m:promise(OrdinaryRef), ordinary),
    [Ordinary] = summaries(Ordinary0),
    ?assertEqual(false, maps:get(stable, Ordinary)),
    ?assertEqual(ordinary_fun, maps:get(continuation_kind, Ordinary)),
    ?assertEqual(undefined, maps:get(frame, Ordinary)),
    ?assert(is_reference(maps:get(parent_ref, Ordinary))),
    {ordinary_reply, OrdinaryDone} = info({OrdinaryRef, ordinary_reply}, Ordinary0),
    ?assertEqual([], summaries(OrdinaryDone)),
    assert_received({ordinary, ordinary_reply, ordinary, empty}),
    {ok, unhandled} = async_m:run_info({OrdinaryRef, late},
        #state.callbacks, OrdinaryDone),
    assert_not_received(ordinary),
    StableRef = make_ref(),
    {ok, Stable0} = run(sf_do_fixture:suspended(StableRef), stable),
    [Stable] = summaries(Stable0),
    ?assertEqual(true, maps:get(stable, Stable)),
    ?assertEqual(sf_frame, maps:get(continuation_kind, Stable)),
    ?assert(is_reference(maps:get(parent_ref, Stable))),
    #{module := sf_do_fixture, protocol := 1, block := suspended_flow,
      step_name := wait, vsn := undefined} = maps:get(frame, Stable),
    {{ok, {resumed, stable_reply}}, StableDone} = info({StableRef, stable_reply}, Stable0),
    ?assertEqual([], summaries(StableDone)),
    assert_received({stable, {ok, {resumed, stable_reply}}, stable, empty}),
    {ok, unhandled} = async_m:run_info({StableRef, duplicate},
        #state.callbacks, StableDone),
    assert_not_received(stable).

%% R11-R12: message invokes current continuation without consuming the action.
message_reply_timeout_down(_Config) ->
    Ref = make_ref(),
    {ok, State0} = run(sf_do_fixture:suspended(Ref), message_reply),
    Summary0 = summaries(State0),
    {{message, notice}, State1} = info({message, Ref, notice}, State0),
    ?assertEqual(Summary0, summaries(State1)),
    assert_not_received(message_reply),
    {{ok, {resumed, reply}}, State2} = info({Ref, reply}, State1),
    ?assertEqual([], summaries(State2)),
    assert_received({message_reply, {ok, {resumed, reply}}, message_reply, empty}),
    TimeoutRef = make_ref(),
    {ok, Timeout0} = run(sf_do_fixture:suspended(TimeoutRef), timeout_case),
    {{ok, {resumed, {error, timeout}}}, Timeout1} =
        info({TimeoutRef, {error, timeout}}, Timeout0),
    ?assertEqual([], summaries(Timeout1)),
    assert_received({timeout_case, {ok, {resumed, {error, timeout}}}, timeout_case, empty}),
    DownRef = make_ref(),
    {ok, Down0} = run(sf_do_fixture:suspended(DownRef), down_case),
    {{ok, {resumed, {error, {process_down, gone}}}}, Down1} =
        info({'DOWN', DownRef, process, self(), gone}, Down0),
    ?assertEqual([], summaries(Down1)),
    assert_received({down_case, {ok, {resumed, {error, {process_down, gone}}}},
        down_case, empty}).

%% R15/R17: public summaries are sanitized and backend-independent.
inspection_privacy_and_backends(_Config) ->
    lists:foreach(fun(Callbacks) ->
        Secret = {secret, make_ref()}, Ref = make_ref(),
        State0 = #state{callbacks = Callbacks, events = [Secret]},
        {ok, State1} = async_m:run_with_sf_cc(sf_do_fixture:capture(Secret, ignored, Ref),
            fun ?MODULE:direct3/3, {self(), backend, Secret},
            #state.callbacks, State0),
        [Summary] = summaries(State1),
        ?assertEqual([continuation_kind, frame, parent_ref, request_mref, stable],
            lists:sort(maps:keys(Summary))),
        ?assertEqual(false, contains_term(Summary, Secret)),
        {{ok, {Secret, reply}}, State2} = info({Ref, reply}, State1),
        ?assertEqual([], summaries(State2)),
        assert_received({backend, {ok, {Secret, reply}}, Secret, empty})
    end, [#{}, [], dict:new()]).

%% R18: matcher exception/non-boolean happens before any commit.
matcher_atomicity(_Config) ->
    A = make_ref(), B = make_ref(),
    {[ok, ok], State0} = run(async_m:par([sf_do_fixture:suspended(A),
        sf_do_fixture:suspended(B)]), atomic),
    MRefs = [maps:get(request_mref, S) || S <- summaries(State0)],
    put(matcher_seen, []),
    CrashLast = fun(S) ->
        Seen = get(matcher_seen) ++ [maps:get(request_mref, S)],
        put(matcher_seen, Seen),
        case length(Seen) =:= length(MRefs) of
            true -> erlang:error(matcher_crash);
            false -> true
        end
    end,
    ?assertError(matcher_crash, async_m:fail_sf_callbacks(
        CrashLast, abort, #state.callbacks, State0)),
    ?assertEqual(MRefs, get(matcher_seen)),
    ?assertEqual(MRefs, [maps:get(request_mref, S) || S <- summaries(State0)]),
    assert_not_received(atomic),
    try async_m:fail_sf_callbacks(fun(_) -> 'maybe' end, abort,
            #state.callbacks, State0) of
        Other -> ct:fail({expected_invalid_matcher_return, Other})
    catch
        error:{invalid_matcher_return, 'maybe'} -> ok
    end,
    ?assertEqual(length(MRefs), length(summaries(State0))),
    erase(matcher_seen).

%% R19: stable commits are ordered; ordinary actions remain pending.
active_failure_statuses(_Config) ->
    StableRef = make_ref(),
    {ok, Stable0} = run(sf_do_fixture:suspended(StableRef), fail_stable),
    [StableSummary] = summaries(Stable0),
    StableMRef = maps:get(request_mref, StableSummary),
    {ok, [#{request_mref := StableMRef, status := forwarded}], Stable1} =
        async_m:fail_sf_callbacks(fun(_) -> true end, requested,
            #state.callbacks, Stable0),
    ?assertEqual([], summaries(Stable1)),
    assert_received({fail_stable, {error, {sf_aborted, requested}}, fail_stable, empty}),
    {ok, [], Stable1} = async_m:fail_sf_callbacks(fun(_) -> true end,
        duplicate, #state.callbacks, Stable1),
    OrdinaryRef = make_ref(),
    {ok, Ordinary0} = run(async_m:promise(OrdinaryRef), fail_ordinary),
    [OrdinarySummary] = summaries(Ordinary0),
    OrdinaryMRef = maps:get(request_mref, OrdinarySummary),
    {ok, [#{request_mref := OrdinaryMRef, status := ordinary}], Ordinary0} =
        async_m:fail_sf_callbacks(fun(_) -> true end, requested,
            #state.callbacks, Ordinary0),
    {ordinary_done, Ordinary1} = info({OrdinaryRef, ordinary_done}, Ordinary0),
    ?assertEqual([], summaries(Ordinary1)),
    assert_received({fail_ordinary, ordinary_done, fail_ordinary, empty}).

%% A non-async message, a missing action callback, and a broken continuation
%% are three distinct routing outcomes. The legacy run_info API retains its
%% historical unhandled result for the first two cases.
info_routing_boundaries(_Config) ->
    State0 = #state{},
    {not_async, State0} = async_m:route_info(application_message,
        #state.callbacks, State0),
    MissingRef = make_ref(),
    {missing_callback,
        #{type := reply, ref := MissingRef, value := late}, State0} =
        async_m:route_info({MissingRef, late}, #state.callbacks, State0),
    {missing_callback,
        #{type := message, ref := MissingRef, value := progress}, State0} =
        async_m:route_info({message, MissingRef, progress},
            #state.callbacks, State0),
    {ok, unhandled} = async_m:run_info({MissingRef, late},
        #state.callbacks, State0),

    InvalidRef = make_ref(),
    InvalidState = State0#state{callbacks = #{InvalidRef => invalid}},
    ?assertError({invalid_callback_entry, InvalidRef},
        async_m:route_info({InvalidRef, reply},
            #state.callbacks, InvalidState)),

    ActionRef = make_ref(),
    {ok, PendingState} = run(async_m:promise(ActionRef), missing_parent),
    [#{parent_ref := ParentRef}] = summaries(PendingState),
    {missing_callback,
        #{type := reply, ref := ParentRef, value := not_an_action},
        PendingState} = async_m:route_info({ParentRef, not_an_action},
            #state.callbacks, PendingState),
    BrokenCallbacks = async_util:remove(
        ParentRef, PendingState#state.callbacks),
    BrokenState = PendingState#state{callbacks = BrokenCallbacks},
    ?assertError({invalid_continuation, {missing_node, ParentRef}},
        info({ActionRef, reply}, BrokenState)),
    assert_not_received(missing_parent).

%% A purged local fun is an unavailable current continuation, not a callback
%% body exception. It becomes an sf_runtime failure and moves one parent only.
upgrade_badfun_forwarding(_Config) ->
    Module = async_upgrade_badfun_fixture,
    Ref = make_ref(),
    {ok, State0} = async_m:run_with_cc(async_m:promise(Ref), Module:cc(),
        #state.callbacks, #state{}),
    {Module, OriginalBinary, OriginalPath} = code:get_object_code(Module),
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 2, export, [{replacement, 0}]},
        {function, 3, replacement, 0,
            [{clause, 3, [], [], [{atom, 3, ok}]}]}
    ],
    {ok, Module, ReplacementBinary} = compile:forms(Forms, [binary]),
    try
        {module, Module} = code:load_binary(
            Module, "async_upgrade_badfun_fixture_replacement", ReplacementBinary),
        _ = code:purge(Module),
        {{legacy_outcome, reply}, State1} =
            info({Ref, reply}, State0),
        ?assertEqual([], summaries(State1))
    after
        code:purge(Module),
        code:delete(Module),
        {module, Module} = code:load_binary(
            Module, OriginalPath, OriginalBinary)
    end.

%% R13/R16: business exceptions retain class/reason.
exception_boundaries(_Config) ->
    Ref = make_ref(),
    Action = async_m:'>>='(async_m:promise(Ref),
        fun(_) -> erlang:error(business_crash) end),
    {ok, State0} = run(Action, business),
    ?assertError(business_crash, info({Ref, reply}, State0)),
    assert_not_received(business).

run(Action, Tag) -> async_m:run_with_sf_cc(Action, fun ?MODULE:direct3/3,
    {self(), Tag, Tag}, #state.callbacks, #state{}).
info(Message, State) -> async_m:run_info(Message, #state.callbacks, State).
summaries(State) ->
    {ok, Summaries} = async_m:inspect_sf_callbacks(fun(_) -> true end,
        #state.callbacks, State),
    Summaries.

async2(Outcome, {Pid, Tag, Opaque}) ->
    Pid ! {Tag, Outcome, Opaque},
    async_r_t:return(Outcome, async_r_t:new(identity)).
direct2(Outcome, {Pid, Tag, Opaque}) -> Pid ! {Tag, Outcome, Opaque}, ignored.
direct3(Outcome, {Pid, Tag, Opaque}, #state{} = State) ->
    Pending = case summaries(State) of [] -> empty; _ -> pending end,
    Pid ! {Tag, Outcome, Opaque, Pending},
    State#state{events = [Tag | State#state.events]}.

contains_term(Term, Needle) when Term =:= Needle -> true;
contains_term(Term, Needle) when is_map(Term) ->
    lists:any(fun({K, V}) -> contains_term(K, Needle) orelse
        contains_term(V, Needle) end, maps:to_list(Term));
contains_term(Term, Needle) when is_tuple(Term) -> contains_term(tuple_to_list(Term), Needle);
contains_term(Term, Needle) when is_list(Term) ->
    lists:any(fun(E) -> contains_term(E, Needle) end, Term);
contains_term(_, _) -> false.

assert_received(Expected) ->
    receive Expected -> ok after 1000 -> ct:fail({missing_message, Expected}) end.
assert_not_received(Tag) ->
    receive Message when element(1, Message) =:= Tag ->
        ct:fail({unexpected_message, Message})
    after 0 -> ok end.
flush_mailbox() -> receive _ -> flush_mailbox() after 0 -> ok end.
