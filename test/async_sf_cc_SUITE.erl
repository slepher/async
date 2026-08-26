%%% Document-driven black-box coverage for the sf-cc runtime contract.
-module(async_sf_cc_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 45}}].

all() ->
    [runner_modes_and_context,
     pending_store_and_scopes,
     completion_cleanup_exactly_once,
     message_reply_timeout_down,
     frame_terminal_and_structural_completion,
     old_three_tuple_resumable,
     old_three_tuple_unavailable,
     old_three_tuple_classifier_negatives,
     matcher_privacy_and_atomicity,
     active_fail_dedupe].

init_per_suite(Config) ->
    async:start(),
    Config.

end_per_suite(_Config) ->
    cleanup_old_module(),
    ok.

init_per_testcase(_Case, Config) ->
    flush_mailbox(),
    Config.

end_per_testcase(_Case, _Config) ->
    flush_mailbox(),
    ok.

%% R1, R4, R5: public runners share descriptors but preserve mode contracts.
runner_modes_and_context(_Config) ->
    Context = {opaque, make_ref()},
    Descriptor = async_sf_cc:new(direct_state, fun ?MODULE:direct2/2, Context),
    ?assertEqual(sf_cc, maps:get(kind, Descriptor)),
    ?assertEqual(direct_state, maps:get(mode, Descriptor)),
    ?assert(is_function(maps:get(callback, Descriptor), 2)),
    ?assertEqual(Context, maps:get(context, Descriptor)),
    ?assertEqual(false, maps:is_key(mfa, Descriptor)),

    State0 = #state{events = [before]},
    {direct2, State0} = async_m:run_with_sf_cc(
        async_m:pure_return(direct2), fun ?MODULE:direct2/2,
        {self(), direct2}, #state.callbacks, State0),
    assert_received({direct2, direct2}),

    {direct3, State1} = async_m:run_with_sf_cc(
        async_m:pure_return(direct3), fun ?MODULE:direct3/3,
        {self(), direct3}, #state.callbacks, State0),
    ?assertEqual([direct3 | State0#state.events], State1#state.events),
    assert_received({direct3, direct3, #{}}),

    MR = async_r_t:new(identity),
    Effect = async_m:run_sf_cc(async_m:pure_return(async_mode),
        fun ?MODULE:async2/2, {self(), async_mode}),
    {async_mode, #state{callbacks = #{}}} = identity:run(
        async_r_t:run(Effect, async_t:state_callbacks_gs(#state.callbacks),
            make_ref(), #state{})),
    assert_received({async_mode, async_mode}),
    ?assertException(error, {invalid_sf_cc, _},
        async_sf_cc:new(async_r_t, fun ?MODULE:direct3/3, bad)),
    ?assertMatch({async_r_t, _}, async_r_t:return(ok, MR)).

%% R6-R8: action/CcRef storage and AccRef/CcRef scopes are distinct.
pending_store_and_scopes(_Config) ->
    RefA = make_ref(),
    RefB = make_ref(),
    Action = async_m:'>>='(async_m:promise(RefA),
        fun(_) -> async_m:promise(RefB) end),
    {ok, State0} = pending(Action, scope),
    {RefA, AccRef, CcRef, Continuation0} = only_action(State0),
    ?assert(is_reference(AccRef)),
    ?assert(is_reference(CcRef)),
    ?assertNotEqual(AccRef, CcRef),
    ?assertEqual(true, async_util:callback_exists(State0#state.callbacks)),
    ?assertMatch({ok, #{kind := sf_cc_ref}},
        async_util:find(CcRef, State0#state.callbacks)),
    ?assert(is_function(Continuation0, 1)),
    ?assertEqual({ok, unhandled}, async_m:run_info({CcRef, ignored},
        #state.callbacks, State0)),

    {ok, State1} = async_m:run_info({RefA, first}, #state.callbacks, State0),
    {RefB, AccRef, CcRef, _Continuation1} = only_action(State1),
    {second, State2} = async_m:run_info(
        {RefB, second}, #state.callbacks, State1),
    ?assertEqual(#{}, State2#state.callbacks),
    assert_received({scope, second, #{}}),

    LocalRef = make_ref(),
    ReboundAccRef = make_ref(),
    {ok, LocalState} = pending(
        async_m:local_ref(ReboundAccRef, async_m:promise(LocalRef)), rebound),
    {LocalRef, ReboundAccRef, ReboundCcRef, _} = only_action(LocalState),
    ?assert(is_reference(ReboundCcRef)),
    ?assertNotEqual(ReboundAccRef, ReboundCcRef).

%% R9-R10: cleanup is visible before callback and completion cannot retry.
completion_cleanup_exactly_once(_Config) ->
    Ref = make_ref(),
    {ok, State0} = pending(async_m:promise(Ref), clean_once),
    {Ref, AccRef, CcRef, _} = only_action(State0),
    StateWithAcc = State0#state{callbacks = maps:put(AccRef, old_acc,
        State0#state.callbacks)},
    {{ok, completed}, CleanState} = async_sf_cc:complete_state(
        CcRef, {ok, completed}, #state.callbacks, StateWithAcc),
    ?assertEqual(#{}, CleanState#state.callbacks),
    assert_received({clean_once, {ok, completed}, #{}}),
    {unhandled, CleanState} = async_sf_cc:complete_state(
        CcRef, duplicate, #state.callbacks, CleanState),
    assert_not_received(clean_once).

%% R11: message preserves both entries; reply/timeout/DOWN complete once.
message_reply_timeout_down(_Config) ->
    Ref = make_ref(),
    {ok, State0} = pending(async_m:promise(Ref), message_reply),
    {{message, notice}, State1} = async_m:run_info(
        {message, Ref, notice}, #state.callbacks, State0),
    ?assertEqual(State0#state.callbacks, State1#state.callbacks),
    assert_not_received(message_reply),
    {reply, State2} = async_m:run_info({Ref, reply},
        #state.callbacks, State1),
    ?assertEqual(#{}, State2#state.callbacks),
    assert_received({message_reply, reply, #{}}),

    TimeoutRef = make_ref(),
    {ok, TimeoutState} = pending(async_m:promise(TimeoutRef), timeout_case),
    {{error, timeout}, TimeoutDone} = async_m:run_info(
        {TimeoutRef, {error, timeout}}, #state.callbacks, TimeoutState),
    ?assertEqual(#{}, TimeoutDone#state.callbacks),
    assert_received({timeout_case, {error, timeout}, #{}}),

    DownRef = make_ref(),
    {ok, DownState} = pending(async_m:promise(DownRef), down_case),
    {{error, {process_down, gone}}, DownDone} = async_m:run_info(
        {'DOWN', DownRef, process, self(), gone}, #state.callbacks, DownState),
    ?assertEqual(#{}, DownDone#state.callbacks),
    assert_received({down_case, {error, {process_down, gone}}, #{}}).

%% R12-R13: persisted frames end in CcRef only; record CcRef drives failure.
frame_terminal_and_structural_completion(_Config) ->
    Ref = make_ref(),
    {ok, State0} = pending(sf_do_fixture:suspended(Ref), frame_case),
    {Ref, _AccRef, CcRef, Continuation} = only_action(State0),
    Terminal = terminal(Continuation),
    ?assertEqual(async_sf_cc:terminal(CcRef), Terminal),
    ?assertEqual(false, contains_key(Continuation, cc)),
    Corrupt = Continuation#{frame => broken},
    State1 = replace_action(Ref, Corrupt, State0),
    {{error, {sf_runtime, {invalid_frame, broken}}}, Done} =
        async_m:run_info({Ref, input}, #state.callbacks, State1),
    ?assertEqual(#{}, Done#state.callbacks),
    assert_received({frame_case,
        {error, {sf_runtime, {invalid_frame, broken}}}, #{}}).

%% R14-R15: real three-tuple state, real old-code purge, and narrow negatives.
old_three_tuple_resumable(_Config) ->
    AccRef = make_ref(),
    UsableRef = make_ref(),
    Usable = fun(Input) -> async_r_t:return({old_resumed, Input},
        async_r_t:new(identity)) end,
    UsableState = #state{callbacks = #{
        UsableRef => {callback, Usable, AccRef}, AccRef => old_local}},
    {{old_resumed, reply}, UsableDone} = async_m:run_info(
        {UsableRef, reply}, #state.callbacks, UsableState),
    ?assertEqual(#{AccRef => old_local}, UsableDone#state.callbacks),
    {ok, [OldSummary]} = async_m:inspect_sf_callbacks(
        fun(_) -> true end, #state.callbacks, UsableState),
    ?assertEqual(false, maps:get(protected, OldSummary)),
    ?assertEqual(undefined, maps:get(sf_cc_mref, OldSummary)).

old_three_tuple_unavailable(_Config) ->
    AccRef = make_ref(),
    PurgedRef = make_ref(),
    PurgedFun = purged_old_fun(),
    PurgedState = #state{callbacks = #{
        PurgedRef => {callback, PurgedFun, AccRef}, AccRef => old_local}},
    {{legacy_outcome, purged_reply, PurgedClean}, PurgedClean} = async_m:run_info(
        {PurgedRef, purged_reply}, #state.callbacks, PurgedState),
    {legacy_outcome, purged_reply, PurgedClean} = async_m:handle_info(
        {PurgedRef, purged_reply}, #state.callbacks, PurgedState),
    ?assertEqual(#{}, PurgedClean#state.callbacks).

old_three_tuple_classifier_negatives(_Config) ->
    AccRef = make_ref(),
    NestedRef = make_ref(),
    NestedUndef = fun(_) -> async_sf_cc_missing_nested:missing() end,
    NestedState = #state{callbacks = #{
        NestedRef => {callback, NestedUndef, AccRef}}},
    ?assertException(error, undef,
        async_m:handle_info({NestedRef, nested}, #state.callbacks, NestedState)),

    BusinessRef = make_ref(),
    Business = fun(_) -> erlang:error(business_failure) end,
    BusinessState = #state{callbacks = #{
        BusinessRef => {callback, Business, AccRef}}},
    ?assertException(error, business_failure,
        async_m:handle_info({BusinessRef, business}, #state.callbacks,
            BusinessState)).

%% R16-R17/R19: sanitized ordered summaries and bit-for-bit atomic preflight.
matcher_privacy_and_atomicity(_Config) ->
    {State0, MRefs, CcRef} = matcher_state(privacy),
    put(matcher_calls, []),
    Matcher = fun(Summary) ->
        put(matcher_calls, get(matcher_calls) ++ [maps:get(request_mref, Summary)]),
        true
    end,
    {ok, Summaries} = async_m:inspect_sf_callbacks(
        Matcher, #state.callbacks, State0),
    ?assertEqual(lists:sort(MRefs), get(matcher_calls)),
    [Summary | _] = Summaries,
    ?assertEqual([continuation_kind, frames, protected, request_mref, sf_cc_mref],
        lists:sort(maps:keys(Summary))),
    [FrameSummary, DownstreamSummary] = maps:get(frames, Summary),
    ?assertEqual({2, 4, 0}, maps:get(vsn, FrameSummary)),
    ?assertEqual(async_sf_cc, maps:get(module, DownstreamSummary)),
    ?assertEqual(undefined, maps:get(vsn, DownstreamSummary)),
    ?assertEqual([async_id, block_path, module, protocol, step_name, vsn],
        lists:sort(maps:keys(FrameSummary))),
    ?assertEqual(false, contains_term(Summary, secret_env)),
    ?assertEqual(false, contains_term(Summary, secret_context)),

    put(preflight_count, 0),
    CrashSecond = fun(_S) ->
        N = get(preflight_count) + 1,
        put(preflight_count, N),
        case N of 1 -> true; 2 -> erlang:error(matcher_crash) end
    end,
    {error, {matcher_failed, error, matcher_crash}, State0} =
        async_m:fail_sf_callbacks(CrashSecond, abort,
            #state.callbacks, State0),
    ?assertMatch({ok, #{kind := sf_cc_ref}},
        async_util:find(CcRef, State0#state.callbacks)),
    assert_not_received(privacy),
    {error, {invalid_matcher_return, 'maybe'}, State0} =
        async_m:fail_sf_callbacks(fun(_) -> 'maybe' end, abort,
            #state.callbacks, State0),
    erase(matcher_calls),
    erase(preflight_count).

%% R18: protected callbacks dedupe by CcRef; old tuples remain untouched.
active_fail_dedupe(_Config) ->
    {Protected0, _MRefs, _CcRef} = matcher_state(dedupe),
    OldRef = make_ref(),
    Old = {callback, fun(A) -> async_m:pure_return(A) end, make_ref()},
    State0 = Protected0#state{callbacks = maps:put(OldRef, Old,
        Protected0#state.callbacks)},
    {ok, Results, State1} = async_m:fail_sf_callbacks(
        fun(_) -> true end, requested, #state.callbacks, State0),
    ?assertEqual(1, length([R || R = #{status := completed} <- Results])),
    ?assertEqual(1, length([R || R = #{status := unprotected} <- Results])),
    ?assertEqual({ok, Old}, async_util:find(OldRef, State1#state.callbacks)),
    receive
        {dedupe, {error, {sf_aborted, requested}}, _CleanCallbacks} -> ok
    after 1000 ->
        ct:fail(missing_dedupe_completion)
    end,
    assert_not_received(dedupe).

pending(Action, Tag) ->
    async_m:run_with_sf_cc(Action, fun ?MODULE:direct3/3,
        {self(), Tag}, #state.callbacks, #state{}).

direct2(Outcome, {Pid, Tag}) ->
    Pid ! {Tag, Outcome},
    ignored_return.

direct3(Outcome, {Pid, Tag}, #state{} = CleanState) ->
    Pid ! {Tag, Outcome, CleanState#state.callbacks},
    CleanState#state{events = [Tag | CleanState#state.events]}.

async2(Outcome, {Pid, Tag}) ->
    Pid ! {Tag, Outcome},
    async_r_t:return(Outcome, async_r_t:new(identity)).

only_action(#state{callbacks = Callbacks}) ->
    [{MRef, Decoded}] = [{Key, D} || {Key, Value} <- maps:to_list(Callbacks),
        {ok, D} <- [async_util:decode_callback(Value)]],
    {MRef, maps:get(acc_ref, Decoded), maps:get(sf_cc_mref, Decoded),
        maps:get(cc, Decoded)}.

replace_action(MRef, NewCc, #state{callbacks = Callbacks} = State) ->
    {ok, Decoded} = async_util:decode_callback(maps:get(MRef, Callbacks)),
    New = {callback, NewCc, maps:get(acc_ref, Decoded),
        maps:get(sf_cc_mref, Decoded)},
    State#state{callbacks = maps:put(MRef, New, Callbacks)}.

terminal(#{kind := sf_next, downstream := Downstream}) -> terminal(Downstream);
terminal(#{kind := sf_legacy_next, downstream := Downstream}) -> terminal(Downstream);
terminal(#{kind := sf_cc_ref} = Terminal) -> Terminal.

matcher_state(Tag) ->
    CcRef = make_ref(),
    AccRef = make_ref(),
    MRef1 = make_ref(),
    MRef2 = make_ref(),
    Descriptor = async_sf_cc:new(direct_state, fun ?MODULE:direct3/3,
        {self(), Tag}),
    Entry = async_sf_cc:new_ref(CcRef, Descriptor#{context := {self(), Tag}}),
    Frame = #{kind => sf_frame, protocol => 1, module => ?MODULE,
        vsn => {2, 4, 0}, block_path => [outer], step_name => wait,
        async_id => <<1,2,3>>, mfa => {?MODULE, missing_generated, 2},
        env => #{private => secret_env}},
    DownstreamFrame = #{kind => sf_frame, protocol => 1,
        module => async_sf_cc, vsn => undefined, block_path => [inner],
        step_name => finish, async_id => <<4,5,6>>,
        mfa => {async_sf_cc, missing_generated, 2}, env => #{}},
    Downstream = #{kind => sf_next, protocol => 1, frame => DownstreamFrame,
        downstream => async_sf_cc:terminal(CcRef)},
    Continuation = #{kind => sf_next, protocol => 1, frame => Frame,
        downstream => Downstream},
    Callbacks = #{
        CcRef => Entry,
        MRef1 => {callback, Continuation, AccRef, CcRef},
        MRef2 => {callback, Continuation, AccRef, CcRef},
        AccRef => secret_accumulator},
    {#state{callbacks = Callbacks, events = [secret_context]},
        [MRef1, MRef2], CcRef}.

purged_old_fun() ->
    Module = async_sf_cc_old_runtime_fixture,
    {ok, Module, V1} = compile_old_module(Module, old_v1),
    {module, Module} = code:load_binary(Module, "old_v1.erl", V1),
    Fun = Module:make(),
    {ok, Module, V2} = compile_old_module(Module, old_v2),
    {module, Module} = code:load_binary(Module, "old_v2.erl", V2),
    _ = code:purge(Module),
    Fun.

compile_old_module(Module, Marker) ->
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 2, export, [{make, 0}]},
        {function, 3, make, 0, [{clause, 3, [], [], [
            {'fun', 3, {clauses, [{clause, 3, [{var, 3, 'Input'}], [], [
                {call, 3, {remote, 3, {atom, 3, async_r_t},
                    {atom, 3, return}}, [
                    {tuple, 3, [{atom, 3, Marker}, {var, 3, 'Input'}]},
                    {call, 3, {remote, 3, {atom, 3, async_r_t},
                        {atom, 3, new}}, [{atom, 3, identity}]}
                ]}
            ]}]}}
        ]}]}
    ],
    compile:forms(Forms, [binary, debug_info]).

cleanup_old_module() ->
    Module = async_sf_cc_old_runtime_fixture,
    code:purge(Module),
    code:delete(Module),
    ok.

contains_key(Term, Key) when is_map(Term) ->
    maps:is_key(Key, Term) orelse lists:any(fun({K, V}) ->
        contains_key(K, Key) orelse contains_key(V, Key)
    end, maps:to_list(Term));
contains_key(Term, Key) when is_tuple(Term) ->
    lists:any(fun(E) -> contains_key(E, Key) end, tuple_to_list(Term));
contains_key(Term, Key) when is_list(Term) ->
    lists:any(fun(E) -> contains_key(E, Key) end, Term);
contains_key(_, _) -> false.

contains_term(Term, Needle) when Term =:= Needle -> true;
contains_term(Term, Needle) when is_map(Term) ->
    lists:any(fun({K, V}) -> contains_term(K, Needle) orelse
        contains_term(V, Needle) end, maps:to_list(Term));
contains_term(Term, Needle) when is_tuple(Term) ->
    lists:any(fun(E) -> contains_term(E, Needle) end, tuple_to_list(Term));
contains_term(Term, Needle) when is_list(Term) ->
    lists:any(fun(E) -> contains_term(E, Needle) end, Term);
contains_term(_, _) -> false.

assert_received(Expected) ->
    receive Expected -> ok after 1000 -> ct:fail({missing_message, Expected}) end.

assert_not_received(Tag) ->
    receive
        Message when element(1, Message) =:= Tag ->
            ct:fail({unexpected_message, Message})
    after 0 -> ok
    end.

flush_mailbox() -> receive _ -> flush_mailbox() after 0 -> ok end.
