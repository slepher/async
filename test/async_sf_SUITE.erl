%%% Document-driven black-box acceptance for sf_do.
-module(async_sf_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {callbacks = #{}, events = []}).

suite() ->
    [{timetrap, {seconds, 45}}].

all() ->
    [{group, source_api_diagnostics},
     {group, same_monad_lowering},
     {group, source_semantics},
     {group, frame_completion},
     {group, lifecycle},
     {group, nested_composition},
     {group, capture_compatibility},
     {group, identity_manifest},
     {group, structural_failures},
     {group, additive_two_pass}].

groups() ->
    [{source_api_diagnostics, [], [source_api_diagnostics]},
     {same_monad_lowering, [], [same_monad_lowering]},
     {source_semantics, [], [source_semantics]},
     {frame_completion, [], [frame_completion]},
     {lifecycle, [], [lifecycle]},
     {nested_composition, [], [nested_composition]},
     {capture_compatibility, [], [capture_compatibility]},
     {identity_manifest, [], [identity_manifest]},
     {structural_failures, [], [structural_failures]},
     {additive_two_pass, [], [additive_two_pass]}].

init_per_suite(Config) ->
    async:start(),
    Config.

end_per_suite(_Config) ->
    code:purge(sf_do_version_fixture),
    code:delete(sf_do_version_fixture),
    ok.

init_per_testcase(_Case, Config) ->
    flush_mailbox(),
    Config.

end_per_testcase(_Case, _Config) ->
    flush_mailbox(),
    ok.

%% Objective 1: accepted source forms, names and stable diagnostics.
source_api_diagnostics(_Config) ->
    Manifest = fixture_manifest(),
    Blocks = maps:get(blocks, Manifest),
    ?assert(maps:is_key([named_flow], Blocks)),
    ?assert(maps:is_key(['-anonymous/1-fun-0-'], Blocks)),
    Named = block_entries(Manifest, [named_flow]),
    ?assertEqual([first, second],
        lists:sort([maps:get(step_name, E) || E <- Named])),
    ?assertEqual({named, value}, run_complete(sf_do_fixture:named(value), api_named)),
    ?assertEqual({anonymous, value},
        run_complete(sf_do_fixture:anonymous(value), api_anonymous)),

    assert_diagnostics("sf_do()",
        [{error, {5, 12}, {invalid_sf_do, {arity, 0}}}]),
    assert_diagnostics(
        "sf_do(block, [step], [async_m || async_m:pure_return(Name)])",
        [{error, {5, 12}, {invalid_sf_do, {arity, 3}}}]),
    assert_diagnostics(
        "sf_do(duplicate, [async_m || one, A <- async_m:pure_return(Name), "
        "one, B <- async_m:pure_return(A), async_m:pure_return(B)])",
        [{error, {5, 78}, {invalid_sf_do, {duplicate_step_name, one}}},
         {error, {5, 85}, generated_function_conflict}]),
    assert_diagnostics(
        "sf_do(consecutive, [async_m || one, two, A <- async_m:pure_return(Name), "
        "async_m:pure_return(A)])",
        [{error, {5, 48}, {invalid_sf_do, consecutive_step_marker}}]),
    assert_diagnostics(
        "sf_do(dangling, [async_m || A <- async_m:pure_return(Name), tail])",
        [{error, {5, 42}, {invalid_sf_do, invalid_last_qualifier}},
         {error, {5, 72}, {invalid_sf_do, dangling_step_marker}}]),
    assert_diagnostics(
        "sf_do(pure_let, [async_m || one, A = Name, async_m:pure_return(A)])",
        [{error, {5, 40}, {invalid_sf_do, step_marker_on_pure_let}}]),
    assert_diagnostics(
        "sf_do(final, [async_m || final_step, async_m:pure_return(Name)])",
        [{error, {5, 37}, {invalid_sf_do, step_marker_on_final_action}}]),

    Unnamed = "sf_do([async_m || A <- async_m:pure_return(Name), "
        "async_m:pure_return(A)])",
    {UnnamedModule, UnnamedBinary} = assert_warning_compile(Unnamed,
        [{warning, {5, 32}, unnamed_sf_action}]),
    load_binary(UnnamedModule, UnnamedBinary),
    ?assertEqual(value, run_complete(UnnamedModule:f(value), unnamed_compile)),
    [UnnamedEntry] = block_entries(UnnamedModule:'$async_sf_manifest'(),
        ['-f/1-fun-0-']),
    ?assertEqual(undefined, maps:get(step_name, UnnamedEntry)),

    Multi = "begin sf_do(first_bad, [async_m || one, two, "
        "A <- async_m:pure_return(Name), async_m:pure_return(A)]), "
        "sf_do(second_bad, [async_m || bad, B = Name, async_m:pure_return(B)]) end",
    MultiModule = fresh_module("sf_do_multi_"),
    {error, MultiErrors, []} = compile_expression(MultiModule, Multi),
    ?assertEqual(
        [{error, {5, 52}, {invalid_sf_do, consecutive_step_marker}},
         {error, {5, 145}, {invalid_sf_do, step_marker_on_pure_let}}],
        normalized_diagnostics(MultiErrors, [])),
    ?assertEqual(false, code:is_loaded(MultiModule)),
    ?assertEqual(non_existing, code:which(MultiModule)),
    ?assertEqual(false, filelib:is_file(filename:join(temp_dir(),
        atom_to_list(MultiModule) ++ ".beam"))).

%% Objective 2: bind/then lowering and final-action treatment.
same_monad_lowering(_Config) ->
    DictionaryProbe = async_m:promise(fun() ->
        {erlang:get({async_t, sf_continuation}),
            erlang:get({async_t, sf_runner})}
    end),
    ?assertEqual({undefined, undefined},
        run_complete(DictionaryProbe, explicit_sf_context)),
    ?assertEqual({ok, {4, 5}}, run_complete(sf_do_fixture:immediate(4), bind)),
    ?assertEqual(kept, run_complete(sf_do_fixture:then_action(kept), then)),
    ?assertEqual(final, run_complete(sf_do_fixture:final_action(final), final)),
    ?assertEqual([], block_entries(fixture_manifest(), [final_flow])),
    Bound = async_m:'>>='(sf_do_fixture:immediate(1),
        fun(Result) -> async_m:pure_return({ordinary_bind, Result}) end),
    ?assertEqual({ordinary_bind, {1, 2}}, run_complete(Bound, ordinary_bind)),
    ?assertEqual({ok, {outer, {ok, {inner, nested}}}},
        run_suspended(sf_do_fixture:nested(make_ref()), nested, nested)).

%% Objective 3: classic-do source semantics.
source_semantics(_Config) ->
    ?assertEqual({ok, {2, 3}}, run_complete(sf_do_fixture:immediate(2), local_return)),
    ?assertEqual({remote_return, remote},
        run_complete(sf_do_fixture:remote_call(remote), remote_call)),
    ?assertEqual({outside_return, outside}, sf_do_fixture:return(outside)),
    ?assertEqual({ok, {fresh, value}}, run_complete(
        sf_do_fixture:shadow(enclosing, async_m:pure_return({fresh, value})), shadow)),
    ?assertEqual({error, monad_badmatch}, run_complete(
        sf_do_fixture:pattern(async_m:pure_return({error, timeout})), pattern_fail)),
    ?assertException(error, {badmatch, wrong},
        run_complete(sf_do_fixture:pure_badmatch(async_m:pure_return(wrong)), pure_badmatch)),
    SfResult = run_complete(sf_do_fixture:ordered(a, b), sf_order),
    ClassicResult = async_m:wait(sf_do_fixture:classic_ordered(a, b)),
    ?assertEqual({ok, {a, b, [first, second]}}, SfResult),
    ?assertEqual(SfResult, ClassicResult).

%% Objective 4: one tagged encoding, external MFA/2 and completion once.
frame_completion(_Config) ->
    Ref = make_ref(),
    {ok, State} = start_suspended(sf_do_fixture:suspended(Ref), frame),
    Tagged = tagged_maps(State),
    ?assert(lists:any(fun(#{kind := sf_next}) -> true; (_) -> false end, Tagged)),
    ?assert(lists:any(fun(#{kind := sf_frame, protocol := 1,
                             step_name := wait,
                             mfa := {sf_do_fixture, Function, 2}}) ->
                              lists:prefix("$sf_", atom_to_list(Function));
                         (_) -> false
                      end, Tagged)),
    ?assert(lists:any(fun(#{kind := sf_cc_ref, protocol := 1,
                             mref := CcRef}) when is_reference(CcRef) -> true;
                         (_) -> false
                      end, Tagged)),
    ?assertEqual(false, contains_any_tag(State, [sf_program, await, done])),
    {{ok, {resumed, reply}}, FinalState} = async_m:run_info({Ref, reply}, 2, State),
    ?assertEqual(#{}, FinalState#state.callbacks),
    assert_completion(frame, {ok, {resumed, reply}}),
    assert_no_completion(frame),

    UnnamedRef = make_ref(),
    {ok, UnnamedState} = start_suspended(
        sf_do_fixture:unnamed_suspended(UnnamedRef), unnamed_runtime),
    UnnamedContinuation = first_continuation(UnnamedState),
    #{frame := UnnamedFrame} = UnnamedContinuation,
    ?assertEqual(undefined, maps:get(step_name, UnnamedFrame)),
    ?assertMatch({{ok, {unnamed, reply}}, _},
        run_continuation(UnnamedContinuation, reply, resume, UnnamedState)),
    assert_completion(unnamed_runtime, {ok, {unnamed, reply}}),
    SentinelFrame = UnnamedFrame#{mfa => {sf_do_fixture, migration_sentinel, 2}},
    SentinelContinuation = UnnamedContinuation#{frame => SentinelFrame},
    ?assertMatch({{error, {appup, incompatible_frame}}, _},
        run_continuation(SentinelContinuation, reply, appup, UnnamedState)),
    assert_completion(unnamed_runtime, {error, {appup, incompatible_frame}}),
    assert_sentinel_not_invoked(),
    ?assertMatch({{error, {appdown, incompatible_frame}}, _},
        run_continuation(SentinelContinuation, reply, appdown, UnnamedState)),
    assert_completion(unnamed_runtime, {error, {appdown, incompatible_frame}}),
    assert_sentinel_not_invoked().

%% Objective 5: reply/timeout/DOWN resume, message non-consumption and cleanup.
lifecycle(_Config) ->
    Ref = make_ref(),
    {ok, State0} = start_suspended(sf_do_fixture:suspended(Ref), life_reply),
    {{message, notice}, State1} = async_m:run_info({message, Ref, notice}, 2, State0),
    ?assertEqual(State0#state.callbacks, State1#state.callbacks),
    assert_no_completion(life_reply),
    {{ok, {resumed, ok}}, State2} = async_m:run_info({Ref, ok}, 2, State1),
    ?assertEqual(#{}, State2#state.callbacks),
    assert_completion(life_reply, {ok, {resumed, ok}}),

    TimeoutRef = make_ref(),
    {ok, TimeoutState} = start_suspended(sf_do_fixture:suspended(TimeoutRef), life_timeout),
    {{ok, {resumed, {error, timeout}}}, TimeoutDone} =
        async_m:run_info({TimeoutRef, {error, timeout}}, 2, TimeoutState),
    ?assertEqual(#{}, TimeoutDone#state.callbacks),
    assert_completion(life_timeout, {ok, {resumed, {error, timeout}}}),

    DownRef = make_ref(),
    {ok, DownState} = start_suspended(sf_do_fixture:suspended(DownRef), life_down),
    {{ok, {resumed, {error, {process_down, crashed}}}}, DownDone} =
        async_m:run_info({'DOWN', DownRef, process, self(), crashed}, 2, DownState),
    ?assertEqual(#{}, DownDone#state.callbacks),
    assert_completion(life_down, {ok, {resumed, {error, {process_down, crashed}}}}).

%% Objective 6: nested stable downstream and lexical Monad stack.
nested_composition(_Config) ->
    Ref = make_ref(),
    {ok, State} = start_suspended(sf_do_fixture:nested(Ref), nested_chain),
    [Continuation | _] = [M || M = #{kind := sf_next} <- tagged_maps(State)],
    ?assertEqual([sf_next, sf_next, sf_cc_ref], continuation_kinds(Continuation)),
    {{ok, {outer, {ok, {inner, reply}}}}, Done} = async_m:run_info({Ref, reply}, 2, State),
    ?assertEqual(#{}, Done#state.callbacks),
    assert_completion(nested_chain, {ok, {outer, {ok, {inner, reply}}}}),
    Outer = async_t:new(identity),
    Inner = async_t:new(identity),
    ?assertEqual({ok, {outer_value, {ok, inner_value}}}, run_complete(
        sf_do_fixture:nested_monads(Outer, Inner), nested_monads)).

%% Objective 7: minimal Env and directional capture compatibility.
capture_compatibility(_Config) ->
    [Entry] = block_entries(fixture_manifest(), [capture_flow]),
    ?assertEqual(['Used'], maps:get(required_captures, Entry)),
    Ref = make_ref(),
    {ok, State} = start_suspended(sf_do_fixture:capture(used, unused, Ref), capture),
    Continuation = first_continuation(State),
    #{frame := Frame} = Continuation,
    ?assertEqual(#{'Used' => used}, maps:get(env, Frame)),
    ExtraFrame = Frame#{env => #{'Used' => used, extra => allowed}},
    Extra = Continuation#{frame => ExtraFrame},
    ?assertMatch({{ok, _}, _}, run_continuation(Extra, reply, resume, State)),
    assert_completion(capture, {ok, {used, reply}}),

    MissingFrame = Frame#{env => #{}},
    Missing = Continuation#{frame => MissingFrame},
    ?assertMatch({{error, {appup, incompatible_frame}}, _},
        run_continuation(Missing, reply, appup, State)),
    assert_completion(capture, {error, {appup, incompatible_frame}}),
    ?assertMatch({{error, {appdown, incompatible_frame}}, _},
        run_continuation(Missing, reply, appdown, State)),
    assert_completion(capture, {error, {appdown, incompatible_frame}}),

    assert_required_captures([case_capture], ['Expected']),
    assert_required_captures([receive_capture], ['Expected']),
    assert_required_captures([try_capture], ['Expected']),
    assert_required_captures([fun_shadow], ['Value']),
    assert_required_captures([named_fun_shadow], ['Value']),
    assert_required_captures([map_key_capture], ['Key']),
    assert_required_captures([map_key_shadow_capture], ['Key']),
    assert_required_captures([binary_size_capture], ['Size']),
    ?assertEqual({ok, {case_equal, expected}}, run_complete(
        sf_do_fixture:case_equality(expected, expected), case_equality)),
    ?assertEqual({ok, {receive_equal, expected}}, run_complete(
        sf_do_fixture:receive_equality(expected, expected), receive_equality)),
    ?assertEqual({ok, {try_equal, expected}}, run_complete(
        sf_do_fixture:try_equality(expected, expected), try_equality)),
    ?assertEqual({ok, {fun_shadow, fresh}}, run_complete(
        sf_do_fixture:fun_shadow(outer, fresh), fun_shadow)),
    ?assertEqual({ok, {named_fun_shadow, fresh}}, run_complete(
        sf_do_fixture:named_fun_shadow(outer, fresh), named_fun_shadow)),
    ?assertEqual({ok, value}, run_complete(
        sf_do_fixture:map_key_capture(
            wanted, async_m:pure_return(#{wanted => value})
        ), map_key_capture)),
    ?assertEqual({ok, shadowed}, run_complete(
        sf_do_fixture:map_key_shadow_capture(
            wanted, async_m:pure_return(#{wanted => shadowed})
        ), map_key_shadow_capture)),
    ?assertEqual({ok, 5}, run_complete(
        sf_do_fixture:binary_size_capture(
            8, async_m:pure_return(<<5:8>>)
        ), binary_size_capture)).

%% Objective 8: identity invariants and manifest schema.
identity_manifest(_Config) ->
    A = load_version("identity_a.src"),
    B = load_version("identity_b.src"),
    Fewer = load_version("fewer_capture.src"),
    More = load_version("more_capture.src"),
    Changed = load_version("action_changed.src"),
    Moved = load_version("action_moved.src"),
    Earlier = load_version("earlier_insert.src"),
    Pattern = load_version("pattern_changed.src"),
    Remainder = load_version("remainder_changed.src"),
    BlockRenamed = load_version("block_renamed.src"),
    StepRenamed = load_version("step_renamed.src"),
    ?assertEqual(entry_id(A), entry_id(B)),
    ?assertEqual(entry_id(A), entry_id(Fewer)),
    ?assertEqual(entry_id(A), entry_id(More)),
    ?assertEqual(entry_id(A), entry_id(Changed)),
    ?assertEqual(entry_id(A), entry_id(Moved)),
    ?assertEqual(entry_id(A), entry_id_for_step(Earlier, [versioned], stable_step)),
    ?assertEqual(entry_id(A), entry_id(Pattern)),
    ?assertEqual(entry_id(A), entry_id(Remainder)),
    ?assertNotEqual(entry_id(A), entry_id(BlockRenamed, [renamed_versioned])),
    ?assertNotEqual(entry_id(A), entry_id(StepRenamed)),
    ?assertEqual(['PrefixValue'], maps:get(required_captures, only_entry(A))),
    ?assertEqual([], maps:get(required_captures, only_entry(Fewer))),
    ?assertEqual(['Extra', 'PrefixValue'], maps:get(required_captures, only_entry(More))),
    {_, Function, 2} = maps:get(mfa, only_entry(A)),
    Name = atom_to_list(Function),
    ?assertEqual(68, length(Name)),
    ?assert(lists:prefix("$sf_", Name)),
    ?assert(lists:all(fun is_lower_hex/1, lists:nthtail(4, Name))),
    lists:foreach(fun(Entry) ->
        ?assertEqual([mfa, required_captures, step_name],
            lists:sort(maps:keys(Entry)))
    end, all_entries(A) ++ all_entries(Earlier) ++ all_entries(fixture_manifest())),
    assert_manifest_public(A),
    assert_manifest_public(fixture_manifest()).

%% Objective 9: structural failures are terminal sf_runtime outcomes.
structural_failures(_Config) ->
    {Cc, StructuralState} = registered_sf_cc(structural),
    BadProtocol = #{kind => sf_next, protocol => 2,
        downstream => Cc, frame => #{protocol => 2}},
    ?assertMatch({{error, {sf_runtime, {unsupported_protocol, 2}}}, _},
        run_continuation(BadProtocol, input, resume, StructuralState)),
    assert_completion(structural, {error, {sf_runtime, {unsupported_protocol, 2}}}),

    BadShape = #{kind => sf_next, frame => broken, downstream => Cc},
    ?assertMatch({{error, {sf_runtime, {invalid_frame, broken}}}, _},
        run_continuation(BadShape, input, resume, StructuralState)),
    assert_completion(structural, {error, {sf_runtime, {invalid_frame, broken}}}),

    MissingModule = fresh_module("sf_do_missing_owner_"),
    MissingMfa = {MissingModule, '$sf_missing', 2},
    Frame = #{kind => sf_frame, protocol => 1, block_path => [suspended_flow],
        step_name => wait,
        async_id => entry_id(fixture_manifest(), [suspended_flow]),
        module => MissingModule, vsn => undefined, mfa => MissingMfa, env => #{}},
    Missing = #{kind => sf_next, frame => Frame, downstream => Cc},
    {{error, {sf_runtime, Reason}}, _} =
        run_continuation(Missing, input, resume, StructuralState),
    ?assert(lists:member(element(1, Reason),
        [missing_generated_mfa, missing_manifest_or_mfa, missing_module])),
    assert_completion(structural, {error, {sf_runtime, Reason}}),
    assert_no_completion(structural),

    ManifestModule = fresh_module("sf_do_crashing_manifest_"),
    ManifestSource = io_lib:format(
        "-module(~p).~n-export(['$async_sf_manifest'/0]).~n"
        "'$async_sf_manifest'() -> erlang:error(manifest_crash).~n",
        [ManifestModule]),
    {ok, ManifestModule, ManifestBinary, []} =
        compile_text(ManifestModule, ManifestSource),
    load_binary(ManifestModule, ManifestBinary),
    try
        {CrashCc, CrashState} = registered_sf_cc(manifest_crash),
        CrashMfa = {ManifestModule, '$sf_missing', 2},
        CrashFrame = Frame#{module := ManifestModule, mfa := CrashMfa},
        Crash = #{kind => sf_next, frame => CrashFrame, downstream => CrashCc},
        ?assertError(manifest_crash,
            run_continuation(Crash, input, resume, CrashState)),
        assert_no_completion(manifest_crash)
    after
        code:purge(ManifestModule),
        code:delete(ManifestModule)
    end.

%% Objective 10: no-sf source remains additive; nested two-pass is stable.
additive_two_pass(_Config) ->
    ?assertEqual(legacy, async_m:wait(async_m:pure_return(legacy))),
    ?assertEqual({outside_return, untouched}, sf_do_fixture:return(untouched)),
    Manifest = fixture_manifest(),
    ?assert(maps:is_key([outer], maps:get(blocks, Manifest))),
    ?assert(maps:is_key([outer, inner], maps:get(blocks, Manifest))),
    Entries = lists:append([maps:values(maps:get(asyncs, Block))
        || Block <- maps:values(maps:get(blocks, Manifest))]),
    ?assertEqual(length(Entries), length(lists:usort(
        [maps:get(mfa, Entry) || Entry <- Entries]))),
    ?assertEqual(false, contains_any_tag(Manifest,
        [sf_program, await, done, sf_cc, migration, history])).

%% Completion and lifecycle helpers.
sf_cc(Tag) ->
    #{kind => sf_cc, protocol => 1,
      mfa => {sf_do_fixture, complete, 2}, context => {self(), Tag}}.

run_complete(Action, Tag) ->
    {Outcome, #state{callbacks = #{}}} =
        async_m:run_sf_frame(Action, sf_cc(Tag), #state.callbacks, #state{}),
    assert_completion(Tag, Outcome),
    assert_no_completion(Tag),
    Outcome.

start_suspended(Action, Tag) ->
    async_m:run_sf_frame(Action, sf_cc(Tag), #state.callbacks, #state{}).

run_suspended(Action, Reply, Tag) ->
    {ok, State} = start_suspended(Action, Tag),
    Ref = only_callback_ref(State),
    {Outcome, _State} = async_m:run_info({Ref, Reply}, #state.callbacks, State),
    assert_completion(Tag, Outcome),
    Outcome.

%% Execute an already-materialized stable continuation with its registered final CC.
run_continuation(Continuation, Input, Direction, State) ->
    MR = async_r_t:new(identity),
    Action0 = async_sf_runtime:continue(
        Continuation, Input, async_t:new(identity), Direction),
    Action = async_r_t:local_sf_cc_ref(terminal_cc_ref(Continuation), Action0, MR),
    identity:run(async_r_t:run(Action, async_t:state_callbacks_gs(#state.callbacks),
        make_ref(), State)).

registered_sf_cc(Tag) ->
    CcRef = make_ref(),
    SfCc = async_sf_cc:new(direct_state, fun sf_do_fixture:complete/2,
        {self(), Tag}),
    Terminal = async_sf_cc:terminal(CcRef),
    Entry = async_sf_cc:new_ref(CcRef, SfCc),
    {Terminal, #state{callbacks = #{CcRef => Entry}}}.

terminal_cc_ref(#{kind := sf_next, downstream := Downstream}) ->
    terminal_cc_ref(Downstream);
terminal_cc_ref(#{kind := sf_cc_ref, mref := CcRef}) ->
    CcRef.

assert_completion(Tag, Outcome) ->
    receive
        {Tag, Outcome} -> ok
    after 1000 ->
        ct:fail({missing_completion, Tag, Outcome})
    end.

assert_no_completion(Tag) ->
    receive
        {Tag, Unexpected} -> ct:fail({duplicate_completion, Tag, Unexpected})
    after 0 ->
        ok
    end.

flush_mailbox() ->
    receive _ -> flush_mailbox() after 0 -> ok end.

only_callback_ref(#state{callbacks = Callbacks}) ->
    [Ref] = [Key || {Key, Value} <- maps:to_list(Callbacks),
        async_util:callback_value_kind(Value) =:= action_callback],
    Ref.

first_continuation(Term) ->
    [Continuation | _] = [M || M = #{kind := sf_next} <- tagged_maps(Term)],
    Continuation.

continuation_kinds(#{kind := sf_next, downstream := Downstream}) ->
    [sf_next | continuation_kinds(Downstream)];
continuation_kinds(#{kind := sf_cc_ref}) ->
    [sf_cc_ref].

tagged_maps(Term) when is_map(Term) ->
    This = case maps:is_key(kind, Term) of true -> [Term]; false -> [] end,
    This ++ lists:append([tagged_maps(K) ++ tagged_maps(V) || {K, V} <- maps:to_list(Term)]);
tagged_maps(Term) when is_tuple(Term) ->
    lists:append([tagged_maps(E) || E <- tuple_to_list(Term)]);
tagged_maps(Term) when is_list(Term) ->
    lists:append([tagged_maps(E) || E <- Term]);
tagged_maps(_) ->
    [].

%% Manifest helpers.
fixture_manifest() ->
    sf_do_fixture:'$async_sf_manifest'().

block_entries(Manifest, Path) ->
    Block = maps:get(Path, maps:get(blocks, Manifest)),
    maps:values(maps:get(asyncs, Block)).

only_entry(Manifest) ->
    [Entry] = block_entries(Manifest, [versioned]),
    Entry.

entry_id_for_step(Manifest, Path, StepName) ->
    Block = maps:get(Path, maps:get(blocks, Manifest)),
    [Id] = [CandidateId || {CandidateId, Entry} <- maps:to_list(maps:get(asyncs, Block)),
        maps:get(step_name, Entry) =:= StepName],
    Id.

all_entries(Manifest) ->
    lists:append([maps:values(maps:get(asyncs, Block))
        || Block <- maps:values(maps:get(blocks, Manifest))]).

assert_required_captures(Path, Expected) ->
    [Entry] = block_entries(fixture_manifest(), Path),
    ?assertEqual(Expected, maps:get(required_captures, Entry)).

entry_id(Manifest) ->
    entry_id(Manifest, [versioned]).

entry_id(Manifest, Path) ->
    Block = maps:get(Path, maps:get(blocks, Manifest)),
    [Id] = maps:keys(maps:get(asyncs, Block)),
    Id.

assert_manifest_public(Manifest) ->
    ?assertEqual(1, maps:get(format, Manifest)),
    ?assertEqual(1, maps:get(protocol, Manifest)),
    ?assertEqual(false, contains_any_tag(Manifest,
        [identity, index, digest, async_form_digest, migration_hash,
         env, context, sf_cc, migration, history, ast, source_ast])),
    ok.

is_lower_hex(C) ->
    (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f).

contains_any_tag(Term, Tags) when is_map(Term) ->
    lists:any(fun(Tag) -> maps:is_key(Tag, Term) end, Tags) orelse
        lists:any(fun({K, V}) -> contains_any_tag(K, Tags) orelse contains_any_tag(V, Tags) end,
            maps:to_list(Term));
contains_any_tag(Term, Tags) when is_tuple(Term) ->
    lists:any(fun(E) -> contains_any_tag(E, Tags) end, tuple_to_list(Term));
contains_any_tag(Term, Tags) when is_list(Term) ->
    lists:any(fun(E) -> contains_any_tag(E, Tags) end, Term);
contains_any_tag(Term, Tags) ->
    lists:member(Term, Tags).

%% Runtime compilation helpers for diagnostics and version compatibility.
assert_diagnostics(Expression, Expected) ->
    Module = fresh_module("sf_do_bad_"),
    {error, Errors, Warnings} = compile_expression(Module, Expression),
    ?assertEqual(Expected, normalized_diagnostics(Errors, Warnings)).

assert_warning_compile(Expression, Expected) ->
    Module = fresh_module("sf_do_warning_"),
    {ok, Module, Binary, Warnings} = compile_expression(Module, Expression),
    ?assertEqual(Expected, normalized_diagnostics([], Warnings)),
    {Module, Binary}.

compile_expression(Module, Expression) ->
    Source = io_lib:format(
        "-module(~p).~n-define(ASYNC_DO,true).~n"
        "-include_lib(\"async/include/async.hrl\").~n"
        "-export([f/1]).~nf(Name) -> ~s.~n", [Module, Expression]),
    compile_text(Module, Source).

fresh_module(Prefix) ->
    list_to_atom(Prefix ++ integer_to_list(erlang:unique_integer([positive]))).

normalized_diagnostics(Errors, Warnings) ->
    normalize_diagnostic_files(error, Errors) ++
        normalize_diagnostic_files(warning, Warnings).

normalize_diagnostic_files(Severity, Files) ->
    [{Severity, Position, normalize_diagnostic(Module, Reason)}
        || {_File, Diagnostics} <- Files,
           {Position, Module, Reason} <- Diagnostics].

normalize_diagnostic(astranaut_lib, {async_sf_transform, Reason}) ->
    normalize_reason(Reason);
normalize_diagnostic(Module, Reason) ->
    {Module, Reason}.

normalize_reason({generated_function_conflict, _Function}) ->
    generated_function_conflict;
normalize_reason(Reason) ->
    Reason.

compile_text(Module, Source) ->
    Dir = temp_dir(),
    File = filename:join(Dir, atom_to_list(Module) ++ ".erl"),
    ok = file:write_file(File, iolist_to_binary(Source)),
    compile:file(File, [binary, return_errors, return_warnings, debug_info]).

load_binary(Module, Binary) ->
    code:purge(Module),
    code:delete(Module),
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary),
    ok.

load_version(Name) ->
    Path = filename:join([filename:dirname(?FILE), "sf_do_fixtures", Name]),
    {ok, Source} = file:read_file(Path),
    {ok, sf_do_version_fixture, Binary, _Warnings} =
        compile_text(sf_do_version_fixture, Source),
    code:purge(sf_do_version_fixture),
    code:delete(sf_do_version_fixture),
    {module, sf_do_version_fixture} = code:load_binary(sf_do_version_fixture, Path, Binary),
    sf_do_version_fixture:'$async_sf_manifest'().

temp_dir() ->
    Base = case os:getenv("TMPDIR") of false -> "/tmp"; Value -> Value end,
    Dir = filename:join(Base, "async_sf_suite"),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    Dir.

assert_sentinel_not_invoked() ->
    receive
        {migration_sentinel_invoked, Input, Env} ->
            ct:fail({migration_invoked_before_rejection, Input, Env})
    after 0 ->
        ok
    end.
