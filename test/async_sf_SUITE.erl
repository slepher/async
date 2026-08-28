%%% Public source/result regression for sf_do. Linked-only gaps live in
%%% async_sf_do_linked_SUITE; this suite retains the E-15 ordinary baseline.
-module(async_sf_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 45}}].
all() -> [source_semantics, lowering_and_lifecycle, capture_manifest,
    identity_stability, compile_diagnostics].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_Config) ->
    code:purge(sf_do_version_fixture), code:delete(sf_do_version_fixture), ok.

%% Existing E-15 source semantics retained unchanged in observable intent.
source_semantics(_Config) ->
    ?assertEqual({ok, {2, 3}}, run_complete(sf_do_fixture:immediate(2), local_return)),
    ?assertEqual({remote_return, remote},
        run_complete(sf_do_fixture:remote_call(remote), remote_call)),
    ?assertEqual({outside_return, outside}, sf_do_fixture:return(outside)),
    ?assertEqual({ok, {fresh, value}}, run_complete(
        sf_do_fixture:shadow(enclosing, async_m:pure_return({fresh, value})), shadow)),
    ?assertEqual({error, monad_badmatch}, run_complete(
        sf_do_fixture:pattern(async_m:pure_return({error, timeout})), pattern_fail)),
    ?assertException(error, {badmatch, wrong}, run_complete(
        sf_do_fixture:pure_badmatch(async_m:pure_return(wrong)), pure_badmatch)),
    Sf = run_complete(sf_do_fixture:ordered(a, b), sf_order),
    ?assertEqual({ok, {a, b, [first, second]}}, Sf),
    ?assertEqual(Sf, async_m:wait(sf_do_fixture:classic_ordered(a, b))).

%% Bind/then/final and reply/message/timeout/DOWN ordinary regression.
lowering_and_lifecycle(_Config) ->
    ?assertEqual({ok, {4, 5}}, run_complete(sf_do_fixture:immediate(4), bind)),
    ?assertEqual(kept, run_complete(sf_do_fixture:then_action(kept), then)),
    ?assertEqual(final, run_complete(sf_do_fixture:final_action(final), final)),
    ?assertEqual([], entries_for_block(manifest(), final_flow)),
    Bound = async_m:'>>='(sf_do_fixture:immediate(1),
        fun(Result) -> async_m:pure_return({ordinary_bind, Result}) end),
    ?assertEqual({ordinary_bind, {1, 2}}, run_complete(Bound, ordinary_bind)),

    Ref = make_ref(), {ok, State0} = start(sf_do_fixture:suspended(Ref), reply),
    {{message, notice}, State1} = info({message, Ref, notice}, State0),
    ?assertEqual(1, length(summaries(State1))),
    assert_no_completion(reply),
    {{ok, {resumed, value}}, State2} = info({Ref, value}, State1),
    ?assertEqual([], summaries(State2)),
    assert_completion(reply, {ok, {resumed, value}}),

    Timeout = make_ref(), {ok, T0} = start(sf_do_fixture:suspended(Timeout), timeout),
    {{ok, {resumed, {error, timeout}}}, T1} = info(
        {Timeout, {error, timeout}}, T0),
    ?assertEqual([], summaries(T1)),
    assert_completion(timeout, {ok, {resumed, {error, timeout}}}),

    Down = make_ref(), {ok, D0} = start(sf_do_fixture:suspended(Down), down),
    {{ok, {resumed, {error, {process_down, gone}}}}, D1} = info(
        {'DOWN', Down, process, self(), gone}, D0),
    ?assertEqual([], summaries(D1)),
    assert_completion(down, {ok, {resumed, {error, {process_down, gone}}}}).

%% Public manifest and capture ABI; no runtime Env or old identity fields.
capture_manifest(_Config) ->
    Manifest = manifest(),
    ?assertEqual([format, frames, owner, protocol, vsn],
        lists:sort(maps:keys(Manifest))),
    ?assertEqual(sf_do_fixture, maps:get(owner, Manifest)),
    ?assertEqual(undefined, maps:get(vsn, Manifest)),
    lists:foreach(fun(Entry) ->
        ?assertEqual([identity, mfa, required_captures],
            lists:sort(maps:keys(Entry)))
    end, maps:get(frames, Manifest)),
    ?assertEqual(['Used'], captures(capture_flow, wait)),
    ?assertEqual(['Expected'], captures(case_capture, compare)),
    ?assertEqual(['Expected'], captures(receive_capture, receive_value)),
    ?assertEqual(['Expected'], captures(try_capture, compare)),
    ?assertEqual(['Value'], captures(fun_shadow, invoke)),
    ?assertEqual(['Value'], captures(named_fun_shadow, invoke)),
    ?assertEqual(['Key'], captures(map_key_capture, match_map)),
    ?assertEqual(['Key'], captures(map_key_shadow_capture, match_map)),
    ?assertEqual(['Size'], captures(binary_size_capture, match_binary)),
    ?assertEqual(false, contains_any(Manifest,
        [async_id, hash, digest, ast, env, context, index, parent, block_path])),
    ?assertEqual({ok, {case_equal, expected}}, run_complete(
        sf_do_fixture:case_equality(expected, expected), case_equal)),
    ?assertEqual({ok, {receive_equal, expected}}, run_complete(
        sf_do_fixture:receive_equality(expected, expected), receive_equal)),
    ?assertEqual({ok, {try_equal, expected}}, run_complete(
        sf_do_fixture:try_equality(expected, expected), try_equal)),
    Ref = make_ref(), {ok, State} = start(sf_do_fixture:capture(used, unused, Ref), capture),
    [#{frame := #{block := capture_flow, step_name := wait}}] = summaries(State),
    {{ok, {used, reply}}, Done} = info({Ref, reply}, State),
    ?assertEqual([], summaries(Done)),
    assert_completion(capture, {ok, {used, reply}}).

%% Named identity is direct, stable under move/content changes, and changes on rename.
identity_stability(_Config) ->
    A = load_version("identity_a.src"),
    B = load_version("identity_b.src"),
    Changed = load_version("action_changed.src"),
    Moved = load_version("action_moved.src"),
    Earlier = load_version("earlier_insert.src"),
    Remainder = load_version("remainder_changed.src"),
    BlockRenamed = load_version("block_renamed.src"),
    StepRenamed = load_version("step_renamed.src"),
    Identity = {1, versioned, stable_step},
    lists:foreach(fun(M) -> ?assertMatch(#{identity := Identity},
        entry(M, versioned, stable_step)) end,
        [A, B, Changed, Moved, Earlier, Remainder]),
    ?assertEqual({sf_do_version_fixture, '$sf_versioned__stable_step', 2},
        maps:get(mfa, entry(A, versioned, stable_step))),
    ?assertEqual({1, renamed_versioned, stable_step},
        maps:get(identity, only_named(BlockRenamed))),
    ?assertEqual({1, versioned, renamed_step},
        maps:get(identity, only_named(StepRenamed))),
    ?assertNotEqual(Identity, maps:get(identity, only_named(BlockRenamed))),
    ?assertNotEqual(Identity, maps:get(identity, only_named(StepRenamed))).

%% Public compiler boundary: Pass 1 aggregates errors and produces no binary;
%% warning-only continues, and generated-name collision is owned by the transform.
compile_diagnostics(_Config) ->
    Multi = "begin sf_do(first_bad, [async_m || one, two, "
        "A <- async_m:pure_return(Name), async_m:pure_return(A)]), "
        "sf_do(second_bad, [async_m || bad, B = Name, async_m:pure_return(B)]) end",
    Module = fresh_module("sf_multi_"),
    {error, Errors, []} = compile_expression(Module, Multi),
    ?assertEqual(2, length(flat_diagnostics(Errors))),
    ?assertEqual(false, code:is_loaded(Module)),
    ?assertEqual(non_existing, code:which(Module)),
    Unnamed = "sf_do([async_m || A <- async_m:pure_return(Name), "
        "async_m:pure_return(A)])",
    {ok, WarningModule, Binary, Warnings} = compile_expression(
        fresh_module("sf_warning_"), Unnamed),
    ?assert(is_binary(Binary)),
    ?assertEqual(1, length(flat_diagnostics(Warnings))),
    ?assertEqual(false, code:is_loaded(WarningModule)),
    CollisionModule = fresh_module("sf_collision_"),
    Source = io_lib:format(
        "-module(~p).~n-define(ASYNC_DO,true).~n"
        "-include_lib(\"async/include/async.hrl\").~n-export([f/1]).~n"
        "f(X) -> sf_do(flow, [async_m || step, A <- async_m:pure_return(X), "
        "async_m:pure_return(A)]).~n"
        "'$sf_flow__step'(A,B) -> {A,B}.~n", [CollisionModule]),
    {error, CollisionErrors, _} = compile_text(CollisionModule, Source),
    ?assert(lists:any(fun({_Pos, astranaut_lib,
        {async_sf_transform, {generated_function_conflict, '$sf_flow__step'}}}) -> true;
        (_) -> false end, flat_diagnostics(CollisionErrors))).

sf_cc(Tag) -> #{kind => sf_cc, protocol => 1,
    mfa => {sf_do_fixture, complete, 2}, context => {self(), Tag}}.
run_complete(Action, Tag) ->
    {Outcome, #state{callbacks = #{}}} = async_m:run_sf_frame(
        Action, sf_cc(Tag), #state.callbacks, #state{}),
    assert_completion(Tag, Outcome), Outcome.
start(Action, Tag) -> async_m:run_sf_frame(
    Action, sf_cc(Tag), #state.callbacks, #state{}).
info(Message, State) -> async_m:run_info(Message, #state.callbacks, State).
summaries(State) ->
    {ok, Result} = async_m:inspect_sf_callbacks(fun(_) -> true end,
        #state.callbacks, State), Result.
assert_completion(Tag, Outcome) ->
    receive {Tag, Outcome} -> ok
    after 1000 -> ct:fail({missing_completion, Tag, Outcome}) end.
assert_no_completion(Tag) ->
    receive {Tag, Outcome} -> ct:fail({unexpected_completion, Tag, Outcome})
    after 0 -> ok end.

manifest() -> sf_do_fixture:'$async_sf_manifest'().
entry(Manifest, Block, Step) ->
    [Entry] = [E || E = #{identity := {1, B, S}} <- maps:get(frames, Manifest),
        B =:= Block, S =:= Step], Entry.
entries_for_block(Manifest, Block) ->
    [E || E = #{identity := {1, B, _}} <- maps:get(frames, Manifest), B =:= Block].
only_named(Manifest) ->
    [Entry] = [E || E = #{identity := Identity} <- maps:get(frames, Manifest),
        Identity =/= undefined], Entry.
captures(Block, Step) -> maps:get(required_captures, entry(manifest(), Block, Step)).

contains_any(Term, Tags) when is_map(Term) ->
    lists:any(fun(Tag) -> maps:is_key(Tag, Term) end, Tags) orelse
        lists:any(fun({K,V}) -> contains_any(K, Tags) orelse contains_any(V, Tags) end,
            maps:to_list(Term));
contains_any(Term, Tags) when is_tuple(Term) -> contains_any(tuple_to_list(Term), Tags);
contains_any(Term, Tags) when is_list(Term) ->
    lists:any(fun(E) -> contains_any(E, Tags) end, Term);
contains_any(Term, Tags) -> lists:member(Term, Tags).

compile_expression(Module, Expression) ->
    Source = io_lib:format("-module(~p).~n-define(ASYNC_DO,true).~n"
        "-include_lib(\"async/include/async.hrl\").~n-export([f/1]).~n"
        "f(Name) -> ~s.~n", [Module, Expression]),
    compile_text(Module, Source).
compile_text(Module, Source) ->
    Dir = filename:join(["_build", "test", "sf_public_fixtures"]),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    File = filename:join(Dir, atom_to_list(Module) ++ ".erl"),
    ok = file:write_file(File, iolist_to_binary(Source)),
    compile:file(File, [binary, return_errors, return_warnings, debug_info]).
fresh_module(Prefix) -> list_to_atom(Prefix ++
    integer_to_list(erlang:unique_integer([positive]))).
flat_diagnostics(Files) -> lists:append([Ds || {_File, Ds} <- Files]).
load_version(Name) ->
    Path = filename:join([filename:dirname(?FILE), "sf_do_fixtures", Name]),
    {ok, Source} = file:read_file(Path),
    {ok, sf_do_version_fixture, Binary, _} = compile_text(sf_do_version_fixture, Source),
    code:purge(sf_do_version_fixture), code:delete(sf_do_version_fixture),
    {module, sf_do_version_fixture} = code:load_binary(sf_do_version_fixture, Path, Binary),
    sf_do_version_fixture:'$async_sf_manifest'().
