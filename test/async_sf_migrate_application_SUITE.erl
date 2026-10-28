%%% Public application, node, controller, and release evidence for test.md.
-module(async_sf_migrate_application_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() -> [{timetrap, {minutes, 10}}].
all() -> [static_fixture_contract, independent_nodes, controller_contract,
    application_scenario_matrix, nested_scenario_matrix, release_artifacts].

init_per_suite(Config) ->
    DistStarted = ensure_distribution(),
    ct:pal("runtime otp=~p erts=~p root=~p rebar3=~s", [
        erlang:system_info(otp_release), erlang:system_info(version),
        code:root_dir(), os:cmd("rebar3 --version")]),
    Priv = ?config(priv_dir, Config),
    ProjectRoot = lists:foldl(fun(_, D) -> filename:dirname(D) end,
        code:lib_dir(async), lists:seq(1, 4)),
    FixtureRoot = filename:join([ProjectRoot, "test", "app_fixtures"]),
    Versions = [{a, "1.0.0", {1,0,0}}, {b, "2.0.0", {2,0,0}},
        {c, "3.0.0", {3,0,0}}],
    Ebins = maps:from_list([{V, compile_fixture(V, FixtureRoot, Priv)} ||
        {V, _, _} <- Versions]),
    BaseConfig = [{distribution_started, DistStarted}, {fixture_root, FixtureRoot},
        {fixture_versions, Versions}, {fixture_ebins, Ebins} | Config],
    [{releases, build_releases(BaseConfig)} | BaseConfig].

end_per_suite(Config) ->
    flush(),
    case ?config(distribution_started, Config) of true -> net_kernel:stop(); false -> ok end,
    ok.

static_fixture_contract(Config) ->
    Root = ?config(fixture_root, Config),
    ExpectedModules = [sf_fixture_app, sf_fixture_sup, order_flow, payment_flow],
    lists:foreach(fun({V, Vsn, Marker}) ->
        Dir = filename:join(Root, atom_to_list(V)),
        {ok, [{application, sf_fixture, App}]} = file:consult(
            filename:join(Dir, "sf_fixture.app")),
        ?assertEqual(Vsn, proplists:get_value(vsn, App)),
        ?assertEqual(ExpectedModules, proplists:get_value(modules, App)),
        ?assertEqual([{marker, V}], proplists:get_value(env, App)),
        ?assertEqual({V, Marker}, {V, version_tuple(Vsn)}),
        {ok, [_]} = file:consult(filename:join(Dir, "sf_fixture.appup"))
    end, ?config(fixture_versions, Config)),
    ?assertEqual(["sf_fixture", "sf_fixture", "sf_fixture"],
        [app_name(V, Root) || V <- [a,b,c]]).

independent_nodes(Config) ->
    Nodes = [start_fixture_node(V, Config) || V <- [a,b,c]],
    try
        Snapshots = [node_snapshot(N) || N <- Nodes],
        ?assertEqual([a,b,c], [maps:get(marker, S) || S <- Snapshots]),
        ?assertEqual(3, length(lists:usort([maps:get(controller, S) ||
            S <- Snapshots]))),
        ?assertEqual(3, length(lists:usort([maps:get(application, S) ||
            S <- Snapshots]))),
        lists:foreach(fun assert_tree/1, Snapshots),
        lists:foreach(fun(S) -> ?assertEqual(#{pending => 0, replied => 0},
            maps:get(controller_counts, S)) end, Snapshots),
        lists:foreach(fun(N) -> baseline_call(N, independent) end, Nodes)
    after
        lists:foreach(fun stop_fixture_node/1, Nodes)
    end.

controller_contract(_Config) ->
    {ok, Controller} = test_async_controller:start_link(self()),
    try
        Unknown = make_ref(),
        ?assertEqual({error, unknown_mref},
            test_async_controller:make_reply(Controller, Unknown, ignored)),
        Parent = self(),
        Caller = spawn(fun() ->
            R = async_gen_server:call(Controller, controller_probe),
            Parent ! {probe_mref, R},
            Parent ! {probe_result, async:promise_mref(R)}
        end),
        {MRef, controller_probe} = held(controller_probe),
        ?assertMatch({pending, {Caller, MRef}},
            test_async_controller:identity(Controller, MRef)),
        ?assertEqual(#{pending => 1, replied => 0},
            test_async_controller:counts(Controller)),
        ?assertEqual(ok, test_async_controller:make_reply(Controller, MRef, ok)),
        ?assertEqual({error, already_replied},
            test_async_controller:make_reply(Controller, MRef, duplicate)),
        ?assertMatch({replied, {Caller, MRef}},
            test_async_controller:identity(Controller, MRef)),
        receive {probe_mref, MRef} -> ok after 5000 -> ct:fail(probe_mref) end,
        receive {probe_result, _Promise} -> ok after 5000 -> ct:fail(probe_result) end,
        ?assertEqual(#{pending => 0, replied => 1},
            test_async_controller:counts(Controller))
    after test_async_controller:stop(Controller) end.

application_scenario_matrix(Config) ->
    Matrix = [{t01, baseline_a}, {t02, baseline_b}, {t03, baseline_c},
        {t04, a_to_b}, {t05, b_to_c}, {t06, a_to_b_to_c_direct},
        {t07, a_to_c_direct}, {t08, reply_before_upgrade},
        {t09, reply_between_upgrades}, {t10, reply_after_upgrade},
        {t11, mixed_a_b}, {t12, mixed_a_b_c}, {t13, same_source_two},
        {t14, duplicate_reply}, {t15, unknown_reply}, {t16, continuity},
        {t17, old_and_new}, {t18, three_owner_versions}],
    ct:pal("T01-T18 matrix: ~p", [Matrix]),
    lists:foreach(fun({Id, Scenario}) -> run_application_scenario(Id, Scenario, Config) end,
        Matrix).

nested_scenario_matrix(Config) ->
    Matrix = [{n01, same_a}, {n02, order_ab_payment_a},
        {n03, order_a_payment_ac}, {n04, order_c_payment_b},
        {n05, both_ac}, {n06, parent_converter_child_raw},
        {n07, parent_raw_child_converter}, {n08, mixed_pending},
        {n09, bounded_child_failure}, {n10, child_late_duplicate}],
    ct:pal("N01-N10 matrix: ~p", [Matrix]),
    lists:foreach(fun({Id, Scenario}) -> run_nested_scenario(Id, Scenario, Config) end,
        Matrix).

release_artifacts(Config) ->
    Releases = ensure_releases(Config),
    lists:foreach(fun({_V, R}) ->
        lists:foreach(fun(Key) -> artifact_evidence(Key, maps:get(Key, R)) end,
            [rel, script, boot, relup, tar]),
        ct:pal("release metadata: ~p", [R])
    end, maps:to_list(Releases)),
    ?assert(filelib:is_file(maps:get(relup, maps:get(b, Releases)))),
    ?assert(filelib:is_file(maps:get(relup, maps:get(c, Releases)))).

run_application_scenario(t01, baseline_a, Config) -> baseline_version(t01, a, Config);
run_application_scenario(t02, baseline_b, Config) -> baseline_version(t02, b, Config);
run_application_scenario(t03, baseline_c, Config) -> baseline_version(t03, c, Config);
run_application_scenario(t04, a_to_b, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t04), N2 = upgrade(N1, b),
    assert_release(N1, N2, {2,0,0}, [R], []),
    reply(N2, R, t04_reply), complete_order(t04, {2,0,0}),
    assert_done(N2, order, {2,0,0}, {route, {{1,0,0},{2,0,0}}}) end, Config);
run_application_scenario(t05, b_to_c, Config) -> with_release_node(fun(N) ->
    N1 = upgrade(N, b), {N2, R} = order_request(N1, t05), N3 = upgrade(N2, c),
    assert_release(N2, N3, {3,0,0}, [R], []),
    reply(N3, R, t05_reply), complete_order(t05, {3,0,0}),
    assert_done(N3, order, {3,0,0}, {route, {{2,0,0},{3,0,0}}}) end, Config);
run_application_scenario(t06, a_to_b_to_c_direct, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t06), N2 = upgrade(N1, b), N3 = upgrade(N2, c),
    assert_release(N2, N3, {3,0,0}, [R], []),
    reply(N3, R, t06_reply), complete_order(t06, {3,0,0}),
    assert_done(N3, order, {3,0,0}, {route, {{1,0,0},{3,0,0}}}),
    assert_exact_counters(N3, order, #{{action,order} => 1,
        {request,order} => 1, {resume,order} => 1,
        {route,{{1,0,0},{3,0,0}}} => 1}),
    assert_no_counter(N3, order, {route, {{1,0,0},{2,0,0}}}),
    assert_no_counter(N3, order, {converter, order}) end, Config);
run_application_scenario(t07, a_to_c_direct, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t07), N2 = upgrade(N1, c),
    assert_release(N1, N2, {3,0,0}, [R], []),
    reply(N2, R, t07_reply), complete_order(t07, {3,0,0}),
    assert_done(N2, order, {3,0,0}, {route, {{1,0,0},{3,0,0}}}),
    assert_exact_counters(N2, order, #{{action,order} => 1,
        {request,order} => 1, {resume,order} => 1,
        {route,{{1,0,0},{3,0,0}}} => 1}),
    assert_no_counter(N2, order, {route, {{1,0,0},{2,0,0}}}),
    assert_no_counter(N2, order, {converter, order}) end, Config);
run_application_scenario(t08, reply_before_upgrade, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t08), reply(N1, R, t08_reply),
    complete_order(t08, {1,0,0}), N2 = upgrade(N1, b),
    assert_release(N1, N2, {2,0,0}, [], []), assert_no_completion(t08) end, Config);
run_application_scenario(t09, reply_between_upgrades, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t09), N2 = upgrade(N1, b),
    reply(N2, R, t09_reply), complete_order(t09, {2,0,0}),
    N3 = upgrade(N2, c), assert_release(N2, N3, {3,0,0}, [], []) end, Config);
run_application_scenario(t10, reply_after_upgrade, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t10), N2 = upgrade(N1, b), N3 = upgrade(N2, c),
    reply(N3, R, t10_reply), complete_order(t10, {3,0,0}),
    assert_done(N3, order, {3,0,0}, {route, {{1,0,0},{3,0,0}}}) end, Config);
run_application_scenario(t11, mixed_a_b, Config) -> with_release_node(fun(N) ->
    {N1, A} = order_request(N, t11_a), N2 = upgrade(N1, b),
    {N3, B} = order_request(N2, t11_b), N4 = upgrade(N3, c),
    assert_release(N3, N4, {3,0,0}, [A,B], []),
    reply(N4, B, t11_b_reply), complete_order(t11_b, {3,0,0}),
    reply(N4, A, t11_a_reply), complete_order(t11_a, {3,0,0}),
    assert_done_count(N4, order, {3,0,0}, 2,
        #{{action,order} => 2, {request,order} => 2, {resume,order} => 2,
          {route,{{1,0,0},{3,0,0}}} => 1,
          {route,{{2,0,0},{3,0,0}}} => 1}) end, Config);
run_application_scenario(t12, mixed_a_b_c, Config) -> with_release_node(fun(N) ->
    {N1, A} = order_request(N, t12_a), N2 = upgrade(N1, b),
    {N3, B} = order_request(N2, t12_b), N4 = upgrade(N3, c),
    assert_release(N3, N4, {3,0,0}, [A,B], []),
    {N5, C} = order_request(N4, t12_c), assert_runtime(N5, {3,0,0}, [A,B,C], []),
    reply(N5, C, t12_c_reply), complete_order(t12_c, {3,0,0}),
    reply(N5, A, t12_a_reply), complete_order(t12_a, {3,0,0}),
    reply(N5, B, t12_b_reply), complete_order(t12_b, {3,0,0}),
    assert_done_count(N5, order, {3,0,0}, 3,
        #{{action,order} => 3, {request,order} => 3, {resume,order} => 3,
          {route,{{1,0,0},{3,0,0}}} => 1,
          {route,{{2,0,0},{3,0,0}}} => 1}) end, Config);
run_application_scenario(t13, same_source_two, Config) -> with_release_node(fun(N) ->
    {N1, A} = order_request(N, t13_a), {N2, B} = order_request(N1, t13_b),
    N3 = upgrade(N2, c), assert_release(N2, N3, {3,0,0}, [A,B], []),
    reply(N3, B, t13_b_reply), complete_order(t13_b, {3,0,0}),
    reply(N3, A, t13_a_reply), complete_order(t13_a, {3,0,0}),
    assert_done_count(N3, order, {3,0,0}, 2,
        #{{action,order} => 2, {request,order} => 2, {resume,order} => 2,
          {route,{{1,0,0},{3,0,0}}} => 2}) end, Config);
run_application_scenario(t14, duplicate_reply, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t14), N2 = upgrade(N1, c),
    reply(N2, R, t14_reply), complete_order(t14, {3,0,0}),
    ?assertEqual({error, already_replied}, reply_result(N2, R, duplicate)),
    assert_done(N2, order, {3,0,0}, {route, {{1,0,0},{3,0,0}}}),
    assert_exact_counters(N2, order, #{{action,order} => 1,
        {request,order} => 1, {resume,order} => 1,
        {route,{{1,0,0},{3,0,0}}} => 1}),
    ?assertEqual(#{pending => 0, replied => 1},
        maps:get(controller_counts, observe(N2))),
    assert_no_completion(t14) end, Config);
run_application_scenario(t15, unknown_reply, Config) -> with_release_node(fun(N) ->
    C = maps:get(controller, N), Before = observe(N), Unknown = make_ref(),
    ?assertEqual({error, unknown_mref}, reply_result(N, Unknown, ignored)),
    ?assertEqual(Before, observe(N)),
    ?assertEqual(#{pending => 0, replied => 0}, test_async_controller:counts(C)) end, Config);
run_application_scenario(t16, continuity, Config) -> with_release_node(fun(N) ->
    {N1, R} = order_request(N, t16), N2 = upgrade(N1, b),
    assert_release(N1, N2, {2,0,0}, [R], []),
    reply(N2, R, t16_reply), complete_order(t16, {2,0,0}),
    assert_done(N2, order, {2,0,0}, {route, {{1,0,0},{2,0,0}}}),
    assert_exact_counters(N2, order, #{{action,order} => 1,
        {request,order} => 1, {resume,order} => 1,
        {route,{{1,0,0},{2,0,0}}} => 1}),
    ?assertEqual(#{pending => 0, replied => 1},
        maps:get(controller_counts, observe(N2))) end, Config);
run_application_scenario(t17, old_and_new, Config) -> with_release_node(fun(N) ->
    N1 = upgrade(N, b), {N2, Old} = order_request(N1, t17_b), N3 = upgrade(N2, c),
    assert_release(N2, N3, {3,0,0}, [Old], []),
    {N4, New} = order_request(N3, t17_c), assert_runtime(N4, {3,0,0}, [Old,New], []),
    reply(N4, New, t17_c_reply), complete_order(t17_c, {3,0,0}),
    reply(N4, Old, t17_b_reply), complete_order(t17_b, {3,0,0}),
    assert_done_count(N4, order, {3,0,0}, 2,
        #{{action,order} => 2, {request,order} => 2, {resume,order} => 2,
          {route,{{2,0,0},{3,0,0}}} => 1}) end, Config);
run_application_scenario(t18, three_owner_versions, Config) -> with_release_node(fun(N) ->
    {N1, A} = order_request(N, t18_a), N2 = upgrade(N1, b),
    {N3, B} = order_request(N2, t18_b), N4 = upgrade(N3, c),
    assert_release(N3, N4, {3,0,0}, [A,B], []),
    {N5, C} = order_request(N4, t18_c), assert_runtime(N5, {3,0,0}, [A,B,C], []),
    reply(N5, A, t18_a_reply), complete_order(t18_a, {3,0,0}),
    reply(N5, C, t18_c_reply), complete_order(t18_c, {3,0,0}),
    reply(N5, B, t18_b_reply), complete_order(t18_b, {3,0,0}),
    assert_done_count(N5, order, {3,0,0}, 3,
        #{{action,order} => 3, {request,order} => 3, {resume,order} => 3,
          {route,{{1,0,0},{3,0,0}}} => 1,
          {route,{{2,0,0},{3,0,0}}} => 1}) end, Config).

run_nested_scenario(n01, same_a, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _PR} = nested_request(N, n01),
    reply(N1, P, n01_child), complete_payment({child, n01}, {1,0,0}),
    reply(N1, O, n01_order), complete_order(n01, {1,0,0}),
    assert_nested_done(N1, {1,0,0}, {1,0,0}) end, Config);
run_nested_scenario(n02, order_ab_payment_a, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n02),
    reply(N1, P, n02_child), complete_payment({child, n02}, {1,0,0}),
    assert_runtime(N1, {1,0,0}, [O], []),
    N2 = upgrade(N1, b), assert_release(N1, N2, {2,0,0}, [O], []),
    reply(N2, O, n02_order), complete_order(n02, {2,0,0}),
    assert_nested_done(N2, {2,0,0}, {2,0,0}) end, Config);
run_nested_scenario(n03, order_a_payment_ac, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n03), N2 = upgrade(N1, b), N3 = upgrade(N2, c),
    assert_release(N2, N3, {3,0,0}, [O,P], []), reply(N3, P, n03_child),
    complete_payment({child, n03}, {3,0,0}), reply(N3, O, n03_order), complete_order(n03, {3,0,0}),
    assert_nested_done(N3, {3,0,0}, {3,0,0}) end, Config);
run_nested_scenario(n04, order_c_payment_b, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n04), N2 = upgrade(N1, b),
    reply(N2, P, n04_child), complete_payment({child, n04}, {2,0,0}), N3 = upgrade(N2, c),
    assert_release(N2, N3, {3,0,0}, [O], []), reply(N3, O, n04_order),
    complete_order(n04, {3,0,0}), assert_nested_done(N3, {3,0,0}, {3,0,0}) end, Config);
run_nested_scenario(n05, both_ac, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n05), N2 = upgrade(N1, c),
    assert_release(N1, N2, {3,0,0}, [O,P], []), reply(N2, P, n05_child),
    complete_payment({child, n05}, {3,0,0}), reply(N2, O, n05_order), complete_order(n05, {3,0,0}),
    ?assertEqual(false, has_counter(N2, order, {route, {{1,0,0},{2,0,0}}})),
    ?assertEqual(false, has_counter(N2, payment, {route, {{1,0,0},{2,0,0}}})) end, Config);
run_nested_scenario(n06, parent_converter_child_raw, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n06), N2 = upgrade(N1, b),
    reply(N2, P, n06_child_raw), complete_payment({child, n06}, {2,0,0}),
    parent_consumed(n06),
    assert_nested_exact(N2, [O], [],
        #{order => #{pending => 5, completion_count => 0,
              counters => #{{request,order} => 1, {action,order} => 1,
                {route,{{1,0,0},{2,0,0}}} => 1,
                {resume,order} => 1, {converter,order} => 1}},
          payment => #{pending => 0, completion_count => 1,
              counters => #{{request,payment} => 1, {action,payment} => 1,
                {route,{{1,0,0},{2,0,0}}} => 1,
                {resume,payment} => 1}},
          controller => #{pending => 1, replied => 1}}),
    reply(N2, O, n06_parent), complete_order(n06, {2,0,0}),
    assert_nested_done(N2, {2,0,0}, {2,0,0}),
    assert_exact_counters(N2, order, #{{request,order} => 1,
        {action,order} => 1, {route, {{1,0,0},{2,0,0}}} => 2,
        {resume,order} => 2, {converter,order} => 2}),
    assert_exact_counters(N2, payment, #{{request,payment} => 1,
        {action,payment} => 1, {route, {{1,0,0},{2,0,0}}} => 1,
        {resume,payment} => 1}) end, Config);
run_nested_scenario(n07, parent_raw_child_converter, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n07), N2 = upgrade(N1, b), N3 = upgrade(N2, c),
    reply(N3, P, n07_child), complete_payment({child, n07}, {3,0,0}),
    parent_consumed(n07),
    assert_nested_exact(N3, [O], [],
        #{order => #{pending => 5, completion_count => 0,
              counters => #{{request,order} => 1, {action,order} => 1,
                {route,{{1,0,0},{3,0,0}}} => 1,
                {resume,order} => 1}},
          payment => #{pending => 0, completion_count => 1,
              counters => #{{request,payment} => 1, {action,payment} => 1,
                {route,{{1,0,0},{3,0,0}}} => 1,
                {resume,payment} => 1, {converter,payment} => 1}},
          controller => #{pending => 1, replied => 1}}),
    assert_no_completion(n07), reply(N3, O, n07_parent),
    complete_order(n07, {3,0,0}),
    assert_nested_done(N3, {3,0,0}, {3,0,0}),
    assert_exact_counters(N3, order, #{{request,order} => 1,
        {action,order} => 1, {route, {{1,0,0},{3,0,0}}} => 2,
        {resume,order} => 2}),
    assert_exact_counters(N3, payment, #{{request,payment} => 1,
        {action,payment} => 1, {route, {{1,0,0},{3,0,0}}} => 1,
        {resume,payment} => 1,
        {converter,payment} => 1}) end, Config);
run_nested_scenario(n08, mixed_pending, Config) -> with_release_node(fun(N) ->
    Sentinel = {privacy_sentinel, make_ref()}, TagA = n08_a,
    TagB = n08_b, TagC = {n08_c, Sentinel},
    {N1, OA, PA, _} = nested_request(N, TagA), N2 = upgrade(N1, b),
    assert_release(N1, N2, {2,0,0}, [OA], [PA]),
    {N3, OB, PB, _} = nested_request(N2, TagB),
    ?assertEqual(lists:sort([OA, OB, PA, PB]),
        lists:sort(maps:get(controller_pending, observe(N3)))),
    N4 = upgrade(N3, c),
    assert_release(N3, N4, {3,0,0}, [OA, OB], [PA, PB]),
    {N5, OC, PC, _} = nested_request(N4, TagC),
    ?assertEqual(lists:sort([OA, OB, OC, PA, PB, PC]),
        lists:sort(maps:get(controller_pending, observe(N5)))),
    reply(N5, PC, n08_c_child), PCOutcome = complete_payment({child, TagC}, {3,0,0}),
    assert_no_sentinel(PCOutcome, Sentinel),
    assert_nested_progress(N5, [OA, OB, OC], [PA, PB], 0, 1),
    reply(N5, OC, n08_c_parent), OCOutcome = complete_order(TagC, {3,0,0}),
    assert_no_sentinel(OCOutcome, Sentinel),
    assert_nested_progress(N5, [OA, OB], [PA, PB], 1, 1),
    reply(N5, PB, n08_b_child), complete_payment({child, TagB}, {3,0,0}),
    assert_nested_progress(N5, [OA, OB], [PA], 1, 2),
    reply(N5, OB, n08_b_parent), complete_order(TagB, {3,0,0}),
    assert_nested_progress(N5, [OA], [PA], 2, 2),
    reply(N5, PA, n08_a_child), complete_payment({child, TagA}, {3,0,0}),
    assert_nested_progress(N5, [OA], [], 2, 3),
    reply(N5, OA, n08_a_parent), complete_order(TagA, {3,0,0}),
    assert_nested_done_count(N5, {3,0,0}, {3,0,0}, 3),
    assert_exact_counters(N5, order, #{{request,order} => 3,
        {action,order} => 3, {route,{{1,0,0},{3,0,0}}} => 2,
        {route,{{2,0,0},{3,0,0}}} => 2, {resume,order} => 6}),
    assert_exact_counters(N5, payment, #{{request,payment} => 3,
        {action,payment} => 3, {route,{{1,0,0},{3,0,0}}} => 1,
        {route,{{2,0,0},{3,0,0}}} => 1, {resume,payment} => 3}) end, Config);
run_nested_scenario(n09, bounded_child_failure, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n09), N2 = upgrade(N1, c),
    reply(N2, P, {error, child_failed}),
    ?assertMatch({ok, {payment, _, {error, child_failed}, _}},
        complete_payment({child, n09}, {3,0,0})),
    reply(N2, O, n09_parent), complete_order_error(n09),
    ?assertEqual(1, maps:get(completion_count, maps:get(order, observe(N2)))),
    ?assertEqual(false, contains_private(maps:get(completions, maps:get(order, observe(N2))))),
    assert_no_completion(n09) end, Config);
run_nested_scenario(n10, child_late_duplicate, Config) -> with_release_node(fun(N) ->
    {N1, O, P, _} = nested_request(N, n10), N2 = upgrade(N1, c),
    ParentCounters = #{{request,order} => 1, {action,order} => 1,
        {route,{{1,0,0},{3,0,0}}} => 1, {resume,order} => 1},
    PaymentPendingCounters = #{{request,payment} => 1, {action,payment} => 1},
    reply(N2, O, n10_parent),
    assert_nested_exact(N2, [], [P],
        #{order => #{pending => 5, completion_count => 0,
              counters => ParentCounters},
          payment => #{pending => 3, completion_count => 0,
              counters => PaymentPendingCounters},
          controller => #{pending => 1, replied => 1}}),
    assert_no_completion(n10),
    reply(N2, P, n10_child),
    ChildOutcome = complete_payment({child, n10}, {3,0,0}),
    ?assertMatch({ok, {payment, _, _, _}}, ChildOutcome),
    parent_consumed(n10),
    Done = #{order => #{pending => 0, completion_count => 1,
            counters => ParentCounters#{{route,{{1,0,0},{3,0,0}}} => 2,
                {resume,order} => 2}},
        payment => #{pending => 0, completion_count => 1,
            counters => PaymentPendingCounters#{{route,{{1,0,0},{3,0,0}}} => 1,
                {resume,payment} => 1}},
        controller => #{pending => 0, replied => 2}},
    assert_nested_exact(N2, [], [], Done),
    ?assertEqual({error, already_replied}, reply_result(N2, P, duplicate_child)),
    assert_nested_exact(N2, [], [], Done), assert_no_completion({child, n10}),
    complete_order(n10, {3,0,0}), assert_nested_done(N2, {3,0,0}, {3,0,0}),
    assert_nested_exact(N2, [], [], Done), assert_no_completion(n10) end, Config).

baseline_version(Id, V, Config) ->
    N = start_fixture_node(V, Config),
    try
        assert_runtime(N, version_tuple_for(V), [], []),
        {N1, R} = order_request(N, Id), assert_runtime(N1, version_tuple_for(V), [R], []),
        reply(N1, R, baseline_reply), complete_order(Id, version_tuple_for(V)),
        assert_done(N1, order, version_tuple_for(V), undefined)
    after stop_fixture_node(N) end.

compile_fixture(V, Root, Priv) ->
    Src = filename:join(Root, atom_to_list(V)),
    Ebin = filename:join([Priv, "fixtures", atom_to_list(V), "ebin"]),
    ok = filelib:ensure_dir(filename:join(Ebin, "placeholder")),
    lists:foreach(fun(F) ->
        Source = filename:join(Src, F),
        {ok, Forms} = epp:parse_file(Source, [{includes, [Root]}]),
        case compile:forms(Forms, [{source, Source}, return_errors,
                return_warnings, debug_info]) of
            {ok, Module, Binary, Warnings} ->
                ok = file:write_file(filename:join(Ebin,
                    atom_to_list(Module) ++ ".beam"), Binary),
                ct:pal("compiled ~p/~s warnings=~p", [V,F,Warnings]);
            {error, Errors, Warnings} -> ct:fail({fixture_compile, V, F, Errors, Warnings})
        end
    end, ["sf_fixture_app.src", "sf_fixture_sup.src", "order_flow.src",
        "payment_flow.src"]),
    {ok, _} = file:copy(filename:join(Src, "sf_fixture.app"),
        filename:join(Ebin, "sf_fixture.app")),
    {ok, _} = file:copy(filename:join(Src, "sf_fixture.appup"),
        filename:join(Ebin, "sf_fixture.appup")),
    Ebin.

start_fixture_node(V, Config) ->
    Ebin = maps:get(V, ?config(fixture_ebins, Config)),
    Name = unique_atom("sf_fixture_" ++ atom_to_list(V) ++ "_"),
    Args = lists:append([["-pa", P] || P <- runtime_paths()]),
    {ok, P, Node} = start_node(Name, Args, undefined, []),
    pong = net_adm:ping(Node),
    true = node_call(P, code, add_patha, [Ebin]),
    {ok, _} = node_call(P, application, ensure_all_started, [sf_fixture]),
    {ok, Controller} = start_controller(P, self()),
    #{peer => P, node => Node, version => V, current => V,
        controller => Controller}.

stop_fixture_node(#{peer := P}) -> catch stop_node(P), ok.

start_node(Name, Args, Exec, Env) ->
    case code:ensure_loaded(peer) of
        {module, peer} ->
            Options = case Exec of
                undefined -> #{name => Name, connection => standard_io, args => Args};
                _ -> #{name => Name, connection => standard_io, exec => Exec,
                    env => Env, args => Args}
            end,
            {ok, Peer, Node} = peer:start_link(Options),
            {ok, {peer, Peer}, Node};
        _ ->
            SlaveEnv = case Exec of
                undefined -> Env;
                _ -> [{"PATH", filename:dirname(Exec) ++ ":" ++ os:getenv("PATH")} |
                    Env]
            end,
            SlaveArgs = Args ++ ["-pa", filename:dirname(code:which(ct_slave))],
            {ok, Node} = ct_slave:start(Name, [{erl_flags,
                string:join(SlaveArgs, " ")}, {env, SlaveEnv}, {boot_timeout, 30}]),
            {ok, {ct_slave, Node}, Node}
    end.

node_call({peer, Peer}, Module, Function, Args) ->
    peer:call(Peer, Module, Function, Args);
node_call({ct_slave, Node}, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args).

start_controller({peer, _} = Peer, Observer) ->
    node_call(Peer, test_async_controller, start_link, [Observer]);
start_controller({ct_slave, _} = Slave, Observer) ->
    node_call(Slave, test_async_controller, start, [Observer]).

stop_node({peer, Peer}) -> peer:stop(Peer);
stop_node({ct_slave, Node}) -> ct_slave:stop(Node).

legacy_erl_wrapper(Root, Erl) ->
    Wrapper = filename:join(Root, "bin/erl"),
    ok = filelib:ensure_dir(Wrapper),
    ok = file:write_file(Wrapper, io_lib:format(
        "#!/bin/sh~nROOTDIR=~s~nBINDIR=~s~nexport ROOTDIR BINDIR~nexec \"$BINDIR/erlexec\" \"$@\"~n",
        [Root, filename:dirname(Erl)])),
    ok = file:change_mode(Wrapper, 8#755),
    Wrapper.

node_snapshot(#{peer := P, controller := C}) ->
    Apps = node_call(P, application, which_applications, []),
    Application = node_call(P, application_controller, get_master, [sf_fixture]),
    ?assert(is_pid(Application)),
    Order = node_call(P, order_flow, status, []),
    Payment = node_call(P, payment_flow, status, []),
    Pending = node_call(P, test_async_controller, pending, [C]),
    #{apps => Apps, application => Application,
      marker => maps:get(marker, Order),
      version => maps:get(version, Order), controller => C,
      controller_counts => node_call(P, test_async_controller, counts, [C]),
      controller_pending => Pending,
      controller_identities => [{MRef,
          node_call(P, test_async_controller, identity, [C, MRef])} ||
          MRef <- Pending],
      order => Order, payment => Payment,
      tree => #{top => node_call(P, erlang, whereis, [sf_fixture_sup]),
          order_sup => node_call(P, erlang, whereis, [order_sup]),
          order_worker => maps:get(pid, Order),
          payment_sup => node_call(P, erlang, whereis, [payment_sup]),
          payment_worker => maps:get(pid, Payment)}}.

assert_tree(S) ->
    ?assert(lists:keymember(sf_fixture, 1, maps:get(apps, S))),
    lists:foreach(fun(K) -> ?assert(is_pid(maps:get(K, maps:get(tree, S)))) end,
        [top, order_sup, order_worker, payment_sup, payment_worker]).

baseline_call(#{peer := P, controller := C}, Tag) ->
    {ok, MRef} = node_call(P, order_flow, start, [C, Tag, self()]),
    held_mref(MRef),
    ?assertEqual(#{pending => 1, replied => 0},
        node_call(P, test_async_controller, counts, [C])),
    ok = node_call(P, test_async_controller, make_reply, [C, MRef, reply]),
    receive {order_complete, Tag, Outcome} -> ?assertEqual(false, contains_private(Outcome))
    after 5000 -> ct:fail({Tag, missing_completion}) end,
    ?assertEqual(#{pending => 0, replied => 1},
        node_call(P, test_async_controller, counts, [C])),
    Status = node_call(P, order_flow, status, []),
    ?assertEqual(0, maps:get(pending, Status)),
    ?assertEqual(1, maps:get(completion_count, Status)).

version_tuple_for(a) -> {1,0,0};
version_tuple_for(b) -> {2,0,0};
version_tuple_for(c) -> {3,0,0}.
current_version(#{current := V}) -> version_tuple_for(V).
order_request(N, Tag) ->
    #{peer := P, controller := C} = N,
    Vsn = current_version(N),
    {ok, MRef} = node_call(P, order_flow, start, [C, Tag, self()]),
    held_request(MRef, {order, Vsn, Tag}),
    assert_runtime_has(N, Vsn, [MRef]),
    ?assertMatch({pending, {_, MRef}},
        node_call(P, test_async_controller, identity, [C, MRef])),
    {N, MRef}.
payment_request(N, Tag) ->
    #{peer := P, controller := C} = N,
    Vsn = current_version(N),
    {ok, MRef} = node_call(P, payment_flow, start, [C, Tag, self()]),
    held_request(MRef, {payment, Vsn, Tag}),
    assert_runtime_has(N, Vsn, [MRef]),
    ?assertMatch({pending, {_, MRef}},
        node_call(P, test_async_controller, identity, [C, MRef])),
    {N, MRef}.
nested_request(N, Tag) ->
    #{peer := P, controller := C} = N,
    Vsn = current_version(N),
    {ok, Refs} = node_call(P, order_flow, start_nested,
        [C, Tag, {child, Tag}, self()]),
    O = maps:get(order, Refs), PRef = maps:get(payment, Refs),
    ParentRef = maps:get(parent, Refs),
    held_request(O, {order, Vsn, Tag}),
    held_request(PRef, {payment, Vsn, {child, Tag}}),
    assert_runtime_has(N, Vsn, [O, PRef]),
    ?assert(is_reference(ParentRef)),
    ?assertMatch({pending, {_, O}},
        node_call(P, test_async_controller, identity, [C, O])),
    ?assertMatch({pending, {_, PRef}},
        node_call(P, test_async_controller, identity, [C, PRef])),
    {N, O, PRef, ParentRef}.
held_request(Expected, Request) ->
    receive {controller_held, Expected, Request} -> ok
    after 5000 -> ct:fail({missing_exact_request, Expected, Request}) end.
reply(N, MRef, Value) -> ?assertEqual(ok, reply_result(N, MRef, Value)).
reply_result(#{peer := P, controller := C}, MRef, Value) ->
    node_call(P, test_async_controller, make_reply, [C, MRef, Value]).
complete_order(Tag, Vsn) ->
    receive {order_complete, Tag, Outcome} ->
        ?assertMatch({ok, {order, _, _, _}}, Outcome),
        ?assertEqual(Vsn, element(2, element(2, Outcome))),
        ?assertEqual(false, contains_private(Outcome)), Outcome
    after 5000 -> ct:fail({Tag, missing_order_completion}) end.
complete_payment(Tag, Vsn) ->
    receive {payment_complete, Tag, Outcome} ->
        ?assertMatch({ok, {payment, _, _, _}}, Outcome),
        ?assertEqual(Vsn, element(2, element(2, Outcome))),
        ?assertEqual(false, contains_private(Outcome)), Outcome
    after 5000 -> ct:fail({Tag, missing_payment_completion}) end.
parent_consumed(Tag) ->
    receive {parent_consumed, Tag} -> ok
    after 5000 -> ct:fail({Tag, missing_parent_consumed}) end.
complete_order_error(Tag) ->
    receive {order_complete, Tag, {ok, {error, {child, Reason, _}}}} ->
        ?assertEqual(child_failed, Reason);
        {order_complete, Tag, Other} -> ct:fail({unexpected_order_error, Other})
    after 5000 -> ct:fail({Tag, missing_order_error}) end.
assert_no_completion(Tag) ->
    receive {order_complete, Tag, _} -> ct:fail({duplicate_order_completion, Tag});
        {payment_complete, Tag, _} -> ct:fail({duplicate_payment_completion, Tag})
    after 0 -> ok end.
observe(N) -> node_snapshot(N).
assert_runtime(N, Vsn, OrderRefs, PaymentRefs) ->
    S = observe(N), assert_tree(S),
    ?assertEqual(Vsn, maps:get(version, S)),
    ?assertEqual(Vsn, maps:get(version, maps:get(order, S))),
    ?assertEqual(Vsn, maps:get(version, maps:get(payment, S))),
    ?assertEqual(lists:sort(OrderRefs ++ PaymentRefs),
        lists:sort(maps:get(controller_pending, S))),
    ?assertEqual(length(OrderRefs) + length(PaymentRefs),
        maps:get(pending, maps:get(controller_counts, S))), S.
assert_runtime_has(N, Vsn, Refs) ->
    S = observe(N), assert_tree(S),
    ?assertEqual(Vsn, maps:get(version, S)),
    ?assertEqual(Vsn, maps:get(version, maps:get(order, S))),
    ?assertEqual(Vsn, maps:get(version, maps:get(payment, S))),
    Pending = maps:get(controller_pending, S),
    lists:foreach(fun(MRef) -> ?assert(lists:member(MRef, Pending)) end, Refs), S.
assert_release(BeforeN, AfterN, Vsn, OrderRefs, PaymentRefs) ->
    _ = BeforeN,
    Before = maps:get(before_upgrade, AfterN),
    After = maps:get(after_upgrade, AfterN),
    ct:pal("release snapshots before=~p after=~p", [Before, After]),
    ?assertEqual(maps:get(application, Before), maps:get(application, After)),
    ?assertEqual(maps:get(controller, Before), maps:get(controller, After)),
    ?assertEqual(maps:get(controller_counts, Before), maps:get(controller_counts, After)),
    ?assertEqual(maps:get(controller_pending, Before), maps:get(controller_pending, After)),
    ?assertEqual(maps:get(controller_identities, Before),
        maps:get(controller_identities, After)),
    ?assertEqual(maps:get(tree, Before), maps:get(tree, After)),
    lists:foreach(fun(MRef) ->
        {MRef, ExpectedIdentity} = lists:keyfind(MRef, 1,
            maps:get(controller_identities, Before)),
        ?assertEqual(ExpectedIdentity,
            node_call(maps:get(peer, AfterN), test_async_controller, identity,
                [maps:get(controller, AfterN), MRef]))
    end, OrderRefs ++ PaymentRefs),
    assert_runtime(AfterN, Vsn, OrderRefs, PaymentRefs).
upgrade(N, Target) ->
    {ok, Before, After} = install_release(N, Target),
    N#{current => Target, before_upgrade => Before, after_upgrade => After}.
assert_done(N, Owner, Vsn, Route) ->
    S = observe(N), Status = maps:get(Owner, S),
    ct:pal("scenario final owner=~p vsn=~p status=~p controller=~p",
        [Owner, Vsn, Status, maps:get(controller_counts, S)]),
    ?assertEqual(Vsn, maps:get(version, Status)),
    ?assertEqual(0, maps:get(pending, Status)),
    ?assertEqual(1, maps:get(completion_count, Status)),
    case Route of undefined -> ok; _ -> ?assert(has_counter(N, Owner, Route)) end,
    ?assertEqual(0, maps:get(pending, maps:get(controller_counts, S))).
assert_done_count(N, Owner, Vsn, Count, ExpectedCounters) ->
    S = observe(N), Status = maps:get(Owner, S),
    ?assertEqual(Vsn, maps:get(version, Status)),
    ?assertEqual(0, maps:get(pending, Status)),
    ?assertEqual(Count, maps:get(completion_count, Status)),
    ?assertEqual(ExpectedCounters, maps:get(counters, Status)),
    ?assertEqual([], maps:get(controller_pending, S)),
    ?assertEqual(#{pending => 0, replied => Count},
        maps:get(controller_counts, S)).
assert_nested_done(N, OrderVsn, PaymentVsn) ->
    assert_nested_done_count(N, OrderVsn, PaymentVsn, 1).
assert_nested_done_count(N, OrderVsn, PaymentVsn, OrderCount) ->
    S = observe(N),
    ?assertEqual(OrderVsn, maps:get(version, maps:get(order, S))),
    ?assertEqual(PaymentVsn, maps:get(version, maps:get(payment, S))),
    ?assertEqual(OrderCount, maps:get(completion_count, maps:get(order, S))),
    ?assertEqual(OrderCount, maps:get(completion_count, maps:get(payment, S))),
    ?assertEqual(0, maps:get(pending, maps:get(order, S))),
    ?assertEqual(0, maps:get(pending, maps:get(payment, S))),
    ?assertEqual(#{pending => 0, replied => OrderCount * 2},
        maps:get(controller_counts, S)).
assert_nested_progress(N, OrderRefs, PaymentRefs, OrderCount, PaymentCount) ->
    S = assert_runtime(N, current_version(N), OrderRefs, PaymentRefs),
    ?assertEqual(OrderCount, maps:get(completion_count, maps:get(order, S))),
    ?assertEqual(PaymentCount, maps:get(completion_count, maps:get(payment, S))),
    S.
assert_nested_exact(N, OrderRefs, PaymentRefs, Expected) ->
    S = assert_runtime(N, current_version(N), OrderRefs, PaymentRefs),
    lists:foreach(fun(Owner) ->
        ?assertEqual(maps:get(Owner, Expected), maps:with(
            [pending, completion_count, counters], maps:get(Owner, S)))
    end, [order, payment]),
    ?assertEqual(maps:get(controller, Expected), maps:get(controller_counts, S)),
    S.
has_counter(N, Owner, Key) ->
    maps:is_key(Key, maps:get(counters, maps:get(Owner, observe(N)))).
assert_exact_counters(N, Owner, Expected) ->
    ?assertEqual(Expected, maps:get(counters, maps:get(Owner, observe(N)))).
assert_no_counter(N, Owner, Key) ->
    ?assertEqual(false, maps:is_key(Key, maps:get(counters, maps:get(Owner, observe(N))))).

held(Expected) ->
    receive {controller_held, MRef, Expected} -> {MRef, Expected}
    after 5000 -> ct:fail({missing_held, Expected}) end.
held_mref(Expected) ->
    receive {controller_held, Expected, _Request} -> ok
    after 5000 -> ct:fail({missing_held_mref, Expected}) end.

runtime_paths() -> [P || P <- code:get_path(), filelib:is_dir(P)].
unique_atom(Prefix) -> list_to_atom(Prefix ++ integer_to_list(
    erlang:unique_integer([positive]))).
ensure_distribution() when node() =/= nonode@nohost -> false;
ensure_distribution() ->
    Name = unique_atom("sf_ct_"),
    {ok, _} = net_kernel:start([Name, shortnames]), true.
version_tuple("1.0.0") -> {1,0,0};
version_tuple("2.0.0") -> {2,0,0};
version_tuple("3.0.0") -> {3,0,0}.
app_name(V, Root) ->
    {ok, [{application, Name, _}]} = file:consult(filename:join(
        [Root, atom_to_list(V), "sf_fixture.app"])), atom_to_list(Name).

contains_private(Term) -> contains_any(Term,
    [env, outcome, state, callback, 'fun', secret, sentinel]).
contains_any(Term, Needles) when is_list(Term) ->
    lists:any(fun(E) -> contains_any(E, Needles) end, Term);
contains_any(Term, Needles) when is_tuple(Term) ->
    contains_any(tuple_to_list(Term), Needles);
contains_any(Term, Needles) when is_map(Term) ->
    contains_any(maps:to_list(Term), Needles);
contains_any(Term, Needles) -> lists:member(Term, Needles).
assert_no_sentinel(Term, Sentinel) -> ?assertEqual(false, contains_term(Term, Sentinel)).
contains_term(Term, Needle) when Term =:= Needle -> true;
contains_term(Term, Needle) when is_list(Term) ->
    lists:any(fun(E) -> contains_term(E, Needle) end, Term);
contains_term(Term, Needle) when is_tuple(Term) ->
    contains_term(tuple_to_list(Term), Needle);
contains_term(Term, Needle) when is_map(Term) ->
    contains_term(maps:to_list(Term), Needle);
contains_term(_, _) -> false.

flush() -> receive _ -> flush() after 0 -> ok end.

%% Filled by the public release harness below; kept in this suite by contract.
ensure_releases(Config) -> ?config(releases, Config).
with_release_node(Fun, Config) ->
    Releases = ensure_releases(Config),
    N = start_release_node(Releases, Config),
    try Fun(N) after stop_fixture_node(N) end.
build_releases(Config) ->
    Priv = ?config(priv_dir, Config),
    Base = filename:join(Priv, "release_roots"),
    Apps = [kernel, stdlib, sasl, crypto, compiler, syntax_tools,
        astranaut, erlando, async, sf_fixture],
    Rs0 = maps:from_list([{V, stage_release(V, Vsn, Base, Apps, Config)} ||
        {V, Vsn, _} <- ?config(fixture_versions, Config)]),
    A = maps:get(a, Rs0), B = maps:get(b, Rs0), C = maps:get(c, Rs0),
    ok = make_relup(B, [A]),
    ok = make_relup(C, [B, A]),
    Rs = maps:map(fun(_, R) -> make_release_tar(R) end, Rs0),
    ARoot = maps:get(root, maps:get(a, Rs)),
    lists:foreach(fun(V) ->
        R = maps:get(V, Rs),
        Name = "sf_release-" ++ maps:get(vsn, R) ++ ".tar.gz",
        {ok, _} = file:copy(maps:get(tar, R), filename:join([ARoot, "releases", Name]))
    end, [b,c]),
    Rs.

stage_release(V, Vsn, Base, Apps, Config) ->
    Root = filename:join(Base, atom_to_list(V)),
    Lib = filename:join(Root, "lib"),
    RelDir = filename:join([Root, "releases", Vsn]),
    ok = filelib:ensure_dir(filename:join(RelDir, "placeholder")),
    ok = link_erts(Root),
    lists:foreach(fun(App) when App =:= astranaut; App =:= erlando; App =:= async ->
            stage_app(App, Lib);
        (sf_fixture) -> stage_fixture(V, Vsn, Lib, Config);
        (App) -> stage_otp_app(App, Lib)
    end, Apps),
    Rel = filename:join(RelDir, "sf_release-" ++ Vsn ++ ".rel"),
    RelTerm = {release, {"sf_release", Vsn},
        {erts, erlang:system_info(version)},
        [{App, app_vsn(App, Vsn)} || App <- Apps]},
    ok = write_term(Rel, RelTerm),
    Paths = [filename:join([Lib, app_dir_name(App, Vsn), "ebin"]) || App <- Apps],
    BaseName = filename:rootname(Rel),
    ScriptResult = systools:make_script(BaseName, [{path, Paths}, silent]),
    ct:pal("systools make_script path=~s result=~p", [BaseName, ScriptResult]),
    ok = systools_ok(ScriptResult),
    #{version => V, vsn => Vsn, root => Root, lib => Lib, rel => Rel,
      base => BaseName, paths => Paths, script => BaseName ++ ".script",
      boot => BaseName ++ ".boot", tar => BaseName ++ ".tar.gz",
      relup => filename:join(RelDir, "relup")}.

make_relup(Current, Old) ->
    Paths = lists:usort(lists:append([maps:get(paths, R) || R <- [Current|Old]])),
    Bases = [maps:get(base, R) || R <- Old],
    Result = systools:make_relup(maps:get(base, Current), Bases, Bases,
        [{path, Paths}, {outdir, filename:dirname(maps:get(rel, Current))}, silent]),
    ct:pal("systools make_relup path=~s result=~p",
        [filename:join(filename:dirname(maps:get(rel, Current)), "relup"), Result]),
    systools_ok(Result).

make_release_tar(R) ->
    Result = systools:make_tar(maps:get(base, R),
        [{path, maps:get(paths, R)}, silent]),
    ct:pal("systools make_tar path=~s result=~p", [maps:get(tar, R), Result]),
    ok = systools_ok(Result), R.

stage_app(App, Lib) ->
    Source = code:lib_dir(App),
    Target = filename:join(Lib, atom_to_list(App) ++ "-" ++ app_vsn(App, "")),
    copy_tree(filename:join(Source, "ebin"), filename:join(Target, "ebin")).

stage_fixture(V, Vsn, Lib, Config) ->
    Target = filename:join(Lib, "sf_fixture-" ++ Vsn),
    Ebin = filename:join(Target, "ebin"),
    ok = filelib:ensure_dir(filename:join(Ebin, "placeholder")),
    Source = maps:get(V, ?config(fixture_ebins, Config)),
    copy_tree(Source, Ebin).

stage_otp_app(App, Lib) ->
    Target = filename:join(Lib, atom_to_list(App) ++ "-" ++ app_vsn(App, "")),
    copy_tree(code:lib_dir(App), Target).

link_erts(Root) ->
    Erts = "erts-" ++ erlang:system_info(version),
    make_symlink(filename:join(code:root_dir(), Erts), filename:join(Root, Erts)).

make_symlink(Source, Target) ->
    ok = filelib:ensure_dir(filename:join(filename:dirname(Target), "placeholder")),
    case file:make_symlink(Source, Target) of ok -> ok; {error, eexist} -> ok end.

copy_tree(Source, Target) ->
    ok = filelib:ensure_dir(filename:join(Target, "placeholder")),
    {ok, Entries} = file:list_dir(Source),
    lists:foreach(fun(Name) ->
        S = filename:join(Source, Name), T = filename:join(Target, Name),
        case filelib:is_dir(S) of
            true -> copy_tree(S, T);
            false -> ok = filelib:ensure_dir(T), {ok, _} = file:copy(S, T)
        end
    end, Entries), ok.

app_vsn(sf_fixture, FixtureVsn) -> FixtureVsn;
app_vsn(App, _) ->
    _ = application:load(App),
    {ok, Vsn} = application:get_key(App, vsn), Vsn.
app_dir_name(sf_fixture, Vsn) -> "sf_fixture-" ++ Vsn;
app_dir_name(App, _) -> atom_to_list(App) ++ "-" ++ app_vsn(App, "").

write_term(Path, Term) -> file:write_file(Path, io_lib:format("~p.~n", [Term])).
systools_ok(ok) -> ok;
systools_ok({ok, _, _}) -> ok;
systools_ok({ok, _, _, _}) -> ok;
systools_ok(Other) -> ct:fail({systools, Other}).

start_release_node(Releases, _Config) ->
    A = maps:get(a, Releases), Root = maps:get(root, A),
    CreateReleases = release_handler:create_RELEASES(Root, maps:get(rel, A)),
    ct:pal("release_handler:create_RELEASES(~s) -> ~p", [maps:get(rel, A),
        CreateReleases]),
    ok = CreateReleases,
    Name = unique_atom("sf_release_a_"),
    TestPath = filename:dirname(code:which(test_async_controller)),
    Erts = "erts-" ++ erlang:system_info(version),
    Erl = filename:join([Root, Erts, "bin", "erl"]),
    Args = ["-boot", maps:get(base, A), "-boot_var", "ROOT", Root,
        "-boot_var", "RELEASE_LIB", maps:get(lib, A), "-pa", TestPath],
    ct:pal("release node exec=~s args=~p env=~p", [Erl, Args,
        [{"ERL_ROOTDIR", Root}]]),
    PeerResult = case code:ensure_loaded(peer) of
        {module, peer} -> start_node(Name, Args, Erl, [{"ERL_ROOTDIR", Root}]);
        _ -> start_node(Name, Args, legacy_erl_wrapper(Root, Erl),
            [{"ERL_ROOTDIR", Root}])
    end,
    ct:pal("release node start result=~p", [PeerResult]),
    {ok, P, Node} = PeerResult,
    pong = net_adm:ping(Node),
    {ok, _} = node_call(P, application, ensure_all_started, [sf_fixture]),
    {ok, C} = start_controller(P, self()),
    N = #{peer => P, node => Node, controller => C, releases => Releases,
      current => a, root => Root},
    ct:pal("release node status node=~p status=~p", [Node, observe(N)]), N.

install_release(#{peer := P, releases := Releases, root := Root} = N, Target) ->
    Before = observe(N),
    ct:pal("release_handler snapshot before target=~p: ~p", [Target, Before]),
    R = maps:get(Target, Releases), Name = "sf_release-" ++ maps:get(vsn, R),
    {ok, _} = file:copy(maps:get(tar, R),
        filename:join([Root, "releases", Name ++ ".tar.gz"])),
    Unpack = node_call(P, release_handler, unpack_release, [Name]),
    ct:pal("release_handler:unpack_release(~p) -> ~p", [Name, Unpack]),
    ?assertMatch({ok, _}, Unpack),
    Check = node_call(P, release_handler, check_install_release,
        [maps:get(vsn, R)]),
    ct:pal("release_handler:check_install_release(~p) -> ~p", [maps:get(vsn,R), Check]),
    ?assertMatch({ok, _, _}, Check),
    Install = node_call(P, release_handler, install_release, [maps:get(vsn, R)]),
    ct:pal("release_handler:install_release(~p) -> ~p", [maps:get(vsn,R), Install]),
    ?assertMatch({ok, _, _}, Install),
    ?assertEqual(maps:get(controller, Before), maps:get(controller, observe(N))),
    ?assertEqual(Target, node_call(P, order_flow, marker, [])),
    Which = node_call(P, release_handler, which_releases, []),
    ct:pal("release_handler:which_releases() -> ~p", [Which]),
    ?assert(lists:any(fun({"sf_release", V, _, Status}) ->
        V =:= maps:get(vsn,R) andalso lists:member(Status,[current,permanent])
    end, Which)),
    After = observe(N),
    ct:pal("release_handler snapshot after target=~p: ~p", [Target, After]),
    {ok, Before, After}.

artifact_evidence(Key, Path) ->
    Exists = filelib:is_file(Path),
    Sha = case Exists of
        true -> {ok, Bin} = file:read_file(Path),
            hex(crypto:hash(sha256, Bin));
        false -> missing
    end,
    ct:pal("release artifact key=~p path=~s exists=~p sha256=~p",
        [Key, Path, Exists, Sha]),
    case Key of relup -> ok; _ -> ?assert(Exists) end.

hex(Bin) -> lists:flatten([io_lib:format("~2.16.0B", [B]) || B <- binary_to_list(Bin)]).
