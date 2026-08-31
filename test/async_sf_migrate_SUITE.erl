%%% Public black-box acceptance for sf-migrate declarations and reply-time bridges.
-module(async_sf_migrate_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [declaration_compiler_contract, compiler_negative_matrix,
    raw_bridge_and_purge,
    compiler_extended_matrix, converter_and_precedence,
    converter_env_and_precedence, converter_observable_matrix,
    converter_incompatible_shapes,
    converter_failure_privacy, fallback_and_failure_privacy,
    downgrade_and_existing_boundary, downgrade_created_in_b].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_Config) -> flush(), ok.

declaration_compiler_contract(_Config) ->
    M = fresh("sf_decl_"),
    {ok, M, Bin, []} = compile_text(M, declaration_source(M)),
    ?assert(is_binary(Bin)),
    {module, M} = code:load_binary(M, "sf_decl", Bin),
    ?assert(is_exported(M, sf_do_code_change, 2)),
    ?assertEqual(unhandled, M:sf_do_code_change({9, 9, 9}, absent)),
    unload(M),
    NoVsn = fresh("sf_no_vsn_"),
    {error, Errors, []} = compile_text(NoVsn, no_vsn_source(NoVsn)),
    ?assert(contains_term(Errors, sf_migrate_requires_explicit_vsn)),
    Partial = fresh("sf_partial_"),
    {error, PartialErrors, []} = compile_text(Partial, partial_source(Partial)),
    ?assert(contains_term(PartialErrors, invalid_sf_migrate_syntax)).

compiler_negative_matrix(_Config) ->
    M = fresh("sf_matrix_"),
    {ok, M, Bin, []} = compile_text(M, declaration_source(M)),
    {module, M} = code:load_binary(M, "sf_matrix", Bin),
    ?assert(is_map(M:sf_do_code_change({1, 0, 0}, old_flow))),
    ?assertEqual(unhandled, M:sf_do_code_change({1, 0, 1}, old_flow)),
    unload(M),
    Cases = [{unnamed, unnamed_source(M), [invalid_sf_migrate_syntax,
            sf_historical_block_not_named, sf_current_block_not_found]},
        {duplicate, duplicate_source(M), [duplicate_sf_migration_source]},
        {missing_block, missing_block_source(M), [sf_current_block_not_found]},
        {missing_step, missing_step_source(M), [sf_step_not_found]},
        {placement, placement_source(M), [invalid_sf_migrate_placement]}],
    lists:foreach(fun({_Name, Source, Reasons}) ->
        {error, Errors, _Warnings} = compile_text(M, Source),
        case lists:any(fun(Reason) -> contains_term(Errors, Reason) end, Reasons) of
            true -> ok;
            false -> ct:fail({unmatched_diagnostic, _Name, Errors})
        end,
        ?assertEqual(false, contains_any(Errors,
            [env, outcome, state, callback, 'fun', payload, ast]))
    end, Cases).

compiler_extended_matrix(_Config) ->
    M = fresh("sf_extended_"),
    {ok, M, LiteralBin, []} = compile_text(M, nonliteral_vsn_source(M)),
    {module, M} = code:load_binary(M, "sf_extended_literal", LiteralBin),
    ?assert(is_map(M:sf_do_code_change(1, old_flow))),
    ?assertEqual(unhandled, M:sf_do_code_change({1, 0, 0}, old_flow)),
    unload(M),
    {ok, M, Bin, []} = compile_text(M, multiple_vsn_source(M)),
    {module, M} = code:load_binary(M, "sf_extended", Bin),
    ?assertMatch(#{source_vsn := {1, 0, 0}, source_block := first_flow},
        M:sf_do_code_change({1, 0, 0}, first_flow)),
    ?assertMatch(#{source_vsn := {2, 0, 0}, source_block := second_flow},
        M:sf_do_code_change({2, 0, 0}, second_flow)),
    unload(M),
    {error, CollisionErrors, []} = compile_text(M, collision_source(M)),
    ?assert(contains_term(CollisionErrors, sf_migration_generated_collision)),
    {error, RequiredErrors, []} = compile_text(M, converter_required_source(M)),
    ?assert(contains_term(RequiredErrors, sf_migration_converter_required)).

raw_bridge_and_purge(_Config) ->
    M = fresh("sf_raw_"), Ref = make_ref(),
    {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, old_flow,
        wait, old, none)),
    {module, M} = code:load_binary(M, "sf_raw_a", A),
    {ok, State0} = start(M, M:action(Ref), raw),
    [#{frame := #{vsn := {1, 0, 0}, block := old_flow, step_name := wait},
        request_mref := Request, parent_ref := Parent}] = summaries(State0),
    {ok, M, B, []} = compile_text(M, version_source(M, {2, 0, 0}, current_flow,
        fetch, migrated, {old_flow, current_flow, wait, fetch})),
    {module, M} = code:load_binary(M, "sf_raw_b", B),
    ?assert(is_reference(Request)), ?assert(is_reference(Parent)),
    ?assertEqual(false, code:purge(M)),
    {RawOutcome, RawDone} = info_reply_state({Ref, reply}, State0),
    ?assertEqual({ok, {migrated, reply}}, RawOutcome),
    assert_completion(raw, {ok, {migrated, reply}}),
    ?assertEqual([], summaries(RawDone)),
    ?assertEqual(1, get_counter(M, {action_calls, old_flow})),
    %% OTP returns false when no old code remains to purge.  Public proof is
    %% completion, counters, and an empty callback store after reply.
    unload(M).

converter_and_precedence(_Config) ->
    M = fresh("sf_convert_"), Ref = make_ref(),
    {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, same_flow,
        wait, old, none)),
    {module, M} = code:load_binary(M, "sf_convert_a", A),
    {ok, State0} = start(M, M:action(Ref), converter),
    {ok, M, B, _Warnings} = compile_text(M, converter_source(M)),
    {module, M} = code:load_binary(M, "sf_convert_b", B),
    ?assertEqual({ok, {migrated, {converted, reply}}}, info_reply({Ref, reply}, State0)),
    ?assertEqual(1, get_counter(M, converter_calls)), unload(M).

converter_failure_privacy(_Config) ->
    lists:foreach(fun({Mode, Reason}) ->
        M = fresh("sf_convert_failure_"), Ref = make_ref(),
        {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0},
            old_flow, wait, old, none)),
        {module, M} = code:load_binary(M, "sf_convert_failure_a", A),
        {ok, State0} = start(M, M:action(Ref), converter_failure),
        {ok, M, B, _Warnings} = compile_text(M, converter_mode_source(M, Mode)),
        {module, M} = code:load_binary(M, "sf_convert_failure_b", B),
        {Outcome, Done} = info_reply_state({Ref, reply}, State0),
        ?assertEqual([], summaries(Done)),
        ?assertMatch({error, {sf_runtime, {incompatible_frame,
            #{module := M, stored_vsn := {1, 0, 0}, current_vsn := {2, 0, 0},
              identity := {1, old_flow, wait}, reason := Reason}}}}, Outcome),
        ?assertEqual(false, contains_any(Outcome,
            [env, outcome, state, callback, 'fun', secret, sentinel])),
        unload(M)
    end, [{error, converter_failed}, {invalid, invalid_converter_result}]).

fallback_and_failure_privacy(_Config) ->
    M = fresh("sf_fallback_"), Ref = make_ref(),
    {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_fallback_a", A),
    {ok, State0} = start(M, M:action(Ref), fallback),
    {ok, M, B, []} = compile_text(M, version_source(M, {2, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_fallback_b", B),
    ?assertEqual({ok, {stable, reply}}, info_reply({Ref, reply}, State0)), unload(M),
    Bad = fresh("sf_bad_"),
    {ok, Bad, BBin, []} = compile_text(Bad, incompatible_source(Bad)),
    {module, Bad} = code:load_binary(Bad, "sf_bad", BBin),
    Secret = {secret, make_ref()},
    Frame = #{kind => sf_frame, protocol => 1, block => old_flow,
        step_name => wait, identity => {1, old_flow, wait}, module => Bad,
        vsn => {1, 0, 0}, mfa => {Bad, '$sf_old_flow__wait', 2},
        env => #{secret => Secret}},
    MR = async_r_t:new(identity),
    Result = identity:run(async_r_t:run(async_sf_runtime:continue(
        Frame, none, reply, MR, resume), async_t:state_callbacks_gs(
        #state.callbacks), make_ref(), #state{})),
    {{error, {sf_runtime, {incompatible_frame, Info}}}, _} = Result,
    ?assertEqual([current_vsn, identity, module, reason, stored_vsn],
        lists:sort(maps:keys(Info))),
    ?assertEqual(#{module => Bad, stored_vsn => {1, 0, 0},
        current_vsn => {2, 0, 0}, identity => {1, old_flow, wait},
        reason => no_compatible_plan}, Info),
    ?assertEqual(false, contains_term(Result, Secret)), unload(Bad).

downgrade_and_existing_boundary(_Config) ->
    M = fresh("sf_down_"), Ref = make_ref(),
    {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_down_a", A),
    {ok, State0} = start(M, M:action(Ref), downgrade),
    {ok, M, B, []} = compile_text(M, version_source(M, {2, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_down_b", B),
    {ok, M, A2, []} = compile_text(M, version_source(M, {1, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_down_a2", A2),
    ?assertEqual({ok, {stable, reply}}, info_reply({Ref, reply}, State0)), unload(M).

converter_env_and_precedence(_Config) ->
    M = fresh("sf_env_converter_"), Ref = make_ref(),
    {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, same_flow,
        wait, old, none)),
    {module, M} = code:load_binary(M, "sf_env_a", A),
    {ok, State0} = start(M, M:action(Ref), env_converter),
    {ok, M, B, _Warnings} = compile_text(M, same_identity_converter_source(M)),
    {module, M} = code:load_binary(M, "sf_env_b", B),
    ?assertEqual({ok, {migrated, {env_only, 0, reply}}},
        info_reply({Ref, reply}, State0)),
    ?assertEqual(1, get_counter(M, converter_calls)), unload(M),
    M2 = fresh("sf_precedence_failure_"), Ref2 = make_ref(),
    {ok, M2, A2, []} = compile_text(M2, version_source(M2, {1, 0, 0},
        same_flow, wait, old, none)),
    {module, M2} = code:load_binary(M2, "sf_precedence_failure_a", A2),
    {ok, State1} = start(M2, M2:action(Ref2), precedence_failure),
    {ok, M2, B2, _Warnings2} = compile_text(M2,
        same_identity_invalid_converter_source(M2)),
    {module, M2} = code:load_binary(M2, "sf_precedence_failure_b", B2),
    {Outcome, _Done} = info_reply_state({Ref2, reply}, State1),
    assert_migration_failure(Outcome, M2, invalid_converter_result,
        {1, same_flow, wait}), unload(M2).

converter_observable_matrix(_Config) ->
    M = fresh("sf_observable_converter_"), Ref = make_ref(),
    {ok, M, A, _Warnings} = compile_text(M, capture_converter_source(M, old)),
    {module, M} = code:load_binary(M, "sf_observable_a", A),
    {ok, State0} = start(M, M:action(Ref), observable),
    {ok, M, B, _Warnings2} = compile_text(M, capture_converter_source(M, current)),
    {module, M} = code:load_binary(M, "sf_observable_b", B),
    ?assertEqual({ok, {migrated, renamed_value, reply}},
        info_reply({Ref, reply}, State0)),
    ?assertEqual(1, get_counter(M, converter_calls)), unload(M).

converter_incompatible_shapes(_Config) ->
    lists:foreach(fun(Mode) ->
        M = fresh("sf_shape_converter_"), Ref = make_ref(),
        {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, old_flow,
            wait, old, none)),
        {module, M} = code:load_binary(M, "sf_shape_a", A),
        {ok, State0} = start(M, M:action(Ref), shape_failure),
        {ok, M, B, _Warnings} = compile_text(M, shape_converter_source(M, Mode)),
        {module, M} = code:load_binary(M, "sf_shape_b", B),
        {Outcome, _Done} = info_reply_state({Ref, reply}, State0),
        assert_migration_failure(Outcome, M, case Mode of
            bad_outcome -> incompatible_outcome;
            bad_env -> incompatible_env
        end), unload(M)
    end, [bad_outcome, bad_env]).

downgrade_created_in_b(_Config) ->
    M = fresh("sf_down_b_"), Ref = make_ref(),
    {ok, M, B, []} = compile_text(M, version_source(M, {2, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_down_b", B),
    {ok, State0} = start(M, M:action(Ref), downgrade_b),
    {ok, M, A, []} = compile_text(M, version_source(M, {1, 0, 0}, stable,
        wait, stable, none)),
    {module, M} = code:load_binary(M, "sf_down_a", A),
    ?assertEqual({ok, {stable, reply}}, info_reply({Ref, reply}, State0)),
    unload(M).

start(M, Action, Tag) -> async_m:run_sf_frame(Action, sf_cc(M, Tag),
    #state.callbacks, #state{}).
info_reply(Message, State) ->
    {Outcome, Done} = async_m:run_info(Message, #state.callbacks, State),
    ?assertEqual([], summaries(Done)), Outcome.
info_reply_state(Message, State) ->
    {Outcome, Done} = async_m:run_info(Message, #state.callbacks, State),
    {Outcome, Done}.
sf_cc(M, Tag) -> #{kind => sf_cc, protocol => 1,
    mfa => {M, complete, 2}, context => {self(), Tag}}.
summaries(State) -> {ok, S} = async_m:inspect_sf_callbacks(
    fun(_) -> true end, #state.callbacks, State), S.
get_counter(M, Key) -> M:counter(Key).
is_exported(M, F, A) -> erlang:function_exported(M, F, A).
contains_term(Term, Needle) when Term =:= Needle -> true;
contains_term(Term, Needle) when is_map(Term) -> lists:any(fun({K,V}) ->
    contains_term(K, Needle) orelse contains_term(V, Needle) end, maps:to_list(Term));
contains_term(Term, Needle) when is_tuple(Term) -> contains_term(tuple_to_list(Term), Needle);
contains_term(Term, Needle) when is_list(Term) -> lists:any(fun(E) -> contains_term(E, Needle) end, Term);
contains_term(_, _) -> false.
contains_any(Term, Tags) when is_map(Term) ->
    lists:any(fun(Tag) -> maps:is_key(Tag, Term) end, Tags) orelse
        lists:any(fun({K, V}) -> contains_any(K, Tags) orelse contains_any(V, Tags) end,
            maps:to_list(Term));
contains_any(Term, Tags) when is_tuple(Term) -> contains_any(tuple_to_list(Term), Tags);
contains_any(Term, Tags) when is_list(Term) -> lists:any(fun(E) -> contains_any(E, Tags) end, Term);
contains_any(Term, Tags) -> lists:member(Term, Tags).

declaration_source(M) -> version_source(M, {2, 0, 0}, current_flow, fetch,
    migrated, {old_flow, current_flow, wait, fetch}).
no_vsn_source(M) -> io_lib:format("-module(~p). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(_,_) -> sf_migrate(1, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>a,to=>current,steps=>#{wait=>#{to=>wait}}}).", [M]).
partial_source(M) -> io_lib:format("-module(~p). -vsn(1). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(_,_) -> sf_migrate(1, [async_m || wait], #{from=>a,to=>current,steps=>#{}}).", [M]).

version_source(M, Vsn, Block, Step, Label, Migration) ->
    Header = io_lib:format("-module(~p).~n-vsn(~p).~n-define(ASYNC_DO,true).~n-include_lib(\"async/include/async.hrl\").~n-export([action/1,complete/2,counter/1]).~n", [M, Vsn]),
    Body = io_lib:format("action(Ref) -> put({action_calls,~p}, 1 + case get({action_calls,~p}) of undefined -> 0; N -> N end), sf_do(~p, [async_m || ~p, Reply <- async_m:promise(Ref), return({~p, Reply})]).~ncomplete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).~ncounter(Key) -> get(Key).~n", [Block, Block, Block, Step, Label]),
    MigrationSource = case Migration of
        none -> [];
        {From, To, Old, New} -> io_lib:format("sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},~p} -> sf_migrate({1,0,0}, [async_m || ~p, OldReply <- async_m:promise(make_ref()), return({old, OldReply})], #{from=>~p,to=>~p,steps=>#{~p=>#{to=>~p}}}); _ -> unhandled end.~n", [From, Old, From, To, Old, New])
    end,
    [Header, Body, MigrationSource].

nonliteral_vsn_source(M) ->
    io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current_flow, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(V,B) -> case {V,B} of {1,old_flow} -> sf_migrate(1, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>old_flow,to=>current_flow,steps=>#{wait=>#{to=>wait}}}); _ -> unhandled end.", [M]).

multiple_vsn_source(M) ->
    io_lib:format("-module(~p). -vsn({3,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current_flow, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(V,B) -> case {V,B} of {{1,0,0},first_flow} -> sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>first_flow,to=>current_flow,steps=>#{wait=>#{to=>wait}}}); {{2,0,0},second_flow} -> sf_migrate({2,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>second_flow,to=>current_flow,steps=>#{wait=>#{to=>wait}}}); _ -> unhandled end.", [M]).

collision_source(M) ->
    Bridge = list_to_atom("$sf_migrate__" ++ integer_to_list(erlang:phash2({{1,0,0},old_flow,wait}, 16#100000000))),
    [version_source(M, {2,0,0}, current_flow, fetch, migrated,
        {old_flow, current_flow, wait, fetch}),
     io_lib:format("~p(_,_) -> collision.~n", [Bridge])].

converter_required_source(M) ->
    io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current_flow, [async_m || fetch, {ok, X} <- async_m:promise(Ref), return(X)]). sf_do_code_change(_,_) -> sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return({old, X})], #{from=>old_flow,to=>current_flow,steps=>#{wait=>#{to=>fetch}}}).", [M]).

converter_mode_source(M, Mode) ->
    Converter = io_lib:format("convert(Outcome, Env) -> put(converter_calls, 1), ~s.~n", [converter_return(Mode)]),
    [version_source(M, {2,0,0}, current_flow, fetch, migrated, none),
     "-export([convert/2]).\n",
     "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},old_flow} -> sf_migrate({1,0,0}, [async_m || wait, Old <- async_m:promise(make_ref()), return({old, Old})], #{from=>old_flow,to=>current_flow,steps=>#{wait=>#{to=>fetch,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
     Converter].

converter_return(error) -> "{error, {secret, sentinel}}";
converter_return(invalid) -> "invalid_converter_value".

converter_source(M) -> [version_source(M, {2, 0, 0}, current_flow, fetch,
    migrated, none),
    "-export([convert/2]).\n",
    "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},same_flow} -> sf_migrate({1,0,0}, [async_m || wait, Old <- async_m:promise(make_ref()), return({old, Old})], #{from=>same_flow,to=>current_flow,steps=>#{wait=>#{to=>fetch,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
    "convert(Outcome, Env) -> put(converter_calls, 1), {ok, {converted, Outcome}, Env}.\n"].

same_identity_converter_source(M) -> [version_source(M, {2, 0, 0}, same_flow, wait,
    migrated, none),
    "-export([convert/2]).\n",
    "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},same_flow} -> sf_migrate({1,0,0}, [async_m || wait, Old <- async_m:promise(make_ref()), return({old, Old})], #{from=>same_flow,to=>same_flow,steps=>#{wait=>#{to=>wait,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
    "convert(_Outcome, Env) -> put(converter_calls, 1), {ok, {env_only, maps:size(Env), reply}, #{}}.\n"].

same_identity_invalid_converter_source(M) -> [version_source(M, {2, 0, 0},
    same_flow, wait, migrated, none),
    "-export([convert/2]).\n",
    "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},same_flow} -> sf_migrate({1,0,0}, [async_m || wait, Old <- async_m:promise(make_ref()), return({old, Old})], #{from=>same_flow,to=>same_flow,steps=>#{wait=>#{to=>wait,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
    "convert(_Outcome, _Env) -> invalid_converter_value.\n"].

capture_converter_source(M, old) -> [
    version_header(M, {1, 0, 0}),
    "action(Ref) -> sf_do(old_flow, [async_m || OldCapture = old_value, wait, Reply <- async_m:promise(Ref), return(Reply)]).\n",
    "complete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).\n",
    "counter(Key) -> get(Key).\n"];
capture_converter_source(M, current) -> [
    version_header(M, {2, 0, 0}),
    "action(Ref) -> sf_do(current_flow, [async_m || RenamedCapture = unused, wait, Reply <- async_m:promise(Ref), return({migrated, RenamedCapture, Reply})]).\n",
    "complete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).\n",
    "counter(Key) -> get(Key).\n",
    "-export([convert/2]).\n",
    "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},old_flow} -> sf_migrate({1,0,0}, [async_m || OldCapture = old_value, wait, Reply <- async_m:promise(make_ref()), return(Reply)], #{from=>old_flow,to=>current_flow,steps=>#{wait=>#{to=>wait,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
    "convert(Outcome, Env) -> put(converter_calls, 1), {ok, Outcome, #{'RenamedCapture' => renamed_value, extra => {old_capture, maps:is_key('OldCapture', Env)}}}.\n"].

version_header(M, Vsn) -> io_lib:format("-module(~p).~n-vsn(~p).~n-define(ASYNC_DO,true).~n-include_lib(\"async/include/async.hrl\").~n-export([action/1,complete/2,counter/1]).~n", [M, Vsn]).

shape_converter_source(M, Mode) -> [shape_target_version_source(M),
    "-export([convert/2]).\n",
    "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},old_flow} -> sf_migrate({1,0,0}, [async_m || wait, Old <- async_m:promise(make_ref()), return({old, Old})], #{from=>old_flow,to=>current_flow,steps=>#{wait=>#{to=>fetch,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
    io_lib:format("convert(_Outcome, _Env) -> ~s.\n", [case Mode of
        bad_outcome -> "{ok, invalid, #{}}";
        bad_env -> "{ok, {migrated, reply}, invalid_env}"
    end])].

shape_target_version_source(M) -> [version_header(M, {2, 0, 0}),
    "action(Ref) -> sf_do(current_flow, [async_m || fetch, {ok, Reply} <- async_m:promise(Ref), return({migrated, Reply})]).\n",
    "complete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).\n",
    "counter(Key) -> get(Key).\n"].
incompatible_source(M) -> version_source(M, {2, 0, 0}, current_flow, other,
    bad, none).

assert_migration_failure(Outcome, M, Reason) ->
    assert_migration_failure(Outcome, M, Reason, {1, old_flow, wait}).

assert_migration_failure(Outcome, M, Reason, Identity) ->
    ?assertMatch({error, {sf_runtime, {incompatible_frame, _}}}, Outcome),
    {error, {sf_runtime, {incompatible_frame, Info}}} = Outcome,
    ?assertEqual([current_vsn, identity, module, reason, stored_vsn],
        lists:sort(maps:keys(Info))),
    ?assertEqual(#{module => M, stored_vsn => {1, 0, 0},
        current_vsn => {2, 0, 0}, identity => Identity,
        reason => Reason}, Info),
    ?assert(lists:member(Reason, [converter_failed, invalid_converter_result,
        incompatible_outcome, incompatible_env, no_compatible_plan,
        invalid_historical_frame])).

unnamed_source(M) -> io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([sf_do_code_change/2]). sf_do_code_change(_,_) -> sf_migrate({1,0,0}, [async_m || X <- async_m:promise(make_ref()), return(X)], #{from=>old_flow,to=>current_flow,steps=>#{}}).", [M]).
duplicate_source(M) -> io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(old_flow, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(V,B) -> case {V,B} of {{1,0,0},old_flow} -> sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>old_flow,to=>old_flow,steps=>#{wait=>#{to=>wait}}}); {{1,0,0},old_flow} -> sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>old_flow,to=>old_flow,steps=>#{wait=>#{to=>wait}}}); _ -> unhandled end.", [M]).
missing_block_source(M) -> io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([sf_do_code_change/2]). sf_do_code_change(_,_) -> sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>old_flow,to=>absent,steps=>#{wait=>#{to=>wait}}}).", [M]).
missing_step_source(M) -> io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current_flow, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(_,_) -> sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>old_flow,to=>current_flow,steps=>#{absent=>#{to=>wait}}}).", [M]).
placement_source(M) -> io_lib:format("-module(~p). -vsn({2,0,0}). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1]). action(Ref) -> begin sf_migrate({1,0,0}, [async_m || wait, X <- async_m:promise(Ref), return(X)], #{from=>old_flow,to=>current_flow,steps=>#{}}) end.", [M]).

compile_text(M, Source) ->
    Dir = filename:join(["_build", "test", "sf_migrate_fixtures"]),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    File = filename:join(Dir, atom_to_list(M) ++ ".erl"),
    ok = file:write_file(File, iolist_to_binary(Source)),
    compile:file(File, [binary, return_errors, return_warnings, debug_info]).
fresh(P) -> list_to_atom(P ++ integer_to_list(erlang:unique_integer([positive]))).
unload(M) -> code:purge(M), code:delete(M), ok.
flush() -> receive _ -> flush() after 0 -> ok end.

assert_completion(Tag, Outcome) ->
    receive {Tag, Outcome} -> ok after 1000 -> ct:fail({missing, Tag, Outcome}) end.
