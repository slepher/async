%%% Public black-box acceptance for sf-migrate declarations and reply-time bridges.
-module(async_sf_migrate_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [declaration_compiler_contract, raw_bridge_and_purge,
    converter_and_precedence, fallback_and_failure_privacy,
    downgrade_and_existing_boundary].
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
    ?assertEqual({ok, {migrated, reply}}, info_reply({Ref, reply}, State0)),
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
    ?assertMatch({{error, {sf_runtime, {incompatible_frame, _}}}, _}, Result),
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

start(M, Action, Tag) -> async_m:run_sf_frame(Action, sf_cc(M, Tag),
    #state.callbacks, #state{}).
info_reply(Message, State) ->
    {Outcome, Done} = async_m:run_info(Message, #state.callbacks, State),
    ?assertEqual([], summaries(Done)), Outcome.
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

declaration_source(M) -> version_source(M, {2, 0, 0}, current_flow, fetch,
    migrated, {old_flow, current_flow, wait, fetch}).
no_vsn_source(M) -> io_lib:format("-module(~p). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(_,_) -> sf_migrate(1, [async_m || wait, X <- async_m:promise(make_ref()), return(X)], #{from=>a,to=>current,steps=>#{wait=>#{to=>wait}}}).", [M]).
partial_source(M) -> io_lib:format("-module(~p). -vsn(1). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([action/1,sf_do_code_change/2]). action(Ref) -> sf_do(current, [async_m || wait, X <- async_m:promise(Ref), return(X)]). sf_do_code_change(_,_) -> sf_migrate(1, [async_m || wait], #{from=>a,to=>current,steps=>#{}}).", [M]).

version_source(M, Vsn, Block, Step, Label, Migration) ->
    Header = io_lib:format("-module(~p).~n-vsn(~p).~n-define(ASYNC_DO,true).~n-include_lib(\"async/include/async.hrl\").~n-export([action/1,complete/2,counter/1]).~n", [M, Vsn]),
    Body = io_lib:format("action(Ref) -> sf_do(~p, [async_m || ~p, Reply <- async_m:promise(Ref), return({~p, Reply})]).~ncomplete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).~ncounter(Key) -> get(Key).~n", [Block, Step, Label]),
    MigrationSource = case Migration of
        none -> [];
        {From, To, Old, New} -> io_lib:format("sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},~p} -> sf_migrate({1,0,0}, [async_m || ~p, OldReply <- async_m:promise(make_ref()), return({old, OldReply})], #{from=>~p,to=>~p,steps=>#{~p=>#{to=>~p}}}); _ -> unhandled end.~n", [From, Old, From, To, Old, New])
    end,
    [Header, Body, MigrationSource].

converter_source(M) -> [version_source(M, {2, 0, 0}, current_flow, fetch,
    migrated, none),
    "-export([convert/2]).\n",
    "sf_do_code_change(V, B) -> case {V,B} of {{1,0,0},same_flow} -> sf_migrate({1,0,0}, [async_m || wait, Old <- async_m:promise(make_ref()), return({old, Old})], #{from=>same_flow,to=>current_flow,steps=>#{wait=>#{to=>fetch,convert=>{?MODULE,convert,2}}}}); _ -> unhandled end.\n",
    "convert(Outcome, Env) -> put(converter_calls, 1), {ok, {converted, Outcome}, Env}.\n"].
incompatible_source(M) -> version_source(M, {2, 0, 0}, current_flow, other,
    bad, none).

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
