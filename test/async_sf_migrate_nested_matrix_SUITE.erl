%%% Pairwise public nested migration matrix.  No private plan/bridge assertions.
-module(async_sf_migrate_nested_matrix_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [parent_converter_child_raw, parent_raw_child_converter,
    direct_ac_and_bc_pairings, omitted_automatic_step,
    mixed_unfinished_converter_parent, nested_failure_isolation_privacy].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_) -> flush(), ok.

parent_converter_child_raw(_) ->
    {O, P} = owners(), load(O, 1, order, none, []),
    load(P, 1, payment, none, []),
    {ok, S0} = start(O, P, parent_converter),
    load(O, 2, order, converter, [1]), load(P, 2, payment, none, [1]),
    {Outcome, S1} = reply(P, parent_converter, S0),
    ?assertMatch({ok, {order, {2,0,0}, {converted, _}, {resume, order}}}, Outcome),
    ?assertEqual([], summaries(S1)), ?assertEqual(1, counter(O, {converter, order})),
    ?assertEqual(undefined, counter(P, {converter, payment})), cleanup(O, P).

parent_raw_child_converter(_) ->
    {O, P} = owners(), load(O, 1, order, none, []),
    load(P, 1, payment, none, []),
    {ok, S0} = start(O, P, child_converter),
    load(O, 2, order, none, [1]), load(P, 2, payment, converter, [1]),
    {Outcome, S1} = reply(P, child_converter, S0),
    ?assertMatch({ok, {order, {2,0,0}, {ok, {payment, {2,0,0},
        {converted, _}, _}},
        {resume, order}}}, Outcome),
    ?assertEqual([], summaries(S1)), ?assertEqual(undefined, counter(O, {converter, order})),
    ?assertEqual(1, counter(P, {converter, payment})), cleanup(O, P).

direct_ac_and_bc_pairings(_) ->
    {O, P} = owners(), load(O, 1, order, none, []), load(P, 1, payment, none, []),
    {ok, S0} = start(O, P, direct_ac),
    load(P, 2, payment, none, [1]), load(P, 3, payment, none, [1,2]),
    load(O, 2, order, none, [1]), load(O, 3, order, none, [1,2]),
    {Outcome, Done} = reply(P, direct_ac, S0),
    ?assertMatch({ok, {order, {3,0,0}, {ok, {payment, {3,0,0}, _, _}}, _}}, Outcome),
    ?assertEqual([], summaries(Done)),
    ?assertEqual(2, counter(P, {route, {1, {3,0,0}}})),
    ?assertEqual(2, counter(O, {route, {1, {3,0,0}}})),
    ?assertEqual(undefined, counter(P, {route, {2, {3,0,0}}})), cleanup(O, P),
    {O2, P2} = owners(), load(O2, 1, order, none, []), load(P2, 1, payment, none, []),
    {ok, S2} = start(O2, P2, reverse_pair),
    load(O2, 2, order, none, [1]), load(O2, 3, order, none, [1,2]),
    load(P2, 3, payment, none, [1]),
    {Outcome2, Done2} = reply(P2, reverse_pair, S2),
    ?assertMatch({ok, {order, {3,0,0}, {ok, {payment, {3,0,0}, _, _}}, _}}, Outcome2),
    ?assertEqual([], summaries(Done2)),
    ?assertEqual(4, counter(O2, {route, {1, {3,0,0}}})),
    ?assertEqual(undefined, counter(O2, {route, {2, {3,0,0}}})),
    ?assertEqual(4, counter(P2, {route, {1, {3,0,0}}})), cleanup(O2, P2).

omitted_automatic_step(_) ->
    {O, P} = owners(), load(O, 1, order, none, []), load(P, 1, payment, none, []),
    {ok, S0} = start(O, P, omitted),
    %% B has no historical declaration: the unchanged nested identity uses the
    %% runtime's automatic-compatible fallback; there is no no-op mapping.
    load(P, 2, payment, none, []), load(O, 2, order, none, []),
    {Outcome, Done} = reply(P, omitted, S0),
    ?assertMatch({ok, {order, {2,0,0}, {ok, {payment, {2,0,0}, _, _}}, _}}, Outcome),
    ?assertEqual([], summaries(Done)), ?assertEqual(undefined, counter(P, {converter, payment})),
    cleanup(O, P).

mixed_unfinished_converter_parent(_) ->
    {O, P} = owners(), load(O, 1, order, none, []), load(P, 1, payment, none, []),
    {ok, S0} = start(O, P, unfinished),
    load(O, 3, order, converter, [1,2]), load(P, 2, payment, none, [1]),
    [#{frame := #{module := P, vsn := {1,0,0}}, parent_ref := Parent}] =
        [S || S <- summaries(S0), frame_matches(S, P, {1,0,0})],
    ?assert(is_reference(Parent)),
    {Outcome, Done} = reply(P, unfinished, S0),
    ?assertMatch({ok, {order, {3,0,0}, {converted, _}, {resume, order}}}, Outcome),
    ?assertEqual([], summaries(Done)),
    ?assertEqual(1, counter(O, {converter, order})), cleanup(O, P).

nested_failure_isolation_privacy(_) ->
    {O, P} = owners(), load(O, 1, order, none, []), load(P, 1, payment, none, []),
    {ok, S0} = start(O, P, failure),
    load(P, 2, payment, invalid, [1]),
    {Outcome, Done} = reply(P, failure, S0),
    ?assertMatch({error, {sf_runtime, {incompatible_frame,
        #{module := P, stored_vsn := {1,0,0}, current_vsn := {2,0,0},
          identity := {1,payment_flow,payment_step},
          reason := invalid_converter_result}}}}, Outcome),
    {error, {sf_runtime, {incompatible_frame, Info}}} = Outcome,
    ?assertEqual([current_vsn, identity, module, reason, stored_vsn],
        lists:sort(maps:keys(Info))),
    ?assertEqual(false, contains_any(Outcome,
        [env, outcome, state, callback, 'fun', secret, sentinel])),
    ?assertEqual([], summaries(Done)),
    ?assertEqual({ok, unhandled}, async_m:run_info({make_ref(), late},
        #state.callbacks, Done)), cleanup(O, P).

owners() -> {fresh("matrix_order_"), async_sf_migrate_nested_fixture:payment_module()}.

start(O, _P, Tag) ->
    Ref = make_ref(),
    async_m:run_sf_frame(O:run({Ref, make_ref()}), cc(O, Tag),
        #state.callbacks, #state{}).
reply(_P, Tag, State) ->
    {Outcome, Done} = async_m:run_info({last_ref(), Tag}, #state.callbacks, State),
    {Outcome, Done}.

%% The child reference is kept in the process dictionary only to keep each case
%% compact; it is set by start/3 through the generated owner source.
last_ref() -> get(matrix_payment_ref).

load(M, V, Kind, Converter, Declarations) ->
    Source = source(M, V, Kind, Converter, Declarations),
    {ok, M, Bin, _} = compile_text(M, Source),
    {module, M} = code:load_binary(M, atom_to_list(M) ++ integer_to_list(V), Bin), ok.

source(M, V, Kind, Converter, Declarations) ->
    Child = async_sf_migrate_nested_fixture:payment_module(),
    Body = case Kind of
        order -> io_lib:format("run({_OrderRef, PaymentRef}) -> put(matrix_payment_ref, PaymentRef), sf_do(order_flow, [async_m || order_step, Child <- ~p:run(PaymentRef), Marker <- async_m:pure_return(mark({resume, order})), return({order, version(), Child, Marker})]).~n", [Child]);
        payment -> "run(Ref) -> sf_do(payment_flow, [async_m || payment_step, Reply <- async_m:promise(Ref), Marker <- async_m:pure_return(mark({resume, payment})), return({payment, version(), Reply, Marker})]).\n"
    end,
    ConverterText = case Converter of
        converter -> io_lib:format("convert(Outcome, Env) -> mark({converter, ~p}), {ok, {converted, Outcome}, Env}.~n", [Kind]);
        invalid -> "convert(_Outcome, _Env) -> invalid_converter_result.\n";
        none -> []
    end,
    Header = io_lib:format("-module(~p). -vsn(~p). -define(ASYNC_DO,true). -include_lib(\"async/include/async.hrl\"). -compile({parse_transform, async_sf_transform}). -export([run/1,complete/2,counter/1,version/0,mark/1~s]).~n", [M, {V,0,0}, case Converter of none -> []; _ -> ",convert/2" end]),
    Common = "complete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)). counter(Key) -> get(Key). version() -> " ++ io_lib:format("~p", [{V,0,0}]) ++ ". mark(Key) -> put(Key, 1 + case get(Key) of undefined -> 0; N -> N end), Key.\n",
    Decl = declarations(Declarations, Kind, Converter, V),
    iolist_to_binary([Header, Body, Common, ConverterText, Decl]).

declarations([], _Kind, _Converter, _TargetVsn) -> [];
declarations(Vsns, Kind, Converter, TargetVsn) ->
    Step = case Kind of order -> order_step; payment -> payment_step end,
    Block = case Kind of order -> order_flow; payment -> payment_flow end,
    Target = case Converter of
        invalid -> io_lib:format("#{to=>~p,convert=>{?MODULE,convert,2}}", [Step]);
        converter -> io_lib:format("#{to=>~p,convert=>{?MODULE,convert,2}}", [Step]);
        none -> io_lib:format("#{to=>~p}", [Step])
    end,
    ["sf_do_code_change(V, B) -> case {V,B} of ",
        [io_lib:format("{{~p,0,0},~p} -> mark({route, {~p, {~p,0,0}}}), sf_migrate({~p,0,0}, [async_m || ~p, X <- async_m:promise(make_ref()), return(X)], #{from=>~p,to=>~p,steps=>#{~p=>~s}});", [Old, Block, Old, TargetVsn, Old, Step, Block, Block, Step, Target]) || Old <- Vsns],
        " _ -> unhandled end.\n"].

cc(M, Tag) -> #{kind => sf_cc, protocol => 1, mfa => {M, complete, 2}, context => {self(), Tag}}.
summaries(S) -> {ok, R} = async_m:inspect_sf_callbacks(fun(_) -> true end, #state.callbacks, S), R.
counter(M, K) -> M:counter(K).
frame_matches(#{frame := #{module := M, vsn := V}}, M, V) -> true;
frame_matches(_, _, _) -> false.
compile_text(M, Source) ->
    D = filename:join(["_build", "test", "sf_migrate_nested_matrix"]), ok = filelib:ensure_dir(filename:join(D, "x")),
    F = filename:join(D, atom_to_list(M) ++ ".erl"), ok = file:write_file(F, Source),
    compile:file(F, [binary, return_errors, return_warnings, debug_info]).
cleanup(O, P) -> code:purge(O), code:delete(O), code:purge(P), code:delete(P), ok.
contains_any(T, Ks) -> lists:any(fun(K) -> contains(T, K) end, Ks).
contains(T, K) when T =:= K -> true;
contains(T, K) when is_tuple(T) -> lists:any(fun(E) -> contains(E, K) end, tuple_to_list(T));
contains(T, K) when is_list(T) -> lists:any(fun(E) -> contains(E, K) end, T);
contains(T, K) when is_map(T) -> lists:any(fun(E) -> contains(E, K) end, maps:to_list(T));
contains(_, _) -> false.
flush() -> receive _ -> flush() after 0 -> ok end.
fresh(P) -> list_to_atom(P ++ integer_to_list(erlang:unique_integer([positive]))).
