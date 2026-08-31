%%% Public black-box nested sf-migrate coverage.
-module(async_sf_migrate_nested_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {callbacks = #{}, events = []}).

suite() -> [{timetrap, {seconds, 60}}].
all() -> [canonical_direct_ac, mixed_rollout_and_lifecycle].
init_per_suite(Config) -> async:start(), Config.
end_per_suite(_) -> flush(), ok.

canonical_direct_ac(_) ->
    {Order, Payment} = owners(),
    load_version(Order, 1, order, []),
    load_version(Payment, 1, payment, []),
    OrderRef = make_ref(), PaymentRef = make_ref(),
    {ok, State0} = async_m:run_sf_frame(
        Order:run({OrderRef, PaymentRef}), cc(Order, canonical),
        #state.callbacks, #state{}),
    Before = summaries(State0),
    ct:pal("nested summaries before migration: ~p", [Before]),
    ?assert(lists:any(fun(S) -> frame_matches(S, Payment, {1,0,0},
        payment_flow, payment_step) end, Before)),
    load_version(Order, 2, order, [1]),
    load_version(Payment, 2, payment, [1]),
    load_version(Order, 3, order, [1,2]),
    load_version(Payment, 3, payment, [1,2]),
    ?assertEqual(false, code:purge(Order)),
    ?assertEqual(false, code:purge(Payment)),
    {Outcome, State1} = info({PaymentRef, payment_reply}, State0),
    ?assertEqual({ok, {order, {3,0,0}, {ok, {payment, {3,0,0}, payment_reply,
        {resume, payment}}}, {resume, order}}}, Outcome),
    ?assertEqual([], summaries(State1)),
    assert_completion(canonical, Outcome),
    assert_no_completion(canonical),
    ?assertEqual(1, counter(Order, {action, order})),
    ?assertEqual(1, counter(Order, {step, order})),
    ?assertEqual(1, counter(Payment, {action, payment})),
    ?assertEqual(1, counter(Payment, {step, payment})),
    ?assertEqual(1, counter(Payment, {request, payment})),
    ?assertEqual(1, counter(Payment, {step, payment})),
    ?assertEqual(undefined, counter(Payment, {old_body, payment})),
    ?assertEqual(undefined, counter(Payment, {converter, payment})),
    ?assertEqual(undefined, counter(Order, {old_body, order})),
    ?assertEqual(undefined, counter(Order, {converter, order})),
    ?assertEqual(1, counter(Order, {resume, order})),
    ?assertEqual(1, counter(Payment, {resume, payment})),
    ?assertEqual({ok, unhandled}, async_m:run_info(
        {PaymentRef, late}, #state.callbacks, State1)),
    unload(Order), unload(Payment).

mixed_rollout_and_lifecycle(_) ->
    {Order, Payment} = owners(),
    load_version(Order, 1, order, []),
    load_version(Payment, 1, payment, []),
    OrderRef = make_ref(), PaymentRef = make_ref(),
    {ok, State0} = async_m:run_sf_frame(
        Order:run({OrderRef, PaymentRef}), cc(Order, mixed),
        #state.callbacks, #state{}),
    load_version(Order, 3, order, [1,2]),
    load_version(Payment, 2, payment, [1]),
    [#{frame := #{module := Payment, vsn := {1,0,0}}, parent_ref := Parent}] =
        [S || S <- summaries(State0), frame_matches(S, Payment, {1,0,0},
            payment_flow, payment_step)],
    ?assert(is_reference(Parent)),
    {Outcome, Done} = info({PaymentRef, mixed_reply}, State0),
    ?assertEqual({ok, {order, {3,0,0}, {ok, {payment, {2,0,0}, mixed_reply,
        {resume, payment}}}, {resume, order}}}, Outcome),
    ?assertEqual([], summaries(Done)),
    assert_completion(mixed, Outcome),
    assert_no_completion(mixed),
    ?assertEqual({ok, unhandled}, async_m:run_info(
        {PaymentRef, duplicate}, #state.callbacks, Done)),
    unload(Order), unload(Payment).

owners() -> {fresh("nested_order_"), async_sf_migrate_nested_fixture:payment_module()}.

load_version(Module, Version, Kind, Declarations) ->
    Source = case Kind of
        order -> async_sf_migrate_nested_fixture:source(Module, {Version,0,0},
            order, Declarations);
        payment -> async_sf_migrate_nested_fixture:source(Module, {Version,0,0},
            payment, Declarations)
    end,
    {ok, Module, Binary, _Warnings} = compile_text(Module, Source),
    {module, Module} = code:load_binary(Module,
        atom_to_list(Module) ++ integer_to_list(Version), Binary), ok.

compile_text(Module, Source) ->
    Dir = filename:join(["_build", "test", "sf_migrate_nested_fixtures"]),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    File = filename:join(Dir, atom_to_list(Module) ++ ".erl"),
    ok = file:write_file(File, Source),
    compile:file(File, [binary, return_errors, return_warnings, debug_info]).

cc(Module, Tag) -> #{kind => sf_cc, protocol => 1,
    mfa => {Module, complete, 2}, context => {self(), Tag}}.
info(Message, State) -> async_m:run_info(Message, #state.callbacks, State).
summaries(State) -> {ok, Result} = async_m:inspect_sf_callbacks(
    fun(_) -> true end, #state.callbacks, State), Result.
counter(Module, Key) -> Module:counter(Key).
frame_matches(#{frame := #{module := Module, vsn := Vsn, block := Block,
        step_name := Step}}, Module, Vsn, Block, Step) -> true;
frame_matches(_, _, _, _, _) -> false.
assert_completion(Tag, Outcome) ->
    receive {Tag, Outcome} -> ok after 1000 -> ct:fail({missing, Tag, Outcome}) end.
assert_no_completion(Tag) ->
    receive {Tag, _} = Message -> ct:fail({duplicate, Message})
    after 0 -> ok end.
unload(Module) -> code:purge(Module), code:delete(Module), ok.
flush() -> receive _ -> flush() after 0 -> ok end.
fresh(Prefix) -> list_to_atom(Prefix ++ integer_to_list(erlang:unique_integer([positive]))).
