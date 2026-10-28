-module(async_review_SUITE).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").
-include("async_internal.hrl").

all() -> [ordinary_purge, legacy_outer, mixed_purge, sandwich_failure,
    parallel_parent, mixed_message, business_exceptions, business_outcome].

ordinary_purge(_) ->
    Ref = make_ref(),
    State = start(async_review_a_fixture:ordinary(async_m:promise(Ref))),
    upgraded(async_review_a_fixture, fun() ->
        {handled, {error, {sf_runtime, unavailable_continuation}}, Clean} =
            async_m:route_info({Ref, reply}, 2, State),
        empty(Clean), completion({error, {sf_runtime, unavailable_continuation}})
    end).

legacy_outer(_) ->
    lists:foreach(fun(Kind) ->
        Ref = make_ref(),
        Action = apply(async_review_a_fixture, Kind, [async_m:promise(Ref)]),
        {ok, State} = async_m:run(Action, async_review_a_fixture:callback(),
            2, {state, #{}}),
        Expected = case Kind of
            ordinary -> {error, {sf_runtime, unavailable_continuation}};
            stable -> {ok, reply}
        end,
        upgraded(async_review_a_fixture, fun() ->
            {legacy_outcome, Expected, Clean} = async_m:handle_info({Ref, reply}, 2, State),
            empty(Clean),
            {legacy_outcome, Expected, Clean} = async_m:route_info({Ref, reply}, 2, State)
        end)
    end, [ordinary, stable]).

mixed_purge(_) ->
    lists:foreach(fun({Outer, Inner, Purged, Expected}) ->
        Ref = make_ref(),
        Child = apply(async_review_b_fixture, Inner, [async_m:promise(Ref)]),
        State = start(apply(async_review_a_fixture, Outer, [Child])),
        upgraded(Purged, fun() ->
            {handled, Expected, Clean} = async_m:route_info({Ref, reply}, 2, State),
            empty(Clean), completion(Expected)
        end)
    end, [{stable, ordinary, async_review_a_fixture, {ok, {ok, reply}}},
          {stable, ordinary, async_review_b_fixture, unavailable()},
          {ordinary, stable, async_review_b_fixture, {ok, reply}},
          {ordinary, stable, async_review_a_fixture, unavailable()}]).

sandwich_failure(_) ->
    lists:foreach(fun(Mode) ->
        Ref = make_ref(),
        Inner = async_review_c_fixture:stable(async_m:promise(Ref)),
        Middle = async_review_b_fixture:ordinary(Inner),
        %% A business error handler must not absorb structural failure.
        Caught = async_m:catch_error(Middle, fun(_) -> async_m:return(swallowed) end),
        State = start(async_review_a_fixture:stable(Caught)),
        #sf_callback{parent_ref = Ordinary} = maps:get(Ref, element(2, State)),
        #{kind := ordinary_continuation, parent_ref := Frame} =
            maps:get(Ordinary, element(2, State)),
        #sf_callback{} = maps:get(Frame, element(2, State)),
        case Mode of
            abort ->
                {ok, _, Clean} = async_m:fail_sf_callbacks(fun(_) -> true end, stop, 2, State),
                empty(Clean), completion({error, {sf_aborted, stop}});
            invalid_frame ->
                Entry = maps:get(Ref, element(2, State)),
                Broken = Entry#sf_callback{frame = (Entry#sf_callback.frame)#{protocol => 99}},
                State1 = setelement(2, State, maps:put(Ref, Broken, element(2, State))),
                {handled, Outcome, Clean} = async_m:route_info({Ref, reply}, 2, State1),
                ?assertMatch({error, {sf_runtime, _}}, Outcome),
                empty(Clean), completion(Outcome)
        end
    end, [abort, invalid_frame]).

parallel_parent(_) ->
    lists:foreach(fun(Combine) ->
        R1 = make_ref(), R2 = make_ref(),
        Children = [async_review_b_fixture:stable(async_m:promise(R)) || R <- [R1, R2]],
        State = start(async_review_a_fixture:stable(Combine(Children))),
        Frames = [F || {_, #sf_callback{acc_ref = undefined} = F} <- maps:to_list(element(2, State))],
        ?assertEqual(1, length(Frames)),
        {ok, _, Clean} = async_m:fail_sf_callbacks(fun(_) -> true end, stop, 2, State),
        empty(Clean), completion({error, {sf_aborted, stop}}),
        ?assertMatch({missing_callback, _, _}, async_m:route_info({R2, late}, 2, Clean))
    end, [fun async_m:par/1, fun async_m:map_promises/1]).

mixed_message(_) ->
    Ref = make_ref(),
    State = start(async_review_a_fixture:stable(
        async_review_b_fixture:ordinary(async_m:promise(Ref)))),
    {handled, {message, progress}, State} = async_m:route_info({message, Ref, progress}, 2, State),
    {handled, Outcome, Clean} = async_m:route_info({Ref, reply}, 2, State),
    empty(Clean), completion(Outcome).

business_outcome(_) ->
    Ref = make_ref(),
    {ok, State} = async_m:run_with_cc(async_m:promise(Ref),
        fun(_) -> async_r_m:return({legacy_outcome, business}) end, 2, {state, #{}}),
    ?assertEqual({handled, {legacy_outcome, business}, {state, #{}}},
        async_m:route_info({Ref, reply}, 2, State)),
    ?assertEqual({state, #{}}, async_m:handle_info({Ref, reply}, 2, State)).

business_exceptions(_) ->
    Ref = make_ref(),
    State = start(async_m:'>>='(async_m:promise(Ref),
        fun(_) -> Bad = invalid_fun(), Bad() end)),
    ?assertError({badfun, invalid}, async_m:route_info({Ref, reply}, 2, State)),
    OuterRef = make_ref(),
    {ok, OuterState} = async_m:run(async_m:promise(OuterRef),
        fun(_, _) -> Bad = invalid_fun(), Bad() end, 2, {state, #{}}),
    ?assertError({badfun, invalid}, async_m:route_info({OuterRef, reply}, 2, OuterState)),
    %% A live continuation may itself call another module's purged function.
    %% That is a business exception, even though it also carries a dead fun.
    Dead = async_review_b_fixture:callback(), OtherRef = make_ref(),
    OtherState = start(async_m:'>>='(async_m:promise(OtherRef),
        fun(_) -> Dead(reply, {state, #{}}) end)),
    upgraded(async_review_b_fixture, fun() ->
        ?assertError({badfun, Dead}, async_m:route_info({OtherRef, reply}, 2, OtherState))
    end).

start(Action) ->
    {_Pending, State} = async_m:run_with_sf_cc(Action, fun ?MODULE:complete/2,
        self(), 2, {state, #{}}),
    State.
complete(Outcome, Pid) -> Pid ! {completed, Outcome}.
empty(State) -> ?assertEqual(#{}, element(2, State)).
unavailable() -> {error, {sf_runtime, unavailable_continuation}}.
completion(Expected) ->
    receive {completed, Expected} -> ok after 1000 -> error(no_completion) end,
    receive {completed, Extra} -> error({duplicate_completion, Extra}) after 0 -> ok end.

upgraded(Module, Test) ->
    {Module, Binary, Path} = code:get_object_code(Module),
    {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(Binary, [abstract_code]),
    try
        lists:foreach(fun(Vsn) ->
            NewForms = [case F of {attribute, L, vsn, _} -> {attribute, L, vsn, Vsn}; _ -> F end || F <- Forms],
            {ok, Module, NewBinary} = compile:forms(NewForms, [binary, debug_info]),
            {module, Module} = code:load_binary(Module, Path, NewBinary),
            code:purge(Module)
        end, [2, 3]),
        Test()
    after
        code:purge(Module), code:delete(Module),
        {module, Module} = code:load_binary(Module, Path, Binary)
    end.

invalid_fun() -> invalid.
