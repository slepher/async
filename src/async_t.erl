%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_t).

-export_type([async_t/4, info_route/2, missing_callback/0]).

-type async_t(S, R, M, A) :: {async_t, inner_async_t(S, R, M, A)}.
-type inner_async_t(S, R, M, A) ::
    fun((fun((reply_t:reply(A)) -> async_r_t:async_r_t(S, M, R)), sf_context()) ->
        async_r_t:async_r_t(S, M, R)).
-type sf_context() :: undefined | #{
    current := {frame, map()} | {ref, reference()},
    parent_ref := reference() | none,
    runner := fun((term()) -> term())
}.
-type callback_or_cc(S, R, M, A) ::
    async_t_cc(S, R, M, A)
    | fun(() -> any())
    | fun((reply_t:reply(A)) -> any())
    | fun((reply_t:reply(A), S) -> any()).
-type async_t_cc(S, R, M, A) :: fun((reply_t:reply(A)) -> async_r_t:async_r_t(S, M, R)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).
-type missing_callback() :: #{
    type := message | reply,
    ref := term(),
    value := term()
}.
-type info_route(S, A) ::
    {handled, A, S}
    | {legacy_outcome, A, S}
    | {not_async, S}
    | {missing_callback, missing_callback(), S}.

-include_lib("erlando/include/do.hrl").
-include_lib("erlando/include/gen_fun.hrl").
-include_lib("erlando/include/erlando_instance.hrl").

-erlando_instance(
    #{
        type => {?MODULE, [async_t/4]},
        adapters =>
            [
                #{
                    mode => source,
                    requires => functor,
                    capabilities => [functor]
                },
                #{
                    mode => source,
                    requires => monad,
                    capabilities =>
                        [
                            applicative,
                            monad,
                            monad_trans,
                            monad_fail,
                            monad_error,
                            monad_cont,
                            monad_frame
                        ]
                }
            ]
    }
).

-include_lib("erlando/include/erlando.hrl").
-include("async_internal.hrl").

%% API
-export([new/1, async_t/1, run_async_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([bind_frame/4, then_frame/4]).
-export([lift/2]).
-export([lift_mr/2]).
-export([fail/2]).
-export([throw_error/2, catch_error/3]).
-export([callCC/2]).

-export([
    get_state/1,
    put_state/2,
    modify_state/2,
    find_ref/2,
    get_ref/3,
    put_ref/3,
    remove_ref/2,
    get_local/1,
    put_local/2,
    modify_local/2,
    local_ref/3,
    local/3,
    get_local_ref/1
]).
-export([
    lift_reply/2,
    lift_final_reply/2,
    pure_return/2,
    ok/1,
    wrapped_return/2,
    wrapped_lift_mr/2,
    message/2,
    add_message/2,
    hijack/2,
    pass/1,
    handle_message/3,
    handle_all/3
]).
-export([with_message/3, with_final/3, with_all/3, provide_message/3]).
-export([promise/2, promise/3, promise_sleep/2, map_promises/2, map_promises/3, par/2, progn_par/2]).
-export([
    wait/2,
    wait_t/3,
    update_cc/3,
    exec_cc/5,
    run_cc/3,
    run_with_cc/5,
    handle_info/4,
    route_info/4,
    run_info/4,
    handle_reply/5,
    run_reply/5,
    wait_receive/4,
    map_async/3,
    map_cont/3,
    callback_to_cc/2
]).
-export([state_callbacks_gs/1]).
-export([run_sf_cc/4, run_with_sf_cc/6, exec_sf_cc/6]).
%% -export([update_callbacks/3]).

-gen_fun(#{
    args => monad,
    sfunctions => [
        get_state/1,
        put_state/2,
        modify_state/2,
        find_ref/2,
        get_ref/3,
        put_ref/3,
        remove_ref/2,
        get_local/1,
        put_local/2,
        modify_local/2,
        local_ref/3,
        local/3,
        get_local_ref/1
    ]
}).

-gen_fun(#{
    args => monad,
    sfunctions => [
        lift_reply/2,
        lift_final_reply/2,
        pure_return/2,
        wrapped_return/2,
        message/2,
        hijack/2,
        pass/1,
        handle_message/3,
        provide_message/3
    ]
}).

-gen_fun(#{args => monad, sfunctions => [par/2, progn_par/2]}).

-gen_fun(#{
    args => monad,
    sfunctions => [
        wait/2,
        wait_t/3,
        exec_cc/5,
        run_cc/3,
        run_with_cc/5,
        handle_info/4,
        route_info/4,
        run_info/4,
        wait_receive/4,
        map_async/3,
        map_cont/3,
        callback_to_cc/2
    ]
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec new(M) -> TM when TM :: monad:class(), M :: monad:class().
new(M) ->
    {?MODULE, M}.

-spec async_t(inner_async_t(S, R, M, A)) -> async_t(S, R, M, A).
async_t(Inner) ->
    {?MODULE, Inner}.

-spec run_async_t(async_t(S, R, M, A)) -> inner_async_t(S, R, M, A).
run_async_t(#undetermined{} = UA) ->
    run_async_t(undetermined:run(UA, ?MODULE));
run_async_t({?MODULE, Inner}) ->
    Inner;
run_async_t(Other) ->
    exit({invalid_async_t, Other}).

-spec fmap(fun((A) -> B), async_t(S, R, M, A)) -> async_t(S, R, M, B).
fmap(F, ATA, {?MODULE, IM}) ->
    map_real(
        fun(RTA) ->
            functor:fmap(F, RTA, IM)
        end,
        ATA
    ).

'<$'(ATB, ATA, {?MODULE, _IM} = AT) ->
    functor:'default_<$'(ATB, ATA, AT).

pure(A, {?MODULE, IM}) ->
    RM = real_new(IM),
    real_to_async_t(applicative:pure(A, RM)).

'<*>'(ARTF, ARTA, {?MODULE, IM}) ->
    RM = real_new(IM),
    contextual_real_to_async_t(fun(SfContext) ->
        STF = async_to_real_t(ARTF, SfContext),
        STA = async_to_real_t(ARTA, SfContext),
        applicative:'<*>'(STF, STA, RM)
    end).

lift_a2(F, ARTA, ARTB, {?MODULE, _IM} = AT) ->
    applicative:default_lift_a2(F, ARTA, ARTB, AT).

'*>'(ARTA, ARTB, {?MODULE, _IM} = AT) ->
    applicative:'default_*>'(ARTA, ARTB, AT).

'<*'(ARTB, ARTA, {?MODULE, _IM} = AT) ->
    applicative:'default_<*'(ARTB, ARTA, AT).

-spec '>>='(async_t(S, R, M, A), fun((A) -> async_t(S, R, M, B))) -> async_t(S, R, M, B).
'>>='(ATA, KATB, {?MODULE, IM} = AT) ->
    RM = real_new(IM),
    with_materialized_context(fun(Runner, SfContext) ->
        KATB1 = fun(A) ->
            Next = case run_k(A, KATB) of
                {ok, Action} -> Action;
                unavailable -> async_t(fun(_K, _Context) ->
                    forward_unavailable_action(context_parent(SfContext),
                        {error, {sf_runtime, unavailable_continuation}}, async_r_t:new(IM))
                end)
            end,
            async_to_real_t(Next, SfContext)
        end,
        Real = monad:'>>='(async_to_real_t(ATA, SfContext), KATB1, RM),
        (run_real_t(Real))(Runner)
    end, AT).

'>>'(ATA, ATB, {?MODULE, _IM} = AT) ->
    monad:'default_>>'(ATA, ATB, AT).

bind_frame(Action, FrameMFA, Context, {?MODULE, _IM} = AT) ->
    async_t(
        fun(DownstreamRunner, SfContext) ->
            Frame = async_sf_runtime:new_frame(FrameMFA, Context),
            run_frame_action(Action, Frame, DownstreamRunner, SfContext, AT)
        end
    ).

then_frame(Action, IgnoreInputMFA, Context, {?MODULE, _IM} = AT) ->
    monad_frame:default_then_frame(Action, IgnoreInputMFA, Context, AT).

-spec return(A, M) -> async_t(_S, _R, M, A).
return(A, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(monad:return(A, MR), AT).

-spec fail(any(), t(M)) -> async_t(_S, _R, M, _A).
fail(E, {?MODULE, IM}) ->
    RM = real_new(IM),
    real_to_async_t(monad_fail:fail(E, RM)).

throw_error(E, {?MODULE, IM}) ->
    fail(E, {?MODULE, IM}).

catch_error(ATA, EATB, {?MODULE, IM} = AT) ->
    RM = real_new(IM),
    with_materialized_context(fun(Runner, SfContext) ->
        Real = monad_error:catch_error(
            async_to_real_t(ATA, SfContext),
            fun(E) -> async_to_real_t(EATB(E), SfContext) end,
            RM),
        (run_real_t(Real))(Runner)
    end, AT).

-spec lift(monad:m(M, A)) -> async_t(_S, _R, M, A).
lift(MA, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:lift(MA, MR), AT).

-spec callCC(fun((fun((A) -> async_t(S, R, M, _B))) -> async_t(S, R, M, A))) -> async_t(S, R, M, A).
callCC(F, {?MODULE, IM}) ->
    CT = cont_t:new(IM),
    RT = reply_t:new(CT),
    contextual_real_to_async_t(fun(SfContext) ->
        reply_t:lift(cont_t:callCC(
            fun(A) -> async_to_real_t(F(A), SfContext) end, CT), RT)
    end).

-spec lift_mr(async_r_t:async_r_t(S, M, A), M) -> async_t(S, _R, M, A).
lift_mr(MRA, {?MODULE, IM}) ->
    ART = async_r_t:new(IM),
    CT = cont_t:new(ART),
    RT = reply_t:new(CT),
    real_to_async_t(monad_trans:lift(monad_trans:lift(MRA, CT), RT)).

wrapped_lift_mr(MRA, {?MODULE, IM}) ->
    ART = async_r_t:new(IM),
    CT = cont_t:new(ART),
    RT = reply_t:new(CT),
    real_to_async_t(reply_t:lift_wrapped(monad_trans:lift(MRA, CT), RT)).

-spec get_state(M) -> async_t(S, _R, M, S).
get_state({?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:get_state(MR), AT).

-spec put_state(S, M) -> async_t(S, _R, M, ok).
put_state(State, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:put_state(State, MR), AT).

-spec modify_state(S, M) -> async_t(S, _R, M, ok).
modify_state(Fun, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:modify_state(Fun, MR), AT).

-spec get_local(M) -> async_t(_S, _R, M, _Local).
get_local({?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:get_local(MR), AT).

-spec put_local(_Local, M) -> async_t(_S, _R, M, ok).
put_local(Local, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:put_local(Local, MR), AT).

-spec modify_local(fun((Local) -> Local), M) -> async_t(_S, _R, M, ok).
modify_local(Fun, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:modify_local(Fun, MR), AT).

-spec local_ref(reference(), async_t(S, R, M, A), M) -> async_t(S, R, M, A).
local_ref(Ref, X, {?MODULE, _IM} = AT) ->
    local(fun(_) -> Ref end, X, AT).

-spec local(fun((T) -> T), async_t(S, R, M, A), M) -> async_t(S, R, M, A).
local(F, ATA, {?MODULE, IM}) ->
    MR = async_r_t:new(IM),
    M1 = cont_t:new(MR),
    Ask = fun() -> async_r_t:get_local_ref(MR) end,
    Local = fun(IF, IMR) -> async_r_t:local(IF, IMR, MR) end,
    map_real(
        fun(RTA) ->
            reply_t:map(
                fun(Cont) ->
                    monad_reader_instance:lift_local(Ask, Local, F, Cont, M1)
                end,
                RTA
            )
        end,
        ATA
    ).

-spec get_local_ref(M) -> async_t(_S, _R, M, reference()).
get_local_ref({?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:get_local_ref(MR), AT).

-spec find_ref(reference() | integer() | binary(), M) ->
    async_t(_S, _R, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:find_ref(MRef, MR), AT).

-spec get_ref(reference() | integer() | binary(), A, M) -> async_t(_S, _R, M, A).
get_ref(MRef, Default, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:get_ref(MRef, Default, MR), AT).

-spec put_ref(reference() | integer() | binary(), _A, M) -> async_t(_S, _R, M, ok).
put_ref(MRef, Value, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:put_ref(MRef, Value, MR), AT).

-spec remove_ref(reference() | integer() | binary(), M) -> async_t(_S, _R, M, ok).
remove_ref(MRef, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:remove_ref(MRef, MR), AT).

-spec lift_reply(async_t(S, R, M, A), M) -> async_t(S, R, M, reply_t:reply(A)).
lift_reply(ATA, {?MODULE, IM}) ->
    RT = real_new(IM),
    map_real(
        fun({reply_t, CTA}) ->
            reply_t:lift(CTA, RT)
        end,
        ATA
    ).

-spec lift_final_reply(async_t(S, R, M, A), M) -> async_t(S, R, M, reply_t:final_reply(A)).
lift_final_reply(ATA, {?MODULE, IM}) ->
    RT = real_new(IM),
    map_real(
        fun(RTA) ->
            reply_t:lift_final(RTA, RT)
        end,
        ATA
    ).

-spec pure_return(A, M) -> async_t(_S, _R, M, A).
pure_return(A, {?MODULE, IM}) ->
    RealM = real_new(IM),
    real_to_async_t(reply_t:pure_return(A, RealM)).

ok({?MODULE, _IM} = AT) ->
    pure_return(ok, AT).

wrapped_return(A, {?MODULE, IM}) ->
    RealM = real_new(IM),
    real_to_async_t(reply_t:wrapped_return(A, RealM)).

-spec message(A, M) -> async_t(_S, _R, M, A).
message(A, {?MODULE, _IM} = AT) ->
    pure_return({message, A}, AT).

add_message(A, {?MODULE, _IM} = AT) ->
    async_t:progn_par([async_t:message(A, AT), async_t:return(ok, AT)]).

-spec promise(any(), M) -> async_t(_S, _R, M, _A).
promise(MRef, {?MODULE, _IM} = Monad) ->
    promise(MRef, infinity, Monad).

-spec promise
    (any(), integer(), M) -> async_t(_S, _R, M, _A);
    (any(), infinity, M) -> async_t(_S, _R, M, _A).
promise(Action, Timeout, {?MODULE, IM} = _Monad) when is_function(Action, 0) ->
    MR = async_r_t:new(IM),
    async_t(fun(K, SfContext) ->
        case Action() of
            MRef when is_reference(MRef) or is_integer(MRef) or is_binary(MRef) ->
                do([
                    {async_r_t, IM}
                 || AccRef <- async_r_t:get_local_ref(MR),
                    begin
                        Entry = action_entry(K, SfContext, AccRef),
                        Registered = async_sf_cc:register_action(MRef, Entry, MR),
                        case Timeout of
                            infinity -> Registered;
                            _ -> monad:'>>'(Registered,
                                async_sf_cc:register_timer(MRef, Timeout, MR), MR)
                        end
                    end
                ]);
            Value ->
                K(Value)
        end
    end);
promise(MRef, Timeout, {?MODULE, _M} = Monad) when
    is_reference(MRef) or is_integer(MRef) or is_binary(MRef)
->
    promise(fun() -> MRef end, Timeout, Monad);
promise(Value, _Timeout, {?MODULE, _M} = Monad) ->
    pure_return(Value, Monad).

promise_sleep(Time, {?MODULE, _IM} = Monad) ->
    F = fun() ->
        Ref = make_ref(),
        erlang:send_after(Time, self(), {Ref, ok}),
        Ref
    end,
    promise(F, Monad).

-spec map_promises
    ([async_t(S, R, M, A)], M) -> async_t(S, R, M, [A]);
    (#{Key => async_t(S, R, M, A)}, M) -> async_t(S, R, M, #{Key => A}).
map_promises(Promises, {?MODULE, _M} = Monad) ->
    map_promises(Promises, #{}, Monad).

-spec map_promises(
    #{Key => async_t(S, R, M, A)},
    #{
        cc => fun((Key, A) -> async_t(S, R, M, _IM)),
        acc0 => Acc,
        limit => non_neg_integer()
    },
    M
) ->
    async_t(S, R, M, Acc).
map_promises(Promises, Options, {?MODULE, IM} = AT) when is_map(Promises) ->
    WRef = make_ref(),
    PRef = make_ref(),
    CRef = make_ref(),
    CC = maps:get(cc, Options, default_map_cc(AT)),
    Acc0 = maps:get(acc0, Options, maps:new()),
    Threads = validate_limit(maps:get(limit, Options, 0)),
    NPromises =
        maps:map(
            fun(Key, Promise) ->
                do([
                    AT
                 || lift_final_reply(
                        provide_message(
                            Promise,
                            fun(Val) ->
                                local_ref(CRef, CC(Key, Val), AT)
                            end,
                            AT
                        ),
                        AT
                    ),
                    Pending <- get_ref(PRef, maps:new(), AT),
                    NWorking <- get_ref(WRef, [], AT),
                    case maps:size(Pending) of
                        0 ->
                            do([
                                AT
                             || remove_ref(PRef, AT),
                                case lists:delete(Key, NWorking) of
                                    [] ->
                                        remove_ref(WRef, AT);
                                    NNWorking ->
                                        put_ref(WRef, NNWorking, AT)
                                end
                            ]);
                        _ ->
                            PKey = lists:nth(1, maps:keys(Pending)),
                            PendingPromise = maps:get(PKey, Pending),
                            NPending = maps:remove(PKey, Pending),
                            do([
                                AT
                             || case maps:size(NPending) of
                                    0 ->
                                        remove_ref(PRef, AT);
                                    _ ->
                                        put_ref(PRef, NPending, AT)
                                end,
                                put_ref(WRef, lists:delete(Key, [PKey | NWorking]), AT),
                                PendingPromise
                            ])
                    end
                ])
            end,
            Promises
        ),
    case maps:size(NPromises) of
        0 ->
            pure_return(Acc0, AT);
        _ ->
            {WPromiseKeys, PPromiseKeys} = split(Threads, maps:keys(NPromises)),
            Action = do([
                {?MODULE, IM}
             || put_ref(CRef, Acc0, AT),
                put_ref(PRef, maps:with(PPromiseKeys, NPromises), AT),
                put_ref(WRef, WPromiseKeys, AT),
                par_acc(CRef, maps:with(WPromiseKeys, NPromises), AT)
            ]),
            with_materialized_context(fun(Runner, SfContext) ->
                MR = async_r_t:new(IM),
                monad:'>>'(async_sf_cc:register_resources(
                    context_parent(SfContext), [CRef, PRef, WRef], MR),
                    raw_run_cc(Action, Runner, SfContext, AT), MR)
            end, AT)
    end;
map_promises(Promises, Options, {?MODULE, IM} = AT) when is_list(Promises) ->
    Promises1 = maps:from_list(lists:zip(lists:seq(1, length(Promises)), Promises)),
    do([
        {?MODULE, IM}
     || Value <- lift_reply(map_promises(Promises1, Options, AT), AT),
        case Value of
            {message, {_Key, Message}} ->
                message(Message, AT);
            _ ->
                pure_return(maps:values(Value), AT)
        end
    ]).

par_acc(CRef, Promises, AT) ->
    do([
        AT
     || par_acc_1(Promises, AT),
        Completed <- get_ref(CRef, maps:new(), AT),
        remove_ref(CRef, AT),
        pure_return(Completed, AT)
    ]).

par_acc_1(Promises, {?MODULE, IM} = AT) ->
    Len = maps:size(Promises),
    with_materialized_context(
        fun(CC, SfContext) ->
            case Len of
                0 ->
                    CC(ok);
                _ ->
                    Ref = make_ref(),
                    AsyncRT = {async_r_t, IM},
                    CC1 = fun
                        ({message, _M} = Message) ->
                            CC(Message);
                        (A) ->
                            do([
                                AsyncRT
                             || Acc <- async_r_t:get_ref(Ref, 0, AsyncRT),
                                Acc1 = Acc + 1,
                                case Acc1 of
                                    Len ->
                                        do([
                                            async_r_t
                                         || async_r_t:remove_ref(Ref, AsyncRT),
                                            CC(A)
                                        ]);
                                    _ ->
                                        async_r_t:put_ref(Ref, Acc1, AsyncRT)
                                end
                            ])
                    end,
                    monad:'>>'(async_sf_cc:register_resources(
                        context_parent(SfContext), [Ref], AsyncRT),
                        sequence_run_cc(maps:values(Promises), CC1, AsyncRT, SfContext, AT), AsyncRT)
            end
        end, AT
    ).

sequence_run_cc([], CC, _AsyncRT, _SfContext, _AT) ->
    CC(ok);
sequence_run_cc([Promise], CC, _AsyncRT, SfContext, AT) ->
    raw_run_cc(Promise, CC, SfContext, AT);
sequence_run_cc([Promise | T], CC, AsyncRT, SfContext, AT) ->
    do([
        AsyncRT
     || raw_run_cc(Promise, CC, SfContext, AT),
        sequence_run_cc(T, CC, AsyncRT, SfContext, AT)
    ]).

%par_acc(CRef, Promises, {?MODULE, _IM} = AT) ->
%   do([AT ||
%           par(Promises),
%           Completed <- get_ref(CRef, maps:new(), AT),
%           remove_ref(CRef, AT),
%           pure_return(Completed, AT)
%      ]).

%% provide extra message and return origin value
-spec provide_message(async_t(S, R, M, A), fun((A) -> async_t(S, R, M, A)), M) ->
    async_t(S, R, M, A).
provide_message(Promise, Then, {?MODULE, _IM} = AT) ->
    do([
        AT
     || Val <- lift_reply(Promise, AT),
        lift_final_reply(Then(Val), AT),
        case Val of
            {message, _Message} ->
                pass(AT);
            _ ->
                pure_return(Val, AT)
        end
    ]).

with_message(Promise, Then, {?MODULE, _IM} = AT) ->
    with_all(
        Promise,
        fun
            ({message, Message}) ->
                Then(Message);
            (Value) ->
                async_m:pure_return(Value)
        end,
        AT
    ).

with_final(Promise, Then, {?MODULE, _IM} = AT) ->
    async_t:'>>='(lift_final_reply(Promise, AT), Then, AT).

with_all(Promise, Then, {?MODULE, _IM} = AT) ->
    async_t:'>>='(
        lift_reply(Promise, AT),
        fun
            ({message, Message}) ->
                do([
                    AT
                 || Then({message, Message}),
                    pass(AT)
                ]);
            (Value) ->
                Then(Value)
        end
    ).

%% this is a dangerous function, only one should return A | {ok, A} | {error, E}
%% others should return {message, IM} or use pass()
%% or it will cause unexpected error
-spec par([async_t(S, R, M, A)], t(M)) -> async_t(S, R, M, [A]).
par(Promises, {?MODULE, _IM} = AT) ->
    with_materialized_context(
        fun(CC, SfContext) ->
            traversable:sequence(
                lists:map(fun(Promise) ->
                    raw_run_cc(Promise, CC, SfContext, AT)
                end, Promises))
        end, AT
    ).

%% acts like par, but only return last value of promises
%% the name of progn is from lisp
-spec progn_par([async_t(S, R, M, A)], t(M)) -> async_t(S, R, M, A).
progn_par([], {?MODULE, _IM}) ->
    exit(invalid_progn_list);
%% progn_par(Promises, {?MODULE, _IM} = AT) when is_list(Promises) ->
%%     do([AT ||
%%            Values <- par(Promises, AT),
%%            async_t:pure_return(lists:nth(length(Values), Values), AT)
%%        ]).
progn_par(Promises, {?MODULE, IM} = AT) when is_list(Promises) ->
    map_async(
        fun(MA) ->
            do([
                IM
             || {Values, S} <- MA,
                return({lists:nth(length(Values), Values), S})
            ])
        end,
        par(Promises, AT),
        AT
    ).

-spec handle_message(async_t(S, R, M, A), callback_or_cc(S, R, M, A), M) -> async_t(S, R, M, A).
handle_message(X, MessageHandler, {?MODULE, _IM} = AT) ->
    NMessageHandler = callback_to_cc(MessageHandler, AT),
    do([
        AT
     || Value <- lift_reply(X, AT),
        case Value of
            {message, Message} ->
                hijack(NMessageHandler(Message), AT);
            Reply ->
                pure_return(Reply, AT)
        end
    ]).

handle_all(X, Handler, {?MODULE, _IM} = AT) ->
    Handler1 = callback_to_cc(Handler, AT),
    do([
        AT
     || Value <- lift_reply(X, AT),
        case Value of
            {message, Message} ->
                hijack(Handler1({message, Message}), AT);
            Reply ->
                lift_mr(Handler1(Reply), AT)
        end
    ]).

-spec hijack(async_r_t:async_r_t(S, M, R), t(M)) -> async_t(S, R, M, _A).
hijack(MR, {?MODULE, _IM}) ->
    async_t(fun(_K, _SfContext) -> MR end).

-spec map_async(fun((monad:m(M, {A, S})) -> monad:m(M, {A, S})), async_t(R, S, M, A), t(M)) ->
    async_t(R, S, M, A).
map_async(F, ATA, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    NF = fun(AsyncR) ->
        async_r_t:map(F, AsyncR, MR)
    end,
    map_cont(NF, ATA, AT).

map_cont(F, ATA, {?MODULE, _IM}) ->
    map_real(
        fun(RTA) ->
            reply_t:map(fun(Cont) -> cont_t:map(F, Cont) end, RTA)
        end,
        ATA
    ).

-spec pass(t(M)) -> async_t(_S, ok, M, _A).
pass({?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    hijack(async_r_t:return(ok, MR), AT).

update_cc(X, CC, {?MODULE, _IM} = AT) ->
    KAsyncT =
        fun(A) ->
            wrapped_lift_mr(CC(A), AT)
        end,
    '>>='(lift_reply(X, AT), KAsyncT, AT).

exec_cc(X, CC, Offset, State, {?MODULE, IM} = AT) ->
    do([
        IM
     || {_A, NState} <- run_with_cc(X, CC, Offset, State, AT),
        return(NState)
    ]).

-spec run_with_cc(async_t(S, R, M, A), async_t_cc(S, R, M, A), integer(), S, t(M)) ->
    monad:m(M, {R, S}).
run_with_cc(X, CC, Offset, State, {?MODULE, _IM} = AT) ->
    CallbacksGS = state_callbacks_gs(Offset),
    Ref = make_ref(),
    MonadMR = run_cc(X, CC, AT),
    async_r_t:run(MonadMR, CallbacksGS, Ref, State).

-spec run_cc(async_t(S, R, M, A), async_t_cc(S, R, M, A), t(M)) -> async_r_t:async_r_t(S, M, A).
run_cc(X, CC, {?MODULE, _IM} = AT) ->
    run_with_descriptor(X, continuation_descriptor(CC), AT).

-spec run_sf_cc(async_t(S, R, M, A),
    fun((reply_t:reply(A), Context) -> async_r_t:async_r_t(S, M, R)),
    Context, t(M)) -> async_r_t:async_r_t(S, M, R).
run_sf_cc(X, Callback, Context, {?MODULE, _IM} = AT) ->
    run_with_descriptor(X, async_sf_cc:new(async_r_t, Callback, Context), AT).

-spec run_with_sf_cc(async_t(S, R, M, A),
    fun((reply_t:reply(A), Context) -> term())
    | fun((reply_t:reply(A), Context, S) -> S), Context, integer(), S, t(M)) ->
    monad:m(M, {R, S}).
run_with_sf_cc(X, Callback, Context, Offset, State, {?MODULE, _IM} = AT) ->
    Descriptor = async_sf_cc:new(direct_state, Callback, Context),
    MR = run_with_descriptor(X, Descriptor, AT),
    async_r_t:run(MR, state_callbacks_gs(Offset), make_ref(), State).

-spec exec_sf_cc(async_t(S, _R, M, A),
    fun((reply_t:reply(A), Context) -> term())
    | fun((reply_t:reply(A), Context, S) -> S), Context, integer(), S, t(M)) ->
    monad:m(M, S).
exec_sf_cc(X, Callback, Context, Offset, State, {?MODULE, IM} = AT) ->
    functor:fmap(fun({_Outcome, NewState}) -> NewState end,
        run_with_sf_cc(X, Callback, Context, Offset, State, AT), IM).

run_with_descriptor(X, Descriptor, {?MODULE, IM} = AT) ->
    OuterRef = make_ref(),
    MR = async_r_t:new(IM),
    Registered = async_sf_cc:register_node(OuterRef, Descriptor, MR),
    Runner = fun
        ({message, _} = Outcome) -> async_sf_cc:dispatch_message(OuterRef, Outcome, MR);
        (Outcome) -> async_sf_cc:dispatch(OuterRef, Outcome, MR)
    end,
    Context = #{current => {ref, OuterRef}, parent_ref => OuterRef, runner => Runner},
    Action = raw_run_cc(X, Runner, Context, AT),
    monad:'>>'(Registered, Action, MR).

continuation_descriptor(Callback) ->
    async_sf_cc:ordinary_outer_node(Callback).

run_frame_action(Action, Frame, Runner,
        #{runner := _ExistingRunner} = SfContext, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    do([
        MR
     || ParentRef <- materialize_parent(Runner, SfContext, MR),
        begin
            FrameRunner = fun(Input) ->
                async_sf_runtime:continue(Frame, ParentRef, Input, MR, resume)
            end,
            FrameContext = #{current => {frame, Frame}, parent_ref => ParentRef,
                runner => FrameRunner},
            Framed = raw_run_cc(Action, FrameRunner, FrameContext, AT),
            Framed
        end
    ]);
run_frame_action(_Action, _Frame, _Runner, undefined, _AT) ->
    erlang:error(sf_frame_requires_stable_runner).

wait(ATA, {?MODULE, _IM} = AT) ->
    wait_t(ATA, #{}, AT).

wait_t(X, Opts, {?MODULE, _IM} = AT) ->
    Callback = maps:get(callback, Opts, fun(A) -> A end),
    State = maps:get(state, Opts, {state, maps:new()}),
    Offset = maps:get(offset, Opts, 2),
    Timeout = maps:get(timeout, Opts, infinity),
    CC =
        case maps:find(cc, Opts) of
            {ok, Val} ->
                Val;
            error ->
                callback_to_cc(Callback, AT)
        end,
    MResult = run_with_cc(X, CC, Offset, State, AT),
    wait_mresult(MResult, Offset, State, Timeout, AT).

-spec wait_receive(integer(), _S, integer() | infinity, M) -> monad:m(M, _A).
wait_receive(Offset, State, Timeout, {?MODULE, _IM} = AT) ->
    receive
        {message, Ref, _Message} = Info when is_reference(Ref) ->
            MResult = run_info(Info, Offset, State, AT),
            wait_mresult(MResult, Offset, State, Timeout, AT);
        {Ref, _Reply} = Info when is_reference(Ref) ->
            MResult = run_info(Info, Offset, State, AT),
            wait_mresult(MResult, Offset, State, Timeout, AT);
        {'DOWN', Ref, _Pid, _Type, _Reason} = Info when is_reference(Ref) ->
            MResult = run_info(Info, Offset, State, AT),
            wait_mresult(MResult, Offset, State, Timeout, AT)
    after Timeout ->
        timeout_callbacks(Offset, State, AT)
    end.

timeout_callbacks(Offset, State, {?MODULE, IM} = AT) ->
    {CallbacksG, _CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    MResult = monad:return({ok, State}, IM),
    NMresult =
        lists:foldl(
            fun({MRef, Value}, MResultAcc) ->
                case async_util:decode_callback(Value) of
                    {ok, _} ->
                    Info = {MRef, {error, timeout}},
                    do([
                        IM
                     || {_A, StateAcc} <- MResultAcc,
                        run_info(Info, Offset, StateAcc, AT)
                    ]);
                    error -> MResultAcc
                end
            end,
            MResult,
            async_util:to_list(Callbacks)
        ),
    wait_mresult(NMresult, Offset, State, 0, AT).

wait_mresult(MResult, Offset, State, Timeout, {?MODULE, IM} = AT) ->
    do([
        IM
     || {A, NState} <- MResult,
        case NState of
            unhandled ->
                wait_receive(Offset, State, Timeout, AT);
            _ ->
                case async_util:same_type_state(NState, State) of
                    true ->
                        {CallbacksG, _CallbacksS} = state_callbacks_gs(Offset),
                        Callbacks = CallbacksG(NState),
                        case async_util:callback_exists(Callbacks) of
                            true ->
                                wait_receive(Offset, NState, Timeout, AT);
                            false ->
                                return(A)
                        end;
                    false ->
                        return(A)
                end
        end
    ]).

-spec handle_info(term(), integer(), S, t(M)) ->
    monad:m(M, S | unhandled | {legacy_outcome, term(), S}).
handle_info(Info, Offset, State, {?MODULE, IM} = AT) ->
    functor:fmap(fun
        ({handled, _Outcome, NewState}) -> NewState;
        ({legacy_outcome, _, _} = Legacy) -> Legacy;
        ({not_async, _}) -> unhandled;
        ({missing_callback, _, _}) -> unhandled
    end, route_info(Info, Offset, State, AT), IM).

-spec route_info(term(), integer(), S, t(M)) -> monad:m(M, info_route(S, term())).
route_info(Info, Offset, State, {?MODULE, IM} = AT) ->
    case info_to_reply(Info) of
        {ReplyType, Ref, Reply} ->
            Opts = #{offset => Offset, ref => monitored_reference, type => ReplyType},
            route_reply_1(Ref, Reply, State, Opts, AT);
        not_async ->
            monad:return({not_async, State}, IM)
    end.

-spec run_info(term(), integer(), S, t(M)) ->
    monad:m(M, {term(), S} | {ok, unhandled}).
run_info(Info, Offset, State, {?MODULE, IM} = AT) ->
    functor:fmap(fun compatibility_info_result/1,
        route_info(Info, Offset, State, AT), IM).

-spec handle_reply(term(), term(), S, map(), t(M)) ->
    monad:m(M, S | unhandled) when S :: tuple().
handle_reply(Ref, Reply, State, Opts, {?MODULE, IM} = AT) when is_map(Opts) ->
    functor:fmap(fun({_A, NState}) -> NState end, run_reply(Ref, Reply, State, Opts, AT), IM).

-spec run_reply(term(), term(), S, map(), t(M)) ->
    monad:m(M, {term(), S} | {ok, unhandled}).
run_reply(MRef, A, State, Opts, {?MODULE, _IM} = AT) ->
    NOpts = maps:merge(#{type => reply}, Opts),
    {?MODULE, IM} = AT,
    functor:fmap(fun compatibility_info_result/1,
        route_reply_1(MRef, A, State, NOpts, AT), IM).

route_reply_1(MRef, A, State, #{offset := Offset, type := ReplyType} = Opts,
        {?MODULE, IM}) ->
    {CallbacksG, CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    case handle_reference(MRef, Callbacks, Opts) of
        {ok, Decoded, NCallbacks} ->
            NState = CallbacksS(NCallbacks, State),
            Direction = maps:get(sf_direction, Opts, resume),
            ARTA = execute_callback_a(Decoded, ReplyType, A,
                {?MODULE, IM}, Direction),
            AccRef = maps:get(acc_ref, Decoded),
            MR = async_r_t:new(IM),
            Scoped = async_r_t:'>>='(ARTA,
                fun(Result) ->
                    do([
                        MR
                     || async_sf_cc:cleanup_accumulator(AccRef, MR),
                        async_r_t:return(Result, MR)
                    ])
                end, MR),
            functor:fmap(fun
                ({#async_legacy_delivery{outcome = Outcome}, NewState}) ->
                    {legacy_outcome, Outcome, NewState};
                ({Outcome, NewState}) -> {handled, Outcome, NewState}
            end, async_r_t:run(Scoped, {CallbacksG, CallbacksS}, AccRef,
                NState, MR), IM);
        missing_callback ->
            Missing = #{type => ReplyType, ref => MRef, value => A},
            monad:return({missing_callback, Missing, State}, IM);
        not_async ->
            monad:return({not_async, State}, IM)
    end.

compatibility_info_result({legacy_outcome, Outcome, State}) ->
    {{legacy_outcome, Outcome}, State};
compatibility_info_result({handled, Outcome, State}) -> {Outcome, State};
compatibility_info_result({not_async, _State}) -> {ok, unhandled};
compatibility_info_result({missing_callback, _Missing, _State}) -> {ok, unhandled}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
split(0, Keys) ->
    {Keys, []};
split(Threads, Keys) when length(Keys) >= Threads ->
    lists:split(Threads, Keys);
split(_Threads, Keys) ->
    {Keys, []}.

validate_limit(Limit) when is_integer(Limit), Limit >= 0 ->
    Limit;
validate_limit(Limit) ->
    exit({invalid_limit, Limit}).

real_new(M) ->
    reply_t:new(cont_t:new(async_r_t:new(M))).

real_t(Inner) ->
    reply_t:reply_t(cont_t:cont_t(Inner)).

run_real_t(Monad) ->
    cont_t:run_cont_t(reply_t:run_reply_t(Monad)).

async_to_real_t(Async, SfContext) ->
    Inner = run_async_t(Async),
    real_t(fun(K) -> Inner(K, SfContext) end).

real_to_async_t(Real) ->
    Inner = run_real_t(Real),
    async_t(fun(K, _SfContext) -> Inner(K) end).

contextual_real_to_async_t(BuildReal) ->
    async_t(fun(K, SfContext) ->
        Inner = run_real_t(BuildReal(SfContext)),
        Inner(K)
    end).

default_map_cc({?MODULE, IM} = AT) ->
    fun
        (Key, {message, Message}) ->
            message({Key, Message}, AT);
        (Key, Value) ->
            do([
                {?MODULE, IM}
             || Acc <- get_local(AT),
                put_local(maps:put(Key, Value, Acc), AT),
                pure_return(Value, AT)
            ])
    end.

callback_to_cc(Callback, {?MODULE, IM}) when is_function(Callback, 0) ->
    MR = async_r_t:new(IM),
    fun(A) ->
        case async_util:apply_available_fun(Callback, []) of
            unavailable -> async_r_t:return(#async_legacy_delivery{outcome = A}, MR);
            {ok, {async_r_t, _Inner} = NMonadMR} ->
                NMonadMR;
            {ok, Result} ->
                async_r_t:return(Result, MR)
        end
    end;
callback_to_cc(Callback, {?MODULE, M}) when is_function(Callback, 1) ->
    MR = async_r_t:new(M),
    fun(A) ->
        case async_util:apply_available_fun(Callback, [A]) of
            unavailable -> async_r_t:return(#async_legacy_delivery{outcome = A}, MR);
            {ok, {async_r_t, _Inner} = NMonadMR} ->
                NMonadMR;
            {ok, Result} ->
                async_r_t:return(Result, MR)
        end
    end;
callback_to_cc(Callback, {?MODULE, IM}) when is_function(Callback, 2) ->
    MR = async_r_t:new(IM),
    fun(A) ->
        do([
            {async_r_t, IM}
         || State <- async_r_t:get_state(MR),
            case async_util:apply_available_fun(Callback, [A, State]) of
                unavailable -> async_r_t:return(#async_legacy_delivery{outcome = A}, MR);
                {ok, {async_r_t, _Inner} = NMonadMR} ->
                    NMonadMR;
                {ok, NState} ->
                    case async_util:same_type_state(NState, State) of
                        true ->
                            do([
                                {async_r_t, IM}
                             || async_r_t:put_state(NState, MR),
                                async_r_t:return(NState, MR)
                            ]);
                        false ->
                            async_r_t:return(NState, MR)
                    end
            end
        ])
    end;
callback_to_cc(Callback, {?MODULE, _IM}) ->
    exit({invalid_callback, Callback}).

info_to_reply({message, MRef, Message}) ->
    {message, MRef, Message};
info_to_reply({MRef, Reply}) ->
    {reply, MRef, Reply};
info_to_reply({'DOWN', MRef, _, _, Reason}) ->
    {reply, MRef, {error, {process_down, Reason}}};
info_to_reply(_Info) ->
    not_async.

handle_reference(MRef, Callbacks, #{type := ReplyType} = Opts) ->
    case match_reference(MRef, Opts) of
        true ->
            case async_sf_cc:route_action(MRef, ReplyType, Callbacks) of
                {ok, Decoded, NCallbacks} ->
                    {ok, Decoded, NCallbacks};
                missing_callback -> missing_callback
            end;
        false ->
            not_async
    end.

match_reference(Ref, #{ref := monitored_reference}) when is_reference(Ref) ->
    true;
match_reference(Ref, #{ref := reference}) when is_reference(Ref) ->
    true;
match_reference(Ref, #{ref := integer}) when is_integer(Ref) ->
    true;
match_reference(Ref, #{ref := binary}) when is_binary(Ref) ->
    true;
match_reference(_, #{ref := all}) ->
    true;
match_reference(_, #{}) ->
    false.

execute_callback_a(#{kind := ordinary_action, cc := Callback,
        parent_ref := ParentRef}, message, A,
        {?MODULE, IM}, _Direction) ->
    MR = async_r_t:new(IM),
    case async_util:apply_available_fun(Callback, [{message, A}]) of
        {ok, {async_r_t, _} = Effect} -> Effect;
        {ok, Other} -> erlang:error({invalid_continuation_return, Other});
        unavailable ->
            forward_unavailable_action(ParentRef, A, MR)
    end;
execute_callback_a(#{kind := stable_action}, message, A,
        {?MODULE, IM}, _Direction) ->
    async_r_t:return({message, A}, async_r_t:new(IM));
execute_callback_a(#{kind := ordinary_action, cc := Callback,
        parent_ref := ParentRef}, reply, A, {?MODULE, IM}, _Direction) ->
    MR = async_r_t:new(IM),
    case async_util:apply_available_fun(Callback, [A]) of
        {ok, {async_r_t, _} = Effect} -> Effect;
        {ok, Other} -> erlang:error({invalid_continuation_return, Other});
        unavailable ->
            forward_unavailable_action(ParentRef, A, MR)
    end;
execute_callback_a(#{kind := stable_action, frame := Frame,
        parent_ref := ParentRef}, reply, A, {?MODULE, IM}, Direction) ->
    async_sf_runtime:continue(Frame, ParentRef, A, async_r_t:new(IM), Direction).

forward_unavailable_action(none, Outcome, MR) ->
    async_r_t:return(#async_legacy_delivery{outcome = Outcome}, MR);
forward_unavailable_action(ParentRef, _Outcome, MR) ->
    async_sf_cc:dispatch_failure(ParentRef,
        {error, {sf_runtime, unavailable_continuation}}, MR).

action_entry(K, #{current := {frame, Frame}, parent_ref := ParentRef,
        runner := K}, AccRef) ->
    #sf_callback{frame = Frame, acc_ref = AccRef,
        parent_ref = ParentRef};
action_entry(K, SfContext, AccRef) ->
    #callback{cc = K, acc_ref = AccRef, parent_ref = context_parent(SfContext)}.

state_callbacks_gs(Offset) ->
    {
        fun(State) -> element(Offset, State) end,
        fun(Callbacks, State) ->
            setelement(Offset, State, Callbacks)
        end
    }.

raw_run_cc(X, CC, SfContext, {?MODULE, _IM}) ->
    CTA = reply_t:run(async_to_real_t(X, SfContext)),
    cont_t:run(CTA, CC).

materialize_parent(Runner, #{runner := Runner, current := {ref, Ref}}, MR)
        when is_reference(Ref) ->
    async_r_t:return(Ref, MR);
materialize_parent(Runner, #{runner := Runner, current := {frame, Frame},
        parent_ref := ParentRef}, MR) ->
    register_materialized(async_sf_cc:frame_node(Frame, ParentRef), MR);
materialize_parent(Callback, SfContext, MR) ->
    register_materialized(async_sf_cc:ordinary_node(
        Callback, context_parent(SfContext)), MR).

register_materialized(Node, MR) ->
    Ref = make_ref(),
    monad:'>>'(async_sf_cc:register_node(Ref, Node, MR),
        async_r_t:return(Ref, MR), MR).

context_parent(#{parent_ref := ParentRef}) -> ParentRef;
context_parent(undefined) -> none.

map_real(F, ATA) ->
    contextual_real_to_async_t(fun(SfContext) ->
        F(async_to_real_t(ATA, SfContext))
    end).

%% Stabilize a frame before a combinator captures its runner or forks branches.
with_materialized_context(Build, {?MODULE, IM}) ->
    async_t(fun(Runner, SfContext) ->
        case SfContext of
            #{current := {frame, _}} ->
                MR = async_r_t:new(IM),
                monad:'>>='(materialize_parent(Runner, SfContext, MR),
                    fun(Ref) ->
                        StableRunner = fun
                            ({message, _} = Input) ->
                                async_sf_cc:dispatch_message(Ref, Input, MR);
                            (Input) -> async_sf_cc:dispatch(Ref, Input, MR)
                        end,
                        Build(StableRunner, #{current => {ref, Ref},
                            parent_ref => Ref, runner => StableRunner})
                    end, MR);
            _ -> Build(Runner, SfContext)
        end
    end).

%% erlando's monad:bind inserts a normalization closure before this adapter.
%% Unwrap only that known closure so availability is checked on the user fun,
%% never by catching badfun from inside an arbitrary user function body.
run_k(A, K) ->
    case {erlang:fun_info(K, module), erlang:fun_info(K, name),
            erlang:fun_info(K, env)} of
        {{module, monad}, {name, '->>=/3-fun-0-'}, {env, [Target, Monad]}}
                when is_function(Target, 1) ->
            case run_k(A, Target) of
                {ok, Action} -> {ok, undetermined:run(Action, Monad)};
                unavailable -> unavailable
            end;
        _ -> async_util:apply_available_fun(K, [A])
    end.
