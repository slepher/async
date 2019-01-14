%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_t).

-erlando_type(?MODULE).

-export_type([async_t/4]).

-opaque async_t(S, R, M, A) :: {async_t, inner_async_t(S, R, M, A)}.
-type inner_async_t(S, R, M, A) :: 
        fun((fun((reply_t:reply(A)) -> async_r_t:async_r_t(S, M, R))) -> async_r_t:async_r_t(S, M, R)).
-type callback_or_cc(S, R, M, A) :: 
        async_t_cc(S, R, M, A) | 
        fun(() -> any()) | 
        fun((reply_t:reply(A)) -> any()) |
        fun((reply_t:reply(A), S) -> any()).
-type async_t_cc(S, R, M, A) :: fun((reply_t:reply(A)) -> async_r_t:async_r_t(S, M, R)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

-compile({parse_transform, do}).
-compile({parse_transform, function_generator}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_cont).

-include_lib("erlando/include/erlando.hrl").
-record(callback, {cc :: fun((A) -> async_r_t:async_r_t(any(), any(), monad:class(), A)),
                   acc_ref :: reference()}).

%% API
-export([new/1, async_t/1, run_async_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([lift/2]).
-export([lift_mr/2]).
-export([fail/2]).
-export([callCC/2]).

-export([get_state/1, put_state/2, modify_state/2, 
         find_ref/2, get_ref/3, put_ref/3, remove_ref/2, 
         get_local/1, put_local/2, modify_local/2, local_ref/3, local/3, get_local_ref/1]).
-export([lift_reply/2, lift_final_reply/2, pure_return/2, wrapped_return/2, wrapped_lift_mr/2,
         message/2, add_message/2, hijack/2, pass/1, handle_message/3, provide_message/3]).
-export([promise/2, promise_t/3, map_promises/2, map_promises_t/3, par/2, progn_par/2]).
-export([wait/2, wait_t/3, 
         update_cc/3, exec_cc/5, run_cc/3, run_with_cc/5, 
         handle_info/4, run_info/4, handle_reply/5, run_reply/5,
         wait_receive/4, map_async/3, map_cont/3, callback_to_cc/2]).
-export([state_callbacks_gs/1]).

-gen_fun(#{args => monad, 
             sfunctions => [get_state/1, put_state/2, modify_state/2, 
                            find_ref/2, get_ref/3, put_ref/3, remove_ref/2, 
                            get_local/1, put_local/2, modify_local/2, local_ref/3, local/3, get_local_ref/1]}).

-gen_fun(#{args => monad, 
             sfunctions => [lift_reply/2, lift_final_reply/2, pure_return/2, wrapped_return/2,
                            message/2, hijack/2, pass/1, handle_message/3, provide_message/3]}).

-gen_fun(#{args => monad, sfunctions => [promise/2, promise_t/3, map_promises/2, map_promises_t/3, par/2, progn_par/2]}).
-gen_fun(#{args => monad, 
             sfunctions => [wait/2, wait_t/3, exec/5, exec_cc/5, run/5, run_cc/3, run_with_cc/5, 
                            handle_info/4, run_info/4, wait_receive/4, map_async/3, map_cont/3, callback_to_cc/2]}).

-gen_fun(#{inner_type => functor, behaviours => [functor]}).
-gen_fun(#{inner_type => monad, behaviours => [applicative, monad, monad_trans, monad_fail, monad_cont]}).

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
      end, ATA).

'<$'(ATB, ATA, {?MODULE, _IM} = AT) ->
    functor:'default_<$'(ATB, ATA, AT).

pure(A, {?MODULE, IM}) ->
    RM = real_new(IM),
    real_to_async_t(applicative:pure(A, RM)).

'<*>'(ARTF, ARTA, {?MODULE, IM}) ->
    STF = async_to_real_t(ARTF),
    STA = async_to_real_t(ARTA),
    RM = real_new(IM),
    real_to_async_t(applicative:'<*>'(STF, STA, RM)).

lift_a2(F, ARTA, ARTB, {?MODULE, _IM} = AT) ->
    applicative:default_lift_a2(F, ARTA, ARTB, AT).

'*>'(ARTA, ARTB, {?MODULE, _IM} = AT) ->
    applicative:'default_*>'(ARTA, ARTB, AT).

'<*'(ARTB, ARTA, {?MODULE, _IM} = AT) ->
    applicative:'default_<*'(ARTB, ARTA, AT).

-spec '>>='(async_t(S, R, M, A), fun( (A) -> async_t(S, R, M, B) )) -> async_t(S, R, M, B).
'>>='(ATA, KATB, {?MODULE, IM}) ->
    RM = real_new(IM),
    real_to_async_t(monad:'>>='(async_to_real_t(ATA), fun(A) -> async_to_real_t(KATB(A)) end, RM)).

'>>'(ATA, ATB, {?MODULE, _IM} = AT) ->
    monad:'default_>>'(ATA, ATB, AT).

-spec return(A, M) -> async_t(_S, _R, M, A).
return(A, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(monad:return(A, MR) , AT).

-spec fail(any(), t(M)) -> async_t(_S, _R, M, _A).
fail(E, {?MODULE, IM}) ->
    RM = real_new(IM),
    real_to_async_t(monad_fail:fail(E, RM)).

-spec lift(monad:m(M, A)) -> async_t(_S, _R, M, A).
lift(MA, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:lift(MA, MR), AT).

-spec callCC(fun((fun( (A) -> async_t(S, R, M, _B) ))-> async_t(S, R, M, A))) -> async_t(S, R, M, A).
callCC(F, {?MODULE, IM}) ->
    CT = cont_t:new(IM),
    RT = reply_t:new(CT),
    real_to_async_t(reply_t:lift(cont_t:callCC(fun(A) -> async_to_real_t(F(A)) end, CT), RT)).

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
                end, RTA)
      end, ATA).

-spec get_local_ref(M) -> async_t(_S, _R, M, reference()).
get_local_ref({?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:get_local_ref(MR), AT).

-spec find_ref(reference(), M) -> async_t(_S, _R, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:find_ref(MRef, MR), AT).

-spec get_ref(reference(), A, M) -> async_t(_S, _R, M, A).
get_ref(MRef, Default, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:get_ref(MRef, Default, MR), AT).

-spec put_ref(reference(), _A, M) -> async_t(_S, _R, M, ok).
put_ref(MRef, Value, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:put_ref(MRef, Value, MR), AT).

-spec remove_ref(reference(), M) -> async_t(_S, _R, M, ok).
remove_ref(MRef, {?MODULE, IM} = AT) ->
    MR = async_r_t:new(IM),
    lift_mr(async_r_t:remove_ref(MRef, MR), AT).

-spec lift_reply(async_t(S, R, M, A), M) -> async_t(S, R, M, reply_t:reply(A)).
lift_reply(ATA, {?MODULE, IM}) ->
    RT = real_new(IM),
    map_real(
      fun({reply_t, CTA}) ->
              reply_t:lift(CTA, RT)
      end, ATA).

-spec lift_final_reply(async_t(S, R, M, A), M) -> async_t(S, R, M, reply_t:final_reply(A)).
lift_final_reply(ATA, {?MODULE, IM}) ->
    RT = real_new(IM),
    map_real(
      fun(RTA) ->
              reply_t:lift_final(RTA, RT)
      end, ATA).

-spec pure_return(A, M) -> async_t(_S, _R, M, A).
pure_return(A, {?MODULE, IM}) ->
    RealM = real_new(IM),
    real_to_async_t(reply_t:pure_return(A, RealM)).

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
    promise_t(MRef, infinity, Monad).

-spec promise_t(any(), integer(), M) -> async_t(_S, _R, M, _A);
             (any(), infinity, M) -> async_t(_S, _R, M, _A).
promise_t(Action, Timeout, {?MODULE, IM} = Monad) when is_function(Action, 0)->
    MR = async_r_t:new(IM),
    async_t(fun(K) ->
                 case Action() of
                     MRef when is_reference(MRef) or is_integer(MRef) or is_binary(MRef) ->
                         do([{async_r_t, IM} || 
                                AccRef <- async_r_t:get_local_ref(MR),
                                begin 
                                    NK = callback_with_timeout(K, MRef, Timeout, Monad),
                                    async_r_t:put_ref(MRef, #callback{cc = NK, acc_ref = AccRef}, MR)
                                end
                            ]);
                     Value ->
                         K(Value)
                 end
         end);
promise_t(MRef, Timeout, {?MODULE, _M} = Monad) when is_reference(MRef) or is_integer(MRef) or is_binary(MRef) ->
    promise_t(fun() -> MRef end, Timeout, Monad);
promise_t(Value, _Timeout, {?MODULE, _M} = Monad) ->
    pure_return(Value, Monad).

-spec map_promises([async_t(S, R, M, A)], M) -> async_t(S, R, M, [A]);
         (#{Key => async_t(S, R, M, A)}, M) -> async_t(S, R, M, #{Key => A}).
map_promises(Promises, {?MODULE, IM} = AT) when is_list(Promises) ->
    NPromises = maps:from_list(lists:zip(lists:seq(1, length(Promises)), Promises)),
    do([{?MODULE, IM} || 
           Value <- lift_reply(map_promises(NPromises, AT), AT),
           case Value of
               {message, {_Key, Message}} ->
                   message(Message, AT);
               _ ->
                  pure_return(maps:values(Value), AT)
           end
       ]);
map_promises(Promises, {?MODULE, _M} = Monad) when is_map(Promises) ->
    map_promises_t(Promises, #{}, Monad).

-spec map_promises_t(#{Key => async_t(S, R, M, A)}, 
          #{cc => fun((Key, A) -> async_r_t:async_r_t(S, M, _IM)),
            acc0 => Acc, limit => integer()}, M) -> 
                 async_t(S, R, M, Acc).
map_promises_t(Promises, Options, {?MODULE, IM} = AT) ->
    WRef = make_ref(),
    PRef = make_ref(),
    CRef = make_ref(),
    CC = maps:get(cc, Options, default_map_cc(AT)),
    Acc0 = maps:get(acc0, Options, maps:new()),
    Threads = maps:get(limit, Options, 0),
    NPromises = 
        maps:map(
          fun(Key, Promise) ->
                  do([AT ||
                         Working <- get_ref(WRef, [], AT),
                         put_ref(WRef, [Key|Working], AT),
                         lift_final_reply(
                           provide_message(
                             Promise,
                             fun(Val) ->
                                     local_ref(CRef, CC(Key, Val), AT)
                             end, AT), AT),
                         Pending <- get_ref(PRef, maps:new(), AT),
                         NWorking <- get_ref(WRef, [], AT),
                         case maps:size(Pending) of
                             0 ->
                                 case lists:delete(Key, NWorking) of
                                     [] ->
                                         do([{?MODULE, IM} ||
                                                Completed <- get_ref(CRef, maps:new(), AT),
                                                remove_ref(WRef, AT),
                                                remove_ref(CRef, AT),
                                                pure_return(Completed, AT)
                                            ]);
                                     NNWorking ->
                                         do([{?MODULE, IM} ||
                                                put_ref(WRef, NNWorking, AT),
                                                pass(AT)
                                            ])
                                 end;
                             _ ->
                                 PKey = lists:nth(1, maps:keys(Pending)), 
                                 PendingPromise = maps:get(PKey, Pending, undefined),
                                 NPending = maps:remove(PKey, Pending),
                                 do([{?MODULE, IM} ||
                                        put_ref(PRef, NPending, AT),
                                        put_ref(WRef, lists:delete(Key, NWorking), AT),
                                        PendingPromise
                                    ])
                         end
                     ])
          end, Promises),
    {WPromiseKeys, PPromiseKeys} = split(Threads, maps:keys(NPromises)),
    do([{?MODULE, IM} ||
           put_ref(CRef, Acc0, AT),
           put_ref(PRef, maps:with(PPromiseKeys, NPromises), AT),
           par(maps:values(maps:with(WPromiseKeys, NPromises)), AT)
       ]).

%% provide extra message and return origin value
-spec provide_message(async_t(S, R, M, A), fun((A) -> async_t(S, R, M, A)), M) -> async_t(S, R, M, A).
provide_message(Promise, Then, {?MODULE, _IM} = AT) ->
    do([AT ||
           Val <- lift_reply(Promise, AT),
           progn_par(
             [% this will only return messages and ignore all normal reply returned in then
              do([AT || 
                     lift_final_reply(Then(Val), AT),
                     pass(AT)
                 ]),
              % this will only return normal reply and ignore messages in promise
              case Val of
                  {message, _Message} ->
                      pass(AT);
                  _ ->
                      pure_return(Val, AT)
              end
             ], AT)
      ]).

%% this is a dangerous function, only one should return A | {ok, A} | {error, E}
%% others should return {message, IM} or use pass()
%% or it will cause unexpected error
-spec par([async_t(S, R, M, A)], t(M)) -> async_t(S, R, M, [A]).
par(Promises, {?MODULE, _IM} = AT) ->
    async_t(
      fun(CC) ->
              traversable:sequence(
                lists:map(fun(Promise) -> run_cc(Promise, CC, AT) end, Promises))
      end).

%% acts like par, but only return last value of promises
%% the name of progn is from lisp
-spec progn_par([async_t(S, R, M, A)], t(M)) -> async_t(S, R, M, A).
progn_par([], {?MODULE, _IM}) ->
    exit(invalid_progn_list);
progn_par(Promises, {?MODULE, IM} = AT) when is_list(Promises) ->
    map_async(
      fun(MA) -> 
              do([IM ||
                     {Values, S} <- MA,
                     return({lists:nth(length(Values), Values), S})
                 ])
      end, 
      par(Promises, AT), AT).

-spec handle_message(async_t(S, R, M, A), callback_or_cc(S, R, M, A), M) -> async_t(S, R, M, A).
handle_message(X, MessageHandler, {?MODULE, _IM} = AT) ->
    NMessageHandler = callback_to_cc(MessageHandler, AT),
    do([AT ||
           Value <- lift_reply(X, AT),
           case Value of               
               {message, Message} ->
                   hijack(NMessageHandler(Message), AT);
               Reply ->
                   pure_return(Reply, AT)
           end
       ]).

-spec hijack(async_r_t:async_r_t(S, M, R), t(M)) -> async_t(S, R, M, _A).
hijack(MR, {?MODULE, _IM}) ->
    async_t(fun(_K) -> MR end).

-spec map_async(fun((monad:m(M, {A, S})) -> monad:m(M, {A, S})), async_t(R, S, M, A), t(M)) -> async_t(R, S, M, A).
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
      end, ATA).

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
    do([IM ||
           {_A, NState} <- run_with_cc(X, CC, Offset, State, AT),
           return(NState)
       ]).

-spec run_with_cc(async_t(S, R, M, A), async_t_cc(S, R, M, A), integer(), S, _MT) -> S.
run_with_cc(X, CC, Offset, State, {?MODULE, _IM} = AT) ->
    CallbacksGS = state_callbacks_gs(Offset),
    Ref = make_ref(),
    NCC = remove_ref_after_cc(Ref, CC, AT),
    MonadMR = run_cc(X, NCC, AT),
    async_r_t:run(MonadMR, CallbacksGS, Ref, State).

-spec run_cc(async_t(S, R, M, A), async_t_cc(S, R, M, A), t(M)) -> async_r_t:async_r_t(S, M, A).
run_cc(X, CC, {?MODULE, _IM}) ->
    CTA = reply_t:run(async_to_real_t(X)),
    cont_t:run(CTA, CC).

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
        Info ->
            case run_info(Info, Offset, State, AT) of
                unhandled ->
                    wait_receive(Offset, State, Timeout, AT);
                MResult ->
                    wait_mresult(MResult, Offset, State, Timeout, AT)
            end
    after Timeout ->
            timeout_callbacks(Offset, State, AT)
    end.

timeout_callbacks(Offset, State, {?MODULE, IM} = AT) ->
    {CallbacksG, _CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    MResult = monad:return({ok, State}, IM),
    NMresult = 
        maps:fold(
          fun(MRef, #callback{}, MResultAcc) ->
                  Info = {MRef, {error, timeout}},
                  do([IM ||
                         {_A, StateAcc} <- MResultAcc,
                         run_info(Info, Offset, StateAcc, AT)
                     ]);
             (_MRef, _Other, Acc) ->
                  Acc
          end, MResult, Callbacks),
    wait_mresult(NMresult, Offset, State, 0, AT).

wait_mresult(MResult, Offset, State, Timeout, {?MODULE, IM} = AT) ->
    do([IM ||
           {A, NState} <- MResult,
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
       ]).

-spec handle_info(_Info, integer(), S, M) -> monad:m(M, S).
handle_info(Info, Offset, State, {?MODULE, IM} = AT) ->
    functor:fmap(fun({_A, NState}) -> NState end, run_info(Info, Offset, State, AT), IM).

run_info(Info, Offset, State, {?MODULE, IM} = AT) ->
    case info_to_reply(Info) of
        {ReplyType, Ref, Reply} ->
            Opts = #{offset => Offset, ref => monitored_reference, type => ReplyType},
            run_reply(Ref, Reply, State, Opts, AT);
        unhandled ->
            %% A is ok, state is unhandled
            monad:return({ok, unhandled}, IM)
    end.

-spec handle_reply(any(), _A, integer(), S, M) -> monad:m(M, S).
handle_reply(Ref, Reply, State, Opts, {?MODULE, IM} = AT) when is_map(Opts) ->
    functor:fmap(fun({_A, NState}) -> NState end, run_reply(Ref, Reply, State, Opts, AT), IM).

-spec run_reply(any(), any(), S, map(), M) -> monad:m(M, {_A, S}).
run_reply(MRef, A, State, Opts, {?MODULE, _IM} = AT) ->
    NOpts = maps:merge(#{type => reply}, Opts),
    run_reply_1(MRef, A, State, NOpts, AT).

run_reply_1(MRef, A, State, #{offset := Offset, type := ReplyType} = Opts, {?MODULE, IM}) ->
    {CallbacksG, CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    case handle_reference(MRef, Callbacks, Opts) of
        {ok, Callback, AccRef, NCallbacks} ->
            NState = CallbacksS(NCallbacks, State),
            ARTA = execute_callback_a(Callback, ReplyType, A),
            async_r_t:run(ARTA, {CallbacksG, CallbacksS}, AccRef, NState);
        error ->
            %% A is ok, state is unhandled
            monad:return({ok, unhandled}, IM)
    end.

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

real_new(M) ->
    reply_t:new(cont_t:new(async_r_t:new(M))).

real_t(Inner) ->
    reply_t:reply_t(cont_t:cont_t(Inner)).

run_real_t(Monad) ->
    cont_t:run_cont_t(reply_t:run_reply_t(Monad)).

async_to_real_t(Async) ->
    real_t(run_async_t(Async)).

real_to_async_t(Real) ->
    async_t(run_real_t(Real)).

default_map_cc({?MODULE, IM} = AT) ->
    fun(Key, {message, Message}) ->
            message({Key, Message}, AT);
       (Key, Value) ->
            do([{?MODULE, IM} ||
                   Acc <- get_local(AT),
                   put_local(maps:put(Key, Value, Acc), AT),
                   pure_return(Value, AT)
               ])
    end.

callback_to_cc(Callback, {?MODULE, IM}) when is_function(Callback, 0) ->
    MR = async_r_t:new(IM),
    fun(_A) ->
            case Callback() of
                {async_r_t, _Inner} = NMonadMR ->
                    NMonadMR;
                Result ->
                    async_r_t:return(Result, MR)
            end
    end;
callback_to_cc(Callback, {?MODULE, M}) when is_function(Callback, 1) ->
    MR = async_r_t:new(M),
    fun(A) ->
            case Callback(A) of
                {async_r_t, _Inner} = NMonadMR ->
                    NMonadMR;
                Result ->
                    async_r_t:return(Result, MR)
            end
    end;
callback_to_cc(Callback, {?MODULE, IM}) when is_function(Callback, 2) ->
    MR = async_r_t:new(IM),
    fun(A) ->
            do([{async_r_t, IM} || 
                   State <- async_r_t:get_state(MR),
                   case Callback(A, State) of
                       {async_r_t, _Inner} = NMonadMR ->
                           NMonadMR;
                       NState ->
                           case async_util:same_type_state(NState, State) of
                               true ->
                                   do([{async_r_t, IM} ||
                                          async_r_t:put_state(NState, MR),
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
    unhandled.

handle_reference(MRef, Callbacks, #{type := ReplyType} = Opts) ->
    case match_reference(MRef, Opts) of
        true ->
            case async_util:find(MRef, Callbacks) of
                {ok, #callback{cc = Callback, acc_ref = Acc}} ->
                    demonitor_ref(MRef, Opts),
                    NCallbacks = 
                        case ReplyType of
                            reply ->
                                async_util:remove(MRef, Callbacks);
                            message ->
                                Callbacks
                        end,
                    {ok, Callback, Acc, NCallbacks};
                error ->
                    error
            end;
        false ->
            error
    end.

match_reference(Ref, #{ref := monitored_reference}) when is_reference(Ref) ->
    true;
match_reference(Ref, #{ref := reference}) when is_reference(Ref) ->
    true;
match_reference(Ref, #{ref := integer}) when is_integer(Ref) ->
    true;
match_reference(Ref, #{ref := binary}) when is_binary(Ref) ->
    true;
match_reference(_,   #{ref := all}) ->
    true;
match_reference(_,   #{}) ->
    false.

demonitor_ref(MRef, #{type := reply, ref := monitored_reference}) when is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]);
demonitor_ref(_Ref, #{}) ->
    ok.

execute_callback_a(Callback, message, A) ->
    Callback({message, A});
execute_callback_a(Callback, reply, A) ->
    Callback(A).

callback_with_timeout(Callback, MRef, Timeout, {?MODULE, _IM}) when is_integer(Timeout) ->
    Timer = erlang:send_after(Timeout, self(), {MRef, {error, wait_timeout}}),
    fun(A) ->
            _ = erlang:cancel_timer(Timer),
            Callback(A)
    end;
callback_with_timeout(Callback, _MRef, _Timeout, {?MODULE, _IM}) ->
    Callback.

state_callbacks_gs(Offset) ->
    {fun(State) ->
             try
                 element(Offset, State)
             catch
                 _:Exception ->
                     exit(Exception)
             end
     end,
     fun(Callbacks, State) ->
             setelement(Offset, State, Callbacks)
     end}.

remove_ref_after_cc(Ref, CC, {?MODULE, IM}) ->
    MR = async_r_t:new(IM),
    fun({message, _M} = Message) ->
            CC(Message);
       (A) ->
            do([MR ||
                   State <- async_r_t:get_state(MR),
                   Val <- CC(A),
                   NState <- async_r_t:do_get_state(MR),
                   case async_util:same_type_state(NState, State) of
                       true ->
                           async_r_t:remove_ref(Ref, MR);
                       false ->
                           async_r_t:return(ok, MR)
                   end,
                   async_r_t:return(Val, MR)
               ])
    end.

map_real(F, ATA) ->
    real_to_async_t(F(async_to_real_t(ATA))).
