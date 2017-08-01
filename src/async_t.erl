%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).

-export_type([async_t/4]).

%% API
-export([new/1, new_mr/1, '>>='/3, return/2, fail/2, lift/2, lift_mr/2]).
-export([get_state/1, put_state/2, modify_state/2, find_ref/2, get_ref/3, put_ref/3, remove_ref/2, 
         get_local/1, put_local/2, modify_local/2, local_ref/3, get_local_ref/1, callCC/2]).
-export([lift_reply/2, lift_reply_all/2, pure_return/2, wrapped_return/2,
         message/2, hijack/2, pass/1, handle_message/3, provide_message/3]).
-export([promise/2, promise/3, then/3, map/2, map/3, par/2, progn_par/2]).
-export([wait/2, wait/3, wait/4, wait/5, wait/6, wait_cc/6]).
-export([exec/5, exec_cc/5, run/5, run_cc/5, handle_info/4, wait_receive/4]).

-opaque async_t(S, R, M, A) :: 
          fun((fun((reply_t:reply_t(identity_m, A)) -> async_r_t:async_r_t(S, M, R))) -> async_r_t:async_r_t(S, M, R)).

-type callback_or_cc(S, R, M, A) :: fun((A | {ok, A} | {error, _E} | {message, _IM}) -> async_r_t:async_r_t(S, M, R)) | 
                                    fun(() -> any()) | 
                                    fun((A | {ok, A} | {error, _E} | {message, _M}) -> any()) |
                                    fun((A | {ok, A} | {error, _E} | {message, _IM}, S) -> any()).

-record(callback, {cc, acc_ref}).

%%%===================================================================
%%% API
%%%===================================================================


-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

new_mr(M) ->
    async_r_t:new(M).

-spec '>>='(async_t(S, R, M, A), fun( (A) -> async_t(S, R, M, B) ), M) -> async_t(S, R, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

-spec return(A, M) -> async_t(_S, _R, M, A).
return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

-spec fail(any(), M) -> async_t(_S, _R, M, _A).
fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

-spec lift(monad:monadic(M, A), M) -> async_t(_S, _R, M, A).
lift(F, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:lift(F)).

-spec lift_mr(async_r_t:async_r_t(S, M, A), M) -> async_t(S, _R, M, A).
lift_mr(MonadR, {?MODULE, M}) ->
    MR = new_mr(M),
    M1 = cont_t:new(MR),
    M2 = reply_t:new(M1),
    M2:lift(M1:lift(MonadR)).

-spec get_state(M) -> async_t(S, _R, M, S).
get_state({?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:get_state()).

-spec put_state(S, M) -> async_t(S, _R, M, ok).
put_state(State, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:put_state(State)).

-spec modify_state(S, M) -> async_t(S, _R, M, ok).
modify_state(Fun, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:modify_state(Fun)).

-spec get_local(M) -> async_t(_S, _R, M, _Local).
get_local({?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:get_local()).

-spec put_local(_Local, M) -> async_t(_S, _R, M, ok).
put_local(Local, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:put_local(Local)).

-spec modify_local(fun((Local) -> Local), M) -> async_t(_S, _R, M, ok).
modify_local(Fun, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:modify_local(Fun)).

-spec local_ref(reference(), async_t(S, R, M, A), M) -> async_t(S, R, M, A).
local_ref(Ref, X, {?MODULE, M}) ->
    MR = new_mr(M),
    fun(K) ->
            do([MR ||
                   ORef <- MR:get_local_ref(),
                   begin 
                       NK = 
                           fun(A) ->
                                   MR:local_ref(ORef, K(A))
                           end,
                       MR:local_ref(Ref, X(NK))
                   end
               ])
    end.
-spec get_local_ref(M) -> async_t(_S, _R, M, reference()).
get_local_ref({?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:get_local_ref()).

-spec find_ref(reference(), M) -> async_t(_S, _R, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:find_ref(MRef)).

-spec get_ref(reference(), A, M) -> async_t(_S, _R, M, A).
get_ref(MRef, Default, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:get_ref(MRef, Default)).

-spec put_ref(reference(), _A, M) -> async_t(_S, _R, M, ok).
put_ref(MRef, Value, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:put_ref(MRef, Value)).

-spec remove_ref(reference(), M) -> async_t(_S, _R, M, ok).
remove_ref(MRef, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:lift_mr(MR:remove_ref(MRef)).

-spec lift_reply_all(async_t(S, R, M, A), M) -> async_t(S, R, M, reply_t:reply_t(identity_m, A)).
lift_reply_all(F, {?MODULE, M}) ->
    Monad = real(M),
    Monad:lift(F).

-spec lift_reply(async_t(S, R, M, A), M) -> async_t(S, R, M, A | {ok, A} | {error, _E}).
lift_reply(F, {?MODULE, M}) ->
    Monad = real(M),
    Monad:lift_reply(F).

-spec pure_return(A, M) -> async_t(_S, _R, M, A).
pure_return(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:pure_return(X).

wrapped_return(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:wrapped_return(X).

-spec message(A, M) -> async_t(_S, _R, M, A).
message(A, {?MODULE, _M} = Monad) ->
    Monad:pure_return({message, A}).

-spec callCC(fun((fun( (A) -> async_t(S, R, M, _B) ))-> async_t(S, R, M, A)), M) -> async_t(S, R, M, A).
callCC(F,  {?MODULE, M}) ->
    MR = new_mr(M),
    M1 = cont_t:new(MR),
    M2 = reply_t:new(M1),
    M2:lift(M1:callCC(F)).

-spec promise(any(), M) -> async_t(_S, _R, M, _A).
promise(MRef, {?MODULE, _M} = Monad) ->
    promise(MRef, infinity, Monad).

-spec promise(any(), integer(), M) -> async_t(_S, _R, M, _A);
             (any(), infinity, M) -> async_t(_S, _R, M, _A).
promise(Action, Timeout, {?MODULE, M} = Monad) when is_function(Action, 0)->
    MR = new_mr(M),
    fun(K) ->
            case Action() of
                MRef when is_reference(MRef) ->
                    do([MR || 
                           AccRef <- MR:get_local_ref(),
                           begin 
                               NK = callback_with_timeout(K, MRef, Timeout, Monad),
                               MR:put_ref(MRef, #callback{cc = NK, acc_ref = AccRef})
                           end
                       ]);
                Value ->
                    K(Value)
            end
    end;
promise(MRef, Timeout, {?MODULE, _M} = Monad) when is_reference(MRef) ->
    Monad:promise(fun() -> MRef end, Timeout);
promise(Value, _Timeout, {?MODULE, _M} = Monad) ->
    Monad:pure_return(Value).

-spec then(async_t(S, R, M, A), fun((A) -> async_t( S, R, M, B)), M) -> async_t(S, R, M, B).
then(X, Then, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(Monad:lift(X), Then).

-spec map([async_t(S, R, M, A)], M) -> async_t(S, R, M, [A]);
         (#{Key => async_t(S, R, M, A)}, M) -> async_t(S, R, M, #{Key => A}).
map(Promises, {?MODULE, _M} = Monad) when is_list(Promises) ->
    NPromises = maps:from_list(lists:zip(lists:seq(1, length(Promises)), Promises)),
    do([Monad || 
           Value <- Monad:lift_reply_all(Monad:map(NPromises)),
           case Value of
               {message, {_Key, Message}} ->
                   Monad:message(Message);
               _ ->
                   Monad:pure_return(maps:values(Value))
           end
       ]);
map(Promises, {?MODULE, _M} = Monad) when is_map(Promises) ->
    map(Promises, #{}, Monad).

-spec map(#{Key => async_t(S, R, M, A)}, 
          #{cc => fun((Key, A) -> async_r_t:async_r_t(S, M, _IM)),
            acc0 => Acc, limit => integer()}, M) -> 
                 async_t(S, R, M, Acc).
map(Promises, Options, {?MODULE, _M} = Monad) ->
    WRef = make_ref(),
    PRef = make_ref(),
    CRef = make_ref(),
    CC = maps:get(cc, Options, default_map_cc(Monad)),
    Acc0 = maps:get(acc0, Options, maps:new()),
    Threads = maps:get(limit, Options, 0),
    NPromises = 
        maps:map(
          fun(Key, Promise) ->
                  do([Monad ||
                         Working <- Monad:get_ref(WRef, []),
                         Monad:put_ref(WRef, [Key|Working]),
                         Monad:lift_reply(
                           Monad:provide_message(
                             Promise,
                             fun(Val) ->
                                     Monad:local_ref(CRef, CC(Key, Val))
                             end)),
                         Pending <- Monad:get_ref(PRef, maps:new()),
                         NWorking <- Monad:get_ref(WRef, []),
                         case maps:size(Pending) of
                             0 ->
                                 case lists:delete(Key, NWorking) of
                                     [] ->
                                         do([Monad ||
                                                Completed <- Monad:get_ref(CRef, maps:new()),
                                                Monad:remove_ref(WRef),
                                                Monad:remove_ref(CRef),
                                                Monad:pure_return(Completed)
                                            ]);
                                     NNWorking ->
                                         do([Monad ||
                                                Monad:put_ref(WRef, NNWorking),
                                                Monad:pass()
                                            ])
                                 end;
                             _ ->
                                 PKey = lists:nth(1, maps:keys(Pending)), 
                                 PPromise = maps:get(PKey, Pending, undefined),
                                 NPending = maps:remove(PKey, Pending),
                                 do([Monad ||
                                        Monad:put_ref(PRef, NPending),
                                        Monad:put_ref(WRef, lists:delete(Key, NWorking)),
                                        PPromise
                                    ])
                         end
                     ])
          end, Promises),
    {WPromiseKeys, PPromiseKeys} = split(Threads, maps:keys(NPromises)),
    do([Monad ||
           Monad:put_ref(CRef, Acc0),
           Monad:put_ref(PRef, maps:with(PPromiseKeys, NPromises)),
           Monad:par(maps:values(maps:with(WPromiseKeys, NPromises)))
       ]).

%% provide extra message and return origin value
-spec provide_message(async_t(S, R, M, A), fun((A) -> async_t(S, R, M, _B)), M) -> async_t(S, R, M, A).
provide_message(Promise, Then, {?MODULE, _M} = Monad) ->
    do([Monad ||
           Val <- Monad:lift_reply_all(Promise),
           Monad:progn_par(
             [% this will only return messages and ignore all normal reply returned in then
              do([Monad || 
                     Monad:lift_reply(Then(Val)),
                     Monad:pass()
                 ]),
              % this will only return normal reply and ignore messages in promise
              case Val of
                  {message, _IM} ->
                      Monad:pass();
                  _ ->
                      Monad:pure_return(Val)
              end
             ])
      ]).

%% this is a dangerous function, only one should return A | {ok, A} | {error, E}
%% others should return {message, IM} or use pass()
%% or it will cause unexpected error
-spec par([async_t(S, R, M, A)], M) -> async_t(S, R, M, A).
par(Promises, {?MODULE, M}) ->
    MR = new_mr(M),
    fun(K) ->
            monad:sequence(MR, lists:map(fun(Promise) -> Promise(K) end, Promises))
    end.


%% acts like par, but only return last value of promises
%% the name of progn is from lisp
progn_par(Promises, {?MODULE, M}) ->
    MR = new_mr(M),
    fun(K) ->
            progn_sequence(MR, lists:map(fun(Promise) -> Promise(K) end, Promises), ok)
    end.

progn_sequence(Monad, [], Acc) ->
    do([Monad || return(Acc)]);
progn_sequence(Monad, [X|Xs], _Acc) ->
    do([Monad || E <- X,
                 progn_sequence(Monad, Xs, E)]).    

-spec handle_message(async_t(S, R, M, A), callback_or_cc(S, R, M, A), M) -> async_t(S, R, M, A).
handle_message(X, MessageHandler, {?MODULE, M} = Monad) ->
    NMessageHandler = callback_to_cc(MessageHandler, {?MODULE, M}),
    do([Monad ||
           Value <- Monad:lift_reply_all(X),
           case Value of
               {message, Message} ->
                   Monad:hijack(NMessageHandler(Message));
               Reply ->
                   Monad:pure_return(Reply)
           end
       ]).
-spec hijack(async_r_t:async_r_t(S, M, R), M) -> async_t(S, R, M, _A).
hijack(MR, {?MODULE, _M}) ->
    fun(_K) ->
            MR
    end.

-spec pass(M) -> async_t(_S, ok, M, _A).
pass({?MODULE, M} = Monad) ->
    MR = new_mr(M),
    Monad:hijack(MR:return(ok)).


exec(X, Callback, Offset, State, Monad) ->
    CC = callback_to_cc(Callback, Monad),
    Monad:exec_cc(X, CC, Offset, State).

exec_cc(X, CC, Offset, State, {?MODULE, M} = Monad) ->
    do([M ||
           {_A, NState} <- run_cc(X, CC, Offset, State, Monad),
           return(NState)
       ]).
    
run(X, Callback, Offset, State, Monad) ->
    CC = callback_to_cc(Callback, Monad),
    Monad:run_cc(X, CC, Offset, State).

-spec run(async_t(S, R, M, A), callback_or_cc(S, R, M, A), integer(), S, M) -> S.
run_cc(X, CC, Offset, State, {?MODULE, M} = Monad) ->
    MR = new_mr(M),
    CallbacksGS = state_callbacks_gs(Offset),
    Ref = make_ref(),
    NCC = remove_ref_after_cc(Ref, CC, Monad),
    MR:run(X(NCC), CallbacksGS, Ref, State).

-spec wait(async_t(_S, A, M, A), M) -> monad:monadic(M, A).
wait(X, {?MODULE, _M} = Monad) ->
    wait(X, infinity, Monad).

-spec wait(async_t(S, R, M, A), callback_or_cc(S, R, M, A), M) -> monad:monadic(M, R);
          (async_t(_S, R, M, _A), integer() | infinity, M) -> monad:monadic(M, R).
wait(X, Callback, {?MODULE, _M} = Monad) when is_function(Callback) ->
    wait(X, Callback, infinity, Monad);
wait(X, Timeout, {?MODULE, _M} = Monad) ->
    wait(X, 2, {state, maps:new()}, Timeout, Monad).

-spec wait(async_t(S, R, M, A), callback_or_cc(S, R, M, A), integer() | infinity, M) -> monad:monadic(M, R).
wait(X, Callback, Timeout, {?MODULE, _M} = Monad) ->
    wait(X, Callback, 2, {state, maps:new()}, Timeout, Monad).

-spec wait(async_t(S, A, M, A), integer(), S, integer() | infinity, M) -> monad:monadic(M, A).
wait(X, Offset, State, Timeout, Monad) ->
    wait(X, fun(A) -> A end, Offset, State, Timeout, Monad).
wait(X, Callback, Offset, State, Timeout, Monad) ->
    CC = callback_to_cc(Callback, Monad),
    Monad:wait_cc(X, CC, Offset, State, Timeout).

-spec wait(async_t(S, R, M, A), callback_or_cc(S, R, M, A), integer(), S, integer() | infinity, M) -> monad:monadic(M, R).
wait_cc(X, CC, Offset, State, Timeout, Monad) ->
    MResult = run_cc(X, CC, Offset, State, Monad),
    wait_mresult(MResult, Offset, State, Timeout, Monad).

-spec wait_receive(integer(), _S, integer() | infinity, M) -> monad:monadic(M, _A).
wait_receive(Offset, State, Timeout, Monad) ->
    receive 
        Info ->
            case run_info(Info, Offset, State, Monad) of
                unhandled ->
                    wait_receive(Offset, State, Timeout, Monad);
                MResult ->
                    wait_mresult(MResult, Offset, State, Timeout, Monad)
            end
    after Timeout ->
            %% TODO: corrent this
            {CallbacksG, _CallbacksS} = state_callbacks_gs(Offset),
            Callbacks = CallbacksG(State),
            maps:fold(
              fun(MRef, #callback{}, Acc) ->
                      self() ! {MRef, {error, timeout}},
                      Acc;
                 (_MRef, _Other, Acc) ->
                      Acc
              end, ok, Callbacks),
            wait_receive(Offset, State, 0, Monad)
    end.

wait_mresult(MResult, Offset, State, Timeout, {?MODULE, M} = Monad) ->
    do([M ||
           {A, NState} <- MResult,
           case async_util:same_type_state(NState, State) of
               true ->
                   {CallbacksG, _CallbacksS} = state_callbacks_gs(Offset),
                   Callbacks = CallbacksG(NState),
                   case async_util:callback_exists(Callbacks) of
                       true ->
                           wait_receive(Offset, NState, Timeout, Monad);
                       false ->
                           M:return(A)
                   end;
               false ->
                   M:return(A)
           end
       ]).

-spec handle_info(_Info, integer(), S, M) -> monad:monadic(M, S).
handle_info(Info, Offset, State, {?MODULE, M}) ->
    do([M || 
           {_A, NState} <- run_info(Info, Offset, State, {?MODULE, M}),
           return(NState)
       ]).

-spec run_info(any(), integer(), S, M) -> monad:monadic(M, {_A, S}).
run_info(Info, Offset, State, {?MODULE, M}) ->
    MR = new_mr(M),
    {CallbacksG, CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    case info_to_a(Info) of
        {MRef, A} ->
            case handle_a(MRef, A, Callbacks) of
                {Callback, AccRef, NCallbacks} ->
                    NState = CallbacksS(NCallbacks, State),
                    MR:run(Callback(A), {CallbacksG, CallbacksS}, AccRef, NState);
                error ->
                    M:return(State)
            end;
        unhandled ->
            unhandled
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

real(M) ->
    reply_t:new(cont_t:new(new_mr(M))).

default_map_cc({?MODULE, _M} = Monad) ->
    fun(Key, {message, Message}) ->
            Monad:message({Key, Message});
       (Key, Value) ->
            do([Monad ||
                   Acc <- Monad:get_local(),
                   Monad:put_local(maps:put(Key, Value, Acc)),
                   Monad:pure_return(Value)
               ])
    end.

callback_to_cc(Callback, {?MODULE, M}) when is_function(Callback, 0) ->
    MR = new_mr(M),
    fun(_A) ->
            case Callback() of
                NMonadMR when is_function(NMonadMR) ->
                    NMonadMR;
                Result ->
                    MR:return(Result)
            end
    end;
callback_to_cc(Callback, {?MODULE, M}) when is_function(Callback, 1) ->
    MR = new_mr(M),
    fun(A) ->
            case Callback(A) of
                NMonadMR when is_function(NMonadMR) ->
                    NMonadMR;
                Result ->
                    MR:return(Result)
            end
    end;
callback_to_cc(Callback, {?MODULE, M}) when is_function(Callback, 2) ->
    MR = new_mr(M),
    fun(A) ->
            do([MR || 
                   State <- MR:get_state(),
                   begin
                       NState = Callback(A, State),
                       case async_util:same_type_state(NState, State) of
                           true ->
                               do([MR ||
                                      MR:put_state(NState),
                                      MR:return(NState)
                                  ]);
                           false ->
                               MR:return(NState)
                       end
                   end
               ])
    end;
callback_to_cc(Callback, {?MODULE, M}) ->
    MR = new_mr(M),
    MR:fail({invalid_callback, Callback}).

info_to_a({message, MRef, Message}) when is_reference(MRef) ->
    {MRef, {message, Message}};
info_to_a({MRef, Reply}) when is_reference(MRef) ->
    {MRef, Reply};
info_to_a({'DOWN', MRef, _, _, Reason}) when is_reference(MRef) ->
    {MRef, {error, {process_down, Reason}}};
info_to_a(_Info) ->
    unhandled.

handle_a(MRef, {message, _Message}, Callbacks) when is_reference(MRef) ->
    case async_util:find(MRef, Callbacks) of
        {ok, #callback{cc = Callback, acc_ref = Acc}} ->
            {Callback, Acc, Callbacks};
        error ->
            error
    end;
handle_a(MRef, _Reply, Callbacks) when is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]),
    case async_util:find(MRef, Callbacks) of
        {ok, #callback{cc = Callback, acc_ref = Acc}} ->
            NCallbacks = async_util:remove(MRef, Callbacks),
            {Callback, Acc, NCallbacks};
        error ->
            error
    end.

callback_with_timeout(Callback, MRef, Timeout, {?MODULE, _M}) when is_integer(Timeout) ->
    Timer = erlang:send_after(Timeout, self(), {MRef, {error, wait_timeout}}),
    fun(A) ->
            erlang:cancel_timer(Timer),
            Callback(A)
    end;
callback_with_timeout(Callback, _MRef, _Timeout, {?MODULE, _M}) ->
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

remove_ref_after_cc(Ref, CC, {?MODULE, M}) ->
    MR = new_mr(M),
    fun({message, _M} = Message) ->
            CC(Message);
       (A) ->
            do([MR ||
                   State <- MR:get_state(),
                   Val <- CC(A),
                   NState <- MR:do_get_state(),
                   case async_util:same_type_state(NState, State) of
                       true ->
                           MR:remove_ref(Ref);
                       false ->
                           MR:return(ok)
                   end,
                   return(Val)
               ])
    end.
