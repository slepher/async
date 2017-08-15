%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------

-module(async_r_t).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-behaviour(monad_trans).

-export_type([async_r_t/3]).

%% API
-export([new/1, async_r_t/1, run_async_r_t/1]).
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([do_get_state/1, do_put_state/2, do_modify_state/2]).
-export([get_state/1, put_state/2, modify_state/2]).
-export([get_local_ref/1, local_ref/3, local/3, get_local/1, put_local/2, modify_local/2]).
-export([find_ref/2, get_ref/3, modify_ref/3, put_ref/3, remove_ref/2]).
-export([exec/5, run/5, map_async_r/3]).

-opaque async_r_t(S, M, A) :: {async_r_t, inner_async_r_t(S, M, A)}.

-type t(M) :: monad_trans:monad_trans(?MODULE, M).
-type inner_async_r_t(S, M, A) :: 
        fun((S) -> fun((reference()) -> fun((callback_gs(S)) -> monad:monadic(M, {S, A})))).
-type callback_gs(S) :: 
        {fun((S) -> #{reference() => Val}), fun((#{reference() => Val}, S) -> S)}.


%%%===================================================f================
%%% API
%%%===================================================================

new(M) ->
    {?MODULE, M}.

async_r_t(Inner) ->
    {?MODULE, Inner}.

run_async_r_t({?MODULE, Inner}) ->
    Inner;
run_async_r_t(Other) ->
    exit({invalid_async_r_t, Other}).

-spec fmap(fun((A) -> B), async_r_t(S, M, A), t(M)) -> async_r_t(S, M, B).
fmap(F, X, {?MODULE, IM}) ->
    RealM = new_real(IM),
    real_to_async_r_t(state_t:fmap(F, async_r_to_real_t(X), RealM)).

-spec '>>='(async_r_t(S, M, A), fun( (A) -> async_r_t(S,  M, B) ),
            {async_r_t, M}) -> async_r_t(S, M, B).
'>>='(X, Fun, {?MODULE, IM}) ->
    RealM = new_real(IM),
    real_to_async_r_t(
      state_t:'>>='(async_r_to_real_t(X), fun(A) -> async_r_to_real_t(Fun(A)) end, RealM)).

-spec return(A, M) -> async_r_t(_S, M, A).
return(A, {?MODULE, IM}) ->
    RealM = new_real(IM),
    real_to_async_r_t(state_t:return(A, RealM)).

-spec fail(any(), M) -> async_r_t(_S, M, _A).
fail(X, {?MODULE, IM}) ->
    RealM = new_real(IM),
    real_to_async_r_t(state_t:fail(X, RealM)).

-spec lift(monad:monadic(M, A), M) -> async_r_t(_S,  M, A).
lift(F, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:lift(reader_t:lift(reader_t:lift(F, M1), M2), M3)).

do_get_state({?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:get(M3)).

do_put_state(State, {?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:put(State, M3)).

do_modify_state(State, {?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:modify(State, M3)).

-spec get_state(M) -> async_r_t(S,  M, S).
get_state({?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           begin
               Callbacks = CallbacksGetter(State),
               return(CallbacksSetter(async_util:clear(Callbacks), State))
           end
       ]).

-spec put_state(S, M) -> async_r_t(S, M, ok).
put_state(NState, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           begin 
               Callbacks = CallbacksGetter(State),
               case async_util:same_type_state(State, NState) of
                   true ->
                       NCallbacks = CallbacksGetter(NState),
                       NNState = CallbacksSetter(
                                   async_util:merge(Callbacks, NCallbacks), NState),
                       do_put_state(NNState, ART);
                   false ->
                       case async_util:callback_exists(Callbacks) of
                           true ->
                               fail({invalid_put_state, NState});
                           false ->
                               do_put_state(NState, ART)
                       end
               end
           end
       ]).

-spec modify_state(fun((S) -> S), M) -> async_r_t(S, M, ok).
modify_state(Fun, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           State <- do_get_state(ART),
           put_state(Fun(State), ART)
       ]).

-spec get_local_ref(M) -> async_r_t(_S, M, reference()).
get_local_ref({?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:lift(reader_t:ask(M2), M3)).

-spec local_ref(reference(), async_r_t(S, M, A), M) -> async_r_t(S, M, A).
local_ref(Ref, X, {?MODULE, _IM} = ART) ->
    local(fun(_) -> Ref end, X, ART).

-spec local(fun((R) -> R), async_r_t(S, M, A), M) -> async_r_t(S, M, A).
local(L, X, {?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(
      state_t:map_state(fun(IMS) -> reader_t:local(L, IMS, M2) end, async_r_to_real_t(X), M3)).

-spec get_local(M) -> async_r_t(_S, M, _C).
get_local({?MODULE, IM} = ART) ->
    do([{?MODULE, IM} || 
           Ref <- get_local_ref(ART),
           get_ref(Ref, undefined, ART)
       ]).

-spec put_local(_C, M) -> async_r_t(_S, M, ok).
put_local(Acc, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} || 
           Ref <- get_local_ref(ART),
           put_ref(Ref, Acc, ART)
       ]).

modify_local(Fun, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           Local <- get_local(ART),
           put_local(Fun(Local), ART)
       ]).
    
-spec find_ref(reference(), M) -> async_r_t(_S, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, _CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           Callbacks = CallbacksGetter(State),
           return(async_util:find(MRef, Callbacks))
       ]).

-spec get_ref(reference(), A, M) -> async_r_t(_S, M, A).
get_ref(MRef, Default, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, _CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           Callbacks = CallbacksGetter(State),
           return(async_util:get(MRef, Callbacks, Default))
       ]).

-spec put_ref(reference(), _A, M) -> async_r_t(_S, M, ok).
put_ref(MRef, Data, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     NCallbacks = async_util:store(MRef, Data, Callbacks),
                     CallbacksSetter(NCallbacks, State)
             end, ART)
       ]).

-spec modify_ref(reference(), fun((A) -> A), M) -> async_r_t(_S, M, ok).
modify_ref(MRef, Fun, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     Data = async_util:get(MRef, Callbacks, undefined),
                     NData = Fun(Data),
                     NCallbacks = async_util:store(MRef, NData, Callbacks),
                     CallbacksSetter(NCallbacks, State)
             end, ART)
       ]).

-spec remove_ref(reference(), M) -> async_r_t(_S, M, ok).
remove_ref(MRef, {?MODULE, IM} = ART) ->
    do([{?MODULE, IM} ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     NCallbacks = async_util:remove(MRef, Callbacks),
                     CallbacksSetter(NCallbacks, State)
           end, ART)
       ]).

-spec exec(async_r_t(S, M, _A), callback_gs(S), _Acc, S, M) -> monad:monadic(M, S).
exec(X, CallbacksGS, Acc, State, {?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    NStateV = state_t:exec_state(async_r_to_real_t(X), State, M3),
    reader_t:run_reader(reader_t:run_reader(NStateV, Acc, M2), CallbacksGS, M1).


-spec run(async_r_t(S, M, A), callback_gs(S), _Acc, S, M) -> monad:monadic(M, {A, S}).
run(X, CallbacksGS, Acc, State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    NStateV = state_t:run_state(async_r_to_real_t(X), State, M3),
    reader_t:run_reader(reader_t:run_reader(NStateV, Acc, M2), CallbacksGS, M1).

-spec map_async_r(fun((monad:monadic(M, {A, S})) -> monad:monadic(N, {B, S})), async_r_t(S, M, A), M) -> async_r_t(S, N, B).
map_async_r(F, X, {?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    F1 = fun(R1) -> reader_t:map_reader(F, R1, M1) end,
    F2 = fun(R2) -> reader_t:map_reader(F1, R2, M2) end,
    real_to_async_r_t(state_t:map_state(F2, async_r_to_real_t(X), M3)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
new_real(M) ->
    state_t:new(reader_t:new(reader_t:new(M))).

-spec async_r_to_real_t(async_r_t(S, M, A)) -> state_t:state_t(S, M, A).
async_r_to_real_t(AsyncR) ->
    state_t:state_t(run_async_r_t(AsyncR)).

-spec real_to_async_r_t(state_t:state_t(S, M, A)) -> async_r_t(S, M, A).
real_to_async_r_t(Real) ->
    async_r_t(state_t:run_state_t(Real)).

ask({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:lift(reader_t:lift(reader_t:ask(M1), M2), M3)).


