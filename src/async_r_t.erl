%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------

-module(async_r_t).

-erlando_type(?MODULE).

-export_type([async_r_t/3]).
-opaque async_r_t(S, M, A) :: {async_r_t, inner_async_r_t(S, M, A)}.
-type t(M) :: monad_trans:monad_trans(?MODULE, M).
-type inner_async_r_t(S, M, A) :: 
        fun((S) -> fun((reference()) -> fun((callback_gs(S)) -> monad:monadic(M, {S, A})))).
-type callback_gs(S) :: 
        {fun((S) -> #{reference() => Val}), fun((#{reference() => Val}, S) -> S)}.

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).

%% API
-export([new/1, async_r_t/1, run_async_r_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([lift/2]).
-export([fail/2]).
-export([ask/1]).
-export([do_get_state/1, do_put_state/2, do_modify_state/2]).
-export([get_state/1, put_state/2, modify_state/2]).
-export([get_local_ref/1, local_ref/3, local/3, get_local/1, put_local/2, modify_local/2]).
-export([find_ref/2, get_ref/3, modify_ref/3, put_ref/3, remove_ref/2]).
-export([eval/5, exec/5, run/5, map/3]).

-transform(#{inner_type => monad, tfunctions => [ask/1]}).
-transform(#{inner_type => monad, tfunctions => [do_get_state/1, do_put_state/2, do_modify_state/2]}).
-transform(#{inner_type => monad, tfunctions => [get_state/1, put_state/2, modify_state/2]}).
-transform(#{inner_type => monad, 
             tfunctions => [get_local_ref/1, local_ref/3, local/3, get_local/1, put_local/2, modify_local/2]}).
-transform(#{inner_type => monad, tfunctions => [find_ref/2, get_ref/3, modify_ref/3, put_ref/3, remove_ref/2]}).
-transform(#{inner_type => monad, tfunctions => [eval/5, exec/5, run/5, map/3]}).

-transform(#{inner_type => functor, behaviours => [functor]}).
-transform(#{inner_type => monad, behaviours => [applicative, monad, monad_trans]}).

%%%===================================================================
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

-spec fmap(fun((A) -> B), async_r_t(S, M, A)) -> async_r_t(S, M, B).
fmap(F, ARTA, {?MODULE, IM}) ->
    RM = new_real(IM),
    map_real(
      fun(STA) ->
              state_t:fmap(F, STA, RM)
      end, ARTA).

'<$'(ARTB, ARTA, {?MODULE, _IM} = ART) ->
    functor:'default_<$'(ARTB, ARTA, ART).

pure(A, {?MODULE, IM}) ->
    RM = new_real(IM),
    real_to_async_r_t(applicative:pure(A, RM)).

'<*>'(ARTF, ARTA, {?MODULE, IM}) ->
    STF = async_r_to_real_t(ARTF),
    STA = async_r_to_real_t(ARTA),
    RM = new_real(IM),
    real_to_async_r_t(applicative:'<*>'(STF, STA, RM)).

lift_a2(F, ARTA, ARTB, {?MODULE, _IM} = ART) ->
    applicative:default_lift_a2(F, ARTA, ARTB, ART).

'*>'(ARTA, ARTB, {?MODULE, _IM} = ART) ->
    applicative:'default_*>'(ARTA, ARTB, ART).

'<*'(ARTB, ARTA, {?MODULE, _IM} = ART) ->
    applicative:'default_<*'(ARTB, ARTA, ART).

-spec '>>='(async_r_t(S, M, A), fun( (A) -> async_r_t(S,  M, B) )) -> async_r_t(S, M, B).
'>>='(ARTA, KARTB, {?MODULE, IM}) ->
    RM = new_real(IM),
    real_to_async_r_t(
      state_t:'>>='(async_r_to_real_t(ARTA), fun(A) -> async_r_to_real_t(KARTB(A)) end, RM)).

'>>'(ARTA, ARTB, {?MODULE, _IM} = ART) ->
    monad:'default_>>'(ARTA, ARTB, ART).

-spec return(A, t(M)) -> async_r_t(_S, M, A).
return(A, {?MODULE, IM}) ->
    RealM = new_real(IM),
    real_to_async_r_t(monad:return(A, RealM)).

-spec lift(monad:monadic(M, A)) -> async_r_t(_S,  M, A).
lift(MA, {?MODULE, IM}) ->
    M0 = reader_t:new(IM),
    M1 = reader_t:new(M0),
    M2 = state_t:new(M1),
    real_to_async_r_t(state_t:lift(reader_t:lift(reader_t:lift(MA, M0), M1), M2)).

-spec fail(any(), M) -> async_r_t(_S, M, _A).
fail(X, {?MODULE, IM}) ->
    RealM = new_real(IM),
    real_to_async_r_t(monad_fail:fail(X, RealM)).

do_get_state({?MODULE, IM}) ->
    RM = new_real(IM),
    real_to_async_r_t(monad_state:get(RM)).

do_put_state(State, {?MODULE, IM}) ->
    RM = new_real(IM),
    real_to_async_r_t(monad_state:put(State, RM)).

do_modify_state(State, {?MODULE, IM}) ->
    RM = new_real(IM),
    real_to_async_r_t(monad_state:modify(State, RM)).

-spec get_state(M) -> async_r_t(S,  M, S).
get_state({?MODULE, _IM} = ART) ->
    do([ART ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           begin
               Callbacks = CallbacksGetter(State),
               return(CallbacksSetter(async_util:clear(Callbacks), State))
           end
       ]).

-spec put_state(S, M) -> async_r_t(S, M, ok).
put_state(NState, {?MODULE, _IM} = ART) ->
    do([ART ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           begin 
               Callbacks = CallbacksGetter(State),
               case async_util:same_type_state(State, NState) of
                   true ->
                       NCallbacks = CallbacksGetter(NState),
                       NNState = CallbacksSetter(async_util:merge(Callbacks, NCallbacks), NState),
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
modify_state(Fun, {?MODULE, _IM} = ART) ->
    do([ART ||
           State <- do_get_state(ART),
           put_state(Fun(State), ART)
       ]).

-spec get_local_ref(M) -> async_r_t(_S, M, reference()).
get_local_ref({?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:lift(reader_t:ask(M2), M3)).

-spec local_ref(reference(), async_r_t(S, M, A), t(M)) -> async_r_t(S, M, A).
local_ref(Ref, X, {?MODULE, _IM} = ART) ->
    local(fun(_) -> Ref end, X, ART).

-spec local(fun((R) -> R), async_r_t(S, M, A), t(M)) -> async_r_t(S, M, A).
local(F, ARTA, {?MODULE, IM}) ->
    RM = new_real(IM),
    map_real(
      fun(STA) ->
              monad_reader:local(F, STA, RM)
      end, ARTA).

-spec get_local(M) -> async_r_t(_S, M, _C).
get_local({?MODULE, _IM} = ART) ->
    do([ART || 
           Ref <- get_local_ref(ART),
           get_ref(Ref, undefined, ART)
       ]).

-spec put_local(_C, M) -> async_r_t(_S, M, ok).
put_local(Acc, {?MODULE, _IM} = ART) ->
    do([ART || 
           Ref <- get_local_ref(ART),
           put_ref(Ref, Acc, ART)
       ]).

modify_local(Fun, {?MODULE, _IM} = ART) ->
    do([ART ||
           Local <- get_local(ART),
           put_local(Fun(Local), ART)
       ]).
    
-spec find_ref(reference(), M) -> async_r_t(_S, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, _IM} = ART) ->
    do([ART ||
           {CallbacksGetter, _CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           Callbacks = CallbacksGetter(State),
           return(async_util:find(MRef, Callbacks))
       ]).

-spec get_ref(reference(), A, M) -> async_r_t(_S, M, A).
get_ref(MRef, Default, {?MODULE, _IM} = ART) ->
    do([ART ||
           {CallbacksGetter, _CallbacksSetter} <- ask(ART),
           State <- do_get_state(ART),
           Callbacks = CallbacksGetter(State),
           return(async_util:get(MRef, Callbacks, Default))
       ]).

-spec put_ref(reference(), _A, M) -> async_r_t(_S, M, ok).
put_ref(MRef, Data, {?MODULE, _IM} = ART) ->
    do([ART ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     NCallbacks = async_util:store(MRef, Data, Callbacks),
                     CallbacksSetter(NCallbacks, State)
             end, ART)
       ]).

-spec modify_ref(reference(), fun((A) -> A), M) -> async_r_t(_S, M, ok).
modify_ref(MRef, Fun, {?MODULE, _IM} = ART) ->
    do([ART ||
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
remove_ref(MRef, {?MODULE, _IM} = ART) ->
    do([ART ||
           {CallbacksGetter, CallbacksSetter} <- ask(ART),
           do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     NCallbacks = async_util:remove(MRef, Callbacks),
                     CallbacksSetter(NCallbacks, State)
           end, ART)
       ]).

eval(X, CallbacksGS, Acc, State, {?MODULE, IM}) ->
    RM = new_real(IM),
    NStateV = state_t:eval(async_r_to_real_t(X), State, RM),
    reader_t:run(reader_t:run(NStateV, Acc), CallbacksGS).

-spec exec(async_r_t(S, M, _A), callback_gs(S), _Acc, S) -> monad:monadic(M, S).
exec(X, CallbacksGS, Acc, State, {?MODULE, IM}) ->
    RM = new_real(IM),
    NStateV = state_t:exec(async_r_to_real_t(X), State, RM),
    reader_t:run(reader_t:run(NStateV, Acc), CallbacksGS).

-spec run(async_r_t(S, M, A), callback_gs(S), _Acc, S) -> monad:monadic(M, {A, S}).
run(X, CallbacksGS, Acc, State, {?MODULE, IM}) ->
    RM = new_real(IM),
    NStateV = state_t:run(async_r_to_real_t(X), State, RM),
    reader_t:run(reader_t:run(NStateV, Acc), CallbacksGS).

-spec map(fun((monad:monadic(M, {A, S})) -> monad:monadic(N, {B, S})), async_r_t(S, M, A)) -> async_r_t(S, N, B).
map(F, X, {?MODULE, IM}) ->
    RM = new_real(IM),
    F1 = fun(R1) -> reader_t:map(F, R1) end,
    F2 = fun(R2) -> reader_t:map(F1, R2) end,
    real_to_async_r_t(state_t:map(F2, async_r_to_real_t(X), RM)).

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

map_real(F, ARTA) ->
    real_to_async_r_t(F(async_r_to_real_t(ARTA))).

ask({?MODULE, IM}) ->
    M1 = reader_t:new(IM),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    real_to_async_r_t(state_t:lift(reader_t:lift(reader_t:ask(M1), M2), M3)).
