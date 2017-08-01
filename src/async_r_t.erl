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
-behaviour(monad_trans).

-export_type([async_r_t/3]).

%% API
-export([new/1, '>>='/3, return/2, fail/2, lift/2]).
-export([do_get_state/1, do_put_state/2, do_modify_state/2]).
-export([get_state/1, put_state/2, modify_state/2]).
-export([get_local_ref/1, local_ref/3, local_local_ref/3, get_local/1, put_local/2, modify_local/2]).
-export([find_ref/2, get_ref/3, put_ref/3, remove_ref/2]).
-export([exec/5, run/5]).

-opaque async_r_t(S, M, A) :: fun((S) -> fun((reference()) -> fun((callback_gs(S)) -> monad:monadic(M, {S, A})))).
-type callback_gs(S) :: {fun((S) -> #{reference() => Val}), fun((#{reference() => Val}, S) -> S)}.

%%%===================================================f================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec '>>='(async_r_t(S, M, A), fun( (A) -> async_r_t(S,  M, B) ), M) -> async_r_t(S, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

-spec return(A, M) -> async_r_t(_S, M, A).
return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

-spec fail(any(), M) -> async_r_t(_S, M, _A).
fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

-spec lift(monad:monadic(M, A), M) -> async_r_t(_S,  M, A).
lift(F, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:lift(M1:lift(F))).

do_get_state({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:get().

do_put_state(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:put(State).

do_modify_state(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:modify(State).

-spec get_state(M) -> async_r_t(S,  M, S).
get_state({?MODULE, M}) ->
    Monad = new(M),
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- ask(Monad),
           State <- Monad:do_get_state(),
           begin
               Callbacks = CallbacksGetter(State),
               return(CallbacksSetter(async_util:clear(Callbacks), State))
           end
       ]).

-spec put_state(S, M) -> async_r_t(S, M, ok).
put_state(NState, {?MODULE, M}) ->
    Monad = new(M),
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- ask(Monad),
           State <- Monad:do_get_state(),
           begin 
               Callbacks = CallbacksGetter(State),
               case async_util:same_type_state(State, NState) of
                   true ->
                       NCallbacks = CallbacksGetter(NState),
                       NNState = CallbacksSetter(async_util:merge(Callbacks, NCallbacks), NState),
                       Monad:do_put_state(NNState);
                   false ->
                       case async_util:callback_exists(Callbacks) of
                           true ->
                               Monad:fail({invalid_put_state, NState});
                           false ->
                               Monad:do_put_state(NState)
                       end
               end
           end
       ]).

-spec modify_state(fun((S) -> S), M) -> async_r_t(S, M, ok).
modify_state(Fun, {?MODULE, M}) ->
    Monad = new(M),
    do([Monad ||
           State <- Monad:do_get_state(),
           Monad:put_state(Fun(State))
       ]).

-spec get_local_ref(M) -> async_r_t(_S, M, reference()).
get_local_ref({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:ask()).

-spec local_ref(reference(), async_r_t(S, M, A), M) -> async_r_t(S, M, A).
local_ref(Ref, X, {?MODULE, _M} = Monad) ->
    Monad:local_local_ref(fun(_) -> Ref end, X).

local_local_ref(L, X, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    fun(S) ->
            M2:local(L, M3:run(X, S))
    end. 

-spec get_local(M) -> async_r_t(_S, M, _C).
get_local({?MODULE, _M} = Monad) ->
    do([Monad || 
           Ref <- Monad:get_local_ref(),
           Monad:get_ref(Ref, undefined)
       ]).

-spec put_local(_C, M) -> async_r_t(_S, M, ok).
put_local(Acc, {?MODULE, _M} = Monad) ->
    do([Monad || 
           Ref <- Monad:get_local_ref(),
           Monad:put_ref(Ref, Acc)
       ]).

modify_local(Fun, {?MODULE, _M} = Monad) ->
    do([Monad ||
           Local <- Monad:get_local(),
           Monad:put_local(Fun(Local))
       ]).
    
-spec find_ref(reference(), M) -> async_r_t(_S, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, _CallbacksSetter} <- ask(Monad),
           State <- Monad:do_get_state(),
           begin
               Callbacks = CallbacksGetter(State),
               return(async_util:find(MRef, Callbacks))
           end
       ]).

-spec get_ref(reference(), A, M) -> async_r_t(_S, M, A).
get_ref(MRef, Default, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, _CallbacksSetter} <- ask(Monad),
           State <- Monad:do_get_state(),
           begin
               Callbacks = CallbacksGetter(State),
               return(maps:get(MRef, Callbacks, Default))
           end
       ]).

-spec put_ref(reference(), _A, M) -> async_r_t(_S, M, ok).
put_ref(MRef, Data, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- ask(Monad),
           Monad:do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     NCallbacks = async_util:store(MRef, Data, Callbacks),
                     CallbacksSetter(NCallbacks, State)
             end)
       ]).

-spec remove_ref(reference(), M) -> async_r_t(_S, M, ok).
remove_ref(MRef, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- ask(Monad),
           Monad:do_modify_state(
             fun(State) ->
                     Callbacks = CallbacksGetter(State),
                     NCallbacks = async_util:remove(MRef, Callbacks),
                     CallbacksSetter(NCallbacks, State)
           end)
       ]).

-spec exec(async_r_t(S, M, _A), callback_gs(S), _Acc, S, M) -> monad:monadic(M, S).
exec(X, CallbacksGS, Acc, State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M1:run((M2:run(M3:exec(X, State), Acc)), CallbacksGS).

-spec run(async_r_t(S, M, A), callback_gs(S), _Acc, S, M) -> monad:monadic(M, {A, S}).
run(X, CallbacksGS, Acc, State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M1:run((M2:run(M3:run(X, State), Acc)), CallbacksGS).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
real(M) ->
    state_t:new(reader_t:new(reader_t:new(M))).

ask({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:lift(M1:ask())).


