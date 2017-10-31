%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_r_m_v5).
-behaviour(monad).

%% API
-export(['>>='/2, '>>'/2, return/1, fail/1, get/0, put/1, modify/1, ask/0, run/3]).

%%%===================================================================
%%% API
%%%===================================================================
'>>='(X, Fun) ->
    state_t:'>>='(X, Fun).

'>>'(Xa, Xb) ->
    state_t:'>>'(Xa, Xb).

return(A) ->
    M = state_t:new(reader_t:new(identity)),
    state_t:return(A, M).

fail(R) ->
    exit(R).

get() ->
    M = state_t:new(reader_t:new(identity)),
    state_t:get(M).

put(S) ->
    M = state_t:new(reader_t:new(identity)),
    state_t:put(S, M).

modify(S) ->
    M = state_t:new(reader_t:new(identity)),
    monad_state:modify(S, M).

ask() ->
    state_t:lift(reader_m:ask()).

run(AsyncRM, Offset, State) ->
    reader_m:run(state_t:exec(AsyncRM, State), Offset).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
