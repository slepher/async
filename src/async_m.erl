%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m).
-behaviour(monad).
-transformer(async_t).
-compile({parse_transform, monad_t_transform}).
-export([then/4, update_state/1]).

%%%===================================================================
%%% API
%%%===================================================================

then(Monad, Callback, Offset, State) ->
    run(Monad, Callback, Offset, State).

update_state(Fun) ->
    modify_state(Fun).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
