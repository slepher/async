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
-compile({parse_transform, monad_m}).
-export([then/4, update_state/1, return_error_m/1]).

%%%===================================================================
%%% API
%%%===================================================================

then(Monad, Callback, Offset, State) ->
    exec(Monad, Callback, Offset, State).

update_state(Fun) ->
    modify_state(Fun).

return_error_m(Monad) ->
    lift_final_reply(Monad).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
