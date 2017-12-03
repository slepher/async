%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_r_m_v5).

-erlando_type({?MODULE, []}).

-compile({parse_transform, function_generator}).

-behaviour(monad).
-behaviour(monad_state).
-behaviour(monad_reader).

-define(ASYNC_R_M_5, {state_t, {reader_t, identity}}).
-define(PG, [[], [?MODULE]]).

-export([run/3]).

%% API
-gen_fun(#{remote => functor,
           patterns_group => ?PG,
           args => [?ASYNC_R_M_5],
           behaviours => [functor]}).

-gen_fun(#{remote => applicative,
           patterns_group => ?PG,
           args => [?ASYNC_R_M_5],
           behaviours => [applicative]}).

-gen_fun(#{remote => monad,
           patterns_group => ?PG,
           args => [?ASYNC_R_M_5],
           behaviours => [monad]}).

-gen_fun(#{remote => monad_state,
           patterns_group => ?PG,
           args => [?ASYNC_R_M_5],
           behaviours => [monad_state]}).

-gen_fun(#{remote => monad_state,
           patterns_group => ?PG,
           args => [?ASYNC_R_M_5],
           sfunctions => [modify/2]}).

-gen_fun(#{remote => monad_reader,
           patterns_group => ?PG,
           args => [?ASYNC_R_M_5],
           behaviours => [monad_reader]}).

%%%===================================================================
%%% API
%%%===================================================================
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
