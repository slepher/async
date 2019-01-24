%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_r_m).

-erlando_type({?MODULE, []}).

-include_lib("erlando/include/gen_fun.hrl").

-behaviour(functor).
-behaviour(monad).

-gen_fun(#{remote => async_r_t, args => identity, 
             functions => [do_get_state/0, do_put_state/1, do_modify_state/1]}).
-gen_fun(#{remote => async_r_t, args => identity, 
             functions => [get_state/0, put_state/1, modify_state/1]}).
-gen_fun(#{remote => async_r_t, args => identity,
             functions => [get_local_ref/0, local_ref/2, local/1, get_local/0, put_local/1, modify_local/1]}).
-gen_fun(#{remote => async_r_t, args => identity,
             functions => [find_ref/1, get_ref/2, modify_ref/2, put_ref/2, remove_ref/1]}).
-gen_fun(#{remote => async_r_t, args => identity, extra_call => {identity, run},
             functions => [eval/4, exec/4, run/4, map/2]}).

-gen_fun(#{remote => async_r_t,inner_type => identity, behaviours => [functor, monad]}).
