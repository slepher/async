%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m).

-erlando_type({?MODULE, []}).

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(monad).

-export([return_error_m/1]).
-export([then/2, then/4]).

-transform(#{remote => async_t, args => identity, 
             functions => [get_state/0, put_state/1, modify_state/1, 
                           find_ref/1, get_ref/2, put_ref/2, remove_ref/1, 
                           get_local/0, put_local/1, modify_local/1, local_ref/2, local/2, get_local_ref/0]}).
-transform(#{remote => async_t, args => identity, 
             functions => [lift_reply/1, lift_final_reply/1, pure_return/1, wrapped_return/1,
                           message/1, hijack/1, pass/0, handle_message/2, provide_message/2]}).
-transform(#{remote => async_t, args => identity, 
             functions =>[promise/1, promise_t/2, map_promises/1, map_promises_t/2, par/1, progn_par/1, callback_to_cc/1]}).
-transform(#{remote => async_t, args => identity, extra_call => {identity, run},
             functions => [wait/1, wait_t/2, exec/4, exec_cc/4, run/4, run_cc/2, run_with_cc/4]}).
-transform(#{remote => async_t, args => identity, extra_call => {identity, run},
             functions => [handle_info/3, run_info/3, wait_receive/3]}).

-transform(#{remote => async_t, inner_type => identity,
             behaviours => [functor, monad, monad_fail]}).

%%%===================================================================
%%% API
%%%===================================================================
return_error_m(Value) ->
    lift_reply(Value).

then(Monad, Callback) ->
    CC = callback_to_cc(Callback),
    '>>='(Monad, CC).

then(Monad, Callback, Offset, State) ->
    exec(Monad, Callback, Offset, State).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
