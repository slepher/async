%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m).

-behaviour(functor).
-behaviour(monad).

-compile({parse_transform, monad_t_transform}).

-transform({async_t, [fmap/2, '<$'/2, '>>='/2, '>>'/2, lift_mr/1, callCC/1]}).
-transform({async_t, [lift_reply/1, lift_final_reply/1, hijack/1, par/1, progn_par/1, run_cc/2]}).
-transform({async_t, identity, [return/1, fail/1]}).
-transform({async_t, identity, [get_state/0, put_state/1, modify_state/1, find_ref/1, get_ref/2, put_ref/2, remove_ref/1,
                            get_local/0, put_local/1, modify_local/1, local_ref/2, local/2, get_local_ref/0]}).
-transform({async_t, identity, [pure_return/1, wrapped_return/1, message/1, pass/0, handle_message/2, provide_message/2]}).
-transform({async_t, identity, [promise/1, promise/2, map/1, map/2]}).
-transform({async_t, identity, identity_run, [wait/1, wait/2, wait/3, wait/4, wait/5, wait_cc/5]}).
-transform({async_t, identity, identity_run, [exec/4, exec_cc/4, run/4, run_with_cc/4]}).
-transform({async_t, identity, identity_run, [handle_info/3, run_info/3, wait_receive/3]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
