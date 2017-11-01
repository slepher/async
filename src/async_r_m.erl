%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_r_m).

-behaviour(functor).
-behaviour(monad).

-compile({parse_transform, monad_t_transform}).

-transform({async_r_t, [fmap/2, '<$'/2, '>>='/2, '>>'/2]}).
-transform({async_r_t, true, [return/1]}).
-transform({async_r_t, true, [do_get_state/0, do_put_state/1, do_modify_state/1]}).
-transform({async_r_t, true, [get_state/0, put_state/1, modify_state/1]}).
-transform({async_r_t, true, [get_local_ref/0, local_ref/1, local/1, get_local/0, put_local/1, modify_local/1]}).
-transform({async_r_t, true, [find_ref/1, get_ref/2, modify_ref/2, put_ref/2, remove_ref/1]}).
-transorrm({async_r_t, true, [], [eval/4, exec/4, run/4, map/2]}).
