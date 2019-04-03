%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  3 Apr 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(async_ext).

-erlando_type(?MODULE).

-include_lib("astranaut/include/astranaut.hrl").
-include_lib("erlando/include/gen_fun.hrl").
-include_lib("erlando/include/do.hrl").

-behaviour(monad).
-behaviour(monad_fail).

%% API
-export(['>>='/3, return/2, fail/2, ok/0]).
-export([async_do/1, format_error/1]).

-gen_fun(#{inner_type => monad, behaviours => [monad, monad_fail]}).

%%%===================================================================
%%% API
%%%===================================================================
'>>='({async_t, _} = AsyncM, K, {?MODULE, _IM}) ->
    NK = fun(A) ->
                 case K(A) of
                     {async_t, _} = Async ->
                         Async;
                     B ->
                         async_t:pure_return(B)
                 end
         end,
    async_t:'>>='(AsyncM, NK);
'>>='(Value, K, {?MODULE, _IM}) ->
    error_m:'>>='(Value, K).

return(Value, {?MODULE, _IM}) ->
    error_m:return(Value).

fail(Reason, {?MODULE, _IM}) ->
    error_m:fail(Reason).

ok() ->
    ok.

async_do({lc, Line, Monad, Comprehensions}) ->
    do_macro:do({lc, Line, quote({async_ext, unquote(Monad)}), Comprehensions});
async_do(_Ast) ->
    {error, expected_list_comprehension}.

format_error(Reason) ->
    astranaut_quote:format_error(Reason).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
