%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m_v4).

-erlando_type(?MODULE).

-behaviour(monad).

%% API
-export(['>>='/3, '>>'/3, return/2, fail/2]).

%%%===================================================================
%%% API
%%%===================================================================
async_r_m(Inner) ->
    {?MODULE, Inner}.

'>>='(X, Fun, ?MODULE) ->
    then(X, Fun).

'>>'(Xa, Xb, ?MODULE) ->
    monad:'default_>>'(Xa, Xb, ?MODULE).

return(A, ?MODULE) ->
    async_r_m(fun(Callback) -> Callback({ok, A}) end).

fail(R, ?MODULE) ->
    fun(Callback) -> Callback({error, R}) end.

then(Promise, Then) ->
    fun(Callback) ->
            Promise(
              fun({error, Reason}) ->
                      Callback({error, Reason});
                 ({ok, Reply}) ->
                      NPromise = Then(Reply),
                      NPromise(fun(NReply) -> Callback(NReply) end);
                 (Reply) ->
                      NPromise = Then(Reply),
                      NPromise(fun(NReply) -> Callback(NReply) end)
              end)
    end.
