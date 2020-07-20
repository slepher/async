%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(async_rpc).

%% API
-export([promise_call/4, promise_wait/2, remote_promise_wait/3]).

%%%===================================================================
%%% API
%%%===================================================================
promise_call(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args) of
        {async_t, _} = Promise ->
            promise_wait(Node, Promise);
        Value ->
            Value
    end.

promise_wait(Node, Promise) ->
    Ref = make_ref(),
    Self = self(),
    rpc:call(Node, async_rpc, remote_promise_wait, [Promise, Ref, Self]),
    async_m:promise(Ref).

remote_promise_wait(Promise, Ref, Self) ->
    spawn(
      fun() ->
              Callback = 
                  fun({message, Message}) ->
                          Self ! {message, Ref, Message};
                     (Reply) ->
                          Self ! {Ref, Reply}
                  end,
              try 
                  async_m:wait_t(Promise, #{callback => Callback})
              catch
                  Class:Exception:StackTrace ->
                      Self ! {Ref, {error, {Class, Exception, StackTrace}}}
              end
      end).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
