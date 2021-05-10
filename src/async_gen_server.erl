%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_gen_server).

%% API
-export([call/2]).
-export([promise_call/2, promise_call/3]).
-export([promise_channel_call/4]).
-export([reply_all/2, default_callback/1]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
call(Process, Request) ->
    async:call(Process, '$gen_call', Request).

promise_call(Name, Request) ->
    promise_call(Name, Request, infinity).

promise_call(Name, Request, Timeout) ->
    async:promise_action(
      fun() ->
              call(Name, Request)
      end, Timeout).

promise_channel_call(Channel, Name, Request, Timeout) ->
    async_channel:call(Channel, Name, '$gen_call', Request, Timeout).

reply_all(Promise, From) ->
     async_m:handle_all(
       Promise,
       fun({message, Message}) ->
               async:message(From, Message),
               ok;
          (Reply) ->
               gen_server:reply(From, Reply),
               ok
       end).

default_callback(From) ->
    fun({message, Message}) ->
            async:message(From, Message);
       (Reply) ->
            gen_server:reply(From, Reply)
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
