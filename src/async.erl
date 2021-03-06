%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Jul 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async).

-include_lib("erlando/include/do.hrl").

%% API
-export([call/3, message/2, promise_action/2, start_and_action/3]).
-export([promise_mref/1, promise_mref/2]).
-export([then/2, exec/4]).
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:ensure_all_started(async).

start_and_action(StartFun, ActionFun, Args) ->
    case StartFun() of
        {ok, Server} ->
            apply(ActionFun, [Server|Args]);
        {error, Reason} ->
            {error, Reason}
    end.

%% Local or remote by pid
call(Pid, Label, Request) when is_pid(Pid) ->
    do_call(Pid, Label, Request);
%% Local by name
call(Name, Label, Request) when is_atom(Name) -> 
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            do_call(Pid, Label, Request);
        undefined ->
            {error, {noproc, Name}}
    end;
%% Global by name
call({global, _Name}=Process, Label, Request) ->
    case where(Process) of
        Pid when is_pid(Pid) ->
           do_call(Pid, Label, Request);
        undefined ->
            {error, noproc}
    end;
%% Local by name in disguise
call({Name, Node}, Label, Request) when Node =:= node() ->
    call(Name, Label, Request);
%% Remote by name
call({Name, Node}=Process, Label, Request) when is_atom(Name), is_atom(Node) ->
    do_call(Process, Label, Request);
call(Name, _Label, _Request) ->
    {error, {invalid_process_name, Name}}.

message({PId, MRef}, Message) ->
    catch PId ! {message, MRef, Message}.

promise_action(Action, Timeout) ->
    async_m:promise(Action, Timeout).

promise_mref(MRef) ->
    promise_mref(MRef, infinity).

promise_mref(MRef, Timeout) ->
    async_m:promise(MRef, Timeout).

then(MRef, Callback) when is_reference(MRef) ->
    async_m:then(promise_mref(MRef), Callback);

then({async_t, _} = Monad, Callback) ->
    async_m:then(Monad, Callback).

exec(MRef, Callback, Offset, State) when is_reference(MRef) ->
    async_m:exec(promise_mref(MRef), Callback, Offset, State);

exec({async_t, _} = Monad, Callback, Offset, State) ->
    async_m:exec(Monad, Callback, Offset, State).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
where({global, Name}) -> global:whereis_name(Name).

do_call(Process, Label, Request) ->
    Mref = erlang:monitor(process, Process),
    erlang:send(Process, {Label, {self(), Mref}, Request}, [noconnect]),
    Mref.
