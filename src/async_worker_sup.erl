%%%-------------------------------------------------------------------
%%% @doc Dynamic supervisor for short-lived async workers.
%%% @end
%%%-------------------------------------------------------------------
-module(async_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    ChildSpec = #{
        id => {async_worker, make_ref()},
        start => {async_worker, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [async_worker]
    },
    supervisor:start_child(?SERVER, ChildSpec).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },
    {ok, {SupFlags, []}}.
