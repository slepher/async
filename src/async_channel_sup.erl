%%%-------------------------------------------------------------------
%%% @doc Dynamic supervisor for named async channels.
%%% @end
%%%-------------------------------------------------------------------
-module(async_channel_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Channel, PoolSize) ->
    ChildSpec = #{
        id => {async_channel, Channel},
        start => {async_channel, start_link, [Channel, PoolSize]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [async_channel]
    },
    supervisor:start_child(?SERVER, ChildSpec).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },
    {ok, {SupFlags, []}}.
