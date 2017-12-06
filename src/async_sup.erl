%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(async_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    AWSup  =  #{id => async_worker_sup,
                start => {supervisor, start_link,
                          [{local, async_worker_sup}, ?MODULE, [async_worker_sup]]},
                restart => transient,
                shutdown => infinity,
                type =>  supervisor,
                modules => []},
    ACWSup = #{id => async_channel_worker_sup,
               start => {supervisor, start_link, 
                         [{local, async_channel_sup}, 
                          ?MODULE, [async_channel_sup]]},
               restart => transient,
               shutdown => infinity,
               type => supervisor,
               modules => []},
    {ok, {SupFlags, [AWSup, ACWSup]}};

init([async_worker_sup]) ->
    {ok,
     {#{strategy => simple_one_for_one},
      [#{id => undefined, start => {async_worker, start_link, []}, 
         restart => temporary}]
     }};
init([async_channel_sup]) ->
    {ok, 
     {#{strategy => simple_one_for_one},
      [#{id => undefined, start => {async_channel, start_link, []}}]
     }}.
              
           


%%%===================================================================
%%% Internal functions
%%%===================================================================
