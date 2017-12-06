%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(async_channel).

-behaviour(gen_server).

%% API
-export([call/5]).
-export([process_name/1]).
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {callbacks = maps:new(), pendings = [], working_pids = [], parent_pids = [], pool_size}).

%%%===================================================================
%%% API
%%%===================================================================
call(Channel, Pid, Label, Request, Timeout) ->
    PName = process_name(Channel),
    async_gen_server:promise_call(PName, {call, Pid, Label, Request}, Timeout).

process_name(Channel) ->
    list_to_atom(lists:flatten(["async_channel_", atom_to_list(Channel)])).

start(Channel, PoolSize) ->
    supervisor:start_child(async_channel_sup, [Channel, PoolSize]).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Channel, PoolSize) ->
    PName = process_name(Channel),
    gen_server:start_link({local, PName}, ?MODULE, [PoolSize], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([PoolSize]) ->
    {ok, #state{pool_size = PoolSize}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call, Pid, Label, Request}, From, #state{pendings = Pendings} = State) ->
    NPendings = Pendings ++ [{Pid, Label, Request, From}],
    NState = process_next_request(State#state{pendings = NPendings}),
    {noreply, NState};
handle_call(Request, _From, State) ->
    {reply, {error, {invalid_request, Request}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    case async_m:handle_info(Info, #state.callbacks, State) of
        unhandled ->
            error_logger:error_msg("unexpected info msg ~p", [Info]),
            {noreply, State};
        NState when is_record(NState, state) ->
            {noreply, NState}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_next_request(#state{pendings = [{Pid, Label, Request, {ParentPid, _Tag} = From}|T],
                            working_pids = WorkingPids, parent_pids = ParentPids,
                            pool_size = PoolSize} = State) ->
    
    NParentPids = 
        case ordsets:is_element(ParentPid, WorkingPids) of
            true ->
                ordsets:add_element(ParentPid, ParentPids);
            false ->
                ParentPids
        end,
    NWorkingPids = ordsets:add_element(Pid, WorkingPids),
    WorkingLen = length(ordsets:subtract(NWorkingPids, NParentPids)),  
    case WorkingLen > PoolSize of
        true ->
            State;
        false ->
            NState = State#state{pendings = T, working_pids = NWorkingPids, parent_pids = NParentPids},
            process_request(Pid, Label, Request, From, NState)
    end;
process_next_request(State) ->
    State.

process_request(Pid, Label, Request, From, State) ->
    MRef = async:call(Pid, Label, Request),
    Monad = async_m:promise(MRef),
    async_m:exec(
      Monad,
      fun({message, M}, S) ->
              async:message(From, M),
              S;
         (Reply, S) ->
              gen:reply(From, Reply),
              process_next_request(S)
      end, #state.callbacks, State).
    
