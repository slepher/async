-module(test_async_controller).
-behaviour(gen_server).

-export([start/1, start_link/1, stop/1, make_reply/3, pending/1, counts/1,
    identity/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

start(Observer) -> gen_server:start(?MODULE, Observer, []).
start_link(Observer) -> gen_server:start_link(?MODULE, Observer, []).
stop(Server) -> gen_server:stop(Server).
make_reply(Server, MRef, Reply) -> gen_server:call(Server, {reply, MRef, Reply}).
pending(Server) -> gen_server:call(Server, pending).
counts(Server) -> gen_server:call(Server, counts).
identity(Server, MRef) -> gen_server:call(Server, {identity, MRef}).

init(Observer) -> {ok, #{observer => Observer, live => #{}, replied => #{}}}.

handle_call({reply, MRef, Reply}, _From,
        #{live := Live, replied := Replied} = State) ->
    case maps:take(MRef, Live) of
        {{Pid, MRef} = Held, Rest} ->
            gen_server:reply(Held, Reply),
            {reply, ok, State#{live := Rest, replied := Replied#{MRef => Pid}}};
        error when is_map_key(MRef, Replied) ->
            {reply, {error, already_replied}, State};
        error ->
            {reply, {error, unknown_mref}, State}
    end;
handle_call(pending, _From, #{live := Live} = State) ->
    {reply, lists:sort(maps:keys(Live)), State};
handle_call(counts, _From, #{live := Live, replied := Replied} = State) ->
    {reply, #{pending => map_size(Live), replied => map_size(Replied)}, State};
handle_call({identity, MRef}, _From,
        #{live := Live, replied := Replied} = State) ->
    Reply = case maps:find(MRef, Live) of
        {ok, Held} -> {pending, Held};
        error -> case maps:find(MRef, Replied) of
            {ok, Pid} -> {replied, {Pid, MRef}};
            error -> unknown
        end
    end,
    {reply, Reply, State};
handle_call(Request, {Pid, MRef}, #{observer := Observer, live := Live} = State) ->
    Observer ! {controller_held, MRef, Request},
    {noreply, State#{live := Live#{MRef => {Pid, MRef}}}}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
