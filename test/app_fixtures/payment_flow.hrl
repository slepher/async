-behaviour(gen_server).
-define(ASYNC_DO, true).
-include_lib("async/include/async.hrl").
-export([start_link/0, start/3, start_child/4, status/0, version/0, marker/0,
    complete/2, convert_payment/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-record(state, {callbacks = #{}, completions = [], errors = [], observers = #{}}).

start_link() -> gen_server:start_link({local, payment_worker}, ?MODULE, [], []).
start(Controller, Tag, Observer) ->
    gen_server:call(payment_worker, {start, Controller, Tag, none, Observer}).
start_child(Controller, Request, Parent, Observer) ->
    gen_server:call(payment_worker, {start, Controller, Request, Parent, Observer}).
status() -> gen_server:call(payment_worker, status).
version() -> ?VSN.
marker() -> ?MARKER.

init([]) -> {ok, #state{}}.
handle_call({start, Controller, Tag, Parent, Observer}, _From, State) ->
    MRef = async_gen_server:call(Controller, {payment, ?VSN, Tag}),
    mark({request, payment}),
    FrameState = State#state{observers = maps:put(Tag, Observer,
        State#state.observers)},
    {_Started, NewState} = async_m:run_sf_frame(action(MRef, child_mode(Tag)),
        cc(Observer, Tag, Parent), #state.callbacks, FrameState),
    {reply, {ok, MRef}, NewState};
handle_call(status, _From, #state{callbacks = Callbacks, completions = Done,
        errors = Errors} = State) ->
    {reply, #{version => ?VSN, marker => ?MARKER, owner => payment_worker,
        pid => self(), pending => map_size(Callbacks), completions => lists:reverse(Done),
        completion_count => length(Done), errors => lists:reverse(Errors),
        counters => counters()}, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info({payment_complete, Tag, Outcome, Parent}, #state{completions = Done,
        errors = Errors, observers = Observers} = State) ->
    NewErrors = case Outcome of {error, _} -> [Outcome | Errors]; _ -> Errors end,
    NewState = State#state{completions = [{Tag, Outcome} | Done],
        errors = NewErrors, observers = maps:remove(Tag, Observers)},
    case maps:find(Tag, Observers) of
        {ok, Observer} ->
            self() ! {emit_payment_complete, Observer, Tag, Outcome, Parent};
        error -> ok
    end,
    {noreply, NewState};
handle_info({emit_payment_complete, Observer, Tag, Outcome, Parent}, State) ->
    Observer ! {payment_complete, Tag, Outcome},
    case Parent of
        none -> ok;
        {ParentPid, ParentRef} -> ParentPid ! {ParentRef, Outcome}
    end,
    {noreply, State};
handle_info(Info, State) ->
    {_Result, NewState} = async_m:run_info(Info, #state.callbacks, State),
    {noreply, NewState}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

action(MRef) -> action(MRef, raw).
action(MRef, child_converter) ->
    mark({action, payment}),
    sf_do(payment_nested_converter_flow, [async_m || wait,
        Reply <- async:promise_mref(MRef),
        Resume <- async_m:pure_return(mark({resume, payment})),
        return({payment, ?VSN, Reply, Resume})]);
action(MRef, raw) ->
    mark({action, payment}),
    sf_do(payment_flow, [async_m || wait,
        Reply <- async:promise_mref(MRef),
        Resume <- async_m:pure_return(mark({resume, payment})),
        return({payment, ?VSN, Reply, Resume})]).

complete(Outcome, {Pid, Observer, Tag, Parent}) ->
    _ = Observer,
    Pid ! {payment_complete, Tag, Outcome, Parent},
    async_r_t:return(Outcome, async_r_t:new(identity)).
cc(Observer, Tag, Parent) -> #{kind => sf_cc, protocol => 1,
    mfa => {?MODULE, complete, 2}, context => {self(), Observer, Tag, Parent}}.
child_mode({child, n07}) -> child_converter;
child_mode(_) -> raw.
convert_payment(Outcome, Env) ->
    mark({converter, payment}), {ok, Outcome, Env}.
mark(Key) -> put(Key, 1 + case get(Key) of undefined -> 0; N -> N end), Key.
counters() -> maps:from_list([{K, V} || {K, V} <- get(), is_tuple(K),
    tuple_size(K) =:= 2]).
