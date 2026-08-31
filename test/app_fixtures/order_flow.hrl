-behaviour(gen_server).
-define(ASYNC_DO, true).
-include_lib("async/include/async.hrl").
-export([start_link/0, start/3, start_nested/4, status/0, version/0, marker/0,
    complete/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-record(state, {callbacks = #{}, completions = [], errors = []}).

start_link() -> gen_server:start_link({local, order_worker}, ?MODULE, [], []).
start(Controller, Tag, Observer) ->
    gen_server:call(order_worker, {start, Controller, Tag, Observer}).
start_nested(Controller, Tag, ChildRequest, Observer) ->
    gen_server:call(order_worker, {nested, Controller, Tag, ChildRequest, Observer}).
status() -> gen_server:call(order_worker, status).
version() -> ?VSN.
marker() -> ?MARKER.

init([]) -> {ok, #state{}}.
handle_call({start, Controller, Tag, Observer}, _From, State) ->
    MRef = async_gen_server:call(Controller, {order, ?VSN, Tag}),
    mark({request, order}),
    {_Started, NewState} = async_m:run_sf_frame(action(MRef), cc(Observer, Tag),
        #state.callbacks, State),
    {reply, {ok, MRef}, NewState};
handle_call({nested, Controller, Tag, ChildRequest, Observer}, _From, State) ->
    MRef = async_gen_server:call(Controller, {order, ?VSN, Tag}),
    ParentRef = make_ref(),
    {ok, ChildMRef} = payment_flow:start_child(
        Controller, ChildRequest, {self(), ParentRef}, Observer),
    mark({request, order}),
    {_Started, NewState} = async_m:run_sf_frame(nested_action(MRef, ParentRef),
        cc(Observer, Tag), #state.callbacks, State),
    {reply, {ok, #{order => MRef, payment => ChildMRef, parent => ParentRef}},
        NewState};
handle_call(status, _From, #state{callbacks = Callbacks, completions = Done,
        errors = Errors} = State) ->
    {reply, #{version => ?VSN, marker => ?MARKER, owner => order_worker,
        pid => self(), pending => map_size(Callbacks), completions => lists:reverse(Done),
        completion_count => length(Done), errors => lists:reverse(Errors),
        counters => counters()}, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info({order_complete, Tag, Outcome}, #state{completions = Done,
        errors = Errors} = State) ->
    NewErrors = case Outcome of {error, _} -> [Outcome | Errors]; _ -> Errors end,
    {noreply, State#state{completions = [{Tag, Outcome} | Done], errors = NewErrors}};
handle_info(Info, State) ->
    {_Result, NewState} = async_m:run_info(Info, #state.callbacks, State),
    {noreply, NewState}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

action(MRef) when is_reference(MRef) -> action([MRef]);
action(MRefs) ->
    mark({action, order}),
    sf_do(order_flow, [async_m || wait,
        Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
        Resume <- async_m:pure_return(mark({resume, order})),
        return(order_result(MRefs, Replies, Resume))]).

nested_action(MRef, ParentRef) ->
    nested_action([MRef, ParentRef]).
nested_action(MRefs) ->
    mark({action, order}),
    sf_do(order_nested_flow, [async_m || wait,
        Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
        Resume <- async_m:pure_return(mark({resume, order})),
        return(order_result(MRefs, Replies, Resume))]).

order_result(_MRefs,
        [OrderReply, {ok, {payment, _, {error, Reason}, _}}], _Resume) ->
    {error, {child, Reason, OrderReply}};
order_result(_MRefs, [OrderReply, {error, Reason}], _Resume) ->
    {error, {child, Reason, OrderReply}};
order_result(_MRefs, {ok, {payment, _, {error, Reason}, _}} = Child, Resume) ->
    put(order_child_error, Reason), {order, ?VSN, Child, Resume};
order_result(_MRefs, OrderReply, Resume) ->
    case erase(order_child_error) of
        undefined -> {order, ?VSN, OrderReply, Resume};
        Reason -> {error, {child, Reason, OrderReply}}
    end.

complete(Outcome, {Pid, Observer, Tag}) ->
    Observer ! {order_complete, Tag, Outcome},
    Pid ! {order_complete, Tag, Outcome},
    async_r_t:return(Outcome, async_r_t:new(identity)).
cc(Observer, Tag) -> #{kind => sf_cc, protocol => 1,
    mfa => {?MODULE, complete, 2}, context => {self(), Observer, Tag}}.
mark(Key) -> put(Key, 1 + case get(Key) of undefined -> 0; N -> N end), Key.
counters() -> maps:from_list([{K, V} || {K, V} <- get(), is_tuple(K),
    tuple_size(K) =:= 2]).
