-module(async_sup_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        supervision_tree,
        temporary_worker,
        permanent_channel
    ].

init_per_suite(Config) ->
    {ok, _} = async:start(),
    Config.

end_per_suite(_Config) ->
    ok.

supervision_tree(_Config) ->
    Children = supervisor:which_children(async_sup),
    ?assertMatch(
        {async_worker_sup, WorkerSup, supervisor, [async_worker_sup]}
            when is_pid(WorkerSup),
        lists:keyfind(async_worker_sup, 1, Children)
    ),
    ?assertMatch(
        {async_channel_sup, ChannelSup, supervisor, [async_channel_sup]}
            when is_pid(ChannelSup),
        lists:keyfind(async_channel_sup, 1, Children)
    ),
    {ok, {SupFlags, _ChildSpecs}} = async_sup:init([]),
    ?assertMatch(
        #{strategy := one_for_one, intensity := 10, period := 5},
        SupFlags
    ).

temporary_worker(_Config) ->
    Before = supervisor:count_children(async_worker_sup),
    {ok, Worker} = async_worker:start(),
    ?assertEqual(proplists:get_value(active, Before) + 1,
                 proplists:get_value(active,
                                     supervisor:count_children(async_worker_sup))),
    Monitor = erlang:monitor(process, Worker),
    exit(Worker, shutdown),
    receive
        {'DOWN', Monitor, process, Worker, shutdown} ->
            ok
    end,
    await(fun() ->
        supervisor:count_children(async_worker_sup) =:= Before
    end).

permanent_channel(_Config) ->
    Channel = async_sup_suite_channel,
    ChildId = {async_channel, Channel},
    {ok, ChannelPid} = async_channel:start(Channel, 1),
    Monitor = erlang:monitor(process, ChannelPid),
    exit(ChannelPid, kill),
    receive
        {'DOWN', Monitor, process, ChannelPid, killed} ->
            ok
    end,
    {ok, RestartedPid} = await(fun() ->
        case whereis(async_channel:process_name(Channel)) of
            Pid when is_pid(Pid), Pid =/= ChannelPid ->
                {ok, Pid};
            _ ->
                false
        end
    end),
    ?assert(is_process_alive(RestartedPid)),
    ok = supervisor:terminate_child(async_channel_sup, ChildId),
    ok = supervisor:delete_child(async_channel_sup, ChildId).

await(Check) ->
    await(Check, 100).

await(_Check, 0) ->
    error(timeout);
await(Check, Attempts) ->
    case Check() of
        false ->
            timer:sleep(10),
            await(Check, Attempts - 1);
        Result ->
            Result
    end.
