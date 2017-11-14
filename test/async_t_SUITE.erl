%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(async_t_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-compile({parse_transform, do}).
-compile({parse_transform, disable_tco}).

-record(state, {callbacks = maps:new(), acc0 = [], acc = []}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    async:start(),
    {ok, PId} = echo_server:start(),
    [{echo_server, PId}|Config].

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [test_async_t, test_chain_async, test_chain_async_fail, 
     test_async_t_with_timeout, 
     test_async_t_with_message, test_async_t_with_message_handler,
     test_async_t_par, test_async_t_pmap, test_async_t_pmap_with_acc, 
     test_async_t_pmap_with_timeout,
     test_local_acc_ref, test_async_t_local_acc_ref].

%% Test cases starts here.
%%--------------------------------------------------------------------

test_async_t() ->
    [{doc, "Describe the main purpose of this test case"}].
test_async_t(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    MRef = echo_server:echo(EchoServer, {ok, hello}),
    M0 = async_m:promise(MRef),
    Reply = async_m:wait(M0),
    ?assertEqual({ok, hello}, Reply).

test_chain_async() ->
    [{doc, "Describe the main purpose of this test case"}].
test_chain_async(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity),
    MRef = echo_server:echo(EchoServer, hello),
    M0 = do([Monad || 
                R1 <- async_m:promise(MRef),
                R2 <- async_m:promise(echo_server:echo(EchoServer, {ok, world})),
                return({R1, R2})
               ]),
    Reply = async_m:wait(M0),
    ?assertEqual({ok, {hello, world}}, Reply).

test_chain_async_fail() ->
    [{doc, "test fail in async_t"}].
test_chain_async_fail(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    MRef = echo_server:echo(EchoServer, {ok, hello}),
    M0 = do([async_m || 
                   R1 <- async_m:promise(MRef),
                   R2 <- async_m:promise(echo_server:echo(EchoServer, {error, world})),
                   R3 <- async_m:promise(echo_server:echo(EchoServer, hello)),
                   return({R1, R2, R3})
               ]),
    Reply = async_m:wait(M0),
    ?assertEqual({error, world}, Reply).

test_async_t_with_timeout() ->
    [{doc, "test async with timeout"}].

test_async_t_with_timeout(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity),
    MRef = echo_server:delayed_echo(EchoServer, 2000, hello),
    M0 = do([Monad || 
                   R1 <- async_m:promise(MRef),
                   R2 <- async_m:promise(echo_server:echo(EchoServer, {error, world})),
                   R3 <- async_m:promise(echo_server:echo(EchoServer, hello)),
                   return({R1, R2, R3})
               ]),
    Reply = async_m:wait_t(M0, #{timeout => 500}),
    ?assertEqual({error, timeout}, Reply).

test_async_t_with_message() ->
    [{doc, "test async_t with message"}].

test_async_t_with_message(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    MRef = echo_server:echo_with_messages(EchoServer, [message, message], hello),
    M0 = do([async_m || 
                   R1 <- async_m:promise(MRef),
                   R2 <- async_m:promise(echo_server:echo_with_messages(EchoServer, [message, message, message], world)),
                   return({R1, R2})
               ]),
    Reply = async_m:wait_t(M0,
                           #{callback =>
                                 fun({ok, R}, #state{acc = Acc}) ->
                                         [R, Acc];
                                    ({message, Message}, #state{acc = Acc} = State)->
                                         NAcc = [Message|Acc],
                                         State#state{acc = NAcc}
                                 end, offset => #state.callbacks, state => #state{}}),
    ?assertEqual([{hello, world}, lists:duplicate(5, message)], Reply).

test_async_t_with_message_handler() ->
    [{doc, "test async_t with message_handler"}].

test_async_t_with_message_handler(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    MRef = echo_server:echo_with_messages(EchoServer, [message, message], hello),
    M0 = do([async_m || 
                R1 <- async_m:promise(MRef),
                R2 <- async_m:handle_message(
                           do([async_m ||
                                  async_m:promise(echo_server:echo_with_messages(
                                                  EchoServer, lists:duplicate(5, message), world)),
                                  async_m:promise(echo_server:echo_with_messages(
                                                  EchoServer, lists:duplicate(3, message), world))
                              ]),
                           fun(Message, #state{acc0 = Acc0} = State) ->
                                   State#state{acc0 = [Message|Acc0]}
                           end),
                R3 <- async_m:promise(
                           echo_server:echo_with_messages(
                             EchoServer, lists:duplicate(3, message), world)),
                return({R1, R2, R3})
               ]),
    Reply = async_m:wait_t(M0,
                         #{callback =>
                               fun({ok, R}, #state{acc0 = Acc0, acc = Acc}) ->
                                       {R, Acc0, Acc};
                                  ({message, Message}, #state{acc = Acc} = State)->
                                       NAcc = [Message|Acc],
                                       State#state{acc = NAcc}
                               end, offset => #state.callbacks, state => #state{}}),
    ?assertEqual({{hello, world, world}, lists:duplicate(8, message), lists:duplicate(5, message)}, Reply).

test_async_t_par(_Config) ->
    MR = async_r_t:new(identity),
    M1 = async_t:progn_par(
                  [async_m:message(hello_message),
                   async_m:fail(hello)]),
    Reply = async_m:wait_t(M1,
                         #{callback =>
                               fun({message, M}) ->
                                       do([MR ||
                                              MR:put_local(M),
                                              MR:return(ok)
                                          ]);
                                  (Reply) ->
                                       do([MR ||
                                              Acc <- MR:get_local(),
                                              MR:return({Acc, Reply})
                                          ])
                               end}),
    ?assertEqual({hello_message, {error, hello}}, Reply).

test_async_t_pmap(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity),
    MR = async_r_t:new(identity),
    M0 = async_m:promise(fun() -> echo_server:echo_with_messages(EchoServer, [message], {error, hello}) end),
    Promises = lists:duplicate(6, M0),
    M1 = do([Monad ||
                async_m:put_local([]),
                async_m:map_promises(Promises)
            ]),
    Reply = async_m:wait_t(
              M1, 
              #{callback =>
                    fun({message, X}) -> 
                            do([MR ||
                                   Acc <- MR:get_local(),
                                   MR:put_local([X|Acc])
                               ]);
                       (X) ->
                            do([MR ||
                                   Acc <- MR:get_local(),
                                   return({X, Acc})
                               ])
                    end
               }),
    ?assertEqual({lists:duplicate(6, {error, hello}), lists:duplicate(6, message)}, Reply).

test_async_t_pmap_with_timeout(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    M0 = async_m:promise(fun() -> echo_server:delayed_echo(EchoServer, 2000, hello) end),
    Promises = lists:duplicate(6, M0),
    M1 = do([async_m ||
                async_m:put_local([]),
                async_m:map_promises(Promises)
            ]),
    Reply = async_m:wait_t(M1, #{timeout => 1000}),
    ?assertEqual(lists:duplicate(6, {error, timeout}), Reply).
                               
test_async_t_pmap_with_acc(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity),
    M0 =  async_m:promise(fun() -> echo_server:echo_with_messages(EchoServer,  [message], {error, hello}) end),
    Promises = lists:foldl(
                 fun(N, Acc0) ->
                         MA = 
                             do([Monad || 
                                    Val <- async_t:lift_final_reply(M0),
                                    Acc <- async_m:get_local(),
                                    async_m:put_local([N|Acc]),
                                    async_m:pure_return(Val)
                                ]),
                         maps:put(N, MA, Acc0)
                 end, maps:new(), lists:seq(1, 5)),
    M1 = do([Monad ||
                async_m:put_local([]),
                async_m:map_promises_t(Promises, #{limit => 2})
            ]),
    MR = async_r_t:new(identity),
    Reply =  async_m:wait_t(
               M1, 
               #{callback =>
                     fun({message, X}) -> 
                             do([MR ||
                                    Acc <- MR:get_local(),
                                    MR:put_local([X|Acc])
                                ]);
                        (X) ->
                             do([MR ||
                                    Acc <- MR:get_local(),
                                    return({X, Acc})
                                ])
                     end
                }),
    ?assertEqual({maps:from_list([{1, {error, hello}},
                                  {2, {error, hello}},
                                  {3, {error, hello}},
                                  {4, {error, hello}},
                                  {5, {error, hello}}
                                 ]), 
                  [5, {5, message}, 4, {4, message}, 3, {3, message}, 2, {2, message}, 1, {1, message}]}, Reply).
                               
test_local_acc_ref(_Config) ->
    Ref0 = make_ref(),
    Ref1 = make_ref(),
    M0 = do([async_r_m ||
                Ref <- async_r_m:get_local_ref(),
                async_r_m:do_put_state(Ref)
            ]),
    M1 = async_r_t:local_ref(Ref1, M0),
    M2 = do([async_r_m ||
                R1 <- async_r_m:local_ref(Ref1, async_r_m:get_local_ref()),
                R0 <- async_r_m:get_local_ref(),
                async_r_m:do_put_state({R0, R1})
            ]),
    ?assertEqual(Ref0, async_r_m:exec(M0, undefined, Ref0, undefined)),
    ?assertEqual(Ref1, async_r_m:exec(M1, undefined, Ref0, undefined)),
    ?assertEqual({Ref0, Ref1}, async_r_m:exec(M2, undefined, Ref0, undefined)).

test_async_t_local_acc_ref(_Config) ->
    Ref = make_ref(),
    M0 = do([async_m ||
                Ref0 <- async_m:get_local_ref(),
                async_m:pure_return(Ref0)
            ]),
    M1 = async_m:local_ref(Ref, M0),
    M2 = do([async_m ||
                Ref0 <- M0,
                Ref1 <- M1,
                Ref2 <- async_m:get_local_ref(),
                async_m:pure_return({Ref0, Ref1, Ref2})
            ]),
    {{R0, R1, R2}, R3} = async_m:wait_t(
                           M2, 
                           #{callback => 
                                 fun(X) -> 
                                         do([async_r_m || 
                                                MRRef <- async_r_m:get_local_ref(),
                                                return({X, MRRef})])
                                 end}),
    ?assertEqual(Ref, R1),
    ?assertEqual(R0, R2),
    ?assertEqual(R0, R3).
