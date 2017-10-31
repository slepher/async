%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(async_r_t_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-compile({parse_transform, do}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-record(state, {callbacks = maps:new(), value}).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% @doc
%% Initialization before the suite.
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    async:start(),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the suite.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a description of the test suite when
%% Clause == doc, and a test specification (list
%% of the conf and test cases in the suite) when
%% Clause == suite.
%% Returns a list of all test cases in this test suite
%%
%% Clause = doc | suite
%%   Indicates expected return value.
%% Descr = [string()] | []
%%   String that describes the test suite.
%% Spec = [TestCase]
%%   A test specification.
%% TestCase = ConfCase | atom()
%%   Configuration case, or the name of a test case function.
%% ConfCase = {conf,Init,Spec,End} |
%%            {conf,Properties,Init,Spec,End}
%% Init = End = {Mod,Func} | Func
%%   Initialization and cleanup function.
%% Mod = Func = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Execution properties of the test cases (may be combined).
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%% Reason = term()
%%   The reason for skipping the test suite.
%%
%% @spec all(Clause) -> TestCases
%% @end
%%--------------------------------------------------------------------

all() -> 
    [test_ask, test_do_get, test_do_put, test_get, test_put,
     test_get_local_ref, test_get_local, test_get_ref, test_put_ref].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Test case function. Returns a description of the test
%%  case (doc), then returns a test specification (suite),
%%  or performs the actual test (Config).
%%
%% Arg = doc | suite | Config
%%   Indicates expected behaviour and return value.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Descr = [string()] | []
%%   String that describes the test case.
%% Spec = [tuple()] | []
%%   A test specification, see all/1.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% @spec TestCase(Arg) -> Descr | Spec | ok | exit() | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
test_ask(doc) -> 
    ["Describe the main purpose of this test case"];

test_ask(suite) -> 
    [];

test_ask(Config) when is_list(Config) ->
    GS = async_t:state_callbacks_gs(2),
    State = #state{},
    MR = async_r_t:new(identity),
    M = async_r_t:ask(MR),
    Result = async_r_t:eval(M, GS, 0, State),
    ?assertEqual({identity, GS}, Result),
    ok.

test_do_get(doc) -> 
    ["Describe the main purpose of this test case"];

test_do_get(suite) -> 
    [];

test_do_get(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    State = #state{},
    MR = async_r_t:new(identity),
    M = async_r_t:do_get_state(MR),
    Result = async_r_t:eval(M, GS, 0, State),
    ?assertEqual({identity, State}, Result).

test_do_put(doc) -> 
    ["Describe the main purpose of this test case"];

test_do_put(suite) -> 
    [];

test_do_put(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    State = #state{},
    MR = async_r_t:new(identity),
    M = async_r_t:do_put_state(10, MR),
    Result = async_r_t:exec(M, GS, 0, State),
    ?assertEqual({identity, 10}, Result).
    
    
test_get(doc) -> 
    ["Describe the main purpose of this test case"];

test_get(suite) -> 
    [];

test_get(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    State = #state{},
    NState = State#state{value = 10},
    MR = async_r_t:new(identity),
    M = async_r_t:get_state(MR),
    Result = identity:run(async_r_t:eval(M, GS, 0, NState)),
    ?assertEqual(NState, Result).

test_put(doc) -> 
    ["Describe the main purpose of this test case"];

test_put(suite) -> 
    [];

test_put(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    State = #state{},
    NState = State#state{value = 10},
    MR = async_r_t:new(identity),
    M = async_r_t:put_state(NState, MR),
    Result = identity:run(async_r_t:exec(M, GS, 0, State)),
    ?assertEqual(NState, Result).

test_get_local_ref(doc) ->
    [];

test_get_local_ref(suite) ->
    [];

test_get_local_ref(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    State = #state{},
    Ref = make_ref(),
    MR = async_r_t:new(identity),
    M = async_r_t:get_local_ref(MR),
    Result = identity:run(async_r_t:eval(M, GS, Ref, State)),
    ?assertEqual(Ref, Result).

test_get_local(_Config) ->    
    GS = async_t:state_callbacks_gs(2),
    Ref = make_ref(),
    State = #state{callbacks = #{Ref => 1}},
    MR = async_r_t:new(identity),
    M = async_r_t:get_local(MR),
    Result = identity:run(async_r_t:eval(M, GS, Ref, State)),
    ?assertEqual(1, Result).
    
test_put_local(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    Ref = make_ref(),
    State = #state{},
    MR = async_r_t:new(identity),
    M = do([monad ||
               async_r_t:put_local(100, MR),
               async_r_t:get_local(MR)
           ]),
    Result = identity:run(async_r_t:eval(M, GS, Ref, State)),
    ?assertEqual(100, Result).
    
test_get_ref(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    Ref = make_ref(),
    Ref1 = make_ref(),
    State = #state{callbacks = #{Ref1 => 10, Ref => 100}},
    MR = async_r_t:new(identity),
    M = async_r_t:get_ref(Ref1, 0, MR),
    Result = identity:run(async_r_t:eval(M, GS, Ref, State)),
    ?assertEqual(10, Result).

test_put_ref(_Config) ->
    GS = async_t:state_callbacks_gs(2),
    Ref = make_ref(),
    Ref1 = make_ref(),
    State = #state{callbacks = #{Ref1 => 10, Ref => 100}},
    MR = async_r_t:new(identity),
    M = do([monad ||
               async_r_t:put_ref(Ref1, 20, MR),
               async_r_t:get_ref(Ref1, 0, MR)
           ]),
    Result = identity:run(async_r_t:eval(M, GS, Ref, State)),
    ?assertEqual(20, Result).
    
