-module(async_review_a_fixture).
-vsn(1).
-include_lib("erlando/include/do.hrl").
-define(ASYNC_DO, true).
-include_lib("async/include/async.hrl").
-export([stable/1, ordinary/1, callback/0]).

stable(Action) ->
    sf_do(review, [async_m || step, Value <- Action, return(Value)]).

ordinary(Action) ->
    do([async_m || Value <- Action, return(Value)]).

callback() -> fun(_Outcome, State) -> State end.
