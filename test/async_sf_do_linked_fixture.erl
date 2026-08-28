-module(async_sf_do_linked_fixture).
-vsn({2, 4, 0}).

-include_lib("erlando/include/do.hrl").
-define(ASYNC_DO, true).
-include_lib("async/include/async.hrl").

-export([immediate/1, suspended/1, two_boundaries/2, nested/1,
    ordinary_bridge/1, capture/3, final_action/1, complete/2,
    remote_return/1, return/1]).

immediate(Value) ->
    sf_do(linked_immediate, [async_m
        || first,
           A <- async_m:pure_return(Value),
           second,
           B <- async_m:pure_return(A + 1),
           return({A, B})
    ]).

suspended(Ref) ->
    sf_do(linked_suspend, [async_m
        || wait,
           Reply <- async_m:promise(Ref),
           return({linked, Reply})
    ]).

two_boundaries(FirstRef, SecondRef) ->
    sf_do(linked_two, [async_m
        || first,
           A <- async_m:promise(FirstRef),
           second,
           B <- async_m:promise(SecondRef),
           return({A, B})
    ]).

nested(Ref) ->
    sf_do(linked_outer, [async_m
        || inner,
           Value <- sf_do(linked_inner, [async_m
               || wait,
                  Reply <- async_m:promise(Ref),
                  return({inner, Reply})
           ]),
           return({outer, Value})
    ]).

ordinary_bridge(Ref) ->
    sf_do(linked_bridge, [async_m
        || stable_wait,
           Value <- async_m:promise(Ref),
           async_m:'>>='(async_m:pure_return(Value),
               fun(V) -> async_m:pure_return({ordinary, V}) end)
    ]).

capture(Used, _Unused, Ref) ->
    sf_do(linked_capture, [async_m
        || wait,
           Reply <- async_m:promise(Ref),
           return({Used, Reply})
    ]).

final_action(Value) ->
    sf_do(linked_final, [async_m || async_m:pure_return(Value)]).

remote_return(Value) -> {remote_return, Value}.
return(Value) -> {outside_return, Value}.

complete(Outcome, {Pid, Token}) ->
    Pid ! {sf_do_linked_complete, Token, Outcome},
    async_r_t:return(Outcome, async_r_t:new(identity)).
