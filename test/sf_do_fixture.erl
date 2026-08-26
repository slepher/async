%%% Public source fixture for async_sf_SUITE.  Keep assertions in the suite;
%%% this module exists to make the compiler-generated API observable.
-module(sf_do_fixture).

-include_lib("erlando/include/do.hrl").
-define(ASYNC_DO, true).
-include_lib("async/include/async.hrl").

-export([
    anonymous/1,
    binary_size_capture/2,
    case_equality/2,
    capture/3,
    complete/2,
    classic_ordered/2,
    final_action/1,
    immediate/1,
    fun_shadow/2,
    migration_sentinel/2,
    map_key_capture/2,
    map_key_shadow_capture/2,
    named/1,
    named_fun_shadow/2,
    nested/1,
    nested_monads/2,
    ordered/2,
    pattern/1,
    pure_badmatch/1,
    remote_call/1,
    remote_return/1,
    receive_equality/2,
    return/1,
    suspended/1,
    shadow/2,
    then_action/1,
    try_equality/2,
    unnamed_suspended/1
]).

named(Value) ->
    sf_do(named_flow, [async_m
        || first,
           A <- async_m:pure_return(Value),
           second,
           async_m:pure_return(ignored),
           async_m:pure_return({named, A})
    ]).

anonymous(Value) ->
    sf_do([async_m
        || A <- async_m:pure_return(Value),
           async_m:pure_return({anonymous, A})
    ]).

immediate(Value) ->
    sf_do(immediate_flow, [async_m
        || first,
           A <- async_m:pure_return(Value),
           second,
           B <- async_m:pure_return(A + 1),
           return({A, B})
    ]).

ordered(First, Second) ->
    erase(sf_do_order),
    Action1 = order_action(first, First),
    Action2 = order_action(second, Second),
    sf_do(order_flow, [async_m
        || first,
           A <- Action1,
           second,
           B <- Action2,
           return({A, B, lists:reverse(get(sf_do_order))})
    ]).

classic_ordered(First, Second) ->
    erase(sf_do_order),
    Action1 = order_action(first, First),
    Action2 = order_action(second, Second),
    do([async_m
        || A <- Action1,
           B <- Action2,
           return({A, B, lists:reverse(get(sf_do_order))})
    ]).

order_action(Tag, Value) ->
    put(sf_do_order, [Tag | case get(sf_do_order) of undefined -> []; L -> L end]),
    async_m:pure_return(Value).

then_action(Value) ->
    sf_do(then_flow, [async_m
        || first,
           async_m:pure_return(discard_me),
           async_m:pure_return(Value)
    ]).

final_action(Value) ->
    sf_do(final_flow, [async_m || async_m:pure_return(Value)]).

suspended(Ref) ->
    sf_do(suspended_flow, [async_m
        || wait,
           Reply <- async_m:promise(Ref),
           return({resumed, Reply})
    ]).

nested(Ref) ->
    sf_do(outer, [async_m
        || inner_result,
           Inner <- sf_do(inner, [async_m
               || wait,
                  Reply <- async_m:promise(Ref),
                  return({inner, Reply})
           ]),
           return({outer, Inner})
    ]).

nested_monads(Outer, Inner) ->
    sf_do(monad_outer, [Outer
        || inner_result,
           InnerValue <- sf_do(monad_inner, [Inner
               || return(inner_value)
           ]),
           return({outer_value, InnerValue})
    ]).

capture(Used, _Unused, Ref) ->
    sf_do(capture_flow, [async_m
        || wait,
           Reply <- async_m:promise(Ref),
           return({Used, Reply})
    ]).

map_key_capture(Key, Action) ->
    sf_do(map_key_capture, [async_m
        || match_map,
           #{Key := Value} <- Action,
           return(Value)
    ]).

map_key_shadow_capture(Key, Action) ->
    sf_do(map_key_shadow_capture, [async_m
        || match_map,
           #{Key := Key} <- Action,
           return(Key)
    ]).

binary_size_capture(Size, Action) ->
    sf_do(binary_size_capture, [async_m
        || match_binary,
           <<Value:Size>> <- Action,
           return(Value)
    ]).

pattern(Action) ->
    sf_do(pattern_flow, [async_m
        || match,
           {ok, Value} <- Action,
           return(Value)
    ]).

shadow(_Expected, Action) ->
    sf_do(shadow_flow, [async_m
        || match,
           {_Expected, Value} <- Action,
           return({_Expected, Value})
    ]).

pure_badmatch(Action) ->
    sf_do(pure_badmatch_flow, [async_m
        || value,
           Value <- Action,
           {ok, Result} = Value,
           return(Result)
    ]).

remote_call(Value) ->
    sf_do(remote_flow, [async_m
        || value,
           A <- async_m:pure_return(Value),
           sf_do_fixture:remote_return(A)
    ]).

unnamed_suspended(Ref) ->
    sf_do([async_m
        || Reply <- async_m:promise(Ref),
           return({unnamed, Reply})
    ]).

case_equality(Expected, Value) ->
    sf_do(case_capture, [async_m
        || compare,
           Reply <- async_m:pure_return(Value),
           case Reply of
               Expected -> return({case_equal, Expected})
           end
    ]).

receive_equality(Expected, Value) ->
    self() ! Value,
    sf_do(receive_capture, [async_m
        || receive_value,
           _Ready <- async_m:pure_return(ready),
           receive
               Expected -> return({receive_equal, Expected})
           after 0 ->
               return(receive_mismatch)
           end
    ]).

try_equality(Expected, Value) ->
    sf_do(try_capture, [async_m
        || compare,
           Reply <- async_m:pure_return(Value),
           try Reply of
               Expected -> return({try_equal, Expected})
           catch
               Class:Reason -> return({try_error, Class, Reason})
           end
    ]).

fun_shadow(_Expected, Value) ->
    sf_do(fun_shadow, [async_m
        || invoke,
           _Ready <- async_m:pure_return(ready),
           Shadow = fun(_Expected) -> _Expected end,
           return({fun_shadow, Shadow(Value)})
    ]).

named_fun_shadow(_Expected, Value) ->
    sf_do(named_fun_shadow, [async_m
        || invoke,
           _Ready <- async_m:pure_return(ready),
           Shadow = fun Loop(_Expected) ->
               case _Expected of
                   {again, Next} -> Loop(Next);
                   _ -> _Expected
               end
           end,
           return({named_fun_shadow, Shadow({again, Value})})
    ]).

migration_sentinel(Input, Env) ->
    self() ! {migration_sentinel_invoked, Input, Env},
    async_m:pure_return(invoked).

remote_return(Value) ->
    async_m:pure_return({remote_return, Value}).

%% Deliberately local and outside sf_do.  Calls to this function, including
%% remote calls from inside a block, must not be rewritten as monad:return/2.
return(Value) ->
    {outside_return, Value}.

complete(Outcome, {Pid, Tag}) ->
    Pid ! {Tag, Outcome},
    Outcome.
