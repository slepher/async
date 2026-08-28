%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_util).

%% API
-export([store/3, find/2, get/3, remove/2]).
-export([clear/1, merge/2]).
-export([same_type_state/2, match_state/3, callback_exists/1]).
-export([decode_callback/1, callback_value_kind/1, to_list/1]).
-export([fun_availability/1]).

-include("async_internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================
store(Key, Value, Dict) when is_map(Dict) ->
    maps:put(Key, Value, Dict);
store(Key, Value, Dict) when is_list(Dict) ->
    orddict:store(Key, Value, Dict);
store(Key, Value, Dict) ->
    dict:store(Key, Value, Dict).

find(Key, Dict) when is_map(Dict) ->
    maps:find(Key, Dict);
find(Key, Dict) when is_list(Dict) ->
    orddict:find(Key, Dict);
find(Key, Dict) ->
    dict:find(Key, Dict).

get(Key, Dict, Default) ->
    case find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

remove(Key, Dict) when is_map(Dict) ->
    maps:remove(Key, Dict);
remove(Key, Dict) when is_list(Dict) ->
    orddict:erase(Key, Dict);
remove(Key, Dict) ->
    dict:erase(Key, Dict).

clear(Dict) when is_map(Dict) ->
    maps:new();
clear(Dict) when is_list(Dict) ->
    orddict:new();
clear(_Dict) ->
    dict:new().

merge(Dict1, Dict2) when is_map(Dict1) ->
    maps:merge(Dict1, Dict2);
merge(Dict1, Dict2) when is_list(Dict1) ->
    orddict:merge(fun(_K, _V1, V2) -> V2 end, Dict1, Dict2);
merge(Dict1, Dict2) ->
    dict:merge(fun(_K, _V1, V2) -> V2 end, Dict1, Dict2).

same_type_state(NState, State) when is_tuple(NState), is_tuple(State) ->
    match_state(NState, element(1, State), size(State));
same_type_state(_NState, _State) ->
    false.

match_state(State, Type, Len) when is_tuple(State) ->
    (element(1, State) == Type) and (size(State) == Len);
match_state(_State, _Type, _Len) ->
    false.

callback_exists(Callbacks) ->
    lists:any(
        fun({_Key, Value}) ->
            lists:member(callback_value_kind(Value), [stable_action, ordinary_action])
        end,
        to_list(Callbacks)
    ).

decode_callback(#callback{cc = Cc, acc_ref = AccRef, parent_ref = ParentRef,
        timer_ref = TimerRef}) ->
    {ok, #{kind => ordinary_action, cc => Cc, acc_ref => AccRef,
        parent_ref => ParentRef, timer_ref => TimerRef}};
decode_callback(#sf_callback{frame = Frame, acc_ref = AccRef,
        parent_ref = ParentRef, timer_ref = TimerRef}) ->
    {ok, #{kind => stable_action, frame => Frame, acc_ref => AccRef,
        parent_ref => ParentRef, timer_ref => TimerRef}};
decode_callback(_) ->
    error.

callback_value_kind(#callback{}) -> ordinary_action;
callback_value_kind(#sf_callback{acc_ref = AccRef}) when is_reference(AccRef) -> stable_action;
callback_value_kind(#sf_callback{}) -> intermediate_node;
callback_value_kind(#{kind := ordinary_continuation, protocol := 1}) -> intermediate_node;
callback_value_kind(#{kind := sf_cc, protocol := 1}) -> outer_node;
callback_value_kind(#{kind := sf_reverse_index, protocol := 1}) -> other;
callback_value_kind(Value) when is_map(Value); is_list(Value); is_tuple(Value) -> accumulator;
callback_value_kind(_) -> other.

to_list(Dict) when is_map(Dict) ->
    maps:to_list(Dict);
to_list(Dict) when is_list(Dict) ->
    Dict;
to_list(Dict) ->
    dict:to_list(Dict).

fun_availability(Fun) when is_function(Fun) ->
    case erlang:fun_info(Fun, type) of
        {type, local} -> ok;
        {type, external} ->
            {module, Module} = erlang:fun_info(Fun, module),
            {name, Function} = erlang:fun_info(Fun, name),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            MFA = {Module, Function, Arity},
            case code:ensure_loaded(Module) of
                {module, Module} ->
                    case erlang:function_exported(Module, Function, Arity) of
                        true -> ok;
                        false -> {unavailable, MFA}
                    end;
                {error, nofile} ->
                    {unavailable, MFA};
                {error, Reason} ->
                    {error, {module_load_failed, MFA, Reason}}
            end
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
