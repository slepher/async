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
        fun({_Key, Value}) -> callback_value_kind(Value) =:= action_callback end,
        to_list(Callbacks)
    ).

decode_callback(#callback{cc = Cc, acc_ref = AccRef, sf_cc_mref = CcRef}) ->
    {ok, #{cc => Cc, acc_ref => AccRef, sf_cc_mref => CcRef}};
decode_callback(_) ->
    error.

callback_value_kind(Value) ->
    case decode_callback(Value) of
        {ok, _} -> action_callback;
        error ->
            case Value of
                #{kind := sf_cc_ref, protocol := 1, cc := _} -> sf_cc_ref;
                _ -> other
            end
    end.

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
