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

-record(callback, {cc, acc_ref}).

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
    (maps:size(Callbacks) =/= 0) and
        (lists:any(fun(#callback{}) ->
                           true;
                      (_) ->
                           false
                   end, maps:values(Callbacks))).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
