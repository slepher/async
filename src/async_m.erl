%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m).

-erlando_type({?MODULE, []}).

-include_lib("erlando/include/do.hrl").
-include_lib("erlando/include/gen_fun.hrl").

-behaviour(functor).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_error).

-export([to_async/1]).
-export([struct/1]).
-export([return_error_m/1]).
-export([then/2, then/4]).
-export([update/2, exec/4, run/4]).
-export([promise/2]).
-export([callback_to_cc/1]).
-export([map_promises/2]).

-gen_fun(#{remote => async_t, args => identity, 
           functions => [get_state/0, put_state/1, modify_state/1, 
                         find_ref/1, get_ref/2, put_ref/2, remove_ref/1, 
                         get_local/0, put_local/1, modify_local/1, local_ref/2, local/2, get_local_ref/0]}).

-gen_fun(#{remote => async_t, args => identity, 
           functions => [lift_reply/1, lift_final_reply/1, pure_return/1, ok/0, wrapped_return/1, lift_mr/1, wrapped_lift_mr/1,
                         message/1, add_message/1, hijack/1, pass/0, handle_message/2, provide_message/2]}).

-gen_fun(#{remote => async_t, args => identity, 
           functions =>[promise/1, promise/2, promise_sleep/1, map_promises/1, map_promises/2,
                        par/1, progn_par/1, update_cc/2, callCC/1, update_callbacks/2]}).

-gen_fun(#{remote => async_t, args => identity, extra_call => {identity, run},
           functions => [wait/1, wait_t/2,  exec_cc/4, run_cc/2, run_with_cc/4]}).
-gen_fun(#{remote => async_t, args => identity, extra_call => {identity, run},
             functions => [handle_info/3, run_info/3, handle_reply/4, run_reply/4, wait_receive/3]}).

-gen_fun(#{remote => async_t, inner_type => identity,
           behaviours => [functor, monad, monad_fail, monad_error]}).

%%%===================================================================
%%% API
%%%===================================================================
to_async({async_t, _} = Async) ->
    Async;
to_async({ok, Val}) ->
    return(Val);
to_async({error, Reason}) ->
    fail(Reason);
to_async(Val) ->
    pure_return(Val).

struct(#{} = Map) ->
    Map#{'__struct__' => async_m}.

callback_to_cc(Callback) when is_function(Callback, 0) ->
    fun(A) ->
            case Callback() of
                #{'__struct__' := ?MODULE} = Struct ->
                    struct_to_async_r_m(Struct, async_r_m:return(A));
                {async_r_t, _Inner} = NMonadMR ->
                    NMonadMR;
                Result ->
                    async_r_m:return(Result)
            end
    end;
callback_to_cc(Callback) when is_function(Callback, 1) ->
    fun(A) ->
            case Callback(A) of
                #{'__struct__' := ?MODULE} = Struct ->
                    struct_to_async_r_m(Struct, async_r_m:return(A));
                {async_r_t, _Inner} = NMonadMR ->
                    NMonadMR;
                Result ->
                    async_r_m:return(Result)
            end
    end;
callback_to_cc(Callback) when is_function(Callback, 2) ->
    fun(A) ->
            do([async_r_m || 
                   State <- async_r_m:get_state(),
                   case Callback(A, State) of
                       #{'__struct__' := ?MODULE} = Struct ->
                           struct_to_async_r_m(Struct, async_r_m:return(A));
                       {async_r_t, _Inner} = NMonadMR ->
                           NMonadMR;
                       NState ->
                           case async_util:same_type_state(NState, State) of
                               true ->
                                   do([async_r_m ||
                                          async_r_m:put_state(NState),
                                          async_r_m:return(A)
                                      ]);
                               false ->
                                   async_r_m:return(NState)
                           end
                   end
               ])
    end;
callback_to_cc(Callback) ->
    exit({invalid_callback, Callback}).

return_error_m(Value) ->
    lift_reply(Value).

then(Monad, Callback) ->
    update(Monad, Callback).

then(Monad, Callback, Offset, State) ->
    exec(Monad, Callback, Offset, State).

update(Async, Callback) ->
    CC = callback_to_cc(Callback),
    update_cc(Async, CC).

exec(X, Callback, Offset, State) ->
    CC = callback_to_cc(Callback),
    exec_cc(X, CC, Offset, State).

run(Async, Callback, Offset, State) ->
    CC = callback_to_cc(Callback),
    run_with_cc(Async, CC, Offset, State).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
struct_to_async_r_m(#{state := State} = Struct, Async) ->
    NStruct = maps:remove(state, Struct),
    NAsync = 
        do([async_r_m ||
               async_r_m:put_state(State),
               Async
           ]),
    struct_to_async_r_m(NStruct, NAsync);
struct_to_async_r_m(#{return := Return} = Struct, Async) ->
    NStruct = maps:remove(return, Struct),
    NAsync = 
        do([async_r_m ||
               Async,
               async_r_m:return(Return)
           ]),
    struct_to_async_r_m(NStruct, NAsync);
struct_to_async_r_m(#{}, Async) ->
    Async.
