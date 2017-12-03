%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reply_t).

-erlando_type(?MODULE).

-export_type([reply_t/2, reply/1, final_reply/1, ok_reply/1, message_reply/0]).

-opaque reply_t(M, A) :: {reply_t, inner_reply_t(M, A)}.
-type reply(A) :: final_reply(A) | message_reply().
-type final_reply(A) :: ok_reply(A) | error_m:error(any()).
-type ok_reply(A) :: error_m:ok(A) | A.
-type message_reply() :: {message, any()}.
-type inner_reply_t(M, A) :: monad:m(M, reply(A)).
-type t(M) :: {reply_t, M}.
-spec new(M) -> TM when TM :: monad:class(), M :: monad:class().

-compile({parse_transform, do}).
-compile({parse_transform, function_generator}).

-behaviour(functor).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_runner).

-include_lib("erlando/include/erlando.hrl").

-export([new/1, reply_t/1, run_reply_t/1]).
-export([fmap/3, '<$'/3]).
% impl of monad.
-export(['>>='/3, '>>'/3, return/2]).
% impl of monad_trans.
-export([lift/2]).
% impl of monad_fail.
-export([fail/2]).
-export([run_nargs/0, run_m/2]).
-export([pure_return/2, wrapped_return/2, lift_final/2]).
-export([run/1, map/2, with/3]).

-gen_fun(#{inner_type => monad, tfunctions => [pure_return/2, wrapped_return/2, lift_final/2, with/3]}).
-gen_fun(#{inner_type => functor, behaviours => [functor]}).
-gen_fun(#{inner_type => monad, behaviours => [monad, monad_trans, monad_fail]}).

new(M) ->
    {?MODULE, M}.

-spec reply_t(inner_reply_t(M, A)) -> reply_t(M, A).
reply_t(Inner) ->
    {?MODULE, Inner}.

-spec run_reply_t(reply_t(M, A)) -> inner_reply_t(M, A).
run_reply_t(#undetermined{} = UA) ->
    run_reply_t(undetermined:run(UA, ?MODULE));
run_reply_t({?MODULE, Inner}) ->
    Inner;
run_reply_t(Other) ->
    exit({invalid_monad, Other}).

-spec fmap(fun((A) -> B), reply_t(M, A)) -> reply_t(M, B).
fmap(F, RTA, {?MODULE, IM}) ->
    map(
      fun(MA) ->
              functor:fmap(F, MA, IM)
      end, RTA).

'<$'(RTB, RTA, {?MODULE, _IM} = RT) ->
    functor:'default_<$'(RTB, RTA, RT).

-spec '>>='(reply_t(M, A), fun( (A) -> reply_t(M, B) )) -> reply_t(M, B).
'>>='(RTA, KRTB, {?MODULE, IM}) ->
    reply_t(
      do([IM ||
             RA <- run_reply_t(RTA),
             case RA of
                 {error, _Err}    -> return(RA);
                 {message, _Msg}  -> return(RA);
                 ignore           -> return(ignore);
                 {ok,  A}         -> run_reply_t(KRTB(A));
                 A                -> run_reply_t(KRTB(A))
             end
         ])).

'>>'(RTA, RTB, {?MODULE, _IM} = RT) ->
    monad:'default_>>'(RTA, RTB, RT).

-spec return(A, t(M)) -> reply_t(M, A).
return(ok, {?MODULE, IM}) -> reply_t(monad:return(ok, IM));
return(A , {?MODULE, IM}) -> reply_t(monad:return({ok, A}, IM)).

-spec lift(monad:m(M, A)) -> reply_t(M, A).
lift(MA, {?MODULE, IM}) ->
    reply_t(
      do([IM || 
             A <- MA,
             return({ok, A})
         ])).

-spec fail(any(), t(M)) -> reply_t(M, _A).
fail(E, {?MODULE, IM}) ->
    reply_t(monad:return({error, E}, IM)).

run_nargs() ->
    0.

run_m(RTA, []) ->
    run(RTA).

-spec pure_return(reply(A), t(M)) -> reply_t(M, A).
pure_return(A, {?MODULE, IM}) ->
    reply_t(monad:return(A, IM)).

-spec wrapped_return(reply(A), t(M)) -> reply_t(M, A).
wrapped_return(A, {?MODULE, IM}) ->
    reply_t(monad:return(wrap_value(A), IM)).

-spec lift_final(reply_t(M, A), t(M)) -> reply_t(M, final_reply(A)).
lift_final(RTA, {?MODULE, IM}) ->
    with(
      fun(A) ->
              case A of
                  {message, _} = Message ->
                      Message;
                  A ->
                      {ok, A}
              end  
      end, RTA, {?MODULE, IM}).

-spec run(reply_t(M, A)) -> inner_reply_t(M, A).
run(EM) -> run_reply_t(EM).

-spec map(fun((monad:m(M, reply(A))) -> monad:m(N, reply(B))), reply_t(M, A)) -> reply_t(N, B). 
map(F, RTA) ->
    reply_t(F(run_reply_t(RTA))).

-spec with(fun((reply(A)) -> reply(B)), reply_t(M, A), t(M)) -> reply_t(M, B). 
with(F, RTA, {?MODULE, IM}) ->
    map(
      fun(MA) ->
              do([IM || 
                     RA <- MA,
                     return(F(RA))
                 ])
      end, RTA).

wrap_value(Value) ->
    case Value of
        {ok, V} ->
            {ok, V};
        {error, R} ->
            {error, R};
        {message, M} ->
            {message, M};
        ok ->
            ok;
        Other ->
            {ok, Other}
    end.


