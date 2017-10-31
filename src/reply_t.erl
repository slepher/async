%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(reply_t).
-compile({parse_transform, do}).

-export_type([reply_t/2, reply/1, final_reply/1, ok_reply/1, message_reply/0]).

-behaviour(type).
-behaviour(functor).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_runner).

-export([new/1, reply_t/1, run_reply_t/1]).
-export([type/0]).
-export([fmap/2, '<$'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([return/2, lift/1]).
-export([fail/1]).
-export([fail/2]).
-export([run_nargs/0, run_m/2]).
-export([pure_return/2, wrapped_return/2, lift_final/1]).
-export([run/1, map/2, with/2]).

-opaque reply_t(M, A) :: {reply_t, inner_reply_t(M, A)}.

-type reply(A) :: final_reply(A) | message_reply().
-type final_reply(A) :: ok_reply(A) | error_m:error(any()).
-type ok_reply(A) :: error_m:ok(A) | A.
-type message_reply() :: {message, any()}.


-type inner_reply_t(M, A) :: monad:monadic(M, reply(A)).

-type t(M) :: {reply_t, M}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec reply_t(inner_reply_t(M, A)) -> reply_t(M, A).
reply_t(Inner) ->
    {?MODULE, Inner}.

-spec run_reply_t(reply_t(M, A)) -> inner_reply_t(M, A).
run_reply_t({undetermined, _} = UA) ->
    run_reply_t(undetermined:run(UA, ?MODULE));
run_reply_t({?MODULE, Inner}) ->
    Inner;
run_reply_t(Other) ->
    exit({invalid_monad, Other}).

type() ->
    type:default_type(?MODULE).

-spec fmap(fun((A) -> B), reply_t(M, A)) -> reply_t(M, B).
fmap(F, RTA) ->
    map(
      fun(MA) ->
              functor:fmap(F, MA)
      end, RTA).

'<$'(RTB, RTA) ->
    functor:'default_<$'(RTB, RTA, ?MODULE).

-spec '>>='(reply_t(M, A), fun( (A) -> reply_t(M, B) )) -> reply_t(M, B).
'>>='(RTA, KRTB) ->
    reply_t(
      do([monad ||
             RA <- run_reply_t(RTA),
             case RA of
                 {error, _Err}    -> return(RA);
                 {message, _Msg}  -> return(RA);
                 ignore           -> return(ignore);
                 {ok,  A}         -> run_reply_t(KRTB(A));
                 A                -> run_reply_t(KRTB(A))
             end
         ])).

'>>'(RTA, RTB) ->
    monad:'default_>>'(RTA, RTB, ?MODULE).

return(A) ->
    return(A, {?MODULE, monad}).

-spec return(A, t(M)) -> reply_t(M, A).
return(ok, {?MODULE, IM}) -> reply_t(monad:return(ok, IM));
return(A , {?MODULE, IM}) -> reply_t(monad:return({ok, A}, IM)).

-spec lift(monad:monadic(M, A)) -> reply_t(M, A).
lift(MA) ->
    reply_t(
      do([monad || 
             A <- MA,
             return({ok, A})])).

fail(E) ->
    fail(E, {?MODULE, monad}).

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

-spec lift_final(reply_t(M, A)) -> reply_t(M, final_reply(A)).
lift_final(RTA) ->
    with(
      fun(A) ->
              case A of
                  {message, _} = Message ->
                      Message;
                  A ->
                      {ok, A}
              end  
      end, RTA).

-spec run(reply_t(M, A)) -> inner_reply_t(M, A).
run(EM) -> run_reply_t(EM).

-spec map(fun((monad:monadic(M, reply(A))) -> monad:monadic(N, reply(B))), reply_t(M, A)) -> reply_t(N, B). 
map(F, RTA) ->
    reply_t(F(run_reply_t(RTA))).

-spec with(fun((reply(A)) -> reply(B)), reply_t(M, A)) -> reply_t(M, B). 
with(F, RTA) ->
    map(
      fun(MA) ->
              do([monad || 
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


