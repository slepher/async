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

-export_type([reply_t/2, reply_value/1]).

-behaviour(monad_trans).
-export([new/1, reply_t/1, run_reply_t/1]).
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([pure_return/2, wrapped_return/2, lift_reply/2]).
-export([run_reply/2, map_reply/3, with_reply/3]).

-opaque reply_t(M, A) :: {reply_t, inner_reply_t(M, A)}.

-type reply_value(A) :: {ok, A} | {error, any()} | {message, any()} | ok | A.

-type inner_reply_t(M, A) :: monad:monadic(M, reply_value(A)).

-type t(M) :: {reply_t, M}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec reply_t(inner_reply_t(M, A)) -> reply_t(M, A).
reply_t(Inner) ->
    {?MODULE, Inner}.

-spec run_reply_t(reply_t(M, A)) -> inner_reply_t(M, A).
run_reply_t({?MODULE, Inner}) ->
    Inner;
run_reply_t(Other) ->
    exit({invalid_monad, Other}).

-spec fmap(fun((A) -> B), reply_t(M, A), t(M)) -> reply_t(M, B).
fmap(F, X, {?MODULE, IM} = RT) ->
    map_reply(
      fun(RIM) ->
              monad:fmap(F, RIM, IM)
      end, X, RT).

-spec '>>='(reply_t(M, A), fun( (A) -> reply_t(M, B) ), t(M)) -> reply_t(M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    reply_t(do([M || R <- run_reply_t(X),
                     case R of
                         {error, _Err} = Error -> return(Error);
                         {message, _Msg} = Message -> return(Message);
                         ignore                  -> return(ignore);
                         {ok,  Result}         -> run_reply_t(Fun(Result));
                         Result                -> run_reply_t(Fun(Result))
                     end
               ])).

-spec return(A, t(M)) -> reply_t(M, A).
return(ok, {?MODULE, M}) -> reply_t(M:return(ok));
return(X , {?MODULE, M}) -> reply_t(M:return({ok, X})).


-spec fail(any(), t(M)) -> reply_t(M, _A).
fail(E, {?MODULE, M}) ->
    reply_t(M:return({error, E})).

-spec lift(monad:monadic(M, A), M) -> reply_t(M, A).
lift(X, {?MODULE, IM}) ->
    reply_t(do([IM || A <- X,
                return({ok, A})])).

-spec pure_return(reply_value(A), t(M)) -> reply_t(M, A).
pure_return(X, {?MODULE, IM}) ->
    reply_t(IM:return(X)).

-spec wrapped_return(reply_value(A), t(M)) -> reply_t(M, A).
wrapped_return(X, {?MODULE, M}) ->
    reply_t(M:return(wrap_value(X))).

-spec lift_reply(reply_t(M, A), t(M)) -> reply_t(M, reply_value(A)).
lift_reply(X, {?MODULE, _IM} = RT) ->
    with_reply(
      fun(A) ->
              case A of
                  {message, _} = Message ->
                      Message;
                  A ->
                      {ok, A}
              end  
      end, X, RT).

-spec run_reply(reply_t(M, A), M) -> inner_reply_t(M, A).
run_reply(EM, _M) -> run_reply_t(EM).

-spec map_reply(fun((monad:monadic(M, reply_value(A))) -> monad:monadic(N, reply_value(B))), reply_t(M, A), M) -> reply_t(N, B). 
map_reply(F, X, {?MODULE, _M}) ->
    reply_t(F(run_reply_t(X))).

-spec with_reply(fun((reply_value(A)) -> reply_value(B)), reply_t(M, A), t(M)) -> reply_t(M, B). 
with_reply(F, X, {?MODULE, IM} = RT) ->
    map_reply(
      fun(MA) ->
              do([IM || 
                     Val <- MA,
                     return(F(Val))
                 ])
      end, X, RT).

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


