%%%-------------------------------------------------------------------
%%% Compile-time source-form gate for stable sf completion callbacks.
%%%-------------------------------------------------------------------
-module(async_sf_cc_macro).

-include_lib("astranaut/include/macro.hrl").

-export_macro([run_sf_cc/3, run_with_sf_cc/5, exec_sf_cc/5]).
-export([format_error/1]).

run_sf_cc(Promise, Callback, Context) ->
    expand(run_sf_cc, [Promise, Callback, Context], Callback, [2]).

run_with_sf_cc(Promise, Callback, Context, Offset, State) ->
    expand(run_with_sf_cc, [Promise, Callback, Context, Offset, State], Callback, [2, 3]).

exec_sf_cc(Promise, Callback, Context, Offset, State) ->
    expand(exec_sf_cc, [Promise, Callback, Context, Offset, State], Callback, [2, 3]).

format_error({invalid_sf_cc_callback, Runner, Reason}) ->
    io_lib:format("invalid ~p callback: ~p", [Runner, Reason]);
format_error(Reason) ->
    io_lib:format("sf-cc macro error: ~p", [Reason]).

expand(Runner, Args, Callback, AllowedArities) ->
    case external_fun_arity(Callback) of
        {ok, Arity} ->
            case lists:member(Arity, AllowedArities) of
                true -> remote_call(position(Callback), Runner, Args);
                false -> diagnostic(Callback,
                    {invalid_sf_cc_callback, Runner, {arity, Arity}})
            end;
        error ->
            diagnostic(Callback,
                {invalid_sf_cc_callback, Runner, external_fun_required})
    end.

external_fun_arity({'fun', _Pos,
        {function, _ModuleExpression, _FunctionExpression, {integer, _, Arity}}}) ->
    {ok, Arity};
external_fun_arity(_) ->
    error.

remote_call(Pos, Function, Args) ->
    {call, Pos, {remote, Pos, {atom, Pos, async_m}, {atom, Pos, Function}}, Args}.

diagnostic(Node, Reason) ->
    astranaut_return:then(
        astranaut_return:formatted_error(position(Node), ?MODULE, Reason),
        astranaut_return:fail()
    ).

position(Node) -> astranaut_syntax:get_pos(Node).
