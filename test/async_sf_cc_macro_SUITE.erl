%%% Source-form acceptance and diagnostic coverage for sf-cc macros.
-module(async_sf_cc_macro_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [accepted_external_matrix, rejected_source_forms, diagnostic_positions].

accepted_external_matrix(_Config) ->
    lists:foreach(fun(Expression) ->
        {ok, Module, Binary, _Warnings} = compile_expression(Expression),
        ?assert(is_binary(Binary)),
        ?assertEqual(false, code:is_loaded(Module))
    end, [
        "run_sf_cc(Promise, fun erlang:send/2, Context)",
        "run_with_sf_cc(Promise, fun erlang:send/2, Context, 2, State)",
        "run_with_sf_cc(Promise, fun maps:get/3, Context, 2, State)",
        "exec_sf_cc(Promise, fun erlang:send/2, Context, 2, State)",
        "exec_sf_cc(Promise, fun maps:get/3, Context, 2, State)"
    ]).

rejected_source_forms(_Config) ->
    Expressions = [
        "run_sf_cc(Promise, fun local/2, Context)",
        "run_sf_cc(Promise, fun(A, B) -> {A, B} end, Context)",
        "run_sf_cc(Promise, Callback, Context)",
        "run_sf_cc(Promise, callback(), Context)",
        "run_sf_cc(Promise, fun maps:get/3, Context)",
        "run_with_sf_cc(Promise, fun erlang:send/1, Context, 2, State)",
        "exec_sf_cc(Promise, fun erlang:send/1, Context, 2, State)"
    ],
    lists:foreach(fun(Expression) ->
        {error, Errors, _Warnings} = compile_expression(Expression),
        ?assert(has_sf_cc_diagnostic(Errors))
    end, Expressions).

diagnostic_positions(_Config) ->
    {error, Errors, _Warnings} = compile_expression(
        "run_sf_cc(Promise, fun(A, B) -> {A, B} end, Context)"),
    Positions = [Position || {_File, Diagnostics} <- Errors,
        {Position, astranaut_lib,
            {async_sf_cc_macro, {invalid_sf_cc_callback, _, _}}} <- Diagnostics],
    ?assertEqual(1, length(Positions)),
    [Position] = Positions,
    ?assert(is_integer(Position) orelse
        (is_tuple(Position) andalso tuple_size(Position) =:= 2)).

compile_expression(Expression) ->
    Module = list_to_atom("sf_cc_macro_fixture_" ++
        integer_to_list(erlang:unique_integer([positive]))),
    Source = io_lib:format(
        "-module(~p).~n-define(ASYNC_DO,false).~n"
        "-define(ASYNC_SF_DO,false).~n"
        "-include_lib(\"async/include/async.hrl\").~n"
        "-export([f/4, local/2, callback/0]).~n"
        "local(A, B) -> {A, B}.~ncallback() -> fun erlang:send/2.~n"
        "f(Promise, Callback, Context, State) -> ~s.~n",
        [Module, Expression]),
    Path = filename:join(temp_dir(), atom_to_list(Module) ++ ".erl"),
    ok = file:write_file(Path, Source),
    compile:file(Path, [binary, return_errors, return_warnings]).

has_sf_cc_diagnostic(Errors) ->
    lists:any(fun({_File, Diagnostics}) ->
        lists:any(fun
            ({_Position, astranaut_lib,
                    {async_sf_cc_macro,
                        {invalid_sf_cc_callback, _Runner, _Reason}}}) -> true;
            (_) -> false
        end, Diagnostics)
    end, Errors).

temp_dir() ->
    Dir = filename:join(["_build", "test", "sf_cc_macro_fixtures"]),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    Dir.
