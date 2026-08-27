-module(async_sf_transform_tests).

-include_lib("eunit/include/eunit.hrl").

format_error_messages_test_() ->
    [
        {lists:flatten(io_lib:format("format ~p", [Reason])),
            ?_assertEqual(
                Expected,
                lists:flatten(async_sf_transform:format_error(Reason))
            )}
     || {Reason, Expected} <- format_error_cases()
    ].

format_error_rejects_unknown_reason_test() ->
    ?assertError(
        function_clause,
        async_sf_transform:format_error({invalid_sf_do, misspelled_reazon})
    ).

astranaut_format_error_fallback_test() ->
    Unknown = {invalid_sf_do, misspelled_reazon},
    ?assertEqual(
        lists:flatten(io_lib:write(Unknown)),
        lists:flatten(
            astranaut_lib:format_error(
                Unknown, fun async_sf_transform:format_error/1
            )
        )
    ).

empty_comprehension_ast_diagnostic_test() ->
    EmptyPos = {3, 14},
    EmptyForms = [
        {attribute, {1, 1}, file, {"sf_do_empty_ast.erl", 1}},
        {attribute, {1, 1}, module, sf_do_empty_ast},
        {function, {3, 1}, f, 0, [
            {clause, {3, 1}, [], [], [
                {call, {3, 8}, {atom, {3, 8}, sf_do}, [
                    {lc, EmptyPos, {atom, {3, 15}, async_m}, []}
                ]}
            ]}
        ]},
        {eof, {4, 1}}
    ],
    {error,
        [{_File,
            [{EmptyPos, astranaut_lib,
                {async_sf_transform, {invalid_sf_do, empty}}}]}],
        []} = async_sf_transform:parse_transform(EmptyForms, []),
    ?assertEqual(
        "sf_do requires at least one qualifier",
        lists:flatten(
            async_sf_transform:format_error({invalid_sf_do, empty})
        )
    ).

format_error_cases() ->
    Summary = #{
        owner => {f, 1},
        occurrence => 2,
        block_depth => 1,
        plan_count => 3,
        consumed_count => 2,
        unconsumed_count => 1
    },
    [
        {{invalid_sf_do, {arity, 0}},
            "sf_do expects one or two arguments; received 0"},
        {{invalid_sf_do, {non_literal, block_name}},
            "sf_do block name must be a literal atom"},
        {{invalid_sf_do, expected_list_comprehension},
            "sf_do expects a list comprehension: [Monad || Qualifiers]"},
        {{invalid_sf_do, invalid_monad_head},
            "sf_do Monad must be an atom, variable, or {Transformer, Inner}"},
        {{invalid_sf_do, empty},
            "sf_do requires at least one qualifier"},
        {{invalid_sf_do, consecutive_step_marker},
            "sf_do step markers cannot be consecutive"},
        {{invalid_sf_do, invalid_last_qualifier},
            "sf_do must end with a monadic action, not a generator or pure match"},
        {{invalid_sf_do, dangling_step_marker},
            "sf_do step marker must be followed by a non-final action; the block must still end with a monadic action"},
        {{invalid_sf_do, step_marker_on_pure_let},
            "sf_do step marker cannot annotate a pure match"},
        {{invalid_sf_do, step_marker_on_final_action},
            "sf_do step marker cannot annotate the final action"},
        {{invalid_sf_do, {duplicate_step_name, one}},
            "duplicate sf_do step name: one"},
        {{duplicate_block_path, [outer, inner]},
            "duplicate sf_do block path: [outer,inner]"},
        {{generated_function_conflict, '$sf_example'},
            "sf_do generated function conflicts with '$sf_example'/2"},
        {{ordering_mismatch, {source_digest, {{f, 1}, [flow], 0}}},
            "internal sf_do traversal digest mismatch at occurrence {{f,1},[flow],0}"},
        {{ordering_mismatch, {unexpected_occurrence, {{f, 1}, 0}}},
            "internal sf_do traversal found an unplanned occurrence {{f,1},0}"},
        {{ordering_mismatch, missing_occurrence},
            "internal sf_do traversal did not rewrite every planned occurrence"},
        {{ordering_mismatch, {incomplete_rewrite, Summary}},
            "internal sf_do traversal ended incomplete: owner={f,1} occurrence=2 block_depth=1 plans=3 consumed=2 unconsumed=1"},
        {unnamed_sf_action,
            "unnamed non-final sf_do action is non-migratable; add a literal atom step marker immediately before it"}
    ].
