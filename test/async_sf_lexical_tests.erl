-module(async_sf_lexical_tests).

-include_lib("eunit/include/eunit.hrl").

pattern_bindings_respect_entry_scope_test() ->
    {match, _, Pattern, _Body} = expression("{Expected, Fresh} = Value"),
    Visible = ordsets:from_list(['Expected']),
    ?assertEqual(
        {ordsets:from_list(['Fresh']), ordsets:from_list(['Expected'])},
        async_sf_lexical:pattern_bindings(Pattern, match, Visible)
    ),
    ?assertEqual(
        {ordsets:from_list(['Expected', 'Fresh']), ordsets:new()},
        async_sf_lexical:pattern_bindings(Pattern, fresh, Visible)
    ),
    ?assertEqual(
        ordsets:from_list(['Fresh']),
        async_sf_lexical:pattern_new_bindings(Pattern, match, Visible)
    ).

map_pattern_key_is_a_reference_test() ->
    Pattern = generator_pattern("[Monad || #{Key := Value} <- Action]"),
    ?assertEqual(
        {ordsets:from_list(['Value']), ordsets:from_list(['Key'])},
        async_sf_lexical:pattern_bindings(
            Pattern, fresh, ordsets:from_list(['Key'])
        )
    ).

binary_pattern_size_is_a_reference_test() ->
    Pattern = generator_pattern("[Monad || <<Value:Size>> <- Action]"),
    ?assertEqual(
        {ordsets:from_list(['Value']), ordsets:from_list(['Size'])},
        async_sf_lexical:pattern_bindings(
            Pattern, fresh, ordsets:from_list(['Size'])
        )
    ).

binary_pattern_can_reference_an_earlier_local_binding_test() ->
    Pattern = generator_pattern("[Monad || <<Size:8, Value:Size>> <- Action]"),
    ?assertEqual(
        {ordsets:from_list(['Size', 'Value']), ordsets:new()},
        async_sf_lexical:pattern_bindings(Pattern, fresh, ordsets:new())
    ).

rename_pattern_references_preserves_binders_test() ->
    Pattern = generator_pattern("[Monad || #{Key := Key} <- Action]"),
    Pattern1 = async_sf_lexical:rename_pattern_references(
        Pattern,
        fresh,
        ordsets:from_list(['Key']),
        #{'Key' => '__CapturedKey'}
    ),
    {map, _, [{map_field_exact, _, Key, Value}]} = Pattern1,
    ?assertMatch({var, _, '__CapturedKey'}, Key),
    ?assertMatch({var, _, 'Key'}, Value).

rename_pattern_references_preserves_local_references_test() ->
    Pattern = generator_pattern("[Monad || <<Size:8, Value:Size>> <- Action]"),
    Pattern1 = async_sf_lexical:rename_pattern_references(
        Pattern,
        fresh,
        ordsets:from_list(['Size']),
        #{'Size' => '__CapturedSize'}
    ),
    ?assertEqual(
        ordsets:from_list(['Size', 'Value']),
        ordsets:from_list(sets:to_list(erl_syntax_lib:variables(Pattern1)))
    ).

case_pattern_uses_outer_equality_reference_test() ->
    Node = expression(
        "case Subject of "
        "Expected when Guard =:= Expected -> {Expected, Value} "
        "end"
    ),
    Outer = ordsets:from_list(['Expected', 'Guard', 'Subject', 'Value']),
    ?assertEqual(Outer, async_sf_lexical:free_expression(Node, Outer)).

fun_parameters_shadow_outer_bindings_test() ->
    Node = expression("fun(Expected) -> {Expected, Value} end"),
    Outer = ordsets:from_list(['Expected', 'Value']),
    ?assertEqual(
        ordsets:from_list(['Value']),
        async_sf_lexical:free_expression(Node, Outer)
    ).

named_fun_name_and_parameters_are_local_test() ->
    Node = expression(
        "fun Loop(Expected) -> "
        "case Expected of {again, Next} -> Loop(Next); _ -> Value end "
        "end"
    ),
    Outer = ordsets:from_list(['Expected', 'Loop', 'Value']),
    ?assertEqual(
        ordsets:from_list(['Value']),
        async_sf_lexical:free_expression(Node, Outer)
    ).

qualifier_sequence_tracks_sequential_bindings_test() ->
    {Qualifiers, FinalExpression} = comprehension(
        "[Monad || A <- Action, B = A, finish({A, B, Outer})]"
    ),
    Outer = ordsets:from_list(['Action', 'Monad', 'Outer']),
    ?assertEqual(
        Outer,
        async_sf_lexical:free_qualifier_sequence(
            Qualifiers, FinalExpression, ordsets:new(), Outer
        )
    ).

initial_local_shadows_entry_binding_test() ->
    {Qualifiers, FinalExpression} = comprehension(
        "[Monad || finish({Expected, Value})]"
    ),
    Outer = ordsets:from_list(['Expected', 'Monad', 'Value']),
    ?assertEqual(
        ordsets:from_list(['Monad', 'Value']),
        async_sf_lexical:free_qualifier_sequence(
            Qualifiers,
            FinalExpression,
            ordsets:from_list(['Expected']),
            Outer
        )
    ).

nested_comprehension_keeps_bindings_local_test() ->
    Node = expression(
        "[Pair || Pair <- Pairs, [X || X <- Pair, use(X, Outer)]]"
    ),
    Outer = ordsets:from_list(['Outer', 'Pair', 'Pairs', 'X']),
    ?assertEqual(
        ordsets:from_list(['Outer', 'Pairs']),
        async_sf_lexical:free_expression(Node, Outer)
    ).

expression(Source) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(Source ++ "."),
    {ok, [Node]} = erl_parse:parse_exprs(Tokens),
    Node.

comprehension(Source) ->
    Node = expression(Source),
    {erl_syntax:list_comp_body(Node), erl_syntax:list_comp_template(Node)}.

generator_pattern(Source) ->
    Node = expression(Source),
    [Generator | _] = erl_syntax:list_comp_body(Node),
    erl_syntax:generator_pattern(Generator).
