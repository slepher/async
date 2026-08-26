%%%-------------------------------------------------------------------
%%% Lexical analysis for sf_do source regions.
%%%
%%% Parent nodes define evaluation order, scope, and pattern mode. A
%%% variable occurrence then has only two behaviours: reference an
%%% existing binding, or introduce/check a binding in a pattern.
%%%-------------------------------------------------------------------
-module(async_sf_lexical).

-export([
    pattern_new_bindings/3,
    pattern_bindings/3,
    rename_pattern_references/4,
    free_expression/2,
    free_qualifier_sequence/4
]).

pattern_new_bindings(Pattern, Mode, Visible) ->
    {Bindings, _References} = pattern_bindings(Pattern, Mode, Visible),
    Bindings.

pattern_bindings(Pattern, {clause, Parent}, Visible) ->
    pattern_bindings(Pattern, clause_pattern_mode(Parent), Visible);
pattern_bindings(Pattern, Mode, Visible) when Mode =:= fresh; Mode =:= match ->
    State0 = (new_state(ordsets:new(), Visible))#{pattern_stack => [Mode]},
    State1 = analyze(Pattern, pattern, State0),
    [Bindings] = maps:get(scopes, State1),
    {Bindings, maps:get(free, State1)}.

rename_pattern_references(Pattern, Mode, Visible, Renames) ->
    State0 = (new_state(ordsets:new(), Visible))#{
        pattern_stack => [Mode],
        reference_renames => Renames
    },
    {Pattern1, _State1} = walk(Pattern, pattern, State0),
    Pattern1.

free_qualifier_sequence(Qualifiers, FinalExpression, InitialLocal, OuterCandidates) ->
    State0 = new_state(InitialLocal, OuterCandidates),
    State1 = lists:foldl(
        fun(Qualifier, State) -> analyze(Qualifier, expression, State) end,
        State0,
        Qualifiers
    ),
    maps:get(free, analyze(FinalExpression, expression, State1)).

free_expression(Node, OuterCandidates) ->
    State0 = new_state(ordsets:new(), OuterCandidates),
    maps:get(free, analyze(Node, expression, State0)).

new_state(InitialLocal, OuterCandidates) ->
    #{
        scopes => [ordsets:from_list(InitialLocal)],
        outer_candidates => ordsets:from_list(OuterCandidates),
        free => ordsets:new(),
        pattern_stack => [],
        reference_renames => #{}
    }.

analyze(Node, Role, State) ->
    {_Node1, State1} = walk(Node, Role, State),
    State1.

walk(Node, Role, State) ->
    {just, {Node1, State1}} = astranaut_return:run(
        astranaut:mapfold(
            fun lexical_walk/3,
            State,
            Node,
            #{traverse => pre, role => Role, normalize => true}
        )
    ),
    {Node1, State1}.

lexical_walk(Node, State, Attr) ->
    Type = astranaut_syntax:type(Node),
    case Type of
        variable ->
            variable_occurrence(Node, State, Attr);
        _ ->
            Node1 = lexical_node(Type, Node, Attr),
            Decorated = astranaut_uniplate:up_attr(
                #{parent => Type},
                astranaut_uniplate:with_subtrees(
                    fun(Subtrees) ->
                        astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
                    end,
                    Node1
                )
            ),
            {Decorated, State}
    end.

lexical_node(list_comp, Node, _Attr) ->
    walk_comprehension(Node);
lexical_node(map_comp, Node, _Attr) ->
    walk_comprehension(Node);
lexical_node(binary_comp, Node, _Attr) ->
    walk_comprehension(Node);
lexical_node(generator, Node, _Attr) ->
    walk_generator(Node);
lexical_node(strict_generator, Node, _Attr) ->
    walk_generator(Node);
lexical_node(binary_generator, Node, _Attr) ->
    walk_generator(Node);
lexical_node(strict_binary_generator, Node, _Attr) ->
    walk_generator(Node);
lexical_node(map_generator, Node, _Attr) ->
    walk_generator(Node);
lexical_node(strict_map_generator, Node, _Attr) ->
    walk_generator(Node);
lexical_node(match_expr, Node, #{node := expression}) ->
    walk_match(Node);
lexical_node(maybe_match_expr, Node, #{node := expression}) ->
    walk_match(Node);
lexical_node(clause, Node, Attr) ->
    walk_clause(Node, maps:get(parent, Attr, undefined));
lexical_node(named_fun_expr, Node, _Attr) ->
    walk_named_fun(Node);
lexical_node(maybe_expr, Node, _Attr) ->
    walk_maybe(Node);
lexical_node(_Type, Node, _Attr) ->
    Node.

walk_comprehension(Comprehension) ->
    Sequence =
        fun(Subtrees) ->
            with_scope(lists:reverse(Subtrees))
        end,
    astranaut_uniplate:with_subtrees(Sequence, fun lists:reverse/1, Comprehension).

walk_generator(Generator) ->
    Sequence =
        fun([Patterns, Expressions]) ->
            Patterns1 = with_pattern(fresh, Patterns),
            Expressions1 = with_scope(Expressions),
            [Expressions1, Patterns1]
        end,
    astranaut_uniplate:with_subtrees(Sequence, fun lists:reverse/1, Generator).

walk_match(Match) ->
    Sequence =
        fun([Patterns, Expressions]) ->
            Patterns1 = with_pattern(match, Patterns),
            [Expressions, Patterns1]
        end,
    astranaut_uniplate:with_subtrees(Sequence, fun lists:reverse/1, Match).

walk_clause(Clause, Parent) ->
    PatternMode = clause_pattern_mode(Parent),
    Sequence =
        fun
            ([Patterns, Guards, Expressions]) ->
                Patterns1 = with_pattern(PatternMode, Patterns),
                with_scope([Patterns1, Guards, Expressions]);
            ([Patterns, Expressions]) ->
                Patterns1 = with_pattern(PatternMode, Patterns),
                with_scope([Patterns1, Expressions])
        end,
    astranaut_uniplate:with_subtrees(Sequence, Clause).

walk_named_fun(NamedFun) ->
    Sequence =
        fun([NameTree | RestTrees]) ->
            NameTree1 = with_pattern(fresh, NameTree),
            with_scope([NameTree1 | RestTrees])
        end,
    astranaut_uniplate:with_subtrees(Sequence, NamedFun).

walk_maybe(Maybe) ->
    Sequence =
        fun([MaybeBody | ElseClauses]) ->
            [with_scope(MaybeBody) | ElseClauses]
        end,
    astranaut_uniplate:with_subtrees(Sequence, Maybe).

with_scope(Trees) ->
    astranaut_uniplate:with(fun push_scope/1, fun pop_scope/1, Trees).

with_pattern(Mode, Trees) ->
    astranaut_uniplate:with(
        fun(State) -> push_pattern(Mode, State) end,
        fun pop_pattern/1,
        Trees
    ).

clause_pattern_mode(fun_expr) -> fresh;
clause_pattern_mode(named_fun_expr) -> fresh;
clause_pattern_mode(_Parent) -> match.

variable_occurrence(Var, State, Attr) ->
    Name = erl_syntax:variable_name(Var),
    case {Name, maps:get(node, Attr, expression), maps:get(pattern_stack, State)} of
        {'_', _Role, _PatternStack} ->
            {Var, State};
        {_Name, pattern, [Mode | _]} ->
            pattern_variable(Var, Name, Mode, State);
        {_Name, _Role, _PatternStack} ->
            reference_variable(Var, Name, State)
    end.

pattern_variable(Var, Name, fresh, State) ->
    {Var, bind_if_new(Name, State)};
pattern_variable(Var, Name, match, State) ->
    case bound_in_scopes(Name, maps:get(scopes, State)) of
        true ->
            {Var, State};
        false ->
            case ordsets:is_element(Name, maps:get(outer_candidates, State)) of
                true -> free_reference(Var, Name, State);
                false -> {Var, bind_current(Name, State)}
            end
    end.

reference_variable(Var, Name, State) ->
    case bound_in_scopes(Name, maps:get(scopes, State)) of
        true -> {Var, State};
        false -> free_reference(Var, Name, State)
    end.

free_reference(Var, Name, State) ->
    Renames = maps:get(reference_renames, State),
    Var1 =
        case maps:find(Name, Renames) of
            {ok, Name1} -> {var, erl_syntax:get_pos(Var), Name1};
            error -> Var
        end,
    {Var1, add_free(Name, State)}.

bind_if_new(Name, State) ->
    case bound_in_scopes(Name, maps:get(scopes, State)) of
        true -> State;
        false -> bind_current(Name, State)
    end.

push_scope(State) ->
    State#{scopes => [ordsets:new() | maps:get(scopes, State)]}.

pop_scope(State) ->
    [_ | Tail] = maps:get(scopes, State),
    State#{scopes => Tail}.

bind_current(Name, State) ->
    [Current | Tail] = maps:get(scopes, State),
    State#{scopes => [ordsets:add_element(Name, Current) | Tail]}.

bound_in_scopes(Name, Scopes) ->
    lists:any(fun(Scope) -> ordsets:is_element(Name, Scope) end, Scopes).

push_pattern(Mode, State) ->
    State#{pattern_stack => [Mode | maps:get(pattern_stack, State)]}.

pop_pattern(State) ->
    [_ | Tail] = maps:get(pattern_stack, State),
    State#{pattern_stack => Tail}.

add_free(Name, State) ->
    State#{free => ordsets:add_element(Name, maps:get(free, State))}.
