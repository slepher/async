%%%-------------------------------------------------------------------
%%% sf_do source transform. Source facts are collected before any AST
%%% replacement; the second Astranaut traversal consumes that frozen plan.
%%%-------------------------------------------------------------------
-module(async_sf_transform).

-include_lib("astranaut/include/do.hrl").

-export([parse_transform/2, format_error/1]).

-define(PROTOCOL, 1).

parse_transform(Forms, _Options) ->
    Module = astranaut_lib:analyze_forms_module(Forms),
    Vsn = module_vsn(Forms),
    Collect0 = #{
        module => Module,
        vsn => Vsn,
        functions => source_functions(Forms),
        owner => undefined,
        block_stack => [],
        occurrence => 0,
        anonymous => 0,
        plans => #{},
        occurrence_plans => #{},
        block_names => #{},
        generated_names => #{},
        scope_stack => []
    },
    astranaut_return:to_compiler(
        do([
            return
         || Collect <- astranaut_return:fail_on_error(
                astranaut:reduce(
                    fun collect/3,
                    Collect0,
                    Forms,
                    #{formatter => ?MODULE, traverse => all}
                )
            ),
            rewrite_module(Forms, Collect)
        ])
    ).

collect_return(Node, State) ->
    astranaut:walk_return(#{
        return => collect_node(Node),
        state => State
    }).

collect_node(Node) ->
    astranaut_uniplate:up_attr(
        #{parent => astranaut_syntax:type(Node)}, Node
    ).

rewrite_module(Forms, Collect) ->
    Rewrite0 = #{
        owner => undefined,
        block_stack => [],
        occurrence => 0,
        occurrence_plans => maps:get(occurrence_plans, Collect),
        plans => maps:get(plans, Collect),
        consumed => #{},
        generated => []
    },
    do([
        return
     || RewriteResult <- astranaut_return:fail_on_error(
            astranaut:mapfold(
                fun rewrite/3,
                Rewrite0,
                Forms,
                #{formatter => ?MODULE, normalize => true, traverse => all}
            )
        ),
        {RewrittenForms, Rewrite} = RewriteResult,
        finish_rewrite(RewrittenForms, Rewrite, Collect)
    ]).

finish_rewrite(RewrittenForms, Rewrite, Collect) ->
    case rewrite_complete(Rewrite) of
        ok ->
            Generated = lists:reverse(maps:get(generated, Rewrite)),
            Manifest = manifest(
                maps:get(module, Collect), maps:get(vsn, Collect),
                maps:values(maps:get(plans, Collect))
            ),
            ManifestForms = manifest_forms(Manifest),
            GeneratedExport = {attribute, 0, export, [
                {Name, Arity} || {function, _, Name, Arity, _} <- Generated
            ]},
            astranaut_return:return(
                astranaut_forms:insert_forms(
                    [GeneratedExport | Generated] ++ ManifestForms, RewrittenForms
                )
            );
        {error, Reason} ->
            astranaut_return:then(
                astranaut_return:formatted_error(0, ?MODULE, Reason),
                astranaut_return:fail()
            )
    end.

format_error({invalid_sf_do, {arity, Arity}}) ->
    io_lib:format("sf_do expects one or two arguments; received ~B", [Arity]);
format_error({invalid_sf_do, {non_literal, block_name}}) ->
    "sf_do block name must be a literal atom";
format_error({invalid_sf_do, expected_list_comprehension}) ->
    "sf_do expects a list comprehension: [Monad || Qualifiers]";
format_error({invalid_sf_do, invalid_monad_head}) ->
    "sf_do Monad must be an atom, variable, or {Transformer, Inner}";
format_error({invalid_sf_do, empty}) ->
    "sf_do requires at least one qualifier";
format_error({invalid_sf_do, consecutive_step_marker}) ->
    "sf_do step markers cannot be consecutive";
format_error({invalid_sf_do, invalid_last_qualifier}) ->
    "sf_do must end with a monadic action, not a generator or pure match";
format_error({invalid_sf_do, dangling_step_marker}) ->
    "sf_do step marker must be followed by a non-final action; the block must still end with a monadic action";
format_error({invalid_sf_do, step_marker_on_pure_let}) ->
    "sf_do step marker cannot annotate a pure match";
format_error({invalid_sf_do, step_marker_on_final_action}) ->
    "sf_do step marker cannot annotate the final action";
format_error({invalid_sf_do, {duplicate_step_name, StepName}}) ->
    io_lib:format("duplicate sf_do step name: ~p", [StepName]);
format_error({duplicate_block_path, Path}) ->
    io_lib:format("duplicate sf_do block path: ~p", [Path]);
format_error({duplicate_block, Block}) ->
    io_lib:format("duplicate sf_do block: ~p", [Block]);
format_error({generated_function_conflict, Function}) ->
    io_lib:format("sf_do generated function conflicts with ~p/2", [Function]);
format_error({generated_function_name_too_long, Block, StepName}) ->
    io_lib:format("sf_do generated function name is too long for ~p/~p",
        [Block, StepName]);
format_error({ordering_mismatch, {source_digest, Id}}) ->
    io_lib:format("internal sf_do traversal digest mismatch at occurrence ~p", [Id]);
format_error({ordering_mismatch, {unexpected_occurrence, Occurrence}}) ->
    io_lib:format("internal sf_do traversal found an unplanned occurrence ~p", [Occurrence]);
format_error({ordering_mismatch, missing_occurrence}) ->
    "internal sf_do traversal did not rewrite every planned occurrence";
format_error(
    {ordering_mismatch,
        {incomplete_rewrite,
            #{
                owner := Owner,
                occurrence := Occurrence,
                block_depth := BlockDepth,
                plan_count := PlanCount,
                consumed_count := ConsumedCount,
                unconsumed_count := UnconsumedCount
            }}}
) ->
    io_lib:format(
        "internal sf_do traversal ended incomplete: owner=~p occurrence=~p block_depth=~B plans=~B consumed=~B unconsumed=~B",
        [
            Owner,
            Occurrence,
            BlockDepth,
            PlanCount,
            ConsumedCount,
            UnconsumedCount
        ]
    );
format_error(unnamed_sf_action) ->
    "unnamed non-final sf_do action is non-migratable; add a literal atom step marker immediately before it".

collect(Node, State, #{step := pre} = Attr) ->
    case astranaut_syntax:type(Node) of
        function ->
            {Name, Arity} = erl_syntax_lib:analyze_function(Node),
            collect_return(Node, State#{
                owner => {Name, Arity},
                occurrence => 0,
                anonymous => 0,
                block_stack => [],
                scope_stack => []
            });
        clause ->
            Visible = collect_visible_scope(State),
            Fresh = async_sf_lexical:pattern_new_bindings(
                erl_syntax:clause_patterns(Node),
                {clause, maps:get(parent, Attr, undefined)},
                Visible
            ),
            collect_return(Node, collect_push_scope(State, Fresh));
        list_comp ->
            astranaut_return:lift_m(
                fun(State1) ->
                    astranaut:walk_return(#{
                        return => collect_node(Node),
                        state => State1,
                        continue => true
                    })
                end,
                collect_list_comp(Node, collect_push_scope(State, ordsets:new()))
            );
        named_fun_expr ->
            collect_return(
                Node,
                collect_push_scope(
                    State, named_fun_variables(erl_syntax:named_fun_expr_name(Node))
                )
            );
        application ->
            case sf_call(Node) of
                false -> collect_return(Node, State);
                Call ->
                    astranaut_return:lift_m(
                        fun(State1) -> collect_return(Node, State1) end,
                        collect_sf(Node, Call, State)
                    )
            end;
        _ ->
            collect_return(Node, State)
    end;
collect(Node, State, #{step := post}) ->
    case {astranaut_syntax:type(Node), sf_call(Node)} of
        {application, Info} when Info =/= false ->
            [_ | Stack] = maps:get(block_stack, State),
            State#{block_stack => Stack};
        {match_expr, _} ->
            Visible = collect_visible_scope(State),
            Fresh = async_sf_lexical:pattern_new_bindings(
                erl_syntax:match_expr_pattern(Node), match, Visible
            ),
            collect_bind_scope(State, Fresh);
        {clause, _} ->
            collect_pop_scope(State);
        {named_fun_expr, _} ->
            collect_pop_scope(State);
        {function, _} ->
            State#{owner => undefined, block_stack => [], scope_stack => []};
        _ ->
            State
    end.

collect_sf(Node, Call, State) ->
    Owner = maps:get(owner, State),
    Occurrence = maps:get(occurrence, State),
    astranaut_return:bind(
        validate_sf_call(Call),
        fun
            ({ok, Info}) ->
                collect_valid_sf(Node, Info, Owner, Occurrence, State);
            (invalid) ->
                astranaut_return:return(collect_invalid_sf(Occurrence, State))
        end
    ).

collect_valid_sf(Node, Info, Owner, Occurrence, State) ->
    do([
        return
     || {Path, State1, PathUnique} <- allocate_block(Info, State),
        Id = {Owner, Path, Occurrence},
        Plan <- analyze_call(Node, Info, Id, State1),
        {Plan1, State2} <- allocate_boundary_names(Plan, PathUnique, State1),
        Plans = maps:put(Id, Plan1, maps:get(plans, State2)),
        OccurrencePlans = maps:put(
            {Owner, Occurrence}, Id, maps:get(occurrence_plans, State2)
        ),
        return(State2#{
            plans => Plans,
            occurrence_plans => OccurrencePlans,
            occurrence => Occurrence + 1,
            block_stack => [Path | maps:get(block_stack, State2)]
        })
    ]).

collect_invalid_sf(Occurrence, State) ->
    Stack = maps:get(block_stack, State),
    State#{
        occurrence => Occurrence + 1,
        block_stack => [current_path(Stack) | Stack]
    }.

collect_list_comp(Node, State0) ->
    QualifierReturn = astranaut_return:foldl_m(
        fun(Qualifier, StateAcc) ->
            case qualifier_type(Qualifier) of
                bind ->
                    do([
                        return
                     || StateBody <- collect_fragment(
                            erl_syntax:generator_body(Qualifier), StateAcc
                        ),
                        Fresh = async_sf_lexical:pattern_new_bindings(
                            erl_syntax:generator_pattern(Qualifier),
                            fresh,
                            ordsets:new()
                        ),
                        return(collect_bind_scope(StateBody, Fresh))
                    ]);
                match ->
                    do([
                        return
                     || StateBody <- collect_fragment(
                            erl_syntax:match_expr_body(Qualifier), StateAcc
                        ),
                        Visible = collect_visible_scope(StateBody),
                        Fresh = async_sf_lexical:pattern_new_bindings(
                            erl_syntax:match_expr_pattern(Qualifier),
                            match,
                            Visible
                        ),
                        return(collect_bind_scope(StateBody, Fresh))
                    ]);
                then ->
                    collect_fragment(Qualifier, StateAcc)
            end
        end,
        State0,
        erl_syntax:list_comp_body(Node)
    ),
    do([
        return
     || State1 <- QualifierReturn,
        State2 <- collect_fragment(erl_syntax:list_comp_template(Node), State1),
        return(collect_pop_scope(State2))
    ]).

collect_fragment(Node, State) ->
    astranaut:reduce(
        fun collect/3,
        State,
        Node,
        #{formatter => ?MODULE, traverse => all}
    ).

collect_push_scope(State, Names) ->
    State#{scope_stack => [ordsets:from_list(Names) | maps:get(scope_stack, State)]}.

collect_pop_scope(State) ->
    [_ | Tail] = maps:get(scope_stack, State),
    State#{scope_stack => Tail}.

collect_bind_scope(State, Names) ->
    [Current | Tail] = maps:get(scope_stack, State),
    State#{scope_stack => [ordsets:union(Current, ordsets:from_list(Names)) | Tail]}.

collect_visible_scope(State) ->
    lists:foldl(fun ordsets:union/2, ordsets:new(), maps:get(scope_stack, State)).

rewrite(Node, State, #{step := pre}) ->
    case astranaut_syntax:type(Node) of
        function ->
            {Name, Arity} = erl_syntax_lib:analyze_function(Node),
            {Node, State#{owner => {Name, Arity}, occurrence => 0, block_stack => []}};
        application ->
            rewrite_sf_pre(Node, State);
        _ ->
            {Node, State}
    end;
rewrite(Node, State, #{step := post}) ->
    case sf_call(Node) of
        Call when Call =/= false ->
            case maps:get(block_stack, State) of
                [#{skip := true} | Stack] ->
                    {Node, State#{block_stack => Stack}};
                [#{plan := Plan} | Stack] ->
                    {Monad, Qualifiers} = rewritten_block(Call, Plan),
                    {Expression, Functions} = lower_block(Plan, Monad, Qualifiers),
                    Generated0 = maps:get(generated, State),
                    {Expression, State#{
                        block_stack => Stack,
                        generated => lists:reverse(Functions) ++ Generated0
                    }}
            end;
        false ->
            rewrite_non_sf_post(Node, State)
    end.

rewrite_sf_pre(Node, State) ->
    case sf_call(Node) of
        false ->
            {Node, State};
        _Call ->
            Owner = maps:get(owner, State),
            Occurrence = maps:get(occurrence, State),
            OccurrenceKey = {Owner, Occurrence},
            case maps:find(OccurrenceKey, maps:get(occurrence_plans, State)) of
                {ok, Id} ->
                    Plan = maps:get(Id, maps:get(plans, State)),
                    Frame = #{plan => Plan},
                    State1 = State#{
                        occurrence => Occurrence + 1,
                        block_stack => [Frame | maps:get(block_stack, State)],
                        consumed => maps:put(Id, true, maps:get(consumed, State))
                    },
                    case node_digest(Node) =:= maps:get(original_digest, Plan) of
                        true -> {Node, State1};
                        false ->
                            astranaut:walk_return(#{
                                return => Node,
                                state => State1,
                                error => {ordering_mismatch, {source_digest, Id}}
                            })
                    end;
                error ->
                    State1 = State#{
                        occurrence => Occurrence + 1,
                        block_stack => [#{skip => true} | maps:get(block_stack, State)]
                    },
                    astranaut:walk_return(#{
                        return => Node,
                        state => State1,
                        error => {ordering_mismatch,
                            {unexpected_occurrence, OccurrenceKey}}
                    })
            end
    end.

rewritten_block(#{args := Args}, Plan) ->
    Comprehension = lists:nth(maps:get(comprehension_arg, Plan), Args),
    SourceQualifiers = erl_syntax:list_comp_body(Comprehension),
    {erl_syntax:list_comp_template(Comprehension), [
        lists:nth(SourceIndex + 1, SourceQualifiers)
     || SourceIndex <- maps:get(executable_indexes, Plan)
    ]}.

rewrite_non_sf_post(Node, State) ->
    case astranaut_syntax:type(Node) of
        function ->
            {Node, State#{owner => undefined, block_stack => []}};
        application ->
            {Node, State};
        _ ->
            {Node, State}
    end.

analyze_call(Node, Info, {_Owner, Path, _Occurrence} = Id, State) ->
    Monad = maps:get(monad, Info),
    Qualifiers = maps:get(qualifiers, Info),
    BoundBefore = collect_visible_scope(State),
    do([
        return
     || QualifierPlan <- pair_step_markers(Qualifiers),
        StepNamesUnique <- validate_unique_step_names(QualifierPlan),
        Boundaries0 = analyze_boundaries(QualifierPlan, Monad, BoundBefore),
        Boundaries = assign_identities(Boundaries0, Id, maps:get(module, State),
            maps:get(vsn, State), maps:get(block_name, Info) =/= anonymous),
        return(#{
            block => Path,
            boundaries => Boundaries,
            comprehension_arg => maps:get(comprehension_arg, Info),
            executable_indexes => [
                maps:get(source_index, Entry) || Entry <- QualifierPlan
            ],
            step_names_unique => StepNamesUnique,
            original_digest => node_digest(Node)
        })
    ]).

validate_sf_call(#{args := Args}) ->
    case Args of
        [Comprehension] ->
            validate_sf_comprehension(Comprehension, anonymous, 1);
        [BlockNameNode, Comprehension] ->
            astranaut_return:bind(
                literal_atom_result(BlockNameNode, block_name, anonymous),
                fun(BlockName) ->
                    validate_sf_comprehension(Comprehension, BlockName, 2)
                end
            );
        _ ->
            astranaut_return:error_ok(
                {invalid_sf_do, {arity, length(Args)}}, invalid
            )
    end.

validate_sf_comprehension(Node, BlockName, ArgumentIndex) ->
    case astranaut_syntax:type(Node) of
        list_comp ->
            Monad = erl_syntax:list_comp_template(Node),
            Qualifiers = erl_syntax:list_comp_body(Node),
            MonadValidation = case valid_monad_head(Monad) of
                true -> astranaut_return:return(ok);
                false -> source_error(Monad, {invalid_sf_do, invalid_monad_head})
            end,
            QualifierValidation = case Qualifiers of
                [] -> source_error(Node, {invalid_sf_do, empty});
                _ -> astranaut_return:return(ok)
            end,
            do([
                return
             || MonadValidation,
                QualifierValidation,
                return({ok, #{
                    monad => Monad,
                    qualifiers => Qualifiers,
                    block_name => BlockName,
                    comprehension_arg => ArgumentIndex
                }})
            ]);
        _ ->
            source_error(Node, {invalid_sf_do, expected_list_comprehension}, invalid)
    end.

valid_monad_head(Node) ->
    case astranaut_syntax:type(Node) of
        atom -> true;
        variable -> true;
        tuple -> length(erl_syntax:tuple_elements(Node)) =:= 2;
        _ -> false
    end.

pair_step_markers(Qualifiers) ->
    pair_step_markers(Qualifiers, 0, undefined, []).

pair_step_markers([Qualifier | Tail], Index, Pending, Entries) ->
    case astranaut_syntax:type(Qualifier) of
        atom ->
            StepName = erl_syntax:atom_value(Qualifier),
            Diagnostic = case Pending of
                undefined -> astranaut_return:return(ok);
                {_PreviousName, _PreviousNode} ->
                    source_error(Qualifier, {invalid_sf_do, consecutive_step_marker})
            end,
            astranaut_return:bind(
                Diagnostic,
                fun(_) ->
                    pair_step_markers(
                        Tail, Index + 1, {StepName, Qualifier}, Entries
                    )
                end
            );
        _ ->
            Type = qualifier_type(Qualifier),
            IsFinal = not lists:any(fun is_executable_qualifier/1, Tail),
            astranaut_return:bind(
                pair_boundary_marker(Type, IsFinal, Pending, Qualifier),
                fun({StepName, MarkerNode}) ->
                    LastQualifierValidation =
                        case Tail =:= [] andalso IsFinal andalso
                                (Type =:= bind orelse Type =:= match) of
                            true ->
                                source_error(
                                    Qualifier, {invalid_sf_do, invalid_last_qualifier}
                                );
                            false ->
                                astranaut_return:return(ok)
                        end,
                    Entry = #{
                        source_index => Index,
                        node => Qualifier,
                        kind => Type,
                        final => IsFinal,
                        step_name => StepName,
                        marker_node => MarkerNode
                    },
                    astranaut_return:then(
                        LastQualifierValidation,
                        pair_step_markers(
                            Tail, Index + 1, undefined, [Entry | Entries]
                        )
                    )
                end
            )
    end;
pair_step_markers([], _Index, Pending, Entries) ->
    Diagnostic = case Pending of
        undefined -> astranaut_return:return(ok);
        {_StepName, MarkerNode} ->
            source_error(MarkerNode, {invalid_sf_do, dangling_step_marker})
    end,
    astranaut_return:lift_m(fun(_) -> lists:reverse(Entries) end, Diagnostic).

is_executable_qualifier(Node) -> astranaut_syntax:type(Node) =/= atom.

qualifier_type(Node) ->
    case astranaut_syntax:type(Node) of
        generator -> bind;
        strict_generator -> bind;
        binary_generator -> bind;
        strict_binary_generator -> bind;
        map_generator -> bind;
        strict_map_generator -> bind;
        match_expr -> match;
        maybe_match_expr -> match;
        _ -> then
    end.

pair_boundary_marker(match, _IsFinal, Pending, _Qualifier) ->
    case Pending of
        undefined -> astranaut_return:return({undefined, undefined});
        {_StepName, MarkerNode} ->
            source_error(
                MarkerNode,
                {invalid_sf_do, step_marker_on_pure_let},
                {undefined, undefined}
            )
    end;
pair_boundary_marker(_Type, true, Pending, _Qualifier) ->
    case Pending of
        undefined -> astranaut_return:return({undefined, undefined});
        {_StepName, MarkerNode} ->
            source_error(
                MarkerNode,
                {invalid_sf_do, step_marker_on_final_action},
                {undefined, undefined}
            )
    end;
pair_boundary_marker(_Type, false, undefined, Qualifier) ->
    source_warning(Qualifier, unnamed_sf_action, {undefined, undefined});
pair_boundary_marker(_Type, false, {StepName, MarkerNode}, _Qualifier) ->
    astranaut_return:return({StepName, MarkerNode}).

validate_unique_step_names(Entries) ->
    astranaut_return:lift_m(
        fun({_Seen, Unique}) -> Unique end,
        astranaut_return:foldl_m(
            fun(Entry, {Seen, Unique}) ->
                case maps:get(step_name, Entry) of
                    undefined ->
                        astranaut_return:return({Seen, Unique});
                    StepName ->
                        case maps:is_key(StepName, Seen) of
                            true ->
                                source_error(
                                    maps:get(marker_node, Entry),
                                    {invalid_sf_do, {duplicate_step_name, StepName}},
                                    {Seen, false}
                                );
                            false ->
                                astranaut_return:return(
                                    {maps:put(StepName, true, Seen), Unique}
                                )
                        end
                end
            end,
            {#{}, true},
            Entries
        )
    ).

analyze_boundaries(QualifierPlan, Monad, InitialBound) ->
    analyze_boundaries(QualifierPlan, Monad, InitialBound, 0, []).

analyze_boundaries([Entry | Tail], Monad, Bound, ExecutableIndex, Acc) ->
    Qualifier = maps:get(node, Entry),
    Type = maps:get(kind, Entry),
    case {Type, maps:get(final, Entry)} of
        {match, _} ->
            Pattern = erl_syntax:match_expr_pattern(Qualifier),
            Fresh = async_sf_lexical:pattern_new_bindings(
                Pattern, match, Bound
            ),
            Bound1 = ordsets:union(Bound, Fresh),
            analyze_boundaries(Tail, Monad, Bound1, ExecutableIndex + 1, Acc);
        {_, true} ->
            lists:reverse(Acc);
        {bind, false} ->
            Pattern = erl_syntax:generator_pattern(Qualifier),
            Captures = required_captures(Pattern, qualifier_nodes(Tail), Monad, Bound),
            Boundary = #{
                qualifier_index => ExecutableIndex,
                kind => bind,
                boundary_ordinal => length(Acc),
                step_name => maps:get(step_name, Entry),
                captures => Captures,
                monad_captures => monad_captures(Monad, Bound),
                pos => astranaut_syntax:get_pos(Qualifier)
            },
            Fresh = async_sf_lexical:pattern_new_bindings(
                Pattern, fresh, Bound
            ),
            Bound1 = ordsets:union(Bound, Fresh),
            analyze_boundaries(Tail, Monad, Bound1, ExecutableIndex + 1, [Boundary | Acc]);
        {then, false} ->
            Captures = required_captures(undefined, qualifier_nodes(Tail), Monad, Bound),
            Boundary = #{
                qualifier_index => ExecutableIndex,
                kind => then,
                boundary_ordinal => length(Acc),
                step_name => maps:get(step_name, Entry),
                captures => Captures,
                monad_captures => monad_captures(Monad, Bound),
                pos => astranaut_syntax:get_pos(Qualifier)
            },
            analyze_boundaries(Tail, Monad, Bound, ExecutableIndex + 1, [Boundary | Acc])
    end;
analyze_boundaries([], _Monad, _Bound, _ExecutableIndex, Acc) ->
    lists:reverse(Acc).

qualifier_nodes(Entries) -> [maps:get(node, Entry) || Entry <- Entries].

required_captures(Pattern, Tail, Monad, Bound) ->
    {InitialLocal, PatternFree} =
        case Pattern of
            undefined -> {ordsets:new(), ordsets:new()};
            _ -> async_sf_lexical:pattern_bindings(Pattern, fresh, Bound)
        end,
    ContinuationFree = async_sf_lexical:free_qualifier_sequence(
        Tail, Monad, InitialLocal, Bound
    ),
    MonadFree = async_sf_lexical:free_expression(Monad, Bound),
    Required = ordsets:intersection(
        ordsets:union(PatternFree, ordsets:union(ContinuationFree, MonadFree)),
        Bound
    ),
    ordsets:to_list(Required).

monad_captures(Monad, Bound) ->
    ordsets:to_list(
        ordsets:intersection(async_sf_lexical:free_expression(Monad, Bound), Bound)
    ).

assign_identities(Boundaries, Id, Module, Vsn, NamedBlock) ->
    [assign_identity(Boundary, Id, Module, Vsn, NamedBlock)
        || Boundary <- Boundaries].

assign_identity(
    #{step_name := StepName} = Boundary,
    {_Owner, Block, _Occurrence},
    Module,
    Vsn,
    true
) when StepName =/= undefined ->
    FunctionText = "$sf_" ++ atom_to_list(Block) ++ "__" ++ atom_to_list(StepName),
    complete_boundary(Boundary, {named, FunctionText},
        {?PROTOCOL, Block, StepName}, Module, Vsn, Block);
assign_identity(Boundary, {_Owner, Block, _Occurrence}, Module, Vsn, _NamedBlock) ->
    Ordinal = maps:get(boundary_ordinal, Boundary),
    FunctionText = "$sf_local_" ++ atom_to_list(Block) ++ "__" ++ integer_to_list(Ordinal),
    complete_boundary(Boundary, {local, FunctionText}, undefined,
        Module, Vsn, Block).

complete_boundary(Boundary, FunctionPlan, Identity, Module, Vsn, Block) ->
    Boundary#{
        function_plan => FunctionPlan,
        identity => Identity,
        module => Module,
        vsn => Vsn,
        block => Block
    }.

allocate_boundary_names(Plan, true, State) ->
    case maps:get(step_names_unique, Plan) of
        true -> allocate_boundary_names(Plan, State);
        false -> astranaut_return:return({Plan, State})
    end;
allocate_boundary_names(_Plan, false, State) ->
    astranaut_return:return({_Plan, State}).

allocate_boundary_names(Plan, State) ->
    astranaut_return:lift_m(
        fun({Boundaries, State1}) ->
            {Plan#{boundaries => lists:reverse(Boundaries)}, State1}
        end,
        astranaut_return:foldl_m(
        fun(Boundary, {BoundaryAcc, StateAcc}) ->
            {_, FunctionText} = maps:get(function_plan, Boundary),
            Pos = maps:get(pos, Boundary),
            Existing = maps:get(functions, StateAcc),
            Generated = maps:get(generated_names, StateAcc),
            case length(FunctionText) =< 255 of
                false ->
                    source_error_at(Pos,
                        {generated_function_name_too_long,
                            maps:get(block, Boundary), maps:get(step_name, Boundary)},
                        {[Boundary | BoundaryAcc], StateAcc});
                true ->
                    Function = list_to_atom(FunctionText),
                    Conflict = maps:is_key({Function, 2}, Existing) orelse
                        maps:is_key(Function, Generated),
                    Boundary1 = Boundary#{function => Function,
                        mfa => {maps:get(module, Boundary), Function, 2}},
                    State1 = StateAcc#{generated_names =>
                        maps:put(Function, true, Generated)},
                    Return = {[Boundary1 | BoundaryAcc], State1},
                    case Conflict of
                        true -> source_error_at(
                            Pos, {generated_function_conflict, Function}, Return);
                        false -> astranaut_return:return(Return)
                    end
            end
        end,
        {[], State},
        maps:get(boundaries, Plan)
    )).

allocate_block(Info, State) ->
    case maps:get(block_name, Info) of
        anonymous ->
            Anonymous = maps:get(anonymous, State),
            {Name, Arity} = maps:get(owner, State),
            Locator = list_to_atom("sf_local_" ++ atom_to_list(Name) ++ "_" ++
                integer_to_list(Arity) ++ "_" ++ integer_to_list(Anonymous)),
            astranaut_return:return({Locator, State#{anonymous => Anonymous + 1}, true});
        Name ->
            register_block(Name, State)
    end.

register_block(Path, State) ->
    Paths = maps:get(block_names, State),
    State1 = State#{block_names => maps:put(Path, true, Paths)},
    case maps:is_key(Path, Paths) of
        true ->
            astranaut_return:error_ok({duplicate_block, Path}, {Path, State1, false});
        false ->
            astranaut_return:return({Path, State1, true})
    end.

current_path([]) -> [];
current_path([Path | _]) -> Path.


lower_block(Plan, Monad, [FirstQualifier | _] = Qualifiers) ->
    Boundaries = maps:get(boundaries, Plan),
    Body = lower_sequence(0, Qualifiers, Boundaries, Monad),
    Pos = astranaut_syntax:get_pos(FirstQualifier),
    Expression = {'call', Pos,
        {'fun', Pos, {clauses, [{clause, Pos, [], [], Body}]}}, []},
    Functions = [generated_frame(Boundary, Qualifiers, Boundaries, Monad)
        || Boundary <- Boundaries],
    {Expression, Functions}.

lower_sequence(Index, Qualifiers, Boundaries, Monad) ->
    Qualifier = lists:nth(Index + 1, Qualifiers),
    case boundary_at(Index, Boundaries) of
        {ok, Boundary} ->
            [frame_call(Boundary, Qualifier, Monad)];
        error ->
            case qualifier_type(Qualifier) of
                match -> [rewrite_local_expr(Qualifier, Monad) |
                    lower_sequence(Index + 1, Qualifiers, Boundaries, Monad)];
                _ ->
                    case Index =:= length(Qualifiers) - 1 of
                        true -> [rewrite_local_expr(Qualifier, Monad)];
                        false -> erlang:error({unexpected_unplanned_qualifier, astranaut_syntax:get_pos(Qualifier)})
                    end
            end
    end.

boundary_at(Index, Boundaries) ->
    case [B || B <- Boundaries, maps:get(qualifier_index, B) =:= Index] of
        [Boundary] -> {ok, Boundary};
        [] -> error
    end.

frame_call(Boundary, Qualifier, Monad) ->
    Pos = astranaut_syntax:get_pos(Qualifier),
    Action =
        case maps:get(kind, Boundary) of
            bind -> erl_syntax:generator_body(Qualifier);
            then -> Qualifier
        end,
    Action1 = rewrite_local_expr(Action, Monad),
    Context = context_ast(Boundary, Pos),
    Function = case maps:get(kind, Boundary) of bind -> bind_frame; then -> then_frame end,
    remote_call(Pos, monad_frame, Function, [Action1, abstract(maps:get(mfa, Boundary)), Context, Monad]).

generated_frame(Boundary, Qualifiers, Boundaries, Monad) ->
    Pos = maps:get(pos, Boundary),
    Used0 = source_used_names(Qualifiers, Monad),
    {InputName, Used1} = allocate_generated_name('__SfInput', Used0),
    {EnvName, Used2} = allocate_generated_name('__SfEnv', Used1),
    Input = {var, Pos, InputName},
    Env = {var, Pos, EnvName},
    Pattern = case maps:get(kind, Boundary) of
        bind -> erl_syntax:generator_pattern(
            lists:nth(maps:get(qualifier_index, Boundary) + 1, Qualifiers));
        then -> undefined
    end,
    PatternBindings =
        case Pattern of
            undefined -> ordsets:new();
            _ -> async_sf_lexical:pattern_new_bindings(
                Pattern, fresh, ordsets:new()
            )
        end,
    Captures = ordsets:from_list(maps:get(captures, Boundary)),
    ShadowedCaptures = ordsets:intersection(Captures, PatternBindings),
    {AliasMap, _Used3} = allocate_capture_aliases(ShadowedCaptures, Used2, #{}),
    DispatchMonad = replace_variables(Monad, AliasMap),
    FramePattern =
        case Pattern of
            undefined -> undefined;
            _ ->
                async_sf_lexical:rename_pattern_references(
                    Pattern, fresh, Captures, AliasMap
                )
        end,
    Restore = [
        {match, Pos,
            {var, Pos, maps:get(Name, AliasMap, Name)},
            remote_call(Pos, maps, get, [{atom, Pos, Name}, Env])}
        || Name <- maps:get(captures, Boundary)
    ],
    Suffix = lower_sequence(
        maps:get(qualifier_index, Boundary) + 1, Qualifiers, Boundaries, DispatchMonad
    ),
    Body =
        case maps:get(kind, Boundary) of
            bind ->
                case astranaut_syntax:type(FramePattern) of
                    variable ->
                        Restore ++ [{match, Pos, FramePattern, Input} | Suffix];
                    _ ->
                        Case = {'case', Pos, Input,
                            [
                                {clause, Pos, [FramePattern], [], Suffix},
                                {clause, Pos, [{var, Pos, '_'}], [], [
                                    remote_call(Pos, monad_fail, fail,
                                        [{atom, Pos, monad_badmatch}, DispatchMonad])
                                ]}
                            ]},
                        Restore ++ [Case]
                end;
            then ->
                Restore ++ Suffix
        end,
    {function, Pos, maps:get(function, Boundary), 2,
        [{clause, Pos, [Input, Env], [], Body}]}.

source_used_names(Qualifiers, Monad) ->
    ordsets:from_list(
        lists:append([syntax_variables(Node) || Node <- Qualifiers] ++ [syntax_variables(Monad)])
    ).

allocate_generated_name(Base, Used) ->
    allocate_generated_name(Base, Used, 0).

allocate_generated_name(Base, Used, Suffix) ->
    Name = case Suffix of
        0 -> Base;
        _ -> list_to_atom(atom_to_list(Base) ++ integer_to_list(Suffix))
    end,
    case ordsets:is_element(Name, Used) of
        true -> allocate_generated_name(Base, Used, Suffix + 1);
        false -> {Name, ordsets:add_element(Name, Used)}
    end.

allocate_capture_aliases([], Used, AliasMap) ->
    {AliasMap, Used};
allocate_capture_aliases([Name | Tail], Used, AliasMap) ->
    {Alias, Used1} = allocate_generated_name(
        list_to_atom("__SfCapture" ++ atom_to_list(Name)), Used
    ),
    allocate_capture_aliases(Tail, Used1, maps:put(Name, Alias, AliasMap)).

rewrite_local_expr(Node, Monad) ->
    astranaut:smap(
        fun(N, #{step := post}) ->
            case local_call_name(N) of
                return ->
                    rewrite_local_dispatch(N, return, Monad);
                fail ->
                    rewrite_local_dispatch(N, fail, Monad);
                _ -> N
            end;
           (N, _Attr) -> N
        end,
        Node,
        #{normalize => true, traverse => all, role => expression}
    ).

rewrite_local_dispatch(Node, return, Monad) ->
    Args = erl_syntax:application_arguments(Node),
    case length(Args) of
        1 -> remote_call(astranaut_syntax:get_pos(Node), monad, return, [hd(Args), Monad]);
        _ -> Node
    end;
rewrite_local_dispatch(Node, fail, Monad) ->
    Args = erl_syntax:application_arguments(Node),
    case length(Args) of
        1 -> remote_call(astranaut_syntax:get_pos(Node), monad_fail, fail, [hd(Args), Monad]);
        _ -> Node
    end.

replace_variables(Node, AliasMap) when map_size(AliasMap) =:= 0 ->
    Node;
replace_variables(Node, AliasMap) ->
    astranaut:smap(
        fun(N, _Attr) ->
            case astranaut_syntax:type(N) of
                variable ->
                    case maps:find(erl_syntax:variable_name(N), AliasMap) of
                        {ok, Alias} -> erl_syntax:variable(Alias);
                        error -> N
                    end;
                _ -> N
            end
        end,
        Node,
        #{normalize => false, traverse => all, role => expression}
    ).

context_ast(Boundary, Pos) ->
    EnvFields = [
        {map_field_assoc, Pos, {atom, Pos, Name}, {var, Pos, Name}}
        || Name <- maps:get(captures, Boundary)
    ],
    {map, Pos, [
        {map_field_assoc, Pos, {atom, Pos, protocol}, {integer, Pos, ?PROTOCOL}},
        {map_field_assoc, Pos, {atom, Pos, block}, abstract(maps:get(block, Boundary))},
        {map_field_assoc, Pos, {atom, Pos, step_name}, abstract(maps:get(step_name, Boundary))},
        {map_field_assoc, Pos, {atom, Pos, identity}, abstract(maps:get(identity, Boundary))},
        {map_field_assoc, Pos, {atom, Pos, module}, abstract(maps:get(module, Boundary))},
        {map_field_assoc, Pos, {atom, Pos, vsn}, abstract(maps:get(vsn, Boundary))},
        {map_field_assoc, Pos, {atom, Pos, env}, {map, Pos, EnvFields}}
    ]}.

manifest(Module, Vsn, Plans) ->
    Frames = lists:sort([
        #{identity => maps:get(identity, Boundary),
            step_name => maps:get(step_name, Boundary),
            mfa => maps:get(mfa, Boundary),
            required_captures => lists:sort(maps:get(captures, Boundary))}
        || Plan <- Plans, Boundary <- maps:get(boundaries, Plan)
    ]),
    #{format => 1, protocol => ?PROTOCOL, owner => Module, vsn => Vsn,
        frames => Frames}.

module_vsn(Forms) ->
    case [Vsn || {attribute, _Pos, vsn, Vsn} <- Forms] of
        [] -> undefined;
        [Vsn | _] -> Vsn
    end.

manifest_forms(Manifest) ->
    Pos = 0,
    [
        {attribute, Pos, export, [{'$async_sf_manifest', 0}]},
        {function, Pos, '$async_sf_manifest', 0,
            [{clause, Pos, [], [], [abstract(Manifest)]}]}
    ].

rewrite_complete(#{
    block_stack := [], owner := undefined, plans := Plans, consumed := Consumed
}) when map_size(Plans) =:= map_size(Consumed) ->
    case lists:all(fun(Id) -> maps:is_key(Id, Consumed) end, maps:keys(Plans)) of
        true -> ok;
        false -> {error, {ordering_mismatch, missing_occurrence}}
    end;
rewrite_complete(State) ->
    {error, {ordering_mismatch,
        {incomplete_rewrite, rewrite_state_summary(State)}}}.

rewrite_state_summary(State) ->
    Plans = maps:get(plans, State, #{}),
    Consumed = maps:get(consumed, State, #{}),
    Unconsumed = [Id || Id <- maps:keys(Plans), not maps:is_key(Id, Consumed)],
    #{
        owner => maps:get(owner, State, undefined),
        occurrence => maps:get(occurrence, State, 0),
        block_depth => length(maps:get(block_stack, State, [])),
        plan_count => map_size(Plans),
        consumed_count => map_size(Consumed),
        unconsumed_count => length(Unconsumed)
    }.

sf_call(Node) ->
    case local_call_name(Node) of
        sf_do -> #{args => erl_syntax:application_arguments(Node)};
        _ -> false
    end.

local_call_name(Node) ->
    case astranaut_syntax:type(Node) of
        application ->
            Operator = erl_syntax:application_operator(Node),
            case astranaut_syntax:type(Operator) of
                atom -> erl_syntax:atom_value(Operator);
                _ -> undefined
            end;
        _ -> undefined
    end.

named_fun_variables(Name) ->
    async_sf_lexical:pattern_new_bindings(
        Name, fresh, ordsets:new()
    ).

literal_atom_result(Node, Kind, Fallback) ->
    case astranaut_syntax:type(Node) of
        atom ->
            astranaut_return:return(erl_syntax:atom_value(Node));
        _ ->
            source_error(Node, {invalid_sf_do, {non_literal, Kind}}, Fallback)
    end.

source_error(Node, Reason) ->
    source_error_at(astranaut_syntax:get_pos(Node), Reason).

source_error(Node, Reason, Return) ->
    source_error_at(astranaut_syntax:get_pos(Node), Reason, Return).

source_error_at(Pos, Reason) ->
    astranaut_return:formatted_error(Pos, ?MODULE, Reason).

source_error_at(Pos, Reason, Return) ->
    astranaut_return:then(
        source_error_at(Pos, Reason),
        astranaut_return:return(Return)
    ).

source_warning(Node, Reason, Return) ->
    astranaut_return:then(
        astranaut_return:formatted_warning(
            astranaut_syntax:get_pos(Node), ?MODULE, Reason
        ),
        astranaut_return:return(Return)
    ).

node_digest(Node) ->
    erlang:phash2(erl_parse:map_anno(
        fun(_Anno) -> 0 end, astranaut_syntax:revert(Node))).

remote_call(Pos, Module, Function, Args) ->
    {call, Pos, {remote, Pos, {atom, Pos, Module}, {atom, Pos, Function}}, Args}.

abstract(Term) -> erl_parse:abstract(Term).

source_functions(Forms) ->
    maps:from_list([{{Name, Arity}, true} || {function, _, Name, Arity, _} <- Forms]).

syntax_variables(Node) ->
    sets:to_list(erl_syntax_lib:variables(Node)).
