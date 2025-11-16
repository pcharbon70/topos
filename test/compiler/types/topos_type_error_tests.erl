%%%-------------------------------------------------------------------
%%% @doc Error Handling and Edge Case Tests for Type System
%%%
%%% Tests defensive programming, error scenarios, and edge cases
%%% that should fail gracefully or be handled properly.
%%%
%%% ## Performance/Stress Tests
%%%
%%% Some tests that check deep nesting limits and large collections
%%% are behind a compile-time flag to avoid slowing down regular
%%% test runs. To enable these tests, compile with:
%%%
%%%   erlc -DTOPOS_ENABLE_STRESS_TESTS -o _build/test/ ...
%%%
%%% Or run the full suite with:
%%%
%%%   make test-stress
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_error_tests).

-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% Invalid Input Tests - Type Construction
%%====================================================================

invalid_construction_test_() ->
    [
     ?_test(test_invalid_type_variable_id()),
     ?_test(test_invalid_constructor_name()),
     ?_test(test_duplicate_record_fields()),
     ?_test(test_duplicate_variant_constructors())
    ].

test_invalid_type_variable_id() ->
    % Negative ID should fail
    ?assertError(function_clause, topos_types:tvar(-1)),

    % Zero ID should fail
    ?assertError(function_clause, topos_types:tvar(0)),

    % Non-integer should fail
    ?assertError(function_clause, topos_types:tvar(foo)),
    ?assertError(function_clause, topos_types:tvar("1")).

test_invalid_constructor_name() ->
    % Non-atom should fail
    ?assertError(function_clause, topos_types:tcon("Integer")),
    ?assertError(function_clause, topos_types:tcon(123)).

test_duplicate_record_fields() ->
    % Duplicate field names should be rejected
    ?assertError({duplicate_record_fields, [x]},
                 topos_types:trecord([
                     {x, topos_types:tcon(integer)},
                     {x, topos_types:tcon(string)}  % Duplicate field
                 ], closed)),

    % Multiple duplicates
    ?assertError({duplicate_record_fields, _},
                 topos_types:trecord([
                     {x, topos_types:tcon(integer)},
                     {y, topos_types:tcon(string)},
                     {x, topos_types:tcon(float)},   % x duplicated
                     {y, topos_types:tcon(boolean)}  % y duplicated
                 ], closed)),

    % No duplicates should work fine
    ValidRecord = topos_types:trecord([
        {x, topos_types:tcon(integer)},
        {y, topos_types:tcon(string)}
    ], closed),
    ?assertMatch({trecord, _, closed}, ValidRecord).

test_duplicate_variant_constructors() ->
    % Duplicate constructor names should be rejected
    ?assertError({duplicate_variant_constructors, ['Some']},
                 topos_types:tvariant([
                     {'Some', [topos_types:tcon(integer)]},
                     {'Some', [topos_types:tcon(string)]}  % Duplicate constructor
                 ])),

    % Multiple duplicates
    ?assertError({duplicate_variant_constructors, _},
                 topos_types:tvariant([
                     {'Red', []},
                     {'Green', []},
                     {'Red', []},    % Red duplicated
                     {'Green', []}   % Green duplicated
                 ])),

    % No duplicates should work fine
    ValidVariant = topos_types:tvariant([
        {'None', []},
        {'Some', [topos_types:tvar(1)]}
    ]),
    ?assertMatch({tvariant, _}, ValidVariant).

%%====================================================================
%% Circular Substitution Tests
%%====================================================================

circular_substitution_test_() ->
    [
      ?_test(test_simple_cycle()),
      ?_test(test_three_way_cycle()),
      ?_test(test_self_reference()),
      ?_test(test_occurs_check_explicit()),
      ?_test(test_depth_limit_exceeded()),
      ?_test(test_cycle_in_record_type()),
      ?_test(test_cycle_in_variant_type()),
      ?_test(test_cycle_in_tuple_type()),
      ?_test(test_cycle_in_function_type()),
      ?_test(test_indirect_cycle_through_record()),
      ?_test(test_indirect_cycle_through_variant()),
      ?_test(test_cycle_with_row_variable()),
      ?_test(test_composition_induced_cycle())
    ].

test_simple_cycle() ->
    % S = {1 ↦ α₂, 2 ↦ α₁}
    % Applying this creates infinite loop: α₁ → α₂ → α₁ → ...

    S = #{
        1 => topos_types:tvar(2),
        2 => topos_types:tvar(1)
    },

    % Occurs check should detect this and raise error
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),

    % Should also fail when starting from variable 2
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(2))).

test_three_way_cycle() ->
    % S = {1 ↦ α₂, 2 ↦ α₃, 3 ↦ α₁}
    % Creates cycle: α₁ → α₂ → α₃ → α₁

    S = #{
        1 => topos_types:tvar(2),
        2 => topos_types:tvar(3),
        3 => topos_types:tvar(1)
    },

    % Should be detected by occurs check at any entry point
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(2))),
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(3))).

test_self_reference() ->
    % S = {1 ↦ List α₁}
    % This is the classic occurs check case

    S = #{
        1 => topos_types:tapp(
            topos_types:tcon('List'),
            [topos_types:tvar(1)]  % Self-reference
        )
    },

    % Should fail occurs check
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),

    % Test the explicit occurs_check function
    ?assert(topos_type_subst:occurs_check(1, topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    ))),

    % Variable that doesn't occur should return false
    ?assertNot(topos_type_subst:occurs_check(2, topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    ))).

test_occurs_check_explicit() ->
    % Test the explicit occurs_check/2 function

    % Variable occurs in itself (wrapped)
    ?assert(topos_type_subst:occurs_check(1, topos_types:tvar(1))),

    % Variable occurs in function type
    FunType = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),
    ?assert(topos_type_subst:occurs_check(1, FunType)),
    ?assert(topos_type_subst:occurs_check(2, FunType)),
    ?assertNot(topos_type_subst:occurs_check(3, FunType)),

    % Variable occurs in nested type application
    NestedType = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tapp(
            topos_types:tcon('Maybe'),
            [topos_types:tvar(5)]
        )]
    ),
    ?assert(topos_type_subst:occurs_check(5, NestedType)),
    ?assertNot(topos_type_subst:occurs_check(1, NestedType)).

test_depth_limit_exceeded() ->
    % Create a very deep substitution chain that exceeds the depth limit
    % Build chain: α₁ → α₂ → α₃ → ... → α₆₀₀

    % Create substitution with 600 chained variables (exceeds 500 limit)
    DeepSubst = lists:foldl(
        fun(I, Acc) ->
            topos_type_subst:extend(Acc, I, topos_types:tvar(I + 1))
        end,
        topos_type_subst:empty(),
        lists:seq(1, 600)
    ),

    % Applying this should hit depth limit
    ?assertError({substitution_depth_exceeded, _, _},
                 topos_type_subst:apply(DeepSubst, topos_types:tvar(1))).

test_cycle_in_record_type() ->
    % S = {1 ↦ {x: α₁}}
    % Record contains reference to itself
    S = #{
        1 => topos_types:trecord([{x, topos_types:tvar(1)}], closed)
    },

    % Should detect cycle
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),

    % Also test with occurs_check
    ?assert(topos_type_subst:occurs_check(1,
        topos_types:trecord([{x, topos_types:tvar(1)}], closed))).

test_cycle_in_variant_type() ->
    % S = {1 ↦ 'Some'(α₁)}
    % Variant constructor contains reference to itself
    S = #{
        1 => topos_types:tvariant([{'Some', [topos_types:tvar(1)]}, {'None', []}])
    },

    % Should detect cycle
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),

    % Test with occurs_check
    ?assert(topos_type_subst:occurs_check(1,
        topos_types:tvariant([{'Some', [topos_types:tvar(1)]}]))).

test_cycle_in_tuple_type() ->
    % S = {1 ↦ (α₁, Int)}
    % Tuple contains reference to itself
    S = #{
        1 => topos_types:ttuple([topos_types:tvar(1), topos_types:tcon(integer)])
    },

    % Should detect cycle
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),

    % Test with occurs_check
    ?assert(topos_type_subst:occurs_check(1,
        topos_types:ttuple([topos_types:tvar(1), topos_types:tcon(integer)]))).

test_cycle_in_function_type() ->
    % S = {1 ↦ (α₁ -> Int)}
    % Function input type contains reference to itself
    S1 = #{
        1 => topos_types:tfun(
            topos_types:tvar(1),
            topos_types:tcon(integer),
            topos_types:empty_effects()
        )
    },

    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S1, topos_types:tvar(1))),

    % S = {2 ↦ (Int -> α₂)}
    % Function output type contains reference to itself
    S2 = #{
        2 => topos_types:tfun(
            topos_types:tcon(integer),
            topos_types:tvar(2),
            topos_types:empty_effects()
        )
    },

    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S2, topos_types:tvar(2))).

test_indirect_cycle_through_record() ->
    % S = {1 ↦ α₂, 2 ↦ {x: α₁}}
    % α₁ → α₂ → {x: α₁} creates indirect cycle
    S = #{
        1 => topos_types:tvar(2),
        2 => topos_types:trecord([{x, topos_types:tvar(1)}], closed)
    },

    % Should detect indirect cycle
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(2))).

test_indirect_cycle_through_variant() ->
    % S = {1 ↦ α₂, 2 ↦ 'Node'(α₁, α₁)}
    % α₁ → α₂ → 'Node'(α₁, α₁) creates indirect cycle
    S = #{
        1 => topos_types:tvar(2),
        2 => topos_types:tvariant([
            {'Node', [topos_types:tvar(1), topos_types:tvar(1)]},
            {'Leaf', []}
        ])
    },

    % Should detect indirect cycle
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(2))).

test_cycle_with_row_variable() ->
    % Test cycle involving row variables in records
    % S = {1 ↦ α₂, 2 ↦ {x: α₁ | ρ₃}}
    % This creates a cycle: α₁ → α₂ → {x: α₁ | ρ₃}
    S = #{
        1 => topos_types:tvar(2),
        2 => topos_types:trecord([{x, topos_types:tvar(1)}], 3)
    },

    % Should detect cycle through the field type
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S, topos_types:tvar(1))),

    % Create another case: α₁ → α₂, α₂ → {y: String | α₁}
    % This creates cycle through the row variable itself
    S2 = #{
        1 => topos_types:tvar(2),
        2 => topos_types:trecord([{y, topos_types:tcon(string)}], 1)
    },

    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S2, topos_types:tvar(1))).

test_composition_induced_cycle() ->
    % S1 = {1 ↦ α₂}
    % S2 = {2 ↦ List α₁}
    % compose(S2, S1) = {1 ↦ List α₁, 2 ↦ List α₁}
    % This creates a cycle when applied

    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S2 = topos_type_subst:singleton(2, topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    )),

    % Compose the substitutions
    S_composed = topos_type_subst:compose(S2, S1),

    % Applying to α₁ should detect cycle
    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S_composed, topos_types:tvar(1))),

    % Test another composition scenario
    % S3 = {3 ↦ α₄}, S4 = {4 ↦ (α₃ -> Int)}
    S3 = topos_type_subst:singleton(3, topos_types:tvar(4)),
    S4 = topos_type_subst:singleton(4, topos_types:tfun(
        topos_types:tvar(3),
        topos_types:tcon(integer),
        topos_types:empty_effects()
    )),

    S_composed2 = topos_type_subst:compose(S4, S3),

    ?assertError({circular_substitution, _},
                 topos_type_subst:apply(S_composed2, topos_types:tvar(3))).

%%====================================================================
%% Deep Nesting Tests
%%====================================================================

deep_nesting_test_() ->
    [
      ?_test(test_moderately_deep_type()),
      ?_test(test_deep_type_variables())
     ] ++ deep_nesting_stress_tests().

test_moderately_deep_type() ->
    % Build type with 50 levels of nesting
    % List<List<List<...<Int>...>>>

    DeepType = lists:foldl(
        fun(_, Acc) ->
            topos_types:tapp(topos_types:tcon('List'), [Acc])
        end,
        topos_types:tcon(integer),
        lists:seq(1, 50)
    ),

    % Should be able to extract variables (none in this case)
    Vars = topos_types:type_vars(DeepType),
    ?assertEqual([], sets:to_list(Vars)),

    % Should be able to pretty-print (though output is long)
    PP = topos_type_pp:pp_type(DeepType),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 100).  % Should be long

test_deep_type_variables() ->
    % Build function type with 20 nested functions
    % α₁ -> (α₂ -> (α₃ -> ... -> α₂₀))

    DeepFun = lists:foldl(
        fun(I, Acc) ->
            topos_types:tfun(
                topos_types:tvar(I),
                Acc,
                topos_types:empty_effects()
            )
        end,
        topos_types:tvar(20),
        lists:seq(19, 1, -1)
    ),

    % Should have 20 unique type variables
    Vars = topos_types:type_vars(DeepFun),
    VarList = lists:sort(sets:to_list(Vars)),
    ?assertEqual(lists:seq(1, 20), VarList).

%% Stress tests are conditionally included based on compile flag
-ifdef(TOPOS_ENABLE_STRESS_TESTS).
deep_nesting_stress_tests() ->
    [
     ?_test(test_very_deep_type_nesting()),
     ?_test(test_extremely_deep_substitution_chain())
    ].
-else.
deep_nesting_stress_tests() ->
    [].
-endif.

-ifdef(TOPOS_ENABLE_STRESS_TESTS).
test_very_deep_type_nesting() ->
    % Build type with 400 levels of nesting
    % This tests the limits of type traversal operations
    % (stays under MAX_SUBST_DEPTH of 500)
    % List<List<List<...<Int>...>>> (400 levels)

    DeepType = lists:foldl(
        fun(_, Acc) ->
            topos_types:tapp(topos_types:tcon('List'), [Acc])
        end,
        topos_types:tcon(integer),
        lists:seq(1, 400)
    ),

    % Should be able to extract variables
    Vars = topos_types:type_vars(DeepType),
    ?assertEqual([], sets:to_list(Vars)),

    % Should be able to apply empty substitution
    Result = topos_type_subst:apply(topos_type_subst:empty(), DeepType),
    ?assertEqual(DeepType, Result),

    % Should be able to pretty-print (though output is very long)
    PP = topos_type_pp:pp_type(DeepType),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 2000).

test_extremely_deep_substitution_chain() ->
    % Create a substitution chain with 400 variables
    % (stays under MAX_SUBST_DEPTH of 500)
    % α₁ → α₂ → α₃ → ... → α₄₀₀ → Int
    % This tests performance of deep substitution application

    DeepSubst = lists:foldl(
        fun(I, Acc) ->
            topos_type_subst:extend(Acc, I, topos_types:tvar(I + 1))
        end,
        topos_type_subst:singleton(400, topos_types:tcon(integer)),
        lists:seq(1, 399)
    ),

    % Applying to α₁ should eventually resolve to Int
    % This will traverse 400 substitution steps
    Result = topos_type_subst:apply(DeepSubst, topos_types:tvar(1)),
    ?assertEqual(topos_types:tcon(integer), Result),

    % Verify domain size
    Domain = topos_type_subst:domain(DeepSubst),
    ?assertEqual(400, length(Domain)).
-endif.

%%====================================================================
%% Large Collection Tests
%%====================================================================

large_collection_test_() ->
    [
      ?_test(test_large_substitution()),
      ?_test(test_substitution_size_limit()),
      ?_test(test_large_environment()),
      ?_test(test_many_effects()),
      ?_test(test_effect_set_operations()),
      ?_test(test_effect_deduplication()),
      ?_test(test_effects_in_function_types())
     ] ++ large_collection_stress_tests().

test_large_substitution() ->
    % Create substitution with 1000 mappings
    LargeSubst = lists:foldl(
        fun(I, Acc) ->
            topos_type_subst:extend(Acc, I, topos_types:tcon(integer))
        end,
        topos_type_subst:empty(),
        lists:seq(1, 1000)
    ),

    % Should have 1000 entries in domain
    Domain = topos_type_subst:domain(LargeSubst),
    ?assertEqual(1000, length(Domain)),

    % Should be able to look up any entry
    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(LargeSubst, 500)).

test_substitution_size_limit() ->
    % Test that exceeding the MAX_SUBSTITUTION_SIZE (10,000) raises an error
    % We build 10,000 mappings first (should succeed)
    Base = lists:foldl(
        fun(I, Acc) ->
            topos_type_subst:extend(Acc, I, topos_types:tcon(integer))
        end,
        topos_type_subst:empty(),
        lists:seq(1, 10000)
    ),

    % Should have 10,000 entries
    ?assertEqual(10000, maps:size(Base)),

    % Attempting to add one more should fail with substitution_too_large
    ?assertError({substitution_too_large, 10001, 10000},
                 topos_type_subst:extend(Base, 10001, topos_types:tcon(integer))).

test_large_environment() ->
    % Create environment with 1000 bindings
    LargeEnv = lists:foldl(
        fun(I, Acc) ->
            VarName = list_to_atom("var_" ++ integer_to_list(I)),
            Scheme = topos_type_scheme:mono(topos_types:tcon(integer)),
            topos_type_env:extend(Acc, VarName, Scheme)
        end,
        topos_type_env:empty(),
        lists:seq(1, 1000)
    ),

    % Should be able to look up any variable
    ?assertMatch({ok, _}, topos_type_env:lookup(LargeEnv, var_500)),

    % FTV should complete (though may be slow)
    Ftv = topos_type_env:ftv_env(LargeEnv),
    ?assert(is_tuple(Ftv)).  % Returns a set

test_many_effects() ->
    % Create effect set with 100 effects
    ManyEffects = topos_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(1, 100)]
    ),

    % Should be normalized (sorted, deduplicated)
    {effect_set, EffList} = ManyEffects,
    ?assertEqual(100, length(EffList)),

    % Should be sorted
    ?assertEqual(EffList, lists:sort(EffList)).

test_effect_set_operations() ->
    % Test union of large effect sets
    Effects1 = topos_types:normalize_effects(
        [list_to_atom("effect_a_" ++ integer_to_list(I)) || I <- lists:seq(1, 50)]
    ),
    Effects2 = topos_types:normalize_effects(
        [list_to_atom("effect_b_" ++ integer_to_list(I)) || I <- lists:seq(1, 50)]
    ),

    % Union should combine both sets
    Union = topos_types:union_effects(Effects1, Effects2),
    {effect_set, UnionList} = Union,
    ?assertEqual(100, length(UnionList)),

    % Should still be sorted and normalized
    ?assertEqual(UnionList, lists:sort(UnionList)),

    % Test union with overlapping effects
    Effects3 = topos_types:normalize_effects(
        [list_to_atom("effect_a_" ++ integer_to_list(I)) || I <- lists:seq(1, 25)]
    ),
    UnionOverlap = topos_types:union_effects(Effects1, Effects3),
    {effect_set, OverlapList} = UnionOverlap,
    ?assertEqual(50, length(OverlapList)),  % No duplicates

    % Empty effect set operations
    Empty = topos_types:empty_effects(),
    UnionWithEmpty = topos_types:union_effects(Effects1, Empty),
    ?assertEqual(Effects1, UnionWithEmpty),

    % Test union of two empty sets
    UnionEmptyEmpty = topos_types:union_effects(Empty, Empty),
    ?assertEqual(Empty, UnionEmptyEmpty),

    % Test is_pure with large sets
    ?assertNot(topos_types:is_pure(Effects1)),
    ?assert(topos_types:is_pure(Empty)).

test_effect_deduplication() ->
    % Create effect list with many duplicates
    EffectsWithDups = [io, io, io, network, network, file, file, io, network],

    % Normalize should remove duplicates and sort
    Normalized = topos_types:normalize_effects(EffectsWithDups),
    {effect_set, EffList} = Normalized,

    ?assertEqual(3, length(EffList)),  % Only io, network, file
    ?assertEqual([file, io, network], EffList),  % Sorted

    % Test with 200 effects with many duplicates
    ManyDuplicates = lists:flatten([
        [list_to_atom("effect_" ++ integer_to_list(I rem 50)) || I <- lists:seq(1, 200)]
    ]),
    NormalizedMany = topos_types:normalize_effects(ManyDuplicates),
    {effect_set, ManyList} = NormalizedMany,

    % Should have 50 unique effects
    ?assertEqual(50, length(ManyList)),
    ?assertEqual(ManyList, lists:usort(ManyList)).

test_effects_in_function_types() ->
    % Create function types with large effect sets
    LargeEffects = topos_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(1, 100)]
    ),

    FunType = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        LargeEffects
    ),

    % Should be able to extract effects
    {ok, ExtractedEffects} = topos_types:extract_function_effects(FunType),
    ?assertEqual(LargeEffects, ExtractedEffects),

    % Should be able to apply substitution
    Subst = topos_type_subst:empty(),
    ResultType = topos_type_subst:apply(Subst, FunType),
    ?assertEqual(FunType, ResultType),

    % Should be able to pretty-print
    PP = topos_type_pp:pp_type(FunType),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 100),  % Should include all effects

    % Create nested function with effects
    NestedFun = topos_types:tfun(
        topos_types:tcon(float),
        FunType,
        topos_types:singleton_effect(async)
    ),

    % Should maintain separate effect sets
    ?assert(topos_types:is_function_type(NestedFun)),
    {ok, OuterEffects} = topos_types:extract_function_effects(NestedFun),
    ?assertNot(topos_types:effects_equal(OuterEffects, LargeEffects)).

-ifdef(TOPOS_ENABLE_STRESS_TESTS).
large_collection_stress_tests() ->
    [
     ?_test(test_massive_substitution()),
     ?_test(test_massive_environment()),
     ?_test(test_massive_record_type()),
     ?_test(test_massive_effect_sets()),
     ?_test(test_effect_set_performance())
    ].
-else.
large_collection_stress_tests() ->
    [].
-endif.

-ifdef(TOPOS_ENABLE_STRESS_TESTS).
test_massive_substitution() ->
    % Create substitution with 10,000 mappings
    % This tests memory usage and lookup performance
    MassiveSubst = lists:foldl(
        fun(I, Acc) ->
            topos_type_subst:extend(Acc, I, topos_types:tcon(integer))
        end,
        topos_type_subst:empty(),
        lists:seq(1, 10000)
    ),

    % Should be able to look up any mapping
    ?assertMatch({ok, _}, topos_type_subst:lookup(MassiveSubst, 5000)),
    ?assertMatch({ok, _}, topos_type_subst:lookup(MassiveSubst, 9999)),

    % Domain should have 10,000 entries
    Domain = topos_type_subst:domain(MassiveSubst),
    ?assertEqual(10000, length(Domain)),

    % Should be able to apply to a type
    Result = topos_type_subst:apply(MassiveSubst, topos_types:tvar(100)),
    ?assertEqual(topos_types:tcon(integer), Result).

test_massive_environment() ->
    % Create environment with 10,000 bindings
    % This tests memory usage and lookup performance
    MassiveEnv = lists:foldl(
        fun(I, Acc) ->
            VarName = list_to_atom("var_" ++ integer_to_list(I)),
            Scheme = topos_type_scheme:mono(topos_types:tcon(integer)),
            topos_type_env:extend(Acc, VarName, Scheme)
        end,
        topos_type_env:empty(),
        lists:seq(1, 10000)
    ),

    % Should be able to look up variables
    ?assertMatch({ok, _}, topos_type_env:lookup(MassiveEnv, var_5000)),
    ?assertMatch({ok, _}, topos_type_env:lookup(MassiveEnv, var_9999)),

    % FTV computation should complete
    Ftv = topos_type_env:ftv_env(MassiveEnv),
    ?assert(is_tuple(Ftv)).

test_massive_record_type() ->
    % Create record type with 500 fields
    % This tests record construction and field lookup performance
    Fields = [{list_to_atom("field_" ++ integer_to_list(I)), topos_types:tcon(integer)}
              || I <- lists:seq(1, 500)],

    MassiveRecord = topos_types:trecord(Fields, closed),

    % Should construct successfully
    ?assertMatch({trecord, _, closed}, MassiveRecord),

    % Should be able to extract type variables
    Vars = topos_types:type_vars(MassiveRecord),
    ?assertEqual([], sets:to_list(Vars)),

    % Should be able to pretty-print
    PP = topos_type_pp:pp_type(MassiveRecord),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 5000).

test_massive_effect_sets() ->
    % Create effect set with 1,000 effects
    % This tests normalization and storage of very large effect sets
    MassiveEffects = topos_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(1, 1000)]
    ),

    % Should be normalized
    {effect_set, EffList} = MassiveEffects,
    ?assertEqual(1000, length(EffList)),
    ?assertEqual(EffList, lists:usort(EffList)),

    % Should support union operations
    MoreEffects = topos_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(500, 1500)]
    ),
    Union = topos_types:union_effects(MassiveEffects, MoreEffects),
    {effect_set, UnionList} = Union,
    ?assertEqual(1500, length(UnionList)),

    % Should support equality checks
    ?assert(topos_types:effects_equal(MassiveEffects, MassiveEffects)),
    ?assertNot(topos_types:effects_equal(MassiveEffects, MoreEffects)),

    % Should be usable in function types
    FunWithMassiveEffects = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        MassiveEffects
    ),

    % Should be able to extract effects
    {ok, Extracted} = topos_types:extract_function_effects(FunWithMassiveEffects),
    ?assertEqual(MassiveEffects, Extracted),

    % Should be able to apply substitutions
    Result = topos_type_subst:apply(topos_type_subst:empty(), FunWithMassiveEffects),
    ?assertEqual(FunWithMassiveEffects, Result).

test_effect_set_performance() ->
    % Test performance of effect set operations with many duplicates
    % Create 5,000 effects with only 100 unique values
    ManyDuplicates = lists:flatten([
        [list_to_atom("effect_" ++ integer_to_list(I rem 100)) || I <- lists:seq(1, 5000)]
    ]),

    % Normalization should handle this efficiently
    Normalized = topos_types:normalize_effects(ManyDuplicates),
    {effect_set, EffList} = Normalized,
    ?assertEqual(100, length(EffList)),

    % Test union of multiple large sets
    Set1 = topos_types:normalize_effects(
        [list_to_atom("set1_" ++ integer_to_list(I)) || I <- lists:seq(1, 200)]
    ),
    Set2 = topos_types:normalize_effects(
        [list_to_atom("set2_" ++ integer_to_list(I)) || I <- lists:seq(1, 200)]
    ),
    Set3 = topos_types:normalize_effects(
        [list_to_atom("set3_" ++ integer_to_list(I)) || I <- lists:seq(1, 200)]
    ),

    % Chain multiple unions
    Union1 = topos_types:union_effects(Set1, Set2),
    Union2 = topos_types:union_effects(Union1, Set3),
    {effect_set, FinalList} = Union2,
    ?assertEqual(600, length(FinalList)),
    ?assertEqual(FinalList, lists:usort(FinalList)),

    % Test with overlapping sets
    Set4 = topos_types:normalize_effects(
        [list_to_atom("set1_" ++ integer_to_list(I)) || I <- lists:seq(1, 100)]
    ),
    UnionOverlap = topos_types:union_effects(Set1, Set4),
    {effect_set, OverlapList} = UnionOverlap,
    ?assertEqual(200, length(OverlapList)),  % No duplicates from overlap

    % Test is_pure predicate
    ?assertNot(topos_types:is_pure(Union2)),
    ?assertNot(topos_types:is_pure(Normalized)).
-endif.

%%====================================================================
%% Substitution Edge Cases
%%====================================================================

substitution_edge_cases_test_() ->
    [
      ?_test(test_empty_substitution_application()),
      ?_test(test_identity_substitution()),
      ?_test(test_substitution_on_substitution()),
      ?_test(test_row_variable_edge_cases())
    ].

%%====================================================================
%% Row Variable Substitution Tests
%%====================================================================

row_variable_substitution_test_() ->
    [
      ?_test(test_row_var_to_row_var()),
      ?_test(test_row_var_to_closed()),
      ?_test(test_row_var_in_nested_records()),
      ?_test(test_multiple_row_vars()),
      ?_test(test_row_var_composition()),
      ?_test(test_row_var_type_vars()),
      ?_test(test_row_var_pretty_printing()),
      ?_test(test_row_var_field_merging())
    ].

test_empty_substitution_application() ->
    Empty = topos_type_subst:empty(),

    % Applying empty substitution should be identity
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),

    ?assertEqual(Type, topos_type_subst:apply(Empty, Type)).

test_identity_substitution() ->
    % S = {1 ↦ α₁} (maps variable to itself)
    S = topos_type_subst:singleton(1, topos_types:tvar(1)),

    Type = topos_types:tvar(1),

    % Should return same variable
    ?assertEqual(Type, topos_type_subst:apply(S, Type)).

test_substitution_on_substitution() ->
    % Apply substitution to the range of another substitution
    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S2 = topos_type_subst:singleton(2, topos_types:tcon(integer)),

    % Compose: should map 1 → Int (via 2)
    Composed = topos_type_subst:compose(S2, S1),

    Result = topos_type_subst:apply(Composed, topos_types:tvar(1)),
    ?assertEqual(topos_types:tcon(integer), Result).

test_row_variable_edge_cases() ->
    % Row variable substituted with closed record
    S = topos_type_subst:singleton(
        1,
        topos_types:trecord([{x, topos_types:tcon(integer)}], closed)
    ),

    OpenRecord = topos_types:trecord(
        [{y, topos_types:tcon(string)}],
        1  % Row variable
    ),

    % After substitution, row variable should be closed
    Result = topos_type_subst:apply(S, OpenRecord),
    ?assertMatch({trecord, _, closed}, Result).

test_row_var_to_row_var() ->
    % Substituting one row variable with another
    % S = {ρ₁ ↦ ρ₂}
    S = topos_type_subst:singleton(1, topos_types:tvar(2)),

    % Record with ρ₁: {x: Int | ρ₁}
    Rec1 = topos_types:trecord([{x, topos_types:tcon(integer)}], 1),

    % After substitution: {x: Int | ρ₂}
    Result = topos_type_subst:apply(S, Rec1),
    ?assertMatch({trecord, [{x, _}], 2}, Result),

    % Test chaining: First apply S1={ρ₁→ρ₂}, then S2={ρ₂→ρ₃}
    % Row variable substitution is shallow, so we need two applications
    % or composition to get full chain resolution
    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S2 = topos_type_subst:singleton(2, topos_types:tvar(3)),

    % Apply S1 first: {x: Int | ρ₁} -> {x: Int | ρ₂}
    Result2a = topos_type_subst:apply(S1, Rec1),
    ?assertMatch({trecord, [{x, _}], 2}, Result2a),

    % Then apply S2: {x: Int | ρ₂} -> {x: Int | ρ₃}
    Result2b = topos_type_subst:apply(S2, Result2a),
    ?assertMatch({trecord, [{x, _}], 3}, Result2b),

    % Identity: S = {ρ₁ ↦ ρ₁}
    SIdentity = topos_type_subst:singleton(1, topos_types:tvar(1)),
    ResultIdentity = topos_type_subst:apply(SIdentity, Rec1),
    ?assertEqual(Rec1, ResultIdentity).

test_row_var_to_closed() ->
    % Substituting row variable with 'closed'
    % This happens when we know the record has no more fields

    % S = {ρ₁ ↦ {}}  (empty closed record)
    S1 = topos_type_subst:singleton(
        1,
        topos_types:trecord([], closed)
    ),

    OpenRec = topos_types:trecord([{x, topos_types:tcon(integer)}], 1),
    Result1 = topos_type_subst:apply(S1, OpenRec),
    ?assertMatch({trecord, [{x, _}], closed}, Result1),

    % S = {ρ₁ ↦ {y: String}}  (closed record with field)
    S2 = topos_type_subst:singleton(
        1,
        topos_types:trecord([{y, topos_types:tcon(string)}], closed)
    ),

    Result2 = topos_type_subst:apply(S2, OpenRec),
    ?assertMatch({trecord, [{x, _}], closed}, Result2),

    % Test that closed stays closed
    ClosedRec = topos_types:trecord([{a, topos_types:tcon(atom)}], closed),
    ResultClosed = topos_type_subst:apply(S1, ClosedRec),
    ?assertEqual(ClosedRec, ResultClosed).

test_row_var_in_nested_records() ->
    % Test row variables in nested record types
    % {outer: {inner: Int | ρ₁} | ρ₂}

    InnerRec = topos_types:trecord([{inner, topos_types:tcon(integer)}], 1),
    OuterRec = topos_types:trecord([{outer, InnerRec}], 2),

    % Substitute inner row variable
    S1 = topos_type_subst:singleton(1, topos_types:tvar(3)),
    Result1 = topos_type_subst:apply(S1, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, [{inner, _}], 3}}], 2}, Result1),

    % Substitute outer row variable
    S2 = topos_type_subst:singleton(2, topos_types:tvar(4)),
    Result2 = topos_type_subst:apply(S2, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, _, 1}}], 4}, Result2),

    % Substitute both
    S3 = topos_type_subst:extend(S1, 2, topos_types:tvar(4)),
    Result3 = topos_type_subst:apply(S3, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, _, 3}}], 4}, Result3),

    % Close inner
    S4 = topos_type_subst:singleton(1, topos_types:trecord([], closed)),
    Result4 = topos_type_subst:apply(S4, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, _, closed}}], 2}, Result4).

test_multiple_row_vars() ->
    % Test multiple independent row variables in the same type
    % For example in a tuple of records: ({a: Int | ρ₁}, {b: String | ρ₂})

    Rec1 = topos_types:trecord([{a, topos_types:tcon(integer)}], 1),
    Rec2 = topos_types:trecord([{b, topos_types:tcon(string)}], 2),
    TupleType = topos_types:ttuple([Rec1, Rec2]),

    % Substitute only ρ₁
    S1 = topos_type_subst:singleton(1, topos_types:tvar(3)),
    Result1 = topos_type_subst:apply(S1, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], 3},
        {trecord, [{b, _}], 2}
    ]}, Result1),

    % Substitute only ρ₂
    S2 = topos_type_subst:singleton(2, topos_types:tvar(4)),
    Result2 = topos_type_subst:apply(S2, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], 1},
        {trecord, [{b, _}], 4}
    ]}, Result2),

    % Substitute both
    S3 = topos_type_subst:extend(S1, 2, topos_types:tvar(4)),
    Result3 = topos_type_subst:apply(S3, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], 3},
        {trecord, [{b, _}], 4}
    ]}, Result3),

    % Close both
    S4 = #{
        1 => topos_types:trecord([], closed),
        2 => topos_types:trecord([], closed)
    },
    Result4 = topos_type_subst:apply(S4, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], closed},
        {trecord, [{b, _}], closed}
    ]}, Result4).

test_row_var_composition() ->
    % Test substitution composition with row variables
    % S1 = {ρ₁ ↦ ρ₂}, S2 = {ρ₂ ↦ ρ₃}
    % compose(S2, S1) should give {ρ₁ ↦ ρ₃, ρ₂ ↦ ρ₃}

    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S2 = topos_type_subst:singleton(2, topos_types:tvar(3)),

    SComposed = topos_type_subst:compose(S2, S1),

    Rec = topos_types:trecord([{x, topos_types:tcon(integer)}], 1),
    Result = topos_type_subst:apply(SComposed, Rec),
    ?assertMatch({trecord, [{x, _}], 3}, Result),

    % S1 = {ρ₁ ↦ ρ₂}, S2 = {ρ₂ ↦ closed}
    S3 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S4 = topos_type_subst:singleton(2, topos_types:trecord([], closed)),

    SComposed2 = topos_type_subst:compose(S4, S3),
    Result2 = topos_type_subst:apply(SComposed2, Rec),
    ?assertMatch({trecord, [{x, _}], closed}, Result2).

test_row_var_type_vars() ->
    % Test that type_vars correctly extracts row variables

    % Open record with row variable
    OpenRec = topos_types:trecord([{x, topos_types:tcon(integer)}], 1),
    Vars1 = topos_types:type_vars(OpenRec),
    ?assertEqual([1], lists:sort(sets:to_list(Vars1))),

    % Closed record has no row variable
    ClosedRec = topos_types:trecord([{x, topos_types:tcon(integer)}], closed),
    Vars2 = topos_types:type_vars(ClosedRec),
    ?assertEqual([], sets:to_list(Vars2)),

    % Record with type variable in field and row variable
    RecWithTVar = topos_types:trecord([{x, topos_types:tvar(2)}], 1),
    Vars3 = topos_types:type_vars(RecWithTVar),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars3))),

    % Nested records with multiple row variables
    InnerRec = topos_types:trecord([{inner, topos_types:tvar(3)}], 1),
    OuterRec = topos_types:trecord([{outer, InnerRec}], 2),
    Vars4 = topos_types:type_vars(OuterRec),
    ?assertEqual([1, 2, 3], lists:sort(sets:to_list(Vars4))).

test_row_var_pretty_printing() ->
    % Test pretty-printing of records with row variables

    % Open record
    OpenRec = topos_types:trecord([{x, topos_types:tcon(integer)}], 1),
    PP1 = topos_type_pp:pp_type(OpenRec),
    ?assert(is_list(PP1)),
    ?assert(length(PP1) > 0),

    % Closed record
    ClosedRec = topos_types:trecord([{x, topos_types:tcon(integer)}], closed),
    PP2 = topos_type_pp:pp_type(ClosedRec),
    ?assert(is_list(PP2)),
    ?assert(length(PP2) > 0),

    % Record with multiple fields and row variable
    MultiFieldRec = topos_types:trecord([
        {x, topos_types:tcon(integer)},
        {y, topos_types:tcon(string)},
        {z, topos_types:tcon(float)}
    ], 5),
    PP3 = topos_type_pp:pp_type(MultiFieldRec),
    ?assert(is_list(PP3)),
    ?assert(length(PP3) > 10),

    % Nested record with row variables
    InnerRec = topos_types:trecord([{inner, topos_types:tcon(atom)}], 2),
    OuterRec = topos_types:trecord([{outer, InnerRec}], 3),
    PP4 = topos_type_pp:pp_type(OuterRec),
    ?assert(is_list(PP4)),
    ?assert(length(PP4) > 5).

test_row_var_field_merging() ->
    % Test scenarios that might involve field merging (conceptually)
    % Even though we don't actually merge, test the substitution behavior

    % S = {ρ₁ ↦ {y: String}}
    % Applied to {x: Int | ρ₁} conceptually gives {x: Int, y: String}
    S = topos_type_subst:singleton(
        1,
        topos_types:trecord([{y, topos_types:tcon(string)}], closed)
    ),

    OpenRec = topos_types:trecord([{x, topos_types:tcon(integer)}], 1),
    Result = topos_type_subst:apply(S, OpenRec),

    % After substitution, row variable becomes closed
    ?assertMatch({trecord, [{x, _}], closed}, Result),

    % With multiple fields on both sides
    S2 = topos_type_subst:singleton(
        1,
        topos_types:trecord([
            {y, topos_types:tcon(string)},
            {z, topos_types:tcon(float)}
        ], closed)
    ),

    OpenRec2 = topos_types:trecord([
        {a, topos_types:tcon(atom)},
        {b, topos_types:tcon(boolean)}
    ], 1),

    Result2 = topos_type_subst:apply(S2, OpenRec2),
    ?assertMatch({trecord, [{a, _}, {b, _}], closed}, Result2),

    % Empty record extension: {| ρ₁} with S = {ρ₁ ↦ {x: Int}}
    EmptyRec = topos_types:trecord([], 1),
    Result3 = topos_type_subst:apply(S, EmptyRec),
    ?assertMatch({trecord, [], closed}, Result3).

%%====================================================================
%% Fresh Variable Generation Edge Cases
%%====================================================================

fresh_var_edge_cases_test_() ->
    [
      ?_test(test_counter_wraparound()),
      ?_test(test_many_fresh_variables())
    ].

test_counter_wraparound() ->
    % Test that counter doesn't overflow (Erlang handles big integers)
    % Generate many variables and check uniqueness

    State0 = topos_type_state:new(),

    % Generate 10,000 variables
    {Vars, _StateFinal} = lists:foldl(
        fun(_, {AccVars, AccState}) ->
            {Var, NewState} = topos_types:fresh_var(AccState),
            {[Var | AccVars], NewState}
        end,
        {[], State0},
        lists:seq(1, 10000)
    ),

    % All should be unique
    UniqueVars = lists:usort(Vars),
    ?assertEqual(10000, length(UniqueVars)),

    % IDs should be sequential (reversed due to cons)
    {tvar, FirstId} = lists:last(Vars),  % First generated
    ?assertEqual(1, FirstId),
    {tvar, LastId} = hd(Vars),  % Last generated
    ?assertEqual(10000, LastId).

test_many_fresh_variables() ->
    % Stress test: generate 50,000 variables
    State0 = topos_type_state:new(),

    % This should not crash or slow down significantly
    Count = 50000,
    {_Vars, StateFinal} = lists:foldl(
        fun(_, {AccVars, AccState}) ->
            {Var, NewState} = topos_types:fresh_var(AccState),
            {[Var | AccVars], NewState}
        end,
        {[], State0},
        lists:seq(1, Count)
    ),

    % Counter should be at expected value
    {_NextVar, StateNext} = topos_types:fresh_var(StateFinal),
    ?assertEqual(Count + 1, topos_type_state:get_counter(StateNext)).

%%====================================================================
%% Pretty-Printing Edge Cases
%%====================================================================

pretty_printing_edge_cases_test_() ->
    [
      ?_test(test_empty_structures()),
      ?_test(test_special_atom_names()),
      ?_test(test_very_long_type())
    ].

test_empty_structures() ->
    % Empty tuple
    EmptyTuple = topos_types:ttuple([]),
    ?assertEqual("()", topos_type_pp:pp_type(EmptyTuple)),

    % Empty variant (edge case, may not be semantically valid)
    EmptyVariant = topos_types:tvariant([]),
    PP = topos_type_pp:pp_type(EmptyVariant),
    ?assert(is_list(PP)),

    % Closed record with no fields
    EmptyRecord = topos_types:trecord([], closed),
    ?assertEqual("{}", topos_type_pp:pp_type(EmptyRecord)).

test_special_atom_names() ->
    % Test atoms with special characters
    SpecialCon = topos_types:tcon('Foo-Bar'),
    PP = topos_type_pp:pp_type(SpecialCon),
    ?assertEqual("Foo-Bar", PP),

    % Atom with spaces (quoted)
    QuotedCon = topos_types:tcon('My Type'),
    PP2 = topos_type_pp:pp_type(QuotedCon),
    ?assertEqual("My Type", PP2).

test_very_long_type() ->
    % Create type with many fields/constructors
    ManyFields = [{list_to_atom("field_" ++ integer_to_list(I)),
                   topos_types:tcon(integer)} || I <- lists:seq(1, 50)],

    LargeRecord = topos_types:trecord(ManyFields, closed),

    % Should produce very long string
    PP = topos_type_pp:pp_type(LargeRecord),
    ?assert(length(PP) > 500),

    % Should contain all field names
    ?assert(string:find(PP, "field_1") =/= nomatch),
    ?assert(string:find(PP, "field_50") =/= nomatch).

%%====================================================================
%% Type Scheme Edge Cases
%%====================================================================

type_scheme_edge_cases_test_() ->
    [
      ?_test(test_generalize_with_no_free_vars()),
      ?_test(test_instantiate_empty_quantifiers()),
      ?_test(test_nested_quantification())
    ].

test_generalize_with_no_free_vars() ->
    % Generalize a concrete type (no variables)
    ConcreteType = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        topos_types:empty_effects()
    ),

    EmptyEnv = topos_type_env:empty(),
    Scheme = topos_type_scheme:generalize(ConcreteType, topos_type_env:ftv_env(EmptyEnv)),

    % Should be monomorphic (no quantified variables)
    ?assertMatch({mono, _}, Scheme).

test_instantiate_empty_quantifiers() ->
    % Create polymorphic scheme with empty quantifier list
    % (should behave like monomorphic)

    State = topos_type_state:new(),
    Type = topos_types:tcon(integer),
    Scheme = topos_type_scheme:poly([], Type),

    % Instantiate should return original type
    {Result, _NewState} = topos_type_scheme:instantiate(Scheme, State),
    ?assertEqual(Type, Result).

test_nested_quantification() ->
    % Generalize twice (should only quantify once)
    State0 = topos_type_state:new(),

    {{tvar, Var1}, _State1} = topos_types:fresh_var(State0),
    Type = topos_types:tvar(Var1),

    EmptyEnv = topos_type_env:empty(),
    EnvFtv = topos_type_env:ftv_env(EmptyEnv),

    % First generalization
    Scheme1 = topos_type_scheme:generalize(Type, EnvFtv),
    ?assertMatch({poly, [_], _}, Scheme1),

    % Can't generalize a scheme (only types)
    % This is a type system invariant
    ok.

%%====================================================================
%% Environment Edge Cases
%%====================================================================

environment_edge_cases_test_() ->
    [
      ?_test(test_remove_nonexistent()),
      ?_test(test_shadow_and_remove()),
      ?_test(test_empty_environment_ftv())
    ].

test_remove_nonexistent() ->
    Env = topos_type_env:singleton(x, topos_type_scheme:mono(topos_types:tcon(integer))),

    % Remove variable that doesn't exist (should be no-op)
    Env2 = topos_type_env:remove(Env, nonexistent),

    % Should still be able to look up original
    ?assertMatch({ok, _}, topos_type_env:lookup(Env2, x)),
    ?assertEqual(none, topos_type_env:lookup(Env2, nonexistent)).

test_shadow_and_remove() ->
    IntScheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    StringScheme = topos_type_scheme:mono(topos_types:tcon(string)),

    Env1 = topos_type_env:singleton(x, IntScheme),
    Env2 = topos_type_env:extend(Env1, x, StringScheme),  % Shadow

    % Should get string scheme
    ?assertEqual({ok, StringScheme}, topos_type_env:lookup(Env2, x)),

    % Remove x (removes shadow, doesn't restore original)
    Env3 = topos_type_env:remove(Env2, x),
    ?assertEqual(none, topos_type_env:lookup(Env3, x)).

test_empty_environment_ftv() ->
    Empty = topos_type_env:empty(),
    Ftv = topos_type_env:ftv_env(Empty),

    % Should be empty set
    ?assertEqual([], sets:to_list(Ftv)).

%%====================================================================
%% Integration: Error Scenarios
%%====================================================================

integration_error_test_() ->
    [
      ?_test(test_complex_substitution_chain()),
      ?_test(test_generalize_after_large_substitution())
    ].

test_complex_substitution_chain() ->
    % Build chain: α₁ → α₂ → α₃ → Int
    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S2 = topos_type_subst:singleton(2, topos_types:tvar(3)),
    S3 = topos_type_subst:singleton(3, topos_types:tcon(integer)),

    % Compose all
    S12 = topos_type_subst:compose(S2, S1),
    S123 = topos_type_subst:compose(S3, S12),

    % Apply to α₁
    Result = topos_type_subst:apply(S123, topos_types:tvar(1)),

    % Should resolve to Int
    ?assertEqual(topos_types:tcon(integer), Result).

test_generalize_after_large_substitution() ->
    % Create large substitution
    LargeSubst = lists:foldl(
        fun(I, Acc) ->
            topos_type_subst:extend(Acc, I, topos_types:tcon(integer))
        end,
        topos_type_subst:empty(),
        lists:seq(1, 100)
    ),

    % Create type with many variables
    ManyVarType = topos_types:ttuple([
        topos_types:tvar(I) || I <- lists:seq(1, 100)
    ]),

    % Apply substitution (all vars become Int)
    SubstitutedType = topos_type_subst:apply(LargeSubst, ManyVarType),

    % Generalize (should be monomorphic, no free vars)
    EmptyEnv = topos_type_env:empty(),
    Scheme = topos_type_scheme:generalize(SubstitutedType, topos_type_env:ftv_env(EmptyEnv)),

    ?assertMatch({mono, _}, Scheme).

%%====================================================================
%% Error Constructor Tests
%%====================================================================

error_constructor_test_() ->
    [
      ?_test(test_error_constructors())
    ].

test_error_constructors() ->
    % Test that all error constructors return correctly structured errors
    ?assertMatch({circular_substitution, 42},
                 topos_type_error:circular_substitution(42)),

    ?assertMatch({substitution_depth_exceeded, 150, 100},
                 topos_type_error:substitution_depth_exceeded(150, 100)),

    ?assertMatch({substitution_too_large, 10001, 10000},
                 topos_type_error:substitution_too_large(10001, 10000)),

    ?assertMatch({duplicate_record_fields, [x, y]},
                 topos_type_error:duplicate_record_fields([x, y])),

    ?assertMatch({duplicate_variant_constructors, ['Some', 'None']},
                 topos_type_error:duplicate_variant_constructors(['Some', 'None'])),

    IntType = topos_types:tcon(integer),
    StringType = topos_types:tcon(string),
    ?assertMatch({unification_error, _, _},
                 topos_type_error:unification_error(IntType, StringType)),

    ?assertMatch({occurs_check, 1, _},
                 topos_type_error:occurs_check(1, IntType)),

    ?assertMatch({type_depth_exceeded, 200, 100},
                 topos_type_error:type_depth_exceeded(200, 100)),

    ?assertMatch({unbound_variable, foo},
                 topos_type_error:unbound_variable(foo)),

    ?assertMatch({environment_too_large, 5001, 5000},
                 topos_type_error:environment_too_large(5001, 5000)),

    ?assertMatch({arity_mismatch, 'List', 1, 2},
                 topos_type_error:arity_mismatch('List', 1, 2)),

    Args = [IntType, StringType],
    ?assertMatch({invalid_type_application, _, _},
                 topos_type_error:invalid_type_application(IntType, Args)),

    Pure = topos_types:empty_effects(),
    Impure = topos_types:singleton_effect(io),
    ?assertMatch({effect_mismatch, _, _},
                 topos_type_error:effect_mismatch(Pure, Impure)),

    RecordType = topos_types:trecord([{x, IntType}], closed),
    ?assertMatch({missing_field, y, _},
                 topos_type_error:missing_field(y, RecordType)).

%%====================================================================
%% Error Formatting Tests
%%====================================================================

error_formatting_test_() ->
    [
      ?_test(test_format_circular_substitution()),
      ?_test(test_format_substitution_depth_exceeded()),
      ?_test(test_format_substitution_too_large()),
      ?_test(test_format_duplicate_record_fields()),
      ?_test(test_format_duplicate_variant_constructors()),
      ?_test(test_format_unification_error()),
      ?_test(test_format_occurs_check()),
      ?_test(test_format_type_depth_exceeded()),
      ?_test(test_format_unbound_variable()),
      ?_test(test_format_environment_too_large()),
      ?_test(test_format_arity_mismatch()),
      ?_test(test_format_invalid_type_application()),
      ?_test(test_format_effect_mismatch_pure_to_impure()),
      ?_test(test_format_effect_mismatch_impure_to_pure()),
      ?_test(test_format_effect_mismatch_different_effects()),
      ?_test(test_format_missing_field()),
      ?_test(test_format_unknown_error())
      % test_format_with_location() excluded - requires topos_location module
    ].

test_format_circular_substitution() ->
    Error = topos_type_error:circular_substitution(42),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Circular") > 0),
    ?assert(string:str(Msg, "α42") > 0).

test_format_substitution_depth_exceeded() ->
    Error = topos_type_error:substitution_depth_exceeded(150, 100),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "substitution depth") > 0),
    ?assert(string:str(Msg, "150") > 0),
    ?assert(string:str(Msg, "100") > 0).

test_format_unification_error() ->
    Type1 = topos_types:tcon(integer),
    Type2 = topos_types:tcon(string),
    Error = topos_type_error:unification_error(Type1, Type2),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "unification") > 0),
    ?assert(string:str(Msg, "integer") > 0),
    ?assert(string:str(Msg, "string") > 0).

test_format_occurs_check() ->
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tcon(integer),
        topos_types:empty_effects()
    ),
    Error = topos_type_error:occurs_check(1, Type),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Occurs check") > 0),
    ?assert(string:str(Msg, "α1") > 0),
    ?assert(string:str(Msg, "infinite") > 0).

test_format_type_depth_exceeded() ->
    Error = topos_type_error:type_depth_exceeded(200, 100),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "depth") > 0),
    ?assert(string:str(Msg, "200") > 0),
    ?assert(string:str(Msg, "100") > 0).

test_format_unbound_variable() ->
    Error = topos_type_error:unbound_variable(foo),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Unbound") > 0),
    ?assert(string:str(Msg, "foo") > 0).

test_format_arity_mismatch() ->
    Error = topos_type_error:arity_mismatch('List', 1, 2),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Arity mismatch") > 0),
    ?assert(string:str(Msg, "List") > 0).

test_format_substitution_too_large() ->
    Error = topos_type_error:substitution_too_large(10001, 10000),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "substitution too large") > 0),
    ?assert(string:str(Msg, "10001") > 0),
    ?assert(string:str(Msg, "10000") > 0).

test_format_duplicate_record_fields() ->
    Error = topos_type_error:duplicate_record_fields([x, y, z]),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Duplicate field") > 0),
    ?assert(string:str(Msg, "x") > 0),
    ?assert(string:str(Msg, "y") > 0),
    ?assert(string:str(Msg, "z") > 0).

test_format_duplicate_variant_constructors() ->
    Error = topos_type_error:duplicate_variant_constructors(['Some', 'None', 'Other']),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Duplicate constructor") > 0),
    ?assert(string:str(Msg, "Some") > 0),
    ?assert(string:str(Msg, "None") > 0),
    ?assert(string:str(Msg, "Other") > 0).

test_format_environment_too_large() ->
    Error = topos_type_error:environment_too_large(5001, 5000),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "environment too large") > 0),
    ?assert(string:str(Msg, "5001") > 0),
    ?assert(string:str(Msg, "5000") > 0).

test_format_invalid_type_application() ->
    Constructor = topos_types:tcon('Maybe'),
    Args = [topos_types:tcon(integer), topos_types:tcon(string)],
    Error = topos_type_error:invalid_type_application(Constructor, Args),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Invalid type application") > 0),
    ?assert(string:str(Msg, "Maybe") > 0),
    ?assert(string:str(Msg, "2") > 0).  % 2 arguments

test_format_effect_mismatch_pure_to_impure() ->
    Pure = topos_types:empty_effects(),
    Impure = topos_types:singleton_effect(io),
    Error = topos_type_error:effect_mismatch(Pure, Impure),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Effect mismatch") > 0),
    ?assert(string:str(Msg, "pure") > 0),
    ?assert(string:str(Msg, "{io}") > 0).

test_format_effect_mismatch_impure_to_pure() ->
    Pure = topos_types:empty_effects(),
    Impure = topos_types:singleton_effect(network),
    Error = topos_type_error:effect_mismatch(Impure, Pure),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Effect mismatch") > 0),
    ?assert(string:str(Msg, "pure") > 0),
    ?assert(string:str(Msg, "{network}") > 0).

test_format_effect_mismatch_different_effects() ->
    Effects1 = topos_types:union_effects(
        topos_types:singleton_effect(io),
        topos_types:singleton_effect(file)
    ),
    Effects2 = topos_types:union_effects(
        topos_types:singleton_effect(network),
        topos_types:singleton_effect(database)
    ),
    Error = topos_type_error:effect_mismatch(Effects1, Effects2),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Effect mismatch") > 0),
    ?assert(string:str(Msg, "file") > 0 orelse string:str(Msg, "io") > 0),
    ?assert(string:str(Msg, "network") > 0 orelse string:str(Msg, "database") > 0).

test_format_unknown_error() ->
    % Test the catch-all case for unknown errors
    UnknownError = {unknown_error_type, some, data},
    Msg = topos_type_error:format_error(UnknownError),
    ?assert(string:str(Msg, "Unknown type error") > 0),
    ?assert(string:str(Msg, "unknown_error_type") > 0).

test_format_missing_field() ->
    RecordType = topos_types:trecord([
        {x, topos_types:tcon(integer)}
    ], closed),
    Error = topos_type_error:missing_field(y, RecordType),
    Msg = topos_type_error:format_error(Error),
    ?assert(string:str(Msg, "Missing field") > 0),
    ?assert(string:str(Msg, "y") > 0).

test_format_with_location() ->
    Loc = topos_location:new(10, 5),
    Error = topos_type_error:unbound_variable(foo),
    Msg = topos_type_error:format_error_with_location(Loc, Error),
    ?assert(string:str(Msg, "10:5") > 0),
    ?assert(string:str(Msg, "foo") > 0).
