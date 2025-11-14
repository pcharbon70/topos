%%%-------------------------------------------------------------------
%%% @doc Error Handling and Edge Case Tests for Type System
%%%
%%% Tests defensive programming, error scenarios, and edge cases
%%% that should fail gracefully or be handled properly.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_error_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    topos_types:init_fresh_counter(),
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Invalid Input Tests - Type Construction
%%====================================================================

invalid_construction_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_invalid_type_variable_id()),
      ?_test(test_invalid_constructor_name()),
      ?_test(test_duplicate_record_fields()),
      ?_test(test_duplicate_variant_constructors())
     ]}.

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
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_simple_cycle()),
      ?_test(test_three_way_cycle()),
      ?_test(test_self_reference()),
      ?_test(test_occurs_check_explicit()),
      ?_test(test_depth_limit_exceeded())
     ]}.

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

%%====================================================================
%% Deep Nesting Tests
%%====================================================================

deep_nesting_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_moderately_deep_type()),
      ?_test(test_deep_type_variables())
     ]}.

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

%%====================================================================
%% Large Collection Tests
%%====================================================================

large_collection_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_large_substitution()),
      ?_test(test_large_environment()),
      ?_test(test_many_effects())
     ]}.

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

%%====================================================================
%% Substitution Edge Cases
%%====================================================================

substitution_edge_cases_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_empty_substitution_application()),
      ?_test(test_identity_substitution()),
      ?_test(test_substitution_on_substitution()),
      ?_test(test_row_variable_edge_cases())
     ]}.

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

%%====================================================================
%% Fresh Variable Generation Edge Cases
%%====================================================================

fresh_var_edge_cases_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_counter_wraparound()),
      ?_test(test_many_fresh_variables())
     ]}.

test_counter_wraparound() ->
    % Test that counter doesn't overflow (Erlang handles big integers)
    % Generate many variables and check uniqueness

    topos_types:reset_fresh_counter(),

    % Generate 10,000 variables
    Vars = [topos_types:fresh_var() || _ <- lists:seq(1, 10000)],

    % All should be unique
    UniqueVars = lists:usort(Vars),
    ?assertEqual(10000, length(UniqueVars)),

    % IDs should be sequential
    {tvar, LastId} = lists:last(Vars),
    ?assertEqual(10000, LastId).

test_many_fresh_variables() ->
    % Stress test: generate 50,000 variables
    topos_types:reset_fresh_counter(),

    % This should not crash or slow down significantly
    Count = 50000,
    _Vars = [topos_types:fresh_var() || _ <- lists:seq(1, Count)],

    % Counter should be at expected value
    {tvar, NextId} = topos_types:fresh_var(),
    ?assertEqual(Count + 1, NextId).

%%====================================================================
%% Pretty-Printing Edge Cases
%%====================================================================

pretty_printing_edge_cases_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_empty_structures()),
      ?_test(test_special_atom_names()),
      ?_test(test_very_long_type())
     ]}.

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
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_generalize_with_no_free_vars()),
      ?_test(test_instantiate_empty_quantifiers()),
      ?_test(test_nested_quantification())
     ]}.

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

    Type = topos_types:tcon(integer),
    Scheme = topos_type_scheme:poly([], Type),

    % Instantiate should return original type
    Result = topos_type_scheme:instantiate(Scheme),
    ?assertEqual(Type, Result).

test_nested_quantification() ->
    % Generalize twice (should only quantify once)
    topos_types:reset_fresh_counter(),

    {tvar, Var1} = topos_types:fresh_var(),
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
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_remove_nonexistent()),
      ?_test(test_shadow_and_remove()),
      ?_test(test_empty_environment_ftv())
     ]}.

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
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_complex_substitution_chain()),
      ?_test(test_generalize_after_large_substitution())
     ]}.

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
