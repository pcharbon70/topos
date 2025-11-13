%%%-------------------------------------------------------------------
%%% @doc Unit Tests for topos_type_subst module
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_subst_tests).

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
%% Substitution Construction Tests
%%====================================================================

construction_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_empty()),
      ?_test(test_singleton()),
      ?_test(test_lookup()),
      ?_test(test_extend())
     ]}.

test_empty() ->
    Empty = topos_type_subst:empty(),
    ?assertEqual(#{}, Empty),
    ?assertEqual([], topos_type_subst:domain(Empty)),
    ?assertEqual([], topos_type_subst:range(Empty)).

test_singleton() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),
    ?assertEqual(#{1 => {tcon, integer}}, S),
    ?assertEqual([1], topos_type_subst:domain(S)),
    ?assertEqual([{tcon, integer}], topos_type_subst:range(S)).

test_lookup() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % Variable in substitution
    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(S, 1)),

    % Variable not in substitution
    ?assertEqual(none, topos_type_subst:lookup(S, 2)).

test_extend() ->
    S1 = topos_type_subst:empty(),
    S2 = topos_type_subst:extend(S1, 1, topos_types:tcon(integer)),
    S3 = topos_type_subst:extend(S2, 2, topos_types:tcon(string)),

    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(S3, 1)),
    ?assertEqual({ok, topos_types:tcon(string)},
                 topos_type_subst:lookup(S3, 2)),

    % Extend can override
    S4 = topos_type_subst:extend(S3, 1, topos_types:tcon(boolean)),
    ?assertEqual({ok, topos_types:tcon(boolean)},
                 topos_type_subst:lookup(S4, 1)).

%%====================================================================
%% Substitution Application Tests
%%====================================================================

application_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_apply_to_var()),
      ?_test(test_apply_to_con()),
      ?_test(test_apply_to_fun()),
      ?_test(test_apply_to_app()),
      ?_test(test_apply_to_tuple()),
      ?_test(test_apply_to_record())
     ]}.

test_apply_to_var() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % Variable in substitution
    Var1 = topos_types:tvar(1),
    ?assertEqual(topos_types:tcon(integer),
                 topos_type_subst:apply(S, Var1)),

    % Variable not in substitution
    Var2 = topos_types:tvar(2),
    ?assertEqual(Var2, topos_type_subst:apply(S, Var2)).

test_apply_to_con() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % Constants are unchanged
    Con = topos_types:tcon(string),
    ?assertEqual(Con, topos_type_subst:apply(S, Con)).

test_apply_to_fun() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % α -> β becomes Int -> β
    Fun = topos_types:tfun(
        topos_types:tvar(1),  % α
        topos_types:tvar(2),  % β
        topos_types:empty_effects()
    ),

    Expected = topos_types:tfun(
        topos_types:tcon(integer),  % α substituted with Int
        topos_types:tvar(2),        % β unchanged
        topos_types:empty_effects()
    ),

    ?assertEqual(Expected, topos_type_subst:apply(S, Fun)).

test_apply_to_app() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % List α becomes List Int
    ListAlpha = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    ),

    Expected = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tcon(integer)]
    ),

    ?assertEqual(Expected, topos_type_subst:apply(S, ListAlpha)).

test_apply_to_tuple() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % (α, String) becomes (Int, String)
    Tuple = topos_types:ttuple([
        topos_types:tvar(1),
        topos_types:tcon(string)
    ]),

    Expected = topos_types:ttuple([
        topos_types:tcon(integer),
        topos_types:tcon(string)
    ]),

    ?assertEqual(Expected, topos_type_subst:apply(S, Tuple)).

test_apply_to_record() ->
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % {x: α, y: String | ρ} becomes {x: Int, y: String | ρ}
    Record = topos_types:trecord(
        [{x, topos_types:tvar(1)}, {y, topos_types:tcon(string)}],
        2  % Row variable
    ),

    Expected = topos_types:trecord(
        [{x, topos_types:tcon(integer)}, {y, topos_types:tcon(string)}],
        2  % Row variable unchanged
    ),

    ?assertEqual(Expected, topos_type_subst:apply(S, Record)).

%%====================================================================
%% Substitution Composition Tests
%%====================================================================

composition_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_compose_empty()),
      ?_test(test_compose_disjoint()),
      ?_test(test_compose_chain()),
      ?_test(test_compose_override())
     ]}.

test_compose_empty() ->
    S1 = topos_type_subst:singleton(1, topos_types:tcon(integer)),
    Empty = topos_type_subst:empty(),

    % S1 ∘ empty = S1
    Result1 = topos_type_subst:compose(S1, Empty),
    ?assertEqual(S1, Result1),

    % empty ∘ S1 = S1
    Result2 = topos_type_subst:compose(Empty, S1),
    ?assertEqual(S1, Result2).

test_compose_disjoint() ->
    % S1 = {1 ↦ Int}
    S1 = topos_type_subst:singleton(1, topos_types:tcon(integer)),
    % S2 = {2 ↦ String}
    S2 = topos_type_subst:singleton(2, topos_types:tcon(string)),

    % S2 ∘ S1 = {1 ↦ Int, 2 ↦ String}
    Result = topos_type_subst:compose(S2, S1),

    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(Result, 1)),
    ?assertEqual({ok, topos_types:tcon(string)},
                 topos_type_subst:lookup(Result, 2)).

test_compose_chain() ->
    % S1 = {1 ↦ α₂}
    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    % S2 = {2 ↦ Int}
    S2 = topos_type_subst:singleton(2, topos_types:tcon(integer)),

    % S2 ∘ S1: Apply S1 first, then S2
    % {1 ↦ α₂} then {2 ↦ Int}
    % Result: {1 ↦ Int, 2 ↦ Int}
    Result = topos_type_subst:compose(S2, S1),

    % α₁ should map to Int (via α₂)
    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(Result, 1)),
    % α₂ should map to Int directly
    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(Result, 2)).

test_compose_override() ->
    % S1 = {1 ↦ α₂}
    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    % S2 = {1 ↦ Int} (overrides S1's binding for 1)
    S2 = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    % S2 ∘ S1: S2's binding takes precedence
    Result = topos_type_subst:compose(S2, S1),

    % α₁ should map to Int (S2's binding wins)
    ?assertEqual({ok, topos_types:tcon(integer)},
                 topos_type_subst:lookup(Result, 1)).

%%====================================================================
%% Substitution Laws (Properties) Tests
%%====================================================================

laws_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_identity_law()),
      ?_test(test_composition_apply()),
      ?_test(test_idempotent_application())
     ]}.

test_identity_law() ->
    % Applying empty substitution is identity
    Empty = topos_type_subst:empty(),

    Type1 = topos_types:tvar(1),
    ?assertEqual(Type1, topos_type_subst:apply(Empty, Type1)),

    Type2 = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tcon(integer),
        topos_types:empty_effects()
    ),
    ?assertEqual(Type2, topos_type_subst:apply(Empty, Type2)).

test_composition_apply() ->
    % Apply (S2 ∘ S1) = Apply S2 (Apply S1 type)

    S1 = topos_type_subst:singleton(1, topos_types:tvar(2)),
    S2 = topos_type_subst:singleton(2, topos_types:tcon(integer)),

    Type = topos_types:tvar(1),

    % Method 1: Compose then apply
    Composed = topos_type_subst:compose(S2, S1),
    Result1 = topos_type_subst:apply(Composed, Type),

    % Method 2: Apply S1, then apply S2
    Intermediate = topos_type_subst:apply(S1, Type),
    Result2 = topos_type_subst:apply(S2, Intermediate),

    ?assertEqual(Result1, Result2),
    ?assertEqual(topos_types:tcon(integer), Result1).

test_idempotent_application() ->
    % Applying the same substitution twice should be idempotent
    S = topos_type_subst:singleton(1, topos_types:tcon(integer)),

    Type = topos_types:tvar(1),
    Applied1 = topos_type_subst:apply(S, Type),
    Applied2 = topos_type_subst:apply(S, Applied1),

    ?assertEqual(Applied1, Applied2),
    ?assertEqual(topos_types:tcon(integer), Applied2).

%%====================================================================
%% Complex Scenario Tests
%%====================================================================

complex_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_nested_substitution()),
      ?_test(test_function_type_substitution())
     ]}.

test_nested_substitution() ->
    % Build complex substitution chain
    % S1 = {1 ↦ List α₂}
    S1 = topos_type_subst:singleton(1,
        topos_types:tapp(topos_types:tcon('List'), [topos_types:tvar(2)])),

    % S2 = {2 ↦ Int}
    S2 = topos_type_subst:singleton(2, topos_types:tcon(integer)),

    % Compose: S2 ∘ S1
    S = topos_type_subst:compose(S2, S1),

    % Apply to α₁
    Type = topos_types:tvar(1),
    Result = topos_type_subst:apply(S, Type),

    % Should get List Int
    Expected = topos_types:tapp(topos_types:tcon('List'),
                                [topos_types:tcon(integer)]),
    ?assertEqual(Expected, Result).

test_function_type_substitution() ->
    % Type: (α₁ -> α₂) -> List α₁ -> List α₂
    InnerFun = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),

    ListAlpha1 = topos_types:tapp(topos_types:tcon('List'), [topos_types:tvar(1)]),
    ListAlpha2 = topos_types:tapp(topos_types:tcon('List'), [topos_types:tvar(2)]),

    MapType = topos_types:tfun(
        InnerFun,
        topos_types:tfun(ListAlpha1, ListAlpha2, topos_types:empty_effects()),
        topos_types:empty_effects()
    ),

    % Substitution: {1 ↦ Int, 2 ↦ String}
    S1 = topos_type_subst:singleton(1, topos_types:tcon(integer)),
    S2 = topos_type_subst:singleton(2, topos_types:tcon(string)),
    S = topos_type_subst:compose(S2, S1),

    % Apply substitution
    Result = topos_type_subst:apply(S, MapType),

    % Expected: (Int -> String) -> List Int -> List String
    ExpectedInner = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        topos_types:empty_effects()
    ),
    ExpectedListInt = topos_types:tapp(topos_types:tcon('List'),
                                       [topos_types:tcon(integer)]),
    ExpectedListString = topos_types:tapp(topos_types:tcon('List'),
                                          [topos_types:tcon(string)]),
    Expected = topos_types:tfun(
        ExpectedInner,
        topos_types:tfun(ExpectedListInt, ExpectedListString,
                        topos_types:empty_effects()),
        topos_types:empty_effects()
    ),

    ?assertEqual(Expected, Result).
