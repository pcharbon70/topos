%%%-------------------------------------------------------------------
%%% @doc Unit Tests for topos_type_scheme module
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_scheme_tests).

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
%% Type Scheme Construction Tests
%%====================================================================

construction_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_mono()),
      ?_test(test_poly())
     ]}.

test_mono() ->
    IntType = topos_types:tcon(integer),
    Scheme = topos_type_scheme:mono(IntType),

    ?assertEqual({mono, {tcon, integer}}, Scheme).

test_poly() ->
    % ∀α. α -> α
    VarAlpha = topos_types:tvar(1),
    FunType = topos_types:tfun(VarAlpha, VarAlpha, topos_types:empty_effects()),

    Scheme = topos_type_scheme:poly([1], FunType),
    ?assertMatch({poly, [1], {tfun, _, _, _}}, Scheme).

%%====================================================================
%% Generalization Tests
%%====================================================================

generalization_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_generalize_no_free_env()),
      ?_test(test_generalize_with_free_env()),
      ?_test(test_generalize_all_bound()),
      ?_test(test_generalize_concrete_type())
     ]}.

test_generalize_no_free_env() ->
    % Type: α -> β
    % Env free vars: {}
    % Expected: ∀α β. α -> β
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),

    EnvFreeVars = sets:new(),
    Scheme = topos_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({poly, [1, 2], _}, Scheme).

test_generalize_with_free_env() ->
    % Type: α -> β
    % Env free vars: {β}
    % Expected: ∀α. α -> β  (β not quantified because it's free in env)
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),

    EnvFreeVars = sets:from_list([2]),  % β is free in environment
    Scheme = topos_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({poly, [1], _}, Scheme),  % Only α is quantified

    % Verify β is still in the type but not quantified
    {poly, QuantVars, _} = Scheme,
    ?assertEqual([1], QuantVars).

test_generalize_all_bound() ->
    % Type: α -> β
    % Env free vars: {α, β}
    % Expected: α -> β (monomorphic because all vars are free in env)
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),

    EnvFreeVars = sets:from_list([1, 2]),  % Both free in environment
    Scheme = topos_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({mono, _}, Scheme).

test_generalize_concrete_type() ->
    % Type: Int -> String
    % Env free vars: {}
    % Expected: Int -> String (monomorphic, no type variables)
    Type = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        topos_types:empty_effects()
    ),

    EnvFreeVars = sets:new(),
    Scheme = topos_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({mono, _}, Scheme).

%%====================================================================
%% Instantiation Tests
%%====================================================================

instantiation_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_instantiate_mono()),
      ?_test(test_instantiate_poly()),
      ?_test(test_instantiate_creates_fresh())
     ]}.

test_instantiate_mono() ->
    % Monomorphic schemes instantiate to themselves
    Type = topos_types:tcon(integer),
    Scheme = topos_type_scheme:mono(Type),

    Instantiated = topos_type_scheme:instantiate(Scheme),
    ?assertEqual(Type, Instantiated).

test_instantiate_poly() ->
    topos_types:reset_fresh_counter(),

    % ∀α. α -> α
    VarAlpha = topos_types:tvar(1),
    FunType = topos_types:tfun(VarAlpha, VarAlpha, topos_types:empty_effects()),
    Scheme = topos_type_scheme:poly([1], FunType),

    % Instantiate should create fresh variables
    Instantiated = topos_type_scheme:instantiate(Scheme),

    % Should be a function type with same structure
    ?assertMatch({tfun, _, _, _}, Instantiated),

    % But with fresh variables
    {tfun, From, To, _} = Instantiated,
    ?assertMatch({tvar, _}, From),
    ?assertMatch({tvar, _}, To),

    % From and To should be the same variable (both were α)
    {tvar, FromId} = From,
    {tvar, ToId} = To,
    ?assertEqual(FromId, ToId).

test_instantiate_creates_fresh() ->
    topos_types:reset_fresh_counter(),

    % ∀α β. (α -> β)
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:poly([1, 2], Type),

    % Two instantiations should create different fresh variables
    Inst1 = topos_type_scheme:instantiate(Scheme),
    Inst2 = topos_type_scheme:instantiate(Scheme),

    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Inst1),
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Inst2),

    % Extract variable IDs
    {tfun, {tvar, From1}, {tvar, To1}, _} = Inst1,
    {tfun, {tvar, From2}, {tvar, To2}, _} = Inst2,

    % First instantiation should have different vars than second
    ?assertNotEqual(From1, From2),
    ?assertNotEqual(To1, To2).

%%====================================================================
%% Free Type Variables Tests
%%====================================================================

ftv_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_ftv_mono()),
      ?_test(test_ftv_poly_no_free()),
      ?_test(test_ftv_poly_with_free())
     ]}.

test_ftv_mono() ->
    % Mono type: α -> β
    % All variables are free
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:mono(Type),

    Ftv = topos_type_scheme:ftv_scheme(Scheme),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([1, 2], FtvList).

test_ftv_poly_no_free() ->
    % ∀α β. α -> β
    % All variables are quantified, no free variables
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:poly([1, 2], Type),

    Ftv = topos_type_scheme:ftv_scheme(Scheme),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([], FtvList).

test_ftv_poly_with_free() ->
    % ∀α. α -> β
    % β is free (not quantified)
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:poly([1], Type),  % Only quantify α

    Ftv = topos_type_scheme:ftv_scheme(Scheme),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([2], FtvList).  % Only β is free

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_generalize_then_instantiate())
     ]}.

test_generalize_then_instantiate() ->
    topos_types:reset_fresh_counter(),

    % Use fresh_var to create variables (this advances the counter)
    {tvar, Var1} = topos_types:fresh_var(),
    {tvar, Var2} = topos_types:fresh_var(),

    % Create type: α₁ -> α₂
    Type = topos_types:tfun(
        topos_types:tvar(Var1),
        topos_types:tvar(Var2),
        topos_types:empty_effects()
    ),

    % Generalize with empty environment
    EnvFreeVars = sets:new(),
    Scheme = topos_type_scheme:generalize(Type, EnvFreeVars),

    % Should be polymorphic with both vars quantified
    ?assertMatch({poly, _, _}, Scheme),

    % Instantiate should create fresh variables
    Inst = topos_type_scheme:instantiate(Scheme),

    % Should have structure with fresh vars
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Inst),

    {tfun, {tvar, FromId}, {tvar, ToId}, _} = Inst,

    % Fresh variables should be different from each other
    ?assertNotEqual(FromId, ToId),

    % And different from the original variables
    ?assertNotEqual(Var1, FromId),
    ?assertNotEqual(Var1, ToId),
    ?assertNotEqual(Var2, FromId),
    ?assertNotEqual(Var2, ToId).
