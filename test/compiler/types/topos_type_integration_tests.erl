%%%-------------------------------------------------------------------
%%% @doc Integration Tests for Type System
%%%
%%% Tests the interaction between all type system modules:
%%% - topos_types (type construction)
%%% - topos_type_subst (substitution)
%%% - topos_type_scheme (generalization/instantiation)
%%% - topos_type_env (environments)
%%% - topos_type_pp (pretty-printing)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_integration_tests).

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
%% Complete Type Inference Workflow Tests
%%====================================================================

workflow_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_identity_function()),
      ?_test(test_const_function()),
      ?_test(test_compose_function()),
      ?_test(test_map_function())
     ]}.

test_identity_function() ->
    % Simulate type checking: let id = λx. x
    topos_types:reset_fresh_counter(),

    % Create type for λx. x: α₁ -> α₁
    {tvar, Alpha} = topos_types:fresh_var(),
    IdType = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tvar(Alpha),
        topos_types:empty_effects()
    ),

    % Generalize in empty environment
    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    IdScheme = topos_type_scheme:generalize(IdType, EnvFreeVars),

    % Should be polymorphic: ∀α₁. α₁ -> α₁
    ?assertMatch({poly, [_], _}, IdScheme),

    % Add to environment
    Env = topos_type_env:extend(EmptyEnv, id, IdScheme),

    % Look up and instantiate
    {ok, SchemeFound} = topos_type_env:lookup(Env, id),
    Inst1 = topos_type_scheme:instantiate(SchemeFound),
    Inst2 = topos_type_scheme:instantiate(SchemeFound),

    % Both instantiations should be function types
    ?assertMatch({tfun, _, _, _}, Inst1),
    ?assertMatch({tfun, _, _, _}, Inst2),

    % But with different fresh variables
    {tfun, {tvar, From1}, {tvar, To1}, _} = Inst1,
    {tfun, {tvar, From2}, {tvar, To2}, _} = Inst2,

    ?assertNotEqual(From1, From2),
    ?assertEqual(From1, To1),  % Same var in each instance
    ?assertEqual(From2, To2),

    % Pretty-print the scheme
    PP = topos_type_pp:pp_scheme(IdScheme),
    ?assertEqual("∀α1. α1 -> α1", PP).

test_const_function() ->
    % Simulate: let const = λx. λy. x
    % Type: ∀α β. α -> β -> α
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),
    {tvar, Beta} = topos_types:fresh_var(),

    ConstType = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tfun(
            topos_types:tvar(Beta),
            topos_types:tvar(Alpha),
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),

    % Generalize
    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    ConstScheme = topos_type_scheme:generalize(ConstType, EnvFreeVars),

    % Should be: ∀α β. α -> β -> α
    ?assertMatch({poly, [_, _], _}, ConstScheme),

    % Pretty-print
    PP = topos_type_pp:pp_scheme(ConstScheme),
    ?assertEqual("∀α1 α2. α1 -> α2 -> α1", PP),

    % Instantiate and verify fresh variables
    Inst = topos_type_scheme:instantiate(ConstScheme),
    ?assertMatch({tfun, {tvar, _}, {tfun, {tvar, _}, {tvar, _}, _}, _}, Inst).

test_compose_function() ->
    % Simulate: let compose = λf. λg. λx. f (g x)
    % Type: ∀α β γ. (β -> γ) -> (α -> β) -> α -> γ
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),
    {tvar, Beta} = topos_types:fresh_var(),
    {tvar, Gamma} = topos_types:fresh_var(),

    % β -> γ
    FType = topos_types:tfun(
        topos_types:tvar(Beta),
        topos_types:tvar(Gamma),
        topos_types:empty_effects()
    ),

    % α -> β
    GType = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tvar(Beta),
        topos_types:empty_effects()
    ),

    % α -> γ
    ResultType = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tvar(Gamma),
        topos_types:empty_effects()
    ),

    % (β -> γ) -> (α -> β) -> α -> γ
    ComposeType = topos_types:tfun(
        FType,
        topos_types:tfun(
            GType,
            ResultType,
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),

    % Generalize
    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    ComposeScheme = topos_type_scheme:generalize(ComposeType, EnvFreeVars),

    % Should be polymorphic with 3 type variables
    ?assertMatch({poly, [_, _, _], _}, ComposeScheme),

    % Pretty-print
    PP = topos_type_pp:pp_scheme(ComposeScheme),
    Expected = "∀α1 α2 α3. (α2 -> α3) -> (α1 -> α2) -> α1 -> α3",
    ?assertEqual(Expected, PP).

test_map_function() ->
    % Simulate: let map = λf. λlist. ...
    % Type: ∀α β. (α -> β) -> List<α> -> List<β>
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),
    {tvar, Beta} = topos_types:fresh_var(),

    % α -> β
    FType = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tvar(Beta),
        topos_types:empty_effects()
    ),

    % List<α>
    ListAlpha = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(Alpha)]
    ),

    % List<β>
    ListBeta = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(Beta)]
    ),

    % (α -> β) -> List<α> -> List<β>
    MapType = topos_types:tfun(
        FType,
        topos_types:tfun(
            ListAlpha,
            ListBeta,
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),

    % Generalize
    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    MapScheme = topos_type_scheme:generalize(MapType, EnvFreeVars),

    % Pretty-print
    PP = topos_type_pp:pp_scheme(MapScheme),
    Expected = "∀α1 α2. (α1 -> α2) -> List<α1> -> List<α2>",
    ?assertEqual(Expected, PP).

%%====================================================================
%% Substitution and Generalization Integration Tests
%%====================================================================

subst_integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_substitution_before_generalization()),
      ?_test(test_environment_with_substitution())
     ]}.

test_substitution_before_generalization() ->
    % Simulate unification: α₁ = Int, then generalize α₁ -> α₂
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),
    {tvar, Beta} = topos_types:fresh_var(),

    % Create type: α₁ -> α₂
    Type = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tvar(Beta),
        topos_types:empty_effects()
    ),

    % Apply substitution {α₁ ↦ Int}
    Subst = topos_type_subst:singleton(Alpha, topos_types:tcon(integer)),
    SubstitutedType = topos_type_subst:apply(Subst, Type),

    % Should be: Int -> α₂
    ?assertMatch({tfun, {tcon, integer}, {tvar, _}, _}, SubstitutedType),

    % Generalize (only α₂ should be quantified)
    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    Scheme = topos_type_scheme:generalize(SubstitutedType, EnvFreeVars),

    % Should be: ∀α₂. Int -> α₂
    ?assertMatch({poly, [_], _}, Scheme),

    PP = topos_type_pp:pp_scheme(Scheme),
    ?assertEqual("∀α2. integer -> α2", PP).

test_environment_with_substitution() ->
    % Build environment with multiple bindings, apply substitution
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),
    {tvar, Beta} = topos_types:fresh_var(),

    % x: α₁
    XScheme = topos_type_scheme:mono(topos_types:tvar(Alpha)),

    % y: α₂
    YScheme = topos_type_scheme:mono(topos_types:tvar(Beta)),

    % Build environment
    Env = topos_type_env:from_list([
        {x, XScheme},
        {y, YScheme}
    ]),

    % FTV should be {α₁, α₂}
    Ftv = topos_type_env:ftv_env(Env),
    FtvList = lists:sort(sets:to_list(Ftv)),
    ?assertEqual([Alpha, Beta], FtvList).

%%====================================================================
%% Complex Type Scenarios
%%====================================================================

complex_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_record_with_polymorphism()),
      ?_test(test_effectful_map()),
      ?_test(test_nested_type_application())
     ]}.

test_record_with_polymorphism() ->
    % ∀α. {x: α, y: α}
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),

    RecordType = topos_types:trecord([
        {x, topos_types:tvar(Alpha)},
        {y, topos_types:tvar(Alpha)}
    ], closed),

    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    Scheme = topos_type_scheme:generalize(RecordType, EnvFreeVars),

    PP = topos_type_pp:pp_scheme(Scheme),
    ?assertEqual("∀α1. {x: α1, y: α1}", PP).

test_effectful_map() ->
    % ∀α β. (α -> β / {io}) -> List<α> -> List<β> / {io}
    topos_types:reset_fresh_counter(),

    {tvar, Alpha} = topos_types:fresh_var(),
    {tvar, Beta} = topos_types:fresh_var(),

    % α -> β / {io}
    FType = topos_types:tfun(
        topos_types:tvar(Alpha),
        topos_types:tvar(Beta),
        topos_types:singleton_effect(io)
    ),

    ListAlpha = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(Alpha)]
    ),

    ListBeta = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(Beta)]
    ),

    % (α -> β / {io}) -> List<α> -> List<β> / {io}
    MapType = topos_types:tfun(
        FType,
        topos_types:tfun(
            ListAlpha,
            ListBeta,
            topos_types:singleton_effect(io)
        ),
        topos_types:empty_effects()
    ),

    EmptyEnv = topos_type_env:empty(),
    EnvFreeVars = topos_type_env:ftv_env(EmptyEnv),
    Scheme = topos_type_scheme:generalize(MapType, EnvFreeVars),

    PP = topos_type_pp:pp_scheme(Scheme),
    Expected = "∀α1 α2. (α1 -> α2 / {io}) -> List<α1> -> List<α2> / {io}",
    ?assertEqual(Expected, PP).

test_nested_type_application() ->
    % List<List<Int>>
    ListListInt = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tapp(
            topos_types:tcon('List'),
            [topos_types:tcon(integer)]
        )]
    ),

    PP = topos_type_pp:pp_type(ListListInt),
    ?assertEqual("List<List<integer>>", PP).

%%====================================================================
%% End-to-End Scenario: Multiple Let Bindings
%%====================================================================

end_to_end_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_multiple_let_bindings())
     ]}.

test_multiple_let_bindings() ->
    % Simulate:
    % let id = λx. x in
    % let const = λx. λy. x in
    % let app = λf. λx. f x in
    % ...
    topos_types:reset_fresh_counter(),

    % id: ∀α. α -> α
    {tvar, Id_Alpha} = topos_types:fresh_var(),
    IdType = topos_types:tfun(
        topos_types:tvar(Id_Alpha),
        topos_types:tvar(Id_Alpha),
        topos_types:empty_effects()
    ),
    EmptyEnv = topos_type_env:empty(),
    IdScheme = topos_type_scheme:generalize(IdType, topos_type_env:ftv_env(EmptyEnv)),

    % const: ∀α β. α -> β -> α
    {tvar, Const_Alpha} = topos_types:fresh_var(),
    {tvar, Const_Beta} = topos_types:fresh_var(),
    ConstType = topos_types:tfun(
        topos_types:tvar(Const_Alpha),
        topos_types:tfun(
            topos_types:tvar(Const_Beta),
            topos_types:tvar(Const_Alpha),
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),
    ConstScheme = topos_type_scheme:generalize(ConstType, topos_type_env:ftv_env(EmptyEnv)),

    % app: ∀α β. (α -> β) -> α -> β
    {tvar, App_Alpha} = topos_types:fresh_var(),
    {tvar, App_Beta} = topos_types:fresh_var(),
    AppType = topos_types:tfun(
        topos_types:tfun(
            topos_types:tvar(App_Alpha),
            topos_types:tvar(App_Beta),
            topos_types:empty_effects()
        ),
        topos_types:tfun(
            topos_types:tvar(App_Alpha),
            topos_types:tvar(App_Beta),
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),
    AppScheme = topos_type_scheme:generalize(AppType, topos_type_env:ftv_env(EmptyEnv)),

    % Build environment
    Env = topos_type_env:from_list([
        {id, IdScheme},
        {const, ConstScheme},
        {app, AppScheme}
    ]),

    % Verify all can be looked up and instantiated
    {ok, IdFound} = topos_type_env:lookup(Env, id),
    {ok, ConstFound} = topos_type_env:lookup(Env, const),
    {ok, AppFound} = topos_type_env:lookup(Env, app),

    IdInst = topos_type_scheme:instantiate(IdFound),
    ConstInst = topos_type_scheme:instantiate(ConstFound),
    AppInst = topos_type_scheme:instantiate(AppFound),

    % All should be function types
    ?assertMatch({tfun, _, _, _}, IdInst),
    ?assertMatch({tfun, _, _, _}, ConstInst),
    ?assertMatch({tfun, _, _, _}, AppInst),

    % Environment should have no free type variables (all are polymorphic)
    EnvFtv = topos_type_env:ftv_env(Env),
    ?assertEqual([], sets:to_list(EnvFtv)),

    % Pretty-print all schemes
    IdPP = topos_type_pp:pp_scheme(IdScheme),
    ConstPP = topos_type_pp:pp_scheme(ConstScheme),
    AppPP = topos_type_pp:pp_scheme(AppScheme),

    ?assertEqual("∀α1. α1 -> α1", IdPP),
    ?assertEqual("∀α2 α3. α2 -> α3 -> α2", ConstPP),
    ?assertEqual("∀α4 α5. (α4 -> α5) -> α4 -> α5", AppPP).
