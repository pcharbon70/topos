%%%-------------------------------------------------------------------
%%% @doc Unit Tests for topos_type_env module
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_env_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%%====================================================================
%% Environment Construction Tests
%%====================================================================

construction_test_() ->
    [
      ?_test(test_empty()),
      ?_test(test_singleton()),
      ?_test(test_from_list())
    ].

test_empty() ->
    Env = topos_type_env:empty(),
    ?assertEqual(#{}, Env).

test_singleton() ->
    Scheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    Env = topos_type_env:singleton(x, Scheme),

    ?assertEqual(#{x => Scheme}, Env).

test_from_list() ->
    IntScheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    StringScheme = topos_type_scheme:mono(topos_types:tcon(string)),

    Env = topos_type_env:from_list([
        {x, IntScheme},
        {y, StringScheme}
    ]),

    ?assertEqual(#{x => IntScheme, y => StringScheme}, Env).

%%====================================================================
%% Environment Operations Tests
%%====================================================================

operations_test_() ->
    [
      ?_test(test_lookup_found()),
      ?_test(test_lookup_not_found()),
      ?_test(test_extend()),
      ?_test(test_extend_shadow()),
      ?_test(test_remove())
    ].

test_lookup_found() ->
    Scheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    Env = topos_type_env:singleton(x, Scheme),

    ?assertEqual({ok, Scheme}, topos_type_env:lookup(Env, x)).

test_lookup_not_found() ->
    Env = topos_type_env:empty(),
    ?assertEqual(none, topos_type_env:lookup(Env, x)).

test_extend() ->
    IntScheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    StringScheme = topos_type_scheme:mono(topos_types:tcon(string)),

    Env1 = topos_type_env:singleton(x, IntScheme),
    Env2 = topos_type_env:extend(Env1, y, StringScheme),

    ?assertEqual({ok, IntScheme}, topos_type_env:lookup(Env2, x)),
    ?assertEqual({ok, StringScheme}, topos_type_env:lookup(Env2, y)).

test_extend_shadow() ->
    IntScheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    StringScheme = topos_type_scheme:mono(topos_types:tcon(string)),

    Env1 = topos_type_env:singleton(x, IntScheme),
    Env2 = topos_type_env:extend(Env1, x, StringScheme),  % Shadow x

    % Should get the new binding
    ?assertEqual({ok, StringScheme}, topos_type_env:lookup(Env2, x)).

test_remove() ->
    IntScheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    StringScheme = topos_type_scheme:mono(topos_types:tcon(string)),

    Env1 = topos_type_env:from_list([
        {x, IntScheme},
        {y, StringScheme}
    ]),

    Env2 = topos_type_env:remove(Env1, x),

    ?assertEqual(none, topos_type_env:lookup(Env2, x)),
    ?assertEqual({ok, StringScheme}, topos_type_env:lookup(Env2, y)).

%%====================================================================
%% Free Type Variables Tests
%%====================================================================

ftv_test_() ->
    [
      ?_test(test_ftv_empty()),
      ?_test(test_ftv_mono_schemes()),
      ?_test(test_ftv_poly_schemes()),
      ?_test(test_ftv_mixed())
    ].

test_ftv_empty() ->
    Env = topos_type_env:empty(),
    Ftv = topos_type_env:ftv_env(Env),

    ?assertEqual([], sets:to_list(Ftv)).

test_ftv_mono_schemes() ->
    % x: α₁
    % y: α₂
    % Expected FTV: {α₁, α₂}
    Scheme1 = topos_type_scheme:mono(topos_types:tvar(1)),
    Scheme2 = topos_type_scheme:mono(topos_types:tvar(2)),

    Env = topos_type_env:from_list([
        {x, Scheme1},
        {y, Scheme2}
    ]),

    Ftv = topos_type_env:ftv_env(Env),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([1, 2], FtvList).

test_ftv_poly_schemes() ->
    % x: ∀α₁. α₁ -> α₁
    % y: ∀α₂. α₂ -> α₃
    % Expected FTV: {α₃}  (only α₃ is free, α₁ and α₂ are quantified)
    Type1 = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(1),
        topos_types:empty_effects()
    ),
    Scheme1 = topos_type_scheme:poly([1], Type1),

    Type2 = topos_types:tfun(
        topos_types:tvar(2),
        topos_types:tvar(3),
        topos_types:empty_effects()
    ),
    Scheme2 = topos_type_scheme:poly([2], Type2),  % Only α₂ quantified

    Env = topos_type_env:from_list([
        {x, Scheme1},
        {y, Scheme2}
    ]),

    Ftv = topos_type_env:ftv_env(Env),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([3], FtvList).  % Only α₃ is free

test_ftv_mixed() ->
    % x: Int (concrete type, no vars)
    % y: α₁ (monomorphic with var)
    % z: ∀α₂. α₂ -> α₃ (polymorphic with free var α₃)
    % Expected FTV: {α₁, α₃}
    Scheme1 = topos_type_scheme:mono(topos_types:tcon(integer)),

    Scheme2 = topos_type_scheme:mono(topos_types:tvar(1)),

    Type3 = topos_types:tfun(
        topos_types:tvar(2),
        topos_types:tvar(3),
        topos_types:empty_effects()
    ),
    Scheme3 = topos_type_scheme:poly([2], Type3),

    Env = topos_type_env:from_list([
        {x, Scheme1},
        {y, Scheme2},
        {z, Scheme3}
    ]),

    Ftv = topos_type_env:ftv_env(Env),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([1, 3], FtvList).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
      ?_test(test_typical_type_checking_scenario())
    ].

test_typical_type_checking_scenario() ->
    % Simulate a typical type checking scenario:
    % let id = λx. x in
    % let const = λx. λy. x in
    % ...

    % Type of id: ∀α. α -> α
    IdType = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(1),
        topos_types:empty_effects()
    ),
    IdScheme = topos_type_scheme:poly([1], IdType),

    % Type of const: ∀α β. α -> β -> α
    ConstType = topos_types:tfun(
        topos_types:tvar(2),
        topos_types:tfun(
            topos_types:tvar(3),
            topos_types:tvar(2),
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),
    ConstScheme = topos_type_scheme:poly([2, 3], ConstType),

    % Build environment
    Env = topos_type_env:from_list([
        {id, IdScheme},
        {const, ConstScheme}
    ]),

    % Lookup id
    {ok, IdSchemeFound} = topos_type_env:lookup(Env, id),
    ?assertEqual(IdScheme, IdSchemeFound),

    % Instantiate id multiple times (should get fresh vars each time)
    State0 = topos_type_state:new(),
    {IdInst1, State1} = topos_type_scheme:instantiate(IdSchemeFound, State0),
    {IdInst2, _State2} = topos_type_scheme:instantiate(IdSchemeFound, State1),

    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, IdInst1),
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, IdInst2),

    % Extract var IDs
    {tfun, {tvar, Id1From}, {tvar, Id1To}, _} = IdInst1,
    {tfun, {tvar, Id2From}, {tvar, Id2To}, _} = IdInst2,

    % Each instantiation should have same var for from/to
    ?assertEqual(Id1From, Id1To),
    ?assertEqual(Id2From, Id2To),

    % But different instantiations should have different vars
    ?assertNotEqual(Id1From, Id2From),

    % Check ftv of environment (should be empty since all are polymorphic)
    Ftv = topos_type_env:ftv_env(Env),
    ?assertEqual([], sets:to_list(Ftv)).
