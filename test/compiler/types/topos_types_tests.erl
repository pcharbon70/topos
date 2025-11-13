%%%-------------------------------------------------------------------
%%% @doc Unit Tests for topos_types module
%%% @end
%%%-------------------------------------------------------------------
-module(topos_types_tests).

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
%% Type Construction Tests
%%====================================================================

type_construction_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_tvar()),
      ?_test(test_tcon()),
      ?_test(test_tapp()),
      ?_test(test_tfun()),
      ?_test(test_trecord()),
      ?_test(test_ttuple()),
      ?_test(test_tvariant())
     ]}.

test_tvar() ->
    Var = topos_types:tvar(1),
    ?assertEqual({tvar, 1}, Var),

    Var2 = topos_types:tvar(42),
    ?assertEqual({tvar, 42}, Var2).

test_tcon() ->
    IntType = topos_types:tcon(integer),
    ?assertEqual({tcon, integer}, IntType),

    BoolType = topos_types:tcon(boolean),
    ?assertEqual({tcon, boolean}, BoolType),

    CustomType = topos_types:tcon('MyType'),
    ?assertEqual({tcon, 'MyType'}, CustomType).

test_tapp() ->
    % List Int
    ListInt = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tcon(integer)]
    ),
    ?assertMatch({tapp, {tcon, 'List'}, [{tcon, integer}]}, ListInt),

    % Maybe String
    MaybeString = topos_types:tapp(
        topos_types:tcon('Maybe'),
        [topos_types:tcon(string)]
    ),
    ?assertMatch({tapp, {tcon, 'Maybe'}, [{tcon, string}]}, MaybeString).

test_tfun() ->
    % Int -> Int (pure)
    IntToInt = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(integer),
        topos_types:empty_effects()
    ),
    ?assertMatch({tfun, {tcon, integer}, {tcon, integer}, {effect_set, []}}, IntToInt),

    % String -> Unit / {IO}
    EffectfulFun = topos_types:tfun(
        topos_types:tcon(string),
        topos_types:tcon(unit),
        topos_types:singleton_effect(io)
    ),
    ?assertMatch({tfun, {tcon, string}, {tcon, unit}, {effect_set, [io]}}, EffectfulFun).

test_trecord() ->
    % {x: Int, y: Float} (closed)
    ClosedRecord = topos_types:trecord(
        [{x, topos_types:tcon(integer)}, {y, topos_types:tcon(float)}],
        closed
    ),
    ?assertMatch({trecord, [{x, {tcon, integer}}, {y, {tcon, float}}], closed}, ClosedRecord),

    % {name: String | ρ} (open with row variable)
    OpenRecord = topos_types:trecord(
        [{name, topos_types:tcon(string)}],
        1  % Row variable ID
    ),
    ?assertMatch({trecord, [{name, {tcon, string}}], 1}, OpenRecord).

test_ttuple() ->
    % (Int, String)
    Tuple2 = topos_types:ttuple([
        topos_types:tcon(integer),
        topos_types:tcon(string)
    ]),
    ?assertMatch({ttuple, [{tcon, integer}, {tcon, string}]}, Tuple2),

    % ()
    EmptyTuple = topos_types:ttuple([]),
    ?assertMatch({ttuple, []}, EmptyTuple).

test_tvariant() ->
    % Maybe a = Some a | None
    MaybeVariant = topos_types:tvariant([
        {'Some', [topos_types:tvar(1)]},
        {'None', []}
    ]),
    ?assertMatch({tvariant, [{'Some', [{tvar, 1}]}, {'None', []}]}, MaybeVariant).

%%====================================================================
%% Fresh Variable Generation Tests
%%====================================================================

fresh_var_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_fresh_var_unique()),
      ?_test(test_fresh_var_sequential()),
      ?_test(test_fresh_var_reset())
     ]}.

test_fresh_var_unique() ->
    Var1 = topos_types:fresh_var(),
    Var2 = topos_types:fresh_var(),
    Var3 = topos_types:fresh_var(),

    % All should be different
    ?assert(Var1 =/= Var2),
    ?assert(Var2 =/= Var3),
    ?assert(Var1 =/= Var3),

    % Should all be type variables
    ?assertMatch({tvar, _}, Var1),
    ?assertMatch({tvar, _}, Var2),
    ?assertMatch({tvar, _}, Var3).

test_fresh_var_sequential() ->
    topos_types:reset_fresh_counter(),

    {tvar, Id1} = topos_types:fresh_var(),
    {tvar, Id2} = topos_types:fresh_var(),
    {tvar, Id3} = topos_types:fresh_var(),

    % Should be sequential
    ?assertEqual(1, Id1),
    ?assertEqual(2, Id2),
    ?assertEqual(3, Id3).

test_fresh_var_reset() ->
    _Var1 = topos_types:fresh_var(),
    _Var2 = topos_types:fresh_var(),

    topos_types:reset_fresh_counter(),

    {tvar, Id} = topos_types:fresh_var(),
    ?assertEqual(1, Id).  % Counter reset

%%====================================================================
%% Effect Set Operations Tests
%%====================================================================

effect_set_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_empty_effects()),
      ?_test(test_singleton_effect()),
      ?_test(test_union_effects()),
      ?_test(test_normalize_effects()),
      ?_test(test_is_pure()),
      ?_test(test_effects_equal())
     ]}.

test_empty_effects() ->
    Empty = topos_types:empty_effects(),
    ?assertEqual({effect_set, []}, Empty),
    ?assert(topos_types:is_pure(Empty)).

test_singleton_effect() ->
    IO = topos_types:singleton_effect(io),
    ?assertEqual({effect_set, [io]}, IO),
    ?assertNot(topos_types:is_pure(IO)),

    Process = topos_types:singleton_effect(process),
    ?assertEqual({effect_set, [process]}, Process).

test_union_effects() ->
    IO = topos_types:singleton_effect(io),
    Process = topos_types:singleton_effect(process),

    % Union of two effects
    Union = topos_types:union_effects(IO, Process),
    ?assertEqual({effect_set, [io, process]}, Union),  % Sorted

    % Union with empty
    Empty = topos_types:empty_effects(),
    UnionEmpty = topos_types:union_effects(IO, Empty),
    ?assertEqual(IO, UnionEmpty),

    % Union with self (should deduplicate)
    SelfUnion = topos_types:union_effects(IO, IO),
    ?assertEqual(IO, SelfUnion).

test_normalize_effects() ->
    % Unordered list
    Normalized = topos_types:normalize_effects([process, io, file]),
    ?assertEqual({effect_set, [file, io, process]}, Normalized),  % Sorted

    % With duplicates
    NormalizedDups = topos_types:normalize_effects([io, process, io, file, process]),
    ?assertEqual({effect_set, [file, io, process]}, NormalizedDups),

    % Empty list
    NormalizedEmpty = topos_types:normalize_effects([]),
    ?assertEqual({effect_set, []}, NormalizedEmpty).

test_is_pure() ->
    ?assert(topos_types:is_pure(topos_types:empty_effects())),
    ?assertNot(topos_types:is_pure(topos_types:singleton_effect(io))),

    Multiple = topos_types:normalize_effects([io, process]),
    ?assertNot(topos_types:is_pure(Multiple)).

test_effects_equal() ->
    IO1 = topos_types:singleton_effect(io),
    IO2 = topos_types:singleton_effect(io),
    ?assert(topos_types:effects_equal(IO1, IO2)),

    Process = topos_types:singleton_effect(process),
    ?assertNot(topos_types:effects_equal(IO1, Process)),

    Empty1 = topos_types:empty_effects(),
    Empty2 = topos_types:empty_effects(),
    ?assert(topos_types:effects_equal(Empty1, Empty2)).

%%====================================================================
%% Type Operations Tests
%%====================================================================

type_operations_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_is_function_type()),
      ?_test(test_is_type_var()),
      ?_test(test_extract_function_effects()),
      ?_test(test_type_vars())
     ]}.

test_is_function_type() ->
    Fun = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(integer),
        topos_types:empty_effects()
    ),
    ?assert(topos_types:is_function_type(Fun)),

    NotFun = topos_types:tcon(integer),
    ?assertNot(topos_types:is_function_type(NotFun)).

test_is_type_var() ->
    Var = topos_types:tvar(1),
    ?assert(topos_types:is_type_var(Var)),

    NotVar = topos_types:tcon(integer),
    ?assertNot(topos_types:is_type_var(NotVar)).

test_extract_function_effects() ->
    Effects = topos_types:singleton_effect(io),
    Fun = topos_types:tfun(
        topos_types:tcon(string),
        topos_types:tcon(unit),
        Effects
    ),
    ?assertEqual({ok, Effects}, topos_types:extract_function_effects(Fun)),

    NotFun = topos_types:tcon(integer),
    ?assertEqual(error, topos_types:extract_function_effects(NotFun)).

test_type_vars() ->
    % Type variable α
    Var1 = topos_types:tvar(1),
    Vars1 = topos_types:type_vars(Var1),
    ?assertEqual([1], lists:sort(sets:to_list(Vars1))),

    % Function α -> β
    Fun = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),
    Vars2 = topos_types:type_vars(Fun),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars2))),

    % No variables in concrete type
    Concrete = topos_types:tcon(integer),
    Vars3 = topos_types:type_vars(Concrete),
    ?assertEqual([], lists:sort(sets:to_list(Vars3))),

    % Tuple (α, β, α) - α appears twice
    Tuple = topos_types:ttuple([
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:tvar(1)
    ]),
    Vars4 = topos_types:type_vars(Tuple),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars4))),  % De-duplicated

    % Record {x: α | ρ}
    Record = topos_types:trecord(
        [{x, topos_types:tvar(1)}],
        3  % Row variable
    ),
    Vars5 = topos_types:type_vars(Record),
    ?assertEqual([1, 3], lists:sort(sets:to_list(Vars5))).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_complex_type_construction())
     ]}.

test_complex_type_construction() ->
    % Build: (α -> β / {IO}) -> List α -> List β / {IO}

    % α -> β / {IO}
    InnerFun = topos_types:tfun(
        topos_types:tvar(1),  % α
        topos_types:tvar(2),  % β
        topos_types:singleton_effect(io)
    ),

    % List α
    ListAlpha = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    ),

    % List β
    ListBeta = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(2)]
    ),

    % (α -> β / {IO}) -> List α -> List β / {IO}
    MapType = topos_types:tfun(
        InnerFun,
        topos_types:tfun(
            ListAlpha,
            ListBeta,
            topos_types:singleton_effect(io)
        ),
        topos_types:empty_effects()  % Outer function is pure
    ),

    % Verify structure
    ?assertMatch({tfun, _, _, {effect_set, []}}, MapType),

    % Extract type variables (should be {1, 2})
    Vars = topos_types:type_vars(MapType),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars))).
