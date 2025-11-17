%%%-------------------------------------------------------------------
%%% @doc Tests for Trait Instance Resolution
%%%
%%% Tests the topos_instance module which implements instance
%%% resolution for trait constraints.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_instance_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Instance Construction Tests
%%%===================================================================

make_instance_test() ->
    Loc = {file, "test.topos", 10, 5},
    ListType = {tcon, list},
    Instance = topos_instance:make_instance('Functor', [ListType], Loc),

    ?assertMatch({instance, 'Functor', [{tcon, list}], _}, Instance),

    % Test without location
    Instance2 = topos_instance:make_instance('Monad', [ListType]),
    ?assertMatch({instance, 'Monad', [{tcon, list}], unknown}, Instance2).

empty_instance_db_test() ->
    DB = topos_instance:empty_instance_db(),
    ?assertEqual(#{}, DB).

add_instance_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Functor', [{tcon, list}]),

    DB1 = topos_instance:add_instance(I1, DB),
    Instances = topos_instance:get_instances('Functor', DB1),

    ?assertEqual(1, length(Instances)),
    ?assertEqual([I1], Instances).

add_multiple_instances_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Functor', [{tcon, list}]),
    I2 = topos_instance:make_instance('Functor', [{tcon, maybe}]),

    DB1 = topos_instance:add_instance(I1, DB),
    DB2 = topos_instance:add_instance(I2, DB1),

    Instances = topos_instance:get_instances('Functor', DB2),
    ?assertEqual(2, length(Instances)).

get_instances_nonexistent_trait_test() ->
    DB = topos_instance:empty_instance_db(),
    Instances = topos_instance:get_instances('NonExistent', DB),
    ?assertEqual([], Instances).

%%%===================================================================
%%% Instance Resolution Tests
%%%===================================================================

resolve_simple_constraint_test() ->
    % Setup: Instance Functor List
    DB = topos_instance:empty_instance_db(),
    Instance = topos_instance:make_instance('Functor', [{tcon, list}]),
    DB1 = topos_instance:add_instance(Instance, DB),

    % Constraint: Functor List
    Constraint = topos_constraint:trait_constraint('Functor', [{tcon, list}]),

    % Resolve
    Result = topos_instance:resolve_constraint(Constraint, DB1),

    ?assertMatch({ok, _, _}, Result),
    {ok, ResolvedInstance, _Subst} = Result,
    ?assertEqual(Instance, ResolvedInstance).

resolve_no_instance_test() ->
    % Empty database
    DB = topos_instance:empty_instance_db(),

    % Constraint that has no instance
    Constraint = topos_constraint:trait_constraint('Functor', [{tcon, list}]),

    % Should fail with no_instance
    Result = topos_instance:resolve_constraint(Constraint, DB),
    ?assertEqual({error, no_instance}, Result).

resolve_with_unification_test() ->
    % Setup: Instance Functor (List α) where α is a type variable
    DB = topos_instance:empty_instance_db(),
    ListOfVar = {tapp, {tcon, list}, [{tvar, 1}]},
    Instance = topos_instance:make_instance('Functor', [ListOfVar]),
    DB1 = topos_instance:add_instance(Instance, DB),

    % Constraint: Functor (List Int)
    ListOfInt = {tapp, {tcon, list}, [{tcon, integer}]},
    Constraint = topos_constraint:trait_constraint('Functor', [ListOfInt]),

    % Should resolve with substitution α -> Int
    Result = topos_instance:resolve_constraint(Constraint, DB1),

    ?assertMatch({ok, _, _}, Result),
    {ok, _Instance, Subst} = Result,

    % Check that substitution maps variable 1 to Int
    IntType = topos_type_subst:lookup(1, Subst),
    ?assertEqual({tcon, integer}, IntType).

resolve_ambiguous_test() ->
    % Setup: Two instances that both match
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),  % Eq α (universal)
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),  % Eq Int (specific)
    DB1 = topos_instance:add_instance(I1, DB),
    DB2 = topos_instance:add_instance(I2, DB1),

    % Constraint: Eq Int (both instances could match!)
    Constraint = topos_constraint:trait_constraint('Eq', [{tcon, integer}]),

    % Should fail with ambiguous
    Result = topos_instance:resolve_constraint(Constraint, DB2),
    ?assertMatch({error, {ambiguous, _}}, Result).

resolve_multiple_constraints_success_test() ->
    % Setup instances
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Functor', [{tcon, list}]),
    I2 = topos_instance:make_instance('Monad', [{tcon, maybe}]),
    DB1 = topos_instance:add_instance(I1, DB),
    DB2 = topos_instance:add_instance(I2, DB1),

    % Constraints
    C1 = topos_constraint:trait_constraint('Functor', [{tcon, list}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tcon, maybe}]),

    % Resolve
    Result = topos_instance:resolve_constraints([C1, C2], DB2),

    ?assertMatch({ok, _}, Result),
    {ok, Solutions} = Result,
    ?assertEqual(2, length(Solutions)).

resolve_multiple_constraints_failure_test() ->
    % Setup: Only one instance
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Functor', [{tcon, list}]),
    DB1 = topos_instance:add_instance(I1, DB),

    % Constraints: one exists, one doesn't
    C1 = topos_constraint:trait_constraint('Functor', [{tcon, list}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tcon, maybe}]),

    % Should fail on C2
    Result = topos_instance:resolve_constraints([C1, C2], DB1),

    ?assertMatch({error, {_, no_instance}}, Result).

%%%===================================================================
%%% Instance Query Tests
%%%===================================================================

has_instance_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Functor', [{tcon, list}]),
    DB1 = topos_instance:add_instance(I1, DB),

    C1 = topos_constraint:trait_constraint('Functor', [{tcon, list}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tcon, maybe}]),

    ?assert(topos_instance:has_instance(C1, DB1)),
    ?assertNot(topos_instance:has_instance(C2, DB1)).

check_overlap_same_instance_test() ->
    % An instance overlaps with itself
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    ?assert(topos_instance:check_overlap(I1, I1)).

check_overlap_different_traits_test() ->
    % Different traits never overlap
    I1 = topos_instance:make_instance('Functor', [{tcon, list}]),
    I2 = topos_instance:make_instance('Monad', [{tcon, list}]),
    ?assertNot(topos_instance:check_overlap(I1, I2)).

check_overlap_unifiable_types_test() ->
    % Eq α and Eq Int overlap (α can be Int)
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    ?assert(topos_instance:check_overlap(I1, I2)).

check_overlap_distinct_types_test() ->
    % Eq Int and Eq String don't overlap
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, string}]),
    ?assertNot(topos_instance:check_overlap(I1, I2)).

%%%===================================================================
%%% Complex Scenarios
%%%===================================================================

realistic_functor_resolution_test() ->
    % Simulate real Functor resolution for polymorphic types
    DB = topos_instance:empty_instance_db(),

    % Add instances: Functor List, Functor Maybe
    I_List = topos_instance:make_instance('Functor',
        [{tapp, {tcon, list}, [{tvar, 1}]}]),
    I_Maybe = topos_instance:make_instance('Functor',
        [{tapp, {tcon, maybe}, [{tvar, 2}]}]),

    DB1 = topos_instance:add_instance(I_List, DB),
    DB2 = topos_instance:add_instance(I_Maybe, DB1),

    % Constraint: Functor (List Int)
    C1 = topos_constraint:trait_constraint('Functor',
        [{tapp, {tcon, list}, [{tcon, integer}]}]),

    Result = topos_instance:resolve_constraint(C1, DB2),
    ?assertMatch({ok, _, _}, Result).

monad_hierarchy_test() ->
    % Test resolution with trait hierarchy (Monad requires Functor)
    % For now, just test basic Monad instance resolution
    DB = topos_instance:empty_instance_db(),

    I_List_Monad = topos_instance:make_instance('Monad',
        [{tapp, {tcon, list}, [{tvar, 1}]}]),

    DB1 = topos_instance:add_instance(I_List_Monad, DB),

    C = topos_constraint:trait_constraint('Monad',
        [{tapp, {tcon, list}, [{tcon, string}]}]),

    Result = topos_instance:resolve_constraint(C, DB1),
    ?assertMatch({ok, _, _}, Result).

find_matching_instances_test() ->
    % Test the filtering of candidate instances
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    I3 = topos_instance:make_instance('Ord', [{tcon, integer}]),  % Different trait

    Instances = [I1, I2, I3],

    C = topos_constraint:trait_constraint('Eq', [{tcon, integer}]),

    Matching = topos_instance:find_matching_instances(C, Instances),

    % Should match I1 and I2 (same trait and arity), but not I3
    ?assertEqual(2, length(Matching)).

unify_instance_concrete_test() ->
    % Test unifying concrete types
    Instance = topos_instance:make_instance('Eq', [{tcon, integer}]),
    Constraint = topos_constraint:trait_constraint('Eq', [{tcon, integer}]),

    Result = topos_instance:unify_instance(Instance, Constraint),

    ?assertMatch({ok, _}, Result),
    {ok, Subst} = Result,

    % Substitution should be empty (no variables to bind)
    ?assertEqual(topos_type_subst:empty(), Subst).

unify_instance_variable_test() ->
    % Test unifying instance variable with concrete constraint
    Instance = topos_instance:make_instance('Eq', [{tvar, 1}]),
    Constraint = topos_constraint:trait_constraint('Eq', [{tcon, string}]),

    Result = topos_instance:unify_instance(Instance, Constraint),

    ?assertMatch({ok, _}, Result),
    {ok, Subst} = Result,

    % Should bind variable 1 to String
    ?assertEqual({tcon, string}, topos_type_subst:lookup(1, Subst)).
