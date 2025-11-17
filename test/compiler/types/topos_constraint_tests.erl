%%%-------------------------------------------------------------------
%%% @doc Tests for Constraint Representation and Operations
%%%
%%% Tests the topos_constraint module which implements constraint
%%% representation for trait constraints and their operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_constraint_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Constraint Construction Tests
%%%===================================================================

trait_constraint_test() ->
    % Create a simple trait constraint
    Loc = {file, "test.topos", 10, 5},
    T1 = {tvar, 1},
    Constraint = topos_constraint:trait_constraint('Functor', [T1], Loc),

    ?assertMatch({trait, 'Functor', [{tvar, 1}], _}, Constraint),

    % Test without location
    Constraint2 = topos_constraint:trait_constraint('Monad', [T1]),
    ?assertMatch({trait, 'Monad', [{tvar, 1}], unknown}, Constraint2).

empty_constraint_set_test() ->
    Set = topos_constraint:empty_constraint_set(),
    ?assertEqual([], Set),
    ?assert(topos_constraint:is_empty(Set)).

add_constraint_test() ->
    Set = topos_constraint:empty_constraint_set(),
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),

    Set1 = topos_constraint:add_constraint(C1, Set),
    ?assertNot(topos_constraint:is_empty(Set1)),
    ?assertEqual(1, length(Set1)).

add_constraints_test() ->
    Set = topos_constraint:empty_constraint_set(),
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tvar, 2}]),

    NewCs = [C1, C2],
    Set1 = topos_constraint:add_constraints(NewCs, Set),
    ?assertEqual(2, length(Set1)).

%%%===================================================================
%%% Constraint Operations Tests
%%%===================================================================

normalize_test() ->
    % Create unsorted constraints
    C1 = topos_constraint:trait_constraint('Monad', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Functor', [{tvar, 2}]),
    C3 = topos_constraint:trait_constraint('Eq', [{tvar, 3}]),

    Unsorted = [C1, C2, C3],
    Sorted = topos_constraint:normalize(Unsorted),

    % Should be sorted by trait name: Eq, Functor, Monad
    ?assertMatch([{trait, 'Eq', _, _},
                  {trait, 'Functor', _, _},
                  {trait, 'Monad', _, _}], Sorted).

remove_duplicates_test() ->
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),  % Duplicate
    C3 = topos_constraint:trait_constraint('Monad', [{tvar, 2}]),

    WithDups = [C1, C2, C3],
    NoDups = topos_constraint:remove_duplicates(WithDups),

    % Should have only 2 constraints
    ?assertEqual(2, length(NoDups)).

simplify_test() ->
    % Simplify should remove duplicates and normalize
    C1 = topos_constraint:trait_constraint('Monad', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Functor', [{tvar, 2}]),
    C3 = topos_constraint:trait_constraint('Monad', [{tvar, 1}]),  % Duplicate

    Input = [C1, C2, C3],
    Output = topos_constraint:simplify(Input),

    % Should have 2 constraints, sorted
    ?assertEqual(2, length(Output)),
    ?assertMatch([{trait, 'Functor', _, _},
                  {trait, 'Monad', _, _}], Output).

substitute_constraints_test() ->
    % Create constraint with type variable
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    Set = [C1],

    % Create substitution: TVar(1) -> Int
    Subst = topos_type_subst:singleton(1, {tcon, integer}),

    % Apply substitution
    NewSet = topos_constraint:substitute(Subst, Set),

    % Should have Int instead of TVar(1)
    ?assertMatch([{trait, 'Functor', [{tcon, integer}], _}], NewSet).

type_vars_test() ->
    % Create constraints with various type variables
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tvar, 2}, {tvar, 3}]),
    C3 = topos_constraint:trait_constraint('Eq', [{tcon, integer}]),

    Set = [C1, C2, C3],
    Vars = topos_constraint:type_vars(Set),

    % Should have variables 1, 2, 3 (sorted and unique)
    ?assertEqual([1, 2, 3], Vars).

%%%===================================================================
%%% Constraint Query Tests
%%%===================================================================

find_constraints_test() ->
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tvar, 2}]),
    C3 = topos_constraint:trait_constraint('Eq', [{tvar, 1}, {tvar, 3}]),

    Set = [C1, C2, C3],

    % Find constraints involving variable 1
    Found = topos_constraint:find_constraints(1, Set),
    ?assertEqual(2, length(Found)),  % C1 and C3

    % Find constraints involving variable 2
    Found2 = topos_constraint:find_constraints(2, Set),
    ?assertEqual(1, length(Found2)),  % Only C2

    % Find constraints involving non-existent variable
    Found3 = topos_constraint:find_constraints(99, Set),
    ?assertEqual(0, length(Found3)).

filter_by_trait_test() ->
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tvar, 2}]),
    C3 = topos_constraint:trait_constraint('Functor', [{tvar, 3}]),

    Set = [C1, C2, C3],

    % Filter for Functor constraints
    Functors = topos_constraint:filter_by_trait('Functor', Set),
    ?assertEqual(2, length(Functors)),

    % Filter for Monad constraints
    Monads = topos_constraint:filter_by_trait('Monad', Set),
    ?assertEqual(1, length(Monads)),

    % Filter for non-existent trait
    Empty = topos_constraint:filter_by_trait('NonExistent', Set),
    ?assertEqual(0, length(Empty)).

has_constraint_test() ->
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tvar, 2}]),

    Set = [C1],

    ?assert(topos_constraint:has_constraint(C1, Set)),
    ?assertNot(topos_constraint:has_constraint(C2, Set)).

%%%===================================================================
%%% Complex Scenarios
%%%===================================================================

complex_constraint_workflow_test() ->
    % Simulate a typical constraint workflow:
    % 1. Generate constraints
    % 2. Add to set
    % 3. Apply substitutions
    % 4. Simplify
    % 5. Query

    % Step 1: Generate constraints
    C1 = topos_constraint:trait_constraint('Functor', [{tvar, 1}]),
    C2 = topos_constraint:trait_constraint('Monad', [{tvar, 1}]),
    C3 = topos_constraint:trait_constraint('Eq', [{tvar, 2}]),

    % Step 2: Add to set
    Set1 = topos_constraint:empty_constraint_set(),
    Set2 = topos_constraint:add_constraints([C1, C2, C3], Set1),
    ?assertEqual(3, length(Set2)),

    % Step 3: Apply substitution (TVar(1) -> List TVar(3))
    ListT3 = {tapp, {tcon, list}, [{tvar, 3}]},
    Subst = topos_type_subst:singleton(1, ListT3),
    Set3 = topos_constraint:substitute(Subst, Set2),

    % Step 4: Simplify
    Set4 = topos_constraint:simplify(Set3),
    ?assertEqual(3, length(Set4)),

    % Step 5: Query
    Functors = topos_constraint:filter_by_trait('Functor', Set4),
    ?assertEqual(1, length(Functors)),

    % Verify the substitution was applied
    [{trait, 'Functor', [Type], _}] = Functors,
    ?assertMatch({tapp, {tcon, list}, [{tvar, 3}]}, Type).

multiple_type_args_test() ->
    % Test constraint with multiple type arguments
    % Example: Bifunctor takes two type parameters
    T1 = {tvar, 1},
    T2 = {tvar, 2},
    C = topos_constraint:trait_constraint('Bifunctor', [T1, T2]),

    ?assertMatch({trait, 'Bifunctor', [{tvar, 1}, {tvar, 2}], _}, C),

    % Apply substitution to first argument only
    Subst = topos_type_subst:singleton(1, {tcon, integer}),
    [CSubst] = topos_constraint:substitute(Subst, [C]),

    ?assertMatch({trait, 'Bifunctor', [{tcon, integer}, {tvar, 2}], _}, CSubst).
