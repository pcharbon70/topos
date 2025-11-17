%%%-------------------------------------------------------------------
%%% @doc Tests for Coherence Checking
%%%
%%% Tests the topos_coherence module which implements coherence
%%% checking for trait instances.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_coherence_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Coherence Checking Tests
%%%===================================================================

check_empty_db_coherent_test() ->
    DB = topos_instance:empty_instance_db(),
    Result = topos_coherence:check_instance_db(DB),
    ?assertEqual({ok, coherent}, Result).

check_single_instance_coherent_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    DB1 = topos_instance:add_instance(I1, DB),

    Result = topos_coherence:check_instance_db(DB1),
    ?assertEqual({ok, coherent}, Result).

check_non_overlapping_instances_coherent_test() ->
    % Two instances for different types don't overlap
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, string}]),

    DB1 = topos_instance:add_instance(I1, DB),
    DB2 = topos_instance:add_instance(I2, DB1),

    Result = topos_coherence:check_instance_db(DB2),
    ?assertEqual({ok, coherent}, Result).

check_overlapping_instances_incoherent_test() ->
    % Universal instance Eq α overlaps with specific Eq Int
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),

    DB1 = topos_instance:add_instance(I1, DB),
    DB2 = topos_instance:add_instance(I2, DB1),

    Result = topos_coherence:check_instance_db(DB2),
    ?assertMatch({error, [_|_]}, Result).

check_different_traits_coherent_test() ->
    % Instances for different traits never overlap
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    I2 = topos_instance:make_instance('Ord', [{tvar, 1}]),

    DB1 = topos_instance:add_instance(I1, DB),
    DB2 = topos_instance:add_instance(I2, DB1),

    Result = topos_coherence:check_instance_db(DB2),
    ?assertEqual({ok, coherent}, Result).

%%%===================================================================
%%% New Instance Checking Tests
%%%===================================================================

check_new_instance_no_overlap_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    DB1 = topos_instance:add_instance(I1, DB),

    % New instance for different type
    I2 = topos_instance:make_instance('Eq', [{tcon, string}]),

    Result = topos_coherence:check_new_instance(I2, DB1),
    ?assertEqual({ok, no_overlap}, Result).

check_new_instance_with_overlap_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    DB1 = topos_instance:add_instance(I1, DB),

    % New instance that overlaps (Int unifies with α)
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),

    Result = topos_coherence:check_new_instance(I2, DB1),
    ?assertMatch({error, [_|_]}, Result).

check_new_instance_empty_db_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),

    Result = topos_coherence:check_new_instance(I1, DB),
    ?assertEqual({ok, no_overlap}, Result).

%%%===================================================================
%%% Find Overlaps Tests
%%%===================================================================

find_overlaps_empty_list_test() ->
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    Result = topos_coherence:find_overlaps(I1, []),
    ?assertEqual([], Result).

find_overlaps_with_self_test() ->
    % An instance shouldn't overlap with itself
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    Result = topos_coherence:find_overlaps(I1, [I1]),
    ?assertEqual([], Result).

find_overlaps_with_different_types_test() ->
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, string}]),

    Result = topos_coherence:find_overlaps(I1, [I2]),
    ?assertEqual([], Result).

find_overlaps_with_unifiable_test() ->
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),

    Result = topos_coherence:find_overlaps(I1, [I2]),
    ?assertEqual(1, length(Result)),
    ?assertEqual([I2], Result).

find_overlaps_multiple_test() ->
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}]),
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    I3 = topos_instance:make_instance('Eq', [{tcon, string}]),
    I4 = topos_instance:make_instance('Ord', [{tcon, integer}]),  % Different trait

    Result = topos_coherence:find_overlaps(I1, [I2, I3, I4]),
    % Should find I2 and I3 (both overlap with I1), but not I4 (different trait)
    ?assertEqual(2, length(Result)).

%%%===================================================================
%%% Coverage Checking Tests
%%%===================================================================

check_coverage_complete_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    DB1 = topos_instance:add_instance(I1, DB),

    % Constraint that has an instance
    C1 = topos_constraint:trait_constraint('Eq', [{tcon, integer}]),

    Result = topos_coherence:check_coverage([C1], DB1),
    ?assertEqual({ok, complete}, Result).

check_coverage_incomplete_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    DB1 = topos_instance:add_instance(I1, DB),

    % Constraint that lacks an instance
    C1 = topos_constraint:trait_constraint('Eq', [{tcon, string}]),

    Result = topos_coherence:check_coverage([C1], DB1),
    ?assertMatch({error, [_|_]}, Result).

check_coverage_mixed_test() ->
    DB = topos_instance:empty_instance_db(),
    I1 = topos_instance:make_instance('Eq', [{tcon, integer}]),
    DB1 = topos_instance:add_instance(I1, DB),

    % One constraint has instance, one doesn't
    C1 = topos_constraint:trait_constraint('Eq', [{tcon, integer}]),
    C2 = topos_constraint:trait_constraint('Eq', [{tcon, string}]),

    Result = topos_coherence:check_coverage([C1, C2], DB1),
    ?assertMatch({error, [_]}, Result),

    {error, [Missing]} = Result,
    ?assertMatch({missing_instance, 'Eq', [{tcon, string}], _}, Missing).

check_coverage_empty_constraints_test() ->
    DB = topos_instance:empty_instance_db(),
    Result = topos_coherence:check_coverage([], DB),
    ?assertEqual({ok, complete}, Result).

%%%===================================================================
%%% Error Formatting Tests
%%%===================================================================

format_overlap_error_test() ->
    I1 = topos_instance:make_instance('Eq', [{tvar, 1}],
                                     {file, "test.topos", 10, 5}),
    I2 = topos_instance:make_instance('Eq', [{tcon, integer}],
                                     {file, "test.topos", 20, 5}),

    ErrorMsg = topos_coherence:format_overlap_error(I1, I2),

    % Check that error message contains key information
    ?assert(string:str(ErrorMsg, "Overlapping instances") > 0),
    ?assert(string:str(ErrorMsg, "Eq") > 0),
    ?assert(string:str(ErrorMsg, "test.topos") > 0).

format_coverage_error_test() ->
    ErrorMsg = topos_coherence:format_coverage_error('Functor',
                                                     [{tcon, list}]),

    % Check that error message contains key information
    ?assert(string:str(ErrorMsg, "Missing instance") > 0),
    ?assert(string:str(ErrorMsg, "Functor") > 0).

%%%===================================================================
%%% Complex Scenarios
%%%===================================================================

realistic_coherence_scenario_test() ->
    % Simulate a realistic scenario with multiple traits and instances
    DB = topos_instance:empty_instance_db(),

    % Add coherent Eq instances
    Eq_Int = topos_instance:make_instance('Eq', [{tcon, integer}]),
    Eq_String = topos_instance:make_instance('Eq', [{tcon, string}]),
    Eq_Bool = topos_instance:make_instance('Eq', [{tcon, boolean}]),

    DB1 = topos_instance:add_instance(Eq_Int, DB),
    DB2 = topos_instance:add_instance(Eq_String, DB1),
    DB3 = topos_instance:add_instance(Eq_Bool, DB2),

    % Add coherent Ord instances
    Ord_Int = topos_instance:make_instance('Ord', [{tcon, integer}]),
    Ord_String = topos_instance:make_instance('Ord', [{tcon, string}]),

    DB4 = topos_instance:add_instance(Ord_Int, DB3),
    DB5 = topos_instance:add_instance(Ord_String, DB4),

    % Should be coherent
    Result = topos_coherence:check_instance_db(DB5),
    ?assertEqual({ok, coherent}, Result).

incoherent_database_scenario_test() ->
    % Create an incoherent database with overlapping instances
    DB = topos_instance:empty_instance_db(),

    % Add overlapping instances: Eq α and Eq Int
    Eq_Universal = topos_instance:make_instance('Eq', [{tvar, 1}]),
    Eq_Int = topos_instance:make_instance('Eq', [{tcon, integer}]),

    DB1 = topos_instance:add_instance(Eq_Universal, DB),
    DB2 = topos_instance:add_instance(Eq_Int, DB1),

    % Should detect incoherence
    Result = topos_coherence:check_instance_db(DB2),
    ?assertMatch({error, [_]}, Result),

    {error, [Error]} = Result,
    ?assertMatch({overlap, _, _, _}, Error).

polymorphic_type_coherence_test() ->
    % Test coherence with polymorphic type constructors
    DB = topos_instance:empty_instance_db(),

    % Functor (List α) and Functor (Maybe β)
    List_Functor = topos_instance:make_instance('Functor',
        [{tapp, {tcon, list}, [{tvar, 1}]}]),
    Maybe_Functor = topos_instance:make_instance('Functor',
        [{tapp, {tcon, maybe}, [{tvar, 2}]}]),

    DB1 = topos_instance:add_instance(List_Functor, DB),
    DB2 = topos_instance:add_instance(Maybe_Functor, DB1),

    % These don't overlap (different type constructors)
    Result = topos_coherence:check_instance_db(DB2),
    ?assertEqual({ok, coherent}, Result).
