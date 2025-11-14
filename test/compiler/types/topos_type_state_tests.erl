%%%-------------------------------------------------------------------
%%% @doc Tests for Type Inference State Management
%%%
%%% Tests the stateless API for fresh variable generation using
%%% explicit state threading.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_state_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% State Creation Tests
%%====================================================================

state_creation_test_() ->
    [
     ?_test(test_new_state()),
     ?_test(test_new_state_with_counter())
    ].

test_new_state() ->
    State = topos_type_state:new(),
    ?assertEqual(0, topos_type_state:get_counter(State)).

test_new_state_with_counter() ->
    State = topos_type_state:new(42),
    ?assertEqual(42, topos_type_state:get_counter(State)).

%%====================================================================
%% Fresh Variable Generation Tests
%%====================================================================

fresh_var_generation_test_() ->
    [
     ?_test(test_fresh_var_id()),
     ?_test(test_fresh_var()),
     ?_test(test_multiple_fresh_vars()),
     ?_test(test_sequential_ids())
    ].

test_fresh_var_id() ->
    State0 = topos_type_state:new(),
    {Id, State1} = topos_type_state:fresh_var_id(State0),

    % First ID should be 1
    ?assertEqual(1, Id),
    % Counter should be updated
    ?assertEqual(1, topos_type_state:get_counter(State1)).

test_fresh_var() ->
    State0 = topos_type_state:new(),
    {Var, State1} = topos_type_state:fresh_var(State0),

    % Should return a type variable
    ?assertMatch({tvar, 1}, Var),
    % Counter should be updated
    ?assertEqual(1, topos_type_state:get_counter(State1)).

test_multiple_fresh_vars() ->
    State0 = topos_type_state:new(),
    {Var1, State1} = topos_type_state:fresh_var(State0),
    {Var2, State2} = topos_type_state:fresh_var(State1),
    {Var3, State3} = topos_type_state:fresh_var(State2),

    % Each variable should have unique ID
    ?assertMatch({tvar, 1}, Var1),
    ?assertMatch({tvar, 2}, Var2),
    ?assertMatch({tvar, 3}, Var3),

    % Final counter should be 3
    ?assertEqual(3, topos_type_state:get_counter(State3)).

test_sequential_ids() ->
    State0 = topos_type_state:new(),

    % Generate 10 fresh variable IDs
    {Ids, StateFinal} = lists:foldl(
        fun(_, {AccIds, AccState}) ->
            {Id, NewState} = topos_type_state:fresh_var_id(AccState),
            {[Id | AccIds], NewState}
        end,
        {[], State0},
        lists:seq(1, 10)
    ),

    % IDs should be 1..10 (reversed because of cons)
    ?assertEqual(lists:seq(10, 1, -1), Ids),
    % Counter should be 10
    ?assertEqual(10, topos_type_state:get_counter(StateFinal)).

%%====================================================================
%% State Independence Tests
%%====================================================================

state_independence_test_() ->
    [
     ?_test(test_state_not_modified()),
     ?_test(test_parallel_states())
    ].

test_state_not_modified() ->
    State0 = topos_type_state:new(),

    % Generate variable but don't use returned state
    {_Var1, _State1} = topos_type_state:fresh_var(State0),

    % Original state unchanged
    ?assertEqual(0, topos_type_state:get_counter(State0)),

    % Generate another from original - should get same ID
    {Var2, State2} = topos_type_state:fresh_var(State0),
    ?assertMatch({tvar, 1}, Var2),
    ?assertEqual(1, topos_type_state:get_counter(State2)).

test_parallel_states() ->
    % Two independent state threads
    StateA0 = topos_type_state:new(),
    StateB0 = topos_type_state:new(),

    % Branch A
    {VarA1, StateA1} = topos_type_state:fresh_var(StateA0),
    {VarA2, StateA2} = topos_type_state:fresh_var(StateA1),

    % Branch B (independent)
    {VarB1, StateB1} = topos_type_state:fresh_var(StateB0),
    {VarB2, StateB2} = topos_type_state:fresh_var(StateB1),

    % Both branches generate same IDs (independent)
    ?assertMatch({tvar, 1}, VarA1),
    ?assertMatch({tvar, 2}, VarA2),
    ?assertMatch({tvar, 1}, VarB1),
    ?assertMatch({tvar, 2}, VarB2),

    % Both counters at 2
    ?assertEqual(2, topos_type_state:get_counter(StateA2)),
    ?assertEqual(2, topos_type_state:get_counter(StateB2)).

%%====================================================================
%% Integration with topos_types Tests
%%====================================================================

integration_test_() ->
    [
     ?_test(test_types_fresh_var_with_state()),
     ?_test(test_types_fresh_var_id_with_state())
    ].

test_types_fresh_var_with_state() ->
    State0 = topos_type_state:new(),

    % Use topos_types wrapper
    {Var1, State1} = topos_types:fresh_var(State0),
    {Var2, State2} = topos_types:fresh_var(State1),

    ?assertMatch({tvar, 1}, Var1),
    ?assertMatch({tvar, 2}, Var2),
    ?assertEqual(2, topos_type_state:get_counter(State2)).

test_types_fresh_var_id_with_state() ->
    State0 = topos_type_state:new(),

    % Use topos_types wrapper
    {Id1, State1} = topos_types:fresh_var_id(State0),
    {Id2, State2} = topos_types:fresh_var_id(State1),

    ?assertEqual(1, Id1),
    ?assertEqual(2, Id2),
    ?assertEqual(2, topos_type_state:get_counter(State2)).

%%====================================================================
%% Integration with topos_type_scheme Tests
%%====================================================================

scheme_instantiation_test_() ->
    [
     ?_test(test_instantiate_monomorphic()),
     ?_test(test_instantiate_polymorphic()),
     ?_test(test_instantiate_multiple())
    ].

test_instantiate_monomorphic() ->
    State0 = topos_type_state:new(),

    % Monomorphic scheme
    Type = topos_types:tcon(integer),
    Scheme = topos_type_scheme:mono(Type),

    % Instantiate
    {InstType, State1} = topos_type_scheme:instantiate(Scheme, State0),

    % Should return same type, state unchanged
    ?assertEqual(Type, InstType),
    ?assertEqual(0, topos_type_state:get_counter(State1)).

test_instantiate_polymorphic() ->
    State0 = topos_type_state:new(),

    % Polymorphic scheme: ∀α. α -> α where α has ID 100
    % (Using 100 instead of 1 to ensure fresh var is different)
    Type = topos_types:tfun(
        topos_types:tvar(100),
        topos_types:tvar(100),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:poly([100], Type),

    % Instantiate
    {InstType, State1} = topos_type_scheme:instantiate(Scheme, State0),

    % Should have fresh variable (ID 1, not 100)
    ?assertMatch(
        {tfun, {tvar, 1}, {tvar, 1}, _},
        InstType
    ),
    ?assertEqual(1, topos_type_state:get_counter(State1)).

test_instantiate_multiple() ->
    State0 = topos_type_state:new(),

    % Polymorphic scheme: ∀α β. (α -> β) where α=100, β=200
    % (Using 100, 200 instead of 1, 2 to ensure fresh vars are different)
    Type = topos_types:tfun(
        topos_types:tvar(100),
        topos_types:tvar(200),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:poly([100, 200], Type),

    % Instantiate twice
    {InstType1, State1} = topos_type_scheme:instantiate(Scheme, State0),
    {InstType2, State2} = topos_type_scheme:instantiate(Scheme, State1),

    % Both should have fresh variables
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, InstType1),
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, InstType2),

    % First instantiation should use IDs 1 and 2
    ?assertMatch({tfun, {tvar, 1}, {tvar, 2}, _}, InstType1),

    % Second instantiation should use IDs 3 and 4
    ?assertMatch({tfun, {tvar, 3}, {tvar, 4}, _}, InstType2),

    % Counter should be 4 (2 vars × 2 instantiations)
    ?assertEqual(4, topos_type_state:get_counter(State2)).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {timeout, 10, [
        ?_test(test_many_fresh_vars())
    ]}.

test_many_fresh_vars() ->
    State0 = topos_type_state:new(),

    % Generate 10,000 fresh variables
    {_Vars, StateFinal} = lists:foldl(
        fun(_, {AccVars, AccState}) ->
            {Var, NewState} = topos_type_state:fresh_var(AccState),
            {[Var | AccVars], NewState}
        end,
        {[], State0},
        lists:seq(1, 10000)
    ),

    % Counter should be 10,000
    ?assertEqual(10000, topos_type_state:get_counter(StateFinal)).
