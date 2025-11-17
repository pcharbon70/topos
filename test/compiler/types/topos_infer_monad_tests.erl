%%%
%%% @doc Tests for Monadic Inference Combinators
%%%
%%% Demonstrates the benefits of monadic style over traditional
%%% nested case statements for type inference.
%%%
%%% @end
%%%
-module(topos_infer_monad_tests).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Monadic Combinator Tests
%%===================================================================

monadic_return_test() ->
    % Test basic return function
    State = topos_infer_state:new(),
    {Result, NewState} = topos_infer_monad:return(42, State),
    ?assertEqual(42, Result),
    ?assertEqual(State, NewState).

monadic_sequence_test() ->
    % Test sequencing multiple successful operations
    State = topos_infer_state:new(),
    
    % Create a sequence of fresh variable operations
    Comps = [
        fun(S) -> topos_infer_state:fresh_var(S) end,
        fun(S) -> topos_infer_state:fresh_var(S) end,
        fun(S) -> topos_infer_state:fresh_var(S) end
    ],
    
    SequenceComp = topos_infer_monad:sequence(Comps),
    {Results, FinalState} = SequenceComp(State),
    
    ?assertEqual(3, length(Results)),
    ?assertEqual(4, topos_infer_state:get_next_var(FinalState)),
    
    % Check we got distinct fresh variables
    {tvar, Id1} = lists:nth(1, Results),
    {tvar, Id2} = lists:nth(2, Results),
    {tvar, Id3} = lists:nth(3, Results),
    ?assert(true, Id1 < Id2),
    ?assert(true, Id2 < Id3).

monadic_with_fresh_var_test() ->
    % Test the with_fresh_var combinator
    State = topos_infer_state:new(100),
    
    % Create computation that uses the fresh variable
    CompFun = fun(FreshVar) ->
        fun(CurrentState) ->
            {FreshVar, CurrentState}
        end
    end,
    
    FreshVarComp = topos_infer_monad:with_fresh_var(CompFun),
    {Result, NewState} = FreshVarComp(State),
    
    % Should get a fresh type variable
    {tvar, VarId} = Result,
    ?assertEqual(101, VarId),
    ?assertEqual(101, topos_infer_state:get_next_var(NewState)).

monadic_with_fresh_vars_test() ->
    % Test generating multiple fresh variables at once
    State = topos_infer_state:new(200),
    
    % Create computation that uses the fresh variables
    CompFun = fun(Vars) ->
        fun(CurrentState) ->
            {lists:sum([Id || {tvar, Id} <- Vars]), CurrentState}
        end
    end,
    
    FreshVarsComp = topos_infer_monad:with_fresh_vars(3, CompFun),
    {Result, NewState} = FreshVarsComp(State),
    
    % Should sum of three consecutive IDs
    ?assertEqual(201 + 202 + 203, Result),
    ?assertEqual(204, topos_infer_state:get_next_var(NewState)).

monadic_combine_test() ->
    % Test combining two computations
    State = topos_infer_state:new(),
    
    CompA = fun(S) -> {{ok, 42}, S} end,
    CompB = fun(S) -> {{error, "test"}, S} end,
    
    % Combine successful with successful
    CompB2 = fun(S) -> {{ok, 24}, S} end,
    CombineSuccess = topos_infer_monad:combine(CompA, CompB2),
    {{42, 24}, _State1} = CombineSuccess(State),
    
    % Combine successful with error should propagate error
    CombineError = topos_infer_monad:combine(CompA, CompB),
    {error, _, _} = CombineError(State).

%%===================================================================
%% Demonstration: Traditional vs Monadic Style
%%===================================================================

traditional_nesting_demo_test() ->
    % This shows what traditional code looks like
    % (simplified example)
    State = topos_infer_state:new(),
    
    % Traditional: deeply nested
    ResultTraditional = case topos_infer_state:fresh_var(State) of
        {Var1, State1} -> 
            case topos_infer_state:fresh_var(State1) of
                {Var2, State2} ->
                    case topos_infer_state:fresh_var(State2) of
                        {Var3, State3} ->
                            {ok, {Var1, Var2, Var3}, State3};
                        {error, Error, State3} ->
                            {error, Error, State3}
                    end;
                {error, Error, State2} ->
                    {error, Error, State2}
            end;
        {error, Error, State1} ->
            {error, Error, State1}
    end,
    
    {ok, {_, _, _}, _} = ResultTraditional.

monadic_style_demo_test() ->
    % This shows the equivalent monadic code
    State = topos_infer_state:new(),
    
    % Monadic: flat structure with clear flow
    MonadicComp = topos_infer_monad:with_fresh_vars(3,
        fun(Vars) ->
            fun(CurrentState) ->
                {ok, Vars, CurrentState}
            end
        end
    ),
    
    {ok, Vars, _FinalState} = MonadicComp(State),
    ?assertEqual(3, length(Vars)).

%%===================================================================
%% Integration Tests
%%===================================================================

monadic_integration_test() ->
    % Test that monadic combinators work with real inference
    State = topos_infer_state:new(),
    
    % Create a sequence that mimics inference steps
    InferSteps = topos_infer_monad:sequence([
        fun(S) -> topos_infer_state:fresh_var(S) end,  % Type variable for result
        fun(S) -> topos_infer_state:fresh_var(S) end,  % Scope variable
        fun(S) -> topos_infer_state:fresh_var(S) end   % Effect variable
    ]),
    
    {[{tvar, 1}, {tvar, 2}, {tvar, 3}], FinalState} = InferSteps(State),
    ?assertEqual(4, topos_infer_state:get_next_var(FinalState)).