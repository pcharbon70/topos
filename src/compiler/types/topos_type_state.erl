%%%-------------------------------------------------------------------
%%% @doc Type Inference State Management
%%%
%%% Manages state for type inference, including fresh variable generation.
%%% Uses explicit state threading (functional approach) instead of
%%% process dictionary for better testability and thread-safety.
%%%
%%% This module provides a stateless API where state is explicitly
%%% passed and returned, following functional programming best practices.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_state).

-export([
    new/0,
    new/1,
    fresh_var/1,
    fresh_var_id/1,
    get_counter/1
]).

-export_type([state/0]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type state() :: #{
    counter => non_neg_integer()
    % Future additions:
    % constraint_counter => non_neg_integer(),
    % type_env => topos_type_env:env(),
    % constraints => [constraint()],
    % etc.
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new initial state with counter starting at 0
-spec new() -> state().
new() ->
    #{counter => 0}.

%% @doc Create a new state with specified starting counter
%% Useful for resuming inference or testing with specific IDs
-spec new(non_neg_integer()) -> state().
new(StartCounter) when is_integer(StartCounter), StartCounter >= 0 ->
    #{counter => StartCounter}.

%% @doc Generate a fresh type variable and return updated state
%% Returns {TypeVariable, NewState}
%%
%% Example:
%%   State0 = topos_type_state:new(),
%%   {Var1, State1} = topos_type_state:fresh_var(State0),
%%   {Var2, State2} = topos_type_state:fresh_var(State1),
%%   Var1 =:= {tvar, 1}, Var2 =:= {tvar, 2}
-spec fresh_var(state()) -> {topos_types:ty(), state()}.
fresh_var(State) ->
    {VarId, NewState} = fresh_var_id(State),
    {topos_types:tvar(VarId), NewState}.

%% @doc Generate a fresh type variable ID and return updated state
%% Returns {VarId, NewState}
%%
%% Example:
%%   State0 = topos_type_state:new(),
%%   {Id1, State1} = topos_type_state:fresh_var_id(State0),
%%   {Id2, State2} = topos_type_state:fresh_var_id(State1),
%%   Id1 =:= 1, Id2 =:= 2
-spec fresh_var_id(state()) -> {topos_types:type_var_id(), state()}.
fresh_var_id(#{counter := Counter} = State) ->
    NewCounter = Counter + 1,
    NewState = State#{counter := NewCounter},
    {NewCounter, NewState}.

%% @doc Get the current counter value without modifying state
%% Useful for debugging and testing
-spec get_counter(state()) -> non_neg_integer().
get_counter(#{counter := Counter}) ->
    Counter.

%%====================================================================
%% Internal Functions
%%====================================================================

% Future additions as type inference grows:
% - Constraint management
% - Type environment threading
% - Unification state
% - Error accumulation
% - Source location tracking
