%%%-------------------------------------------------------------------
%%% @doc Type Schemes for Polymorphism
%%%
%%% Implements type schemes (forall-quantified types) for let-polymorphism
%%% in Algorithm W. Type schemes represent polymorphic types like:
%%%   ∀α. α -> α (the identity function)
%%%   ∀α β. (α -> β) -> List α -> List β (the map function)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_scheme).

-export([
    mono/1,
    poly/2,
    generalize/2,
    instantiate/1,      % Stateful (uses process dictionary) - DEPRECATED
    instantiate/2,      % Stateless (explicit state threading) - RECOMMENDED
    ftv_scheme/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type scheme: monomorphic type or polymorphic type with quantified variables
-type scheme() :: {mono, topos_types:ty()}                          % Monomorphic
                | {poly, [topos_types:type_var_id()], topos_types:ty()}.  % Polymorphic

-export_type([scheme/0]).

%%====================================================================
%% Type Scheme Construction
%%====================================================================

-spec mono(topos_types:ty()) -> scheme().
%% @doc Create a monomorphic type scheme (no quantified variables)
mono(Type) ->
    {mono, Type}.

-spec poly([topos_types:type_var_id()], topos_types:ty()) -> scheme().
%% @doc Create a polymorphic type scheme with explicitly quantified variables
poly(Vars, Type) when is_list(Vars) ->
    {poly, Vars, Type}.

%%====================================================================
%% Generalization and Instantiation
%%====================================================================

-spec generalize(topos_types:ty(), sets:set(topos_types:type_var_id())) -> scheme().
%% @doc Generalize a type into a type scheme
%% Quantifies over all type variables in the type that are NOT in the environment
%% This implements the generalization step in Algorithm W
%%
%% Example:
%%   Type: α -> β
%%   Env free vars: {β}
%%   Result: ∀α. α -> β  (only α is quantified, β is free in environment)
generalize(Type, EnvFreeVars) ->
    TypeVars = topos_types:type_vars(Type),
    % Quantify over variables that appear in type but not in environment
    QuantVars = sets:subtract(TypeVars, EnvFreeVars),
    QuantVarsList = lists:sort(sets:to_list(QuantVars)),

    case QuantVarsList of
        [] -> {mono, Type};           % No quantified variables
        _ -> {poly, QuantVarsList, Type}  % Polymorphic scheme
    end.

-spec instantiate(scheme()) -> topos_types:ty().
%% @doc Instantiate a type scheme using process dictionary (DEPRECATED)
%% This implements the instantiation step in Algorithm W
%%
%% DEPRECATED: Use instantiate/2 with explicit state for new code
%%
%% Example:
%%   Scheme: ∀α β. (α -> β) -> List α -> List β
%%   Result: (γ₁ -> γ₂) -> List γ₁ -> List γ₂  (fresh variables)
instantiate({mono, Type}) ->
    % Monomorphic types instantiate to themselves
    Type;
instantiate({poly, QuantVars, Type}) ->
    % Create substitution mapping each quantified variable to a fresh one
    Subst = lists:foldl(
        fun(VarId, Acc) ->
            FreshVar = topos_types:fresh_var(),
            topos_type_subst:extend(Acc, VarId, FreshVar)
        end,
        topos_type_subst:empty(),
        QuantVars
    ),
    % Apply substitution to get fresh instance
    topos_type_subst:apply(Subst, Type).

-spec instantiate(scheme(), topos_type_state:state()) ->
    {topos_types:ty(), topos_type_state:state()}.
%% @doc Instantiate a type scheme with explicit state (RECOMMENDED)
%% This implements the instantiation step in Algorithm W
%%
%% This is the recommended approach as it's more functional and testable.
%% Use this for all new code, especially in type inference.
%%
%% Example:
%%   State0 = topos_type_state:new(),
%%   Scheme = {poly, [1, 2], {tfun, {tvar, 1}, {tvar, 2}, {effect_set, []}}},
%%   {InstType, State1} = topos_type_scheme:instantiate(Scheme, State0)
instantiate({mono, Type}, State) ->
    % Monomorphic types instantiate to themselves
    {Type, State};
instantiate({poly, QuantVars, Type}, State0) ->
    % Create substitution mapping each quantified variable to a fresh one
    % Thread state through the fold
    {Subst, StateFinal} = lists:foldl(
        fun(VarId, {AccSubst, AccState}) ->
            {FreshVar, NewState} = topos_types:fresh_var(AccState),
            NewSubst = topos_type_subst:extend(AccSubst, VarId, FreshVar),
            {NewSubst, NewState}
        end,
        {topos_type_subst:empty(), State0},
        QuantVars
    ),
    % Apply substitution to get fresh instance
    InstType = topos_type_subst:apply(Subst, Type),
    {InstType, StateFinal}.

%%====================================================================
%% Free Type Variables
%%====================================================================

-spec ftv_scheme(scheme()) -> sets:set(topos_types:type_var_id()).
%% @doc Compute free type variables of a type scheme
%% Free variables are those that appear in the type but are not quantified
ftv_scheme({mono, Type}) ->
    % All variables in monomorphic type are free
    topos_types:type_vars(Type);
ftv_scheme({poly, QuantVars, Type}) ->
    % Free vars = all vars in type minus quantified vars
    TypeVars = topos_types:type_vars(Type),
    QuantVarsSet = sets:from_list(QuantVars),
    sets:subtract(TypeVars, QuantVarsSet).

%%====================================================================
%% Internal Functions
%%====================================================================

% (None yet)
