%%%
%%% @doc Type Schemes for Polymorphism
%%%
%%% Implements type schemes (forall-quantified types) for let-polymorphism
%%% in Algorithm W. Type schemes represent polymorphic types like:
%%%   ∀α. α -> α (the identity function)
%%%   ∀α β. (α -> β) -> List α -> List β (the map function)
%%%
%%% @end
%%%
-module(topos_type_scheme).

-export([
    mono/1,
    poly/2,
    generalize/2,
    instantiate/2,
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

%% @doc Create a monomorphic type scheme.
%%
%% Wraps a concrete type in a monomorphic scheme with no quantified variables.
%% Monomorphic types cannot be instantiated with different types - they
%% represent specific, non-polymorphic types.
%%
%% @param Type The concrete type to wrap
%% @returns A monomorphic type scheme `{mono, Type}'
%%
%% @see poly/2
%% @see generalize/2
%%
%% @example
%% ```
%% %% Monomorphic Int type
%% IntScheme = topos_type_scheme:mono(topos_types:tcon(integer)).
%% %% → {mono, {tcon, integer}}
%%
%% %% Monomorphic function: Int -> String
%% FuncScheme = topos_type_scheme:mono(
%%     topos_types:tfun(
%%         topos_types:tcon(integer),
%%         topos_types:tcon(string),
%%         topos_types:empty_effects()
%%     )
%% ).
%% '''
-spec mono(topos_types:ty()) -> scheme().
mono(Type) ->
    {mono, Type}.

%% @doc Create a polymorphic type scheme with explicit quantification.
%%
%% Constructs a polymorphic scheme by explicitly listing which type variables
%% are quantified (bound by forall). The quantified variables can be replaced
%% with different types at each instantiation site.
%%
%% @param Vars List of type variable IDs to quantify
%% @param Type The type containing the quantified variables
%% @returns A polymorphic type scheme `{poly, Vars, Type}'
%%
%% @see mono/1
%% @see generalize/2
%% @see instantiate/2
%%
%% @example
%% ```
%% %% Identity function: ∀α. α -> α
%% State0 = topos_type_state:new(),
%% {Alpha, _State1} = topos_types:fresh_var(State0),
%% IdentityScheme = topos_type_scheme:poly(
%%     [1],  %% Quantify α₁
%%     topos_types:tfun(Alpha, Alpha, topos_types:empty_effects())
%% ).
%% %% → {poly, [1], {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}}}
%%
%% %% Map function: ∀α β. (α -> β) -> List α -> List β
%% {Beta, _State2} = topos_types:fresh_var(State0),
%% MapScheme = topos_type_scheme:poly([1, 2], MapType).
%% '''
-spec poly([topos_types:type_var_id()], topos_types:ty()) -> scheme().
poly(Vars, Type) when is_list(Vars) ->
    {poly, Vars, Type}.

%%====================================================================
%% Generalization and Instantiation
%%====================================================================

%% @doc Generalize a type into a polymorphic type scheme.
%%
%% This implements the generalization step in Algorithm W (Hindley-Milner
%% type inference). Quantifies over all type variables that appear in the
%% type but NOT in the environment's free variables.
%%
%% Generalization creates polymorphic types at let-bindings, allowing
%% different uses of the same function to have different instantiated types.
%% Variables free in the environment cannot be quantified, as they may already
%% be constrained by outer scopes.
%%
%% @param Type The type to generalize
%% @param EnvFreeVars Set of type variables free in the current environment
%% @returns A type scheme quantifying over local variables
%%
%% @see poly/2
%% @see instantiate/2
%%
%% @example
%% ```
%% %% Generalize with no environment constraints
%% %% Type: α₁ -> α₁
%% State0 = topos_type_state:new(),
%% {Alpha, _State1} = topos_types:fresh_var(State0),
%% IdType = topos_types:tfun(Alpha, Alpha, topos_types:empty_effects()),
%% Scheme = topos_type_scheme:generalize(IdType, sets:new()).
%% %% → {poly, [1], {tfun, {tvar, 1}, {tvar, 1}, ...}}
%%
%% %% Generalize with environment constraints
%% %% Type: α₁ -> α₂
%% %% Environment has free variable α₂
%% {Beta, _State2} = topos_types:fresh_var(State0),
%% FuncType = topos_types:tfun(Alpha, Beta, topos_types:empty_effects()),
%% EnvVars = sets:from_list([2]),  %% β is free in environment
%% Scheme2 = topos_type_scheme:generalize(FuncType, EnvVars).
%% %% → {poly, [1], {tfun, {tvar, 1}, {tvar, 2}, ...}}
%% %% Only α₁ is quantified; α₂ remains free
%% '''
-spec generalize(topos_types:ty(), sets:set(topos_types:type_var_id())) -> scheme().
generalize(Type, EnvFreeVars) ->
    TypeVars = topos_types:type_vars(Type),
    % Quantify over variables that appear in type but not in environment
    QuantVars = sets:subtract(TypeVars, EnvFreeVars),
    QuantVarsList = lists:sort(sets:to_list(QuantVars)),

    case QuantVarsList of
        [] -> {mono, Type};           % No quantified variables
        _ -> {poly, QuantVarsList, Type}  % Polymorphic scheme
    end.

%% @doc Instantiate a type scheme with fresh type variables.
%%
%% This implements the instantiation step in Algorithm W. Replaces all
%% quantified variables with fresh type variables, allowing the same
%% polymorphic function to be used at different types.
%%
%% Uses explicit state threading to ensure all fresh variables have
%% unique IDs. Monomorphic schemes instantiate to themselves unchanged.
%%
%% @param Scheme The type scheme to instantiate
%% @param State The current type state for fresh variable generation
%% @returns Tuple `{InstantiatedType, NewState}'
%%
%% @see generalize/2
%% @see poly/2
%%
%% @example
%% ```
%% %% Instantiate identity function: ∀α. α -> α
%% State0 = topos_type_state:new(),
%% IdScheme = {poly, [1], {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}}},
%% {InstType1, State1} = topos_type_scheme:instantiate(IdScheme, State0).
%% %% → {{tfun, {tvar, 2}, {tvar, 2}, ...}, State1}
%% %% α₁ replaced with fresh α₂
%%
%% %% Second instantiation gets different fresh variables
%% {InstType2, State2} = topos_type_scheme:instantiate(IdScheme, State1).
%% %% → {{tfun, {tvar, 3}, {tvar, 3}, ...}, State2}
%% %% α₁ replaced with fresh α₃
%%
%% %% Monomorphic schemes don't change
%% MonoScheme = {mono, {tcon, integer}},
%% {IntType, State3} = topos_type_scheme:instantiate(MonoScheme, State2).
%% %% → {{tcon, integer}, State3}
%% '''
-spec instantiate(scheme(), topos_type_state:state()) ->
    {topos_types:ty(), topos_type_state:state()}.
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

%% @doc Compute the free type variables of a type scheme.
%%
%% Free type variables are those that appear in the type but are NOT
%% quantified by the scheme's forall. For monomorphic schemes, all
%% variables are free. For polymorphic schemes, free variables are
%% those in the type minus the quantified ones.
%%
%% Essential for type environment operations and generalization,
%% as it determines which variables are still "unknowns" that can
%% be constrained by unification.
%%
%% @param Scheme The type scheme to analyze
%% @returns Set of free type variable IDs
%%
%% @see generalize/2
%% @see topos_types:type_vars/1
%%
%% @example
%% ```
%% %% Monomorphic scheme - all variables are free
%% State0 = topos_type_state:new(),
%% {Alpha, _State1} = topos_types:fresh_var(State0),
%% MonoScheme = {mono, Alpha},
%% FreeVars1 = topos_type_scheme:ftv_scheme(MonoScheme).
%% %% → set containing 1
%%
%% %% Polymorphic scheme - quantified variables not free
%% %% ∀α. α -> β  (β is free, α is quantified)
%% {Beta, _State2} = topos_types:fresh_var(State0),
%% PolyScheme = {poly, [1], {tfun, Alpha, Beta, {effect_set, []}}},
%% FreeVars2 = topos_type_scheme:ftv_scheme(PolyScheme).
%% %% → set containing only 2 (β is free, α is bound)
%%
%% %% Fully quantified scheme - no free variables
%% FullyQuantified = {poly, [1, 2], {tfun, Alpha, Beta, {effect_set, []}}},
%% FreeVars3 = topos_type_scheme:ftv_scheme(FullyQuantified).
%% %% → empty set
%% '''
-spec ftv_scheme(scheme()) -> sets:set(topos_types:type_var_id()).
ftv_scheme({mono, Type}) ->
    % All variables in monomorphic type are free
    topos_types:type_vars(Type);
ftv_scheme({poly, QuantVars, Type}) ->
    % Free vars = all vars in type minus quantified vars
    TypeVars = topos_types:type_vars(Type),
    QuantVarsSet = sets:from_list(QuantVars),
    sets:subtract(TypeVars, QuantVarsSet).
