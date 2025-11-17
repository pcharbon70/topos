%%%-------------------------------------------------------------------
%%% @doc Constraint Representation and Operations
%%%
%%% This module implements constraint representation for trait constraints
%%% and their operations during type inference. Constraints represent
%%% requirements that must be satisfied for a type to be valid.
%%%
%%% Key Concepts:
%%% - Trait Constraints: Requirements like "T must implement Functor"
%%% - Constraint Generation: Creating constraints during inference
%%% - Constraint Simplification: Reducing to canonical form
%%% - Constraint Solving: Finding satisfying instances
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_constraint).

%% Constraint construction
-export([
    trait_constraint/2,
    trait_constraint/3,
    empty_constraint_set/0,
    add_constraint/2,
    add_constraints/2,
    is_empty/1
]).

%% Constraint operations
-export([
    simplify/1,
    normalize/1,
    substitute/2,
    type_vars/1,
    remove_duplicates/1
]).

%% Constraint queries
-export([
    find_constraints/2,
    filter_by_trait/2,
    has_constraint/2
]).

-export_type([constraint/0, constraint_set/0, trait_name/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Trait name (e.g., 'Functor', 'Monad', 'Eq')
-type trait_name() :: atom().

%% Source location for error reporting
-type loc() :: {file, string(), non_neg_integer(), non_neg_integer()}
             | unknown.

%% Constraint: A requirement that must be satisfied
-type constraint() ::
    {trait, trait_name(), [topos_types:ty()], loc()}.
    % {trait, TraitName, TypeArgs, Location}
    % Example: {trait, 'Functor', [{tvar, 1}], {file, "test.topos", 10, 5}}

%% Constraint set: Collection of constraints
-type constraint_set() :: [constraint()].

%%%===================================================================
%%% Constraint Construction
%%%===================================================================

%% @doc Create a trait constraint
%%
%% Example:
%%   Constraint = topos_constraint:trait_constraint('Functor', [{tvar, 1}], Loc)
%%
-spec trait_constraint(trait_name(), [topos_types:ty()], loc()) -> constraint().
trait_constraint(TraitName, TypeArgs, Loc)
  when is_atom(TraitName), is_list(TypeArgs) ->
    {trait, TraitName, TypeArgs, Loc}.

%% @doc Create a trait constraint with unknown location
-spec trait_constraint(trait_name(), [topos_types:ty()]) -> constraint().
trait_constraint(TraitName, TypeArgs) ->
    trait_constraint(TraitName, TypeArgs, unknown).

%% @doc Create an empty constraint set
-spec empty_constraint_set() -> constraint_set().
empty_constraint_set() ->
    [].

%% @doc Add a single constraint to a constraint set
-spec add_constraint(constraint(), constraint_set()) -> constraint_set().
add_constraint(Constraint, Set) when is_list(Set) ->
    [Constraint | Set].

%% @doc Add multiple constraints to a constraint set
-spec add_constraints(constraint_set(), constraint_set()) -> constraint_set().
add_constraints(New, Existing) when is_list(New), is_list(Existing) ->
    New ++ Existing.

%% @doc Check if constraint set is empty
-spec is_empty(constraint_set()) -> boolean().
is_empty([]) -> true;
is_empty(_) -> false.

%%%===================================================================
%%% Constraint Operations
%%%===================================================================

%% @doc Simplify a constraint set
%%
%% Simplification performs:
%% 1. Remove duplicates
%% 2. Normalize (sort by trait name, then type args)
%% 3. Detect contradictions (currently none in trait system)
%%
-spec simplify(constraint_set()) -> constraint_set().
simplify(Constraints) ->
    NoDups = remove_duplicates(Constraints),
    normalize(NoDups).

%% @doc Normalize constraints (sort for canonical form)
%%
%% Sorting order:
%% 1. By trait name (alphabetically)
%% 2. By type arguments (structurally)
%%
-spec normalize(constraint_set()) -> constraint_set().
normalize(Constraints) ->
    lists:sort(fun constraint_compare/2, Constraints).

%% @doc Apply type substitution to all constraints
%%
%% This is critical for constraint solving - as we learn more about
%% type variables through unification, we must propagate those
%% substitutions to all constraints.
%%
-spec substitute(topos_type_subst:subst(), constraint_set()) -> constraint_set().
substitute(Subst, Constraints) ->
    [substitute_constraint(Subst, C) || C <- Constraints].

%% @doc Get all type variables mentioned in constraints
-spec type_vars(constraint_set()) -> [topos_types:type_var_id()].
type_vars(Constraints) ->
    AllVars = lists:flatmap(fun constraint_type_vars/1, Constraints),
    lists:usort(AllVars).

%% @doc Remove duplicate constraints
-spec remove_duplicates(constraint_set()) -> constraint_set().
remove_duplicates(Constraints) ->
    remove_dups_helper(lists:sort(Constraints), []).

%%%===================================================================
%%% Constraint Queries
%%%===================================================================

%% @doc Find all constraints involving a specific type variable
-spec find_constraints(topos_types:type_var_id(), constraint_set()) ->
    constraint_set().
find_constraints(VarId, Constraints) ->
    lists:filter(
        fun(C) -> lists:member(VarId, constraint_type_vars(C)) end,
        Constraints
    ).

%% @doc Filter constraints by trait name
-spec filter_by_trait(trait_name(), constraint_set()) -> constraint_set().
filter_by_trait(TraitName, Constraints) ->
    lists:filter(
        fun({trait, Name, _, _}) -> Name =:= TraitName end,
        Constraints
    ).

%% @doc Check if a specific constraint exists in the set
-spec has_constraint(constraint(), constraint_set()) -> boolean().
has_constraint(Constraint, Set) ->
    lists:member(Constraint, Set).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Compare two constraints for sorting
-spec constraint_compare(constraint(), constraint()) -> boolean().
constraint_compare({trait, Name1, Args1, _}, {trait, Name2, Args2, _}) ->
    case Name1 =:= Name2 of
        true ->
            % Same trait name, compare type arguments lexicographically
            compare_type_lists(Args1, Args2);
        false ->
            % Different trait names, alphabetical order
            Name1 =< Name2
    end.

%% Compare two type lists lexicographically
-spec compare_type_lists([topos_types:ty()], [topos_types:ty()]) -> boolean().
compare_type_lists([], []) -> true;
compare_type_lists([], _) -> true;
compare_type_lists(_, []) -> false;
compare_type_lists([T1|Rest1], [T2|Rest2]) ->
    case compare_types(T1, T2) of
        equal -> compare_type_lists(Rest1, Rest2);
        less -> true;
        greater -> false
    end.

%% Compare two types (simplified structural comparison)
-spec compare_types(topos_types:ty(), topos_types:ty()) -> equal | less | greater.
compare_types(T1, T2) when T1 =:= T2 -> equal;
compare_types({tvar, N1}, {tvar, N2}) when N1 < N2 -> less;
compare_types({tvar, N1}, {tvar, N2}) when N1 > N2 -> greater;
compare_types({tvar, _}, _) -> less;  % tvars come first
compare_types(_, {tvar, _}) -> greater;
compare_types({tcon, A1}, {tcon, A2}) when A1 < A2 -> less;
compare_types({tcon, A1}, {tcon, A2}) when A1 > A2 -> greater;
compare_types({tcon, _}, _) -> less;
compare_types(_, {tcon, _}) -> greater;
compare_types(_, _) -> equal.  % Default for complex types

%% Apply substitution to a single constraint
-spec substitute_constraint(topos_type_subst:subst(), constraint()) -> constraint().
substitute_constraint(Subst, {trait, Name, TypeArgs, Loc}) ->
    NewArgs = [topos_type_subst:apply(Subst, Ty) || Ty <- TypeArgs],
    {trait, Name, NewArgs, Loc}.

%% Get type variables from a single constraint
-spec constraint_type_vars(constraint()) -> [topos_types:type_var_id()].
constraint_type_vars({trait, _Name, TypeArgs, _Loc}) ->
    lists:flatmap(fun topos_types:type_vars/1, TypeArgs).

%% Helper for removing duplicates from sorted list
-spec remove_dups_helper(constraint_set(), constraint_set()) -> constraint_set().
remove_dups_helper([], Acc) ->
    lists:reverse(Acc);
remove_dups_helper([C], Acc) ->
    lists:reverse([C | Acc]);
remove_dups_helper([C, C | Rest], Acc) ->
    % Skip duplicate
    remove_dups_helper([C | Rest], Acc);
remove_dups_helper([C1, C2 | Rest], Acc) ->
    % Different constraints
    remove_dups_helper([C2 | Rest], [C1 | Acc]).
