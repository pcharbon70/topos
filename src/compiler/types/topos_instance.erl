%%%-------------------------------------------------------------------
%%% @doc Trait Instance Resolution
%%%
%%% This module implements instance resolution for trait constraints.
%%% It searches for trait implementations (instances) and unifies them
%%% with constraints to verify that trait requirements are satisfied.
%%%
%%% Key Concepts:
%%% - Instance: A trait implementation for specific types
%%% - Instance Resolution: Finding instances that match constraints
%%% - Unification: Checking if an instance satisfies a constraint
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_instance).

%% Instance management
-export([
    make_instance/2,
    make_instance/3,
    empty_instance_db/0,
    add_instance/2,
    get_instances/2
]).

%% Instance resolution
-export([
    resolve_constraint/2,
    resolve_constraints/2,
    find_matching_instances/2,
    unify_instance/2
]).

%% Instance queries
-export([
    has_instance/2,
    check_overlap/2
]).

-export_type([instance/0, instance_db/0, resolution_result/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Trait instance: implementation of a trait for specific type(s)
-type instance() :: {
    instance,
    topos_constraint:trait_name(),  % Trait being implemented
    [topos_types:ty()],              % Type parameters
    topos_constraint:loc()           % Source location
}.

%% Instance database: maps trait names to their instances
-type instance_db() :: #{topos_constraint:trait_name() => [instance()]}.

%% Resolution result
-type resolution_result() ::
    {ok, instance(), topos_type_subst:subst()} |  % Success with unifier
    {error, no_instance} |                         % No matching instance
    {error, {ambiguous, [instance()]}}.            % Multiple matches

%%%===================================================================
%%% Instance Construction
%%%===================================================================

%% @doc Create a trait instance
%%
%% Example:
%%   Instance = topos_instance:make_instance('Functor', [{tcon, list}], Loc)
%%
-spec make_instance(topos_constraint:trait_name(), [topos_types:ty()],
                    topos_constraint:loc()) -> instance().
make_instance(TraitName, TypeArgs, Loc)
  when is_atom(TraitName), is_list(TypeArgs) ->
    {instance, TraitName, TypeArgs, Loc}.

%% @doc Create a trait instance with unknown location
-spec make_instance(topos_constraint:trait_name(), [topos_types:ty()]) ->
    instance().
make_instance(TraitName, TypeArgs) ->
    make_instance(TraitName, TypeArgs, unknown).

%% @doc Create an empty instance database
-spec empty_instance_db() -> instance_db().
empty_instance_db() ->
    #{}.

%% @doc Add an instance to the database
%%
%% Instances are organized by trait name for efficient lookup.
%%
-spec add_instance(instance(), instance_db()) -> instance_db().
add_instance({instance, TraitName, _TypeArgs, _Loc} = Instance, DB) ->
    Existing = maps:get(TraitName, DB, []),
    DB#{TraitName => [Instance | Existing]}.

%% @doc Get all instances for a trait
-spec get_instances(topos_constraint:trait_name(), instance_db()) ->
    [instance()].
get_instances(TraitName, DB) ->
    maps:get(TraitName, DB, []).

%%%===================================================================
%%% Instance Resolution
%%%===================================================================

%% @doc Resolve a single constraint
%%
%% Attempts to find a unique instance that satisfies the constraint.
%% Returns the instance and a substitution that makes them match.
%%
%% Resolution succeeds if:
%% 1. Exactly one instance matches the constraint
%% 2. The instance head unifies with the constraint
%%
%% Resolution fails if:
%% 1. No instances match (no_instance)
%% 2. Multiple instances match (ambiguous - coherence violation)
%%
-spec resolve_constraint(topos_constraint:constraint(), instance_db()) ->
    resolution_result().
resolve_constraint({trait, TraitName, TypeArgs, _Loc}, DB) ->
    Instances = get_instances(TraitName, DB),
    Matching = find_matching_instances({trait, TraitName, TypeArgs, unknown},
                                      Instances),
    case Matching of
        [] ->
            {error, no_instance};
        [Instance] ->
            % Unique instance found, unify it
            case unify_instance(Instance, {trait, TraitName, TypeArgs, unknown}) of
                {ok, Subst} ->
                    {ok, Instance, Subst};
                {error, Reason} ->
                    {error, Reason}
            end;
        Multiple ->
            % Ambiguous - multiple instances match
            {error, {ambiguous, Multiple}}
    end.

%% @doc Resolve multiple constraints
%%
%% Attempts to resolve all constraints in the set.
%% Returns:
%% - {ok, Solutions} where Solutions is [{Constraint, Instance, Subst}]
%% - {error, {Constraint, Reason}} for the first failing constraint
%%
-spec resolve_constraints(topos_constraint:constraint_set(), instance_db()) ->
    {ok, [{topos_constraint:constraint(), instance(), topos_type_subst:subst()}]} |
    {error, {topos_constraint:constraint(), term()}}.
resolve_constraints(Constraints, DB) ->
    resolve_constraints_acc(Constraints, DB, []).

%% @doc Find instances that could potentially match a constraint
%%
%% This is a filtering step before unification. An instance "matches"
%% if its trait name is the same and it has the same arity.
%%
-spec find_matching_instances(topos_constraint:constraint(), [instance()]) ->
    [instance()].
find_matching_instances({trait, TraitName, TypeArgs, _}, Instances) ->
    Arity = length(TypeArgs),
    lists:filter(
        fun({instance, Name, Args, _}) ->
            Name =:= TraitName andalso length(Args) =:= Arity
        end,
        Instances
    ).

%% @doc Unify an instance with a constraint
%%
%% Attempts to unify the instance's type parameters with the
%% constraint's type arguments. Returns a substitution if successful.
%%
%% Example:
%%   Instance: Functor [List α]
%%   Constraint: Functor [List Int]
%%   Result: {ok, {α -> Int}}
%%
-spec unify_instance(instance(), topos_constraint:constraint()) ->
    {ok, topos_type_subst:subst()} | {error, term()}.
unify_instance({instance, _TraitName1, InstanceArgs, _Loc},
               {trait, _TraitName2, ConstraintArgs, _}) ->
    % Use the unification algorithm to match instance args with constraint args
    try
        Subst = unify_type_lists(InstanceArgs, ConstraintArgs,
                                topos_type_subst:empty()),
        {ok, Subst}
    catch
        error:Reason ->
            {error, Reason}
    end.

%%%===================================================================
%%% Instance Queries
%%%===================================================================

%% @doc Check if an instance exists for a constraint
%%
%% This is a simpler check than full resolution - just checks
%% if at least one instance could match.
%%
-spec has_instance(topos_constraint:constraint(), instance_db()) -> boolean().
has_instance({trait, TraitName, TypeArgs, _}, DB) ->
    Instances = get_instances(TraitName, DB),
    Matching = find_matching_instances({trait, TraitName, TypeArgs, unknown},
                                      Instances),
    length(Matching) > 0.

%% @doc Check if two instances overlap
%%
%% Two instances overlap if there exists a substitution that makes
%% their heads unify. Overlapping instances violate coherence.
%%
%% Example of overlap:
%%   Instance 1: Eq (List α)
%%   Instance 2: Eq (List Int)
%%   These overlap because α can be Int
%%
-spec check_overlap(instance(), instance()) -> boolean().
check_overlap({instance, Trait1, Args1, _}, {instance, Trait2, Args2, _}) ->
    % Different traits never overlap
    case Trait1 =:= Trait2 of
        false ->
            false;
        true ->
            % Same trait - check if args can unify
            try
                _Subst = unify_type_lists(Args1, Args2, topos_type_subst:empty()),
                true  % Unification succeeded, instances overlap
            catch
                error:_ ->
                    false  % Unification failed, no overlap
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Resolve constraints accumulator
-spec resolve_constraints_acc(
    topos_constraint:constraint_set(),
    instance_db(),
    [{topos_constraint:constraint(), instance(), topos_type_subst:subst()}]
) ->
    {ok, [{topos_constraint:constraint(), instance(), topos_type_subst:subst()}]} |
    {error, {topos_constraint:constraint(), term()}}.
resolve_constraints_acc([], _DB, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_constraints_acc([C | Rest], DB, Acc) ->
    case resolve_constraint(C, DB) of
        {ok, Instance, Subst} ->
            resolve_constraints_acc(Rest, DB, [{C, Instance, Subst} | Acc]);
        {error, Reason} ->
            {error, {C, Reason}}
    end.

%% Unify two lists of types pairwise
%%
%% This is a helper for instance resolution. It unifies corresponding
%% elements and accumulates the substitution.
%%
-spec unify_type_lists([topos_types:ty()], [topos_types:ty()],
                       topos_type_subst:subst()) ->
    topos_type_subst:subst().
unify_type_lists([], [], Subst) ->
    Subst;
unify_type_lists([T1 | Rest1], [T2 | Rest2], Subst) ->
    % Apply current substitution to both types
    T1_subst = topos_type_subst:apply(Subst, T1),
    T2_subst = topos_type_subst:apply(Subst, T2),

    % Unify the substituted types
    case topos_infer_unify:unify(T1_subst, T2_subst) of
        {ok, NewSubst} ->
            % Compose substitutions
            ComposedSubst = topos_type_subst:compose(NewSubst, Subst),
            unify_type_lists(Rest1, Rest2, ComposedSubst);
        {error, Reason} ->
            error(Reason)
    end;
unify_type_lists(_, _, _) ->
    error(arity_mismatch).
