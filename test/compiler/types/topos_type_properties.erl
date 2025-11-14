%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for Type System
%%%
%%% Uses PropEr for property-based testing of type system invariants,
%%% mathematical laws, and correctness properties.
%%%
%%% ## Running Property Tests
%%%
%%% Run all properties:
%%%   rebar3 proper
%%%
%%% Run with more test cases:
%%%   rebar3 proper -n 1000
%%%
%%% Run specific property:
%%%   erl -pa _build/test/lib/*/ebin -eval "proper:quickcheck(topos_type_properties:prop_subst_identity())" -s init stop
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_properties).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    prop_subst_identity/0,
    prop_subst_composition/0,
    prop_subst_idempotent/0,
    prop_effect_union_commutative/0,
    prop_effect_union_associative/0,
    prop_effect_normalize_idempotent/0,
    prop_type_vars_substitution/0,
    prop_occurs_check_soundness/0
]).

%%====================================================================
%% Type Generators
%%====================================================================

%% @doc Generate a simple type (base types and type variables)
simple_type() ->
    oneof([
        {tcon, atom()},
        {tvar, pos_integer()}
    ]).

%% @doc Generate a type of limited depth
%% This prevents infinite recursion in generators
type(0) ->
    simple_type();
type(MaxDepth) ->
    oneof([
        simple_type(),
        {tapp, type(MaxDepth - 1), list(type(MaxDepth - 1))},
        {tfun, type(MaxDepth - 1), type(MaxDepth - 1), effect_set()},
        {trecord, list({atom(), type(MaxDepth - 1)}), row_var()},
        {ttuple, list(type(MaxDepth - 1))},
        {tvariant, list({atom(), list(type(MaxDepth - 1))})}
    ]).

%% @doc Generate a type with default max depth
gen_type() ->
    type(3).

%% @doc Generate a row variable (either closed or a type variable ID)
row_var() ->
    oneof([
        closed,
        pos_integer()
    ]).

%% @doc Generate an effect set
effect_set() ->
    ?LET(Effects, list(atom()),
         topos_types:normalize_effects(Effects)).

%% @doc Generate a type variable ID
type_var_id() ->
    pos_integer().

%% @doc Generate a substitution (map from type var IDs to types)
%% Limited to avoid infinite types
substitution() ->
    ?LET(Pairs, list({pos_integer(), simple_type()}),
         maps:from_list(Pairs)).

%% @doc Generate a substitution that doesn't create cycles
safe_substitution() ->
    ?SUCHTHAT(Subst, substitution(),
              not has_cycle(Subst)).

%% @doc Check if a substitution would create a cycle
%% Simple check: ensure no variable maps to itself or appears in its own definition
has_cycle(Subst) ->
    maps:fold(
        fun(VarId, Type, Acc) ->
            Acc orelse occurs_in_type(VarId, Type)
        end,
        false,
        Subst
    ).

occurs_in_type(VarId, {tvar, VarId}) -> true;
occurs_in_type(VarId, {tvar, _}) -> false;
occurs_in_type(_, {tcon, _}) -> false;
occurs_in_type(VarId, {tapp, Con, Args}) ->
    occurs_in_type(VarId, Con) orelse lists:any(fun(T) -> occurs_in_type(VarId, T) end, Args);
occurs_in_type(VarId, {tfun, From, To, _}) ->
    occurs_in_type(VarId, From) orelse occurs_in_type(VarId, To);
occurs_in_type(VarId, {trecord, Fields, _}) ->
    lists:any(fun({_, FType}) -> occurs_in_type(VarId, FType) end, Fields);
occurs_in_type(VarId, {ttuple, Elements}) ->
    lists:any(fun(T) -> occurs_in_type(VarId, T) end, Elements);
occurs_in_type(VarId, {tvariant, Constructors}) ->
    lists:any(fun({_, ArgTypes}) ->
        lists:any(fun(T) -> occurs_in_type(VarId, T) end, ArgTypes)
    end, Constructors).

%%====================================================================
%% Properties: Substitution Laws
%%====================================================================

%% @doc Property: Applying empty substitution is identity
%% For all types T: apply(∅, T) = T
prop_subst_identity() ->
    ?FORALL(T, gen_type(),
        begin
            topos_types:reset_fresh_counter(),
            Empty = topos_type_subst:empty(),
            Result = topos_type_subst:apply(Empty, T),
            equals(Result, T)
        end).

%% @doc Property: Substitution composition is associative
%% For all S1, S2, S3: compose(compose(S3, S2), S1) = compose(S3, compose(S2, S1))
prop_subst_composition() ->
    ?FORALL({S1, S2, S3}, {safe_substitution(), safe_substitution(), safe_substitution()},
        ?IMPLIES(all_disjoint([S1, S2, S3]),
            begin
                topos_types:reset_fresh_counter(),
                % Left: compose(compose(S3, S2), S1)
                S2_S1 = topos_type_subst:compose(S2, S1),
                Left = topos_type_subst:compose(S3, S2_S1),

                % Right: compose(S3, compose(S2, S1))
                S3_S2 = topos_type_subst:compose(S3, S2),
                Right = topos_type_subst:compose(S3_S2, S1),

                % They should be equal
                maps:equal(Left, Right)
            end)).

%% @doc Property: Applying a substitution twice is idempotent
%% For idempotent substitutions S: apply(S, apply(S, T)) = apply(S, T)
prop_subst_idempotent() ->
    ?FORALL({S, T}, {safe_substitution(), gen_type()},
        ?IMPLIES(is_idempotent(S),
            begin
                topos_types:reset_fresh_counter(),
                Once = topos_type_subst:apply(S, T),
                Twice = topos_type_subst:apply(S, Once),
                equals(Once, Twice)
            end)).

%% @doc Check if a substitution is idempotent
%% S is idempotent if applying S to all types in S's range doesn't change them
is_idempotent(Subst) ->
    maps:fold(
        fun(_VarId, Type, Acc) ->
            Acc andalso equals(Type, topos_type_subst:apply(Subst, Type))
        end,
        true,
        Subst
    ).

%%====================================================================
%% Properties: Effect Sets
%%====================================================================

%% @doc Property: Effect set union is commutative
%% For all E1, E2: union(E1, E2) = union(E2, E1)
prop_effect_union_commutative() ->
    ?FORALL({E1, E2}, {effect_set(), effect_set()},
        begin
            U1 = topos_types:union_effects(E1, E2),
            U2 = topos_types:union_effects(E2, E1),
            topos_types:effects_equal(U1, U2)
        end).

%% @doc Property: Effect set union is associative
%% For all E1, E2, E3: union(union(E1, E2), E3) = union(E1, union(E2, E3))
prop_effect_union_associative() ->
    ?FORALL({E1, E2, E3}, {effect_set(), effect_set(), effect_set()},
        begin
            Left = topos_types:union_effects(topos_types:union_effects(E1, E2), E3),
            Right = topos_types:union_effects(E1, topos_types:union_effects(E2, E3)),
            topos_types:effects_equal(Left, Right)
        end).

%% @doc Property: Normalizing an effect set is idempotent
%% For all E: normalize(normalize(E)) = normalize(E)
prop_effect_normalize_idempotent() ->
    ?FORALL(EffList, list(atom()),
        begin
            E1 = topos_types:normalize_effects(EffList),
            {effect_set, List1} = E1,
            E2 = topos_types:normalize_effects(List1),
            topos_types:effects_equal(E1, E2)
        end).

%%====================================================================
%% Properties: Type Variables
%%====================================================================

%% @doc Property: Substitution removes or preserves type variables correctly
%% If VarId is in type_vars(T) and not in domain(S), it's in type_vars(apply(S, T))
%% If VarId is in domain(S), it might be replaced
prop_type_vars_substitution() ->
    ?FORALL({T, S}, {gen_type(), safe_substitution()},
        begin
            topos_types:reset_fresh_counter(),
            VarsBefore = topos_types:type_vars(T),
            Result = topos_type_subst:apply(S, T),
            VarsAfter = topos_types:type_vars(Result),
            Domain = sets:from_list(topos_type_subst:domain(S)),

            % Variables not in domain should be preserved
            NotInDomain = sets:subtract(VarsBefore, Domain),
            sets:is_subset(NotInDomain, VarsAfter)
        end).

%%====================================================================
%% Properties: Occurs Check
%%====================================================================

%% @doc Property: occurs_check is sound
%% If occurs_check(V, T) returns true, then V appears in type_vars(T)
prop_occurs_check_soundness() ->
    ?FORALL({VarId, T}, {type_var_id(), gen_type()},
        begin
            topos_types:reset_fresh_counter(),
            OccursResult = topos_type_subst:occurs_check(VarId, T),
            TypeVars = topos_types:type_vars(T),
            InTypeVars = sets:is_element(VarId, TypeVars),

            % If occurs_check says it occurs, it must be in type_vars
            (not OccursResult) orelse InTypeVars
        end).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Check if two types are equal
equals(T1, T2) ->
    T1 =:= T2.

%% @doc Check if all substitutions have disjoint domains
all_disjoint([]) -> true;
all_disjoint([_]) -> true;
all_disjoint([S1, S2 | Rest]) ->
    D1 = sets:from_list(maps:keys(S1)),
    D2 = sets:from_list(maps:keys(S2)),
    Disjoint = sets:size(sets:intersection(D1, D2)) =:= 0,
    Disjoint andalso all_disjoint([S2 | Rest]).

%%====================================================================
%% EUnit Integration
%%====================================================================

%% Run PropEr tests through EUnit
proper_test_() ->
    {timeout, 60, [
        {"Substitution identity", ?_assert(proper:quickcheck(prop_subst_identity(), [{numtests, 100}, {to_file, user}]))},
        {"Effect union commutative", ?_assert(proper:quickcheck(prop_effect_union_commutative(), [{numtests, 100}, {to_file, user}]))},
        {"Effect union associative", ?_assert(proper:quickcheck(prop_effect_union_associative(), [{numtests, 100}, {to_file, user}]))},
        {"Effect normalize idempotent", ?_assert(proper:quickcheck(prop_effect_normalize_idempotent(), [{numtests, 100}, {to_file, user}]))},
        {"Type vars substitution", ?_assert(proper:quickcheck(prop_type_vars_substitution(), [{numtests, 100}, {to_file, user}]))},
        {"Occurs check soundness", ?_assert(proper:quickcheck(prop_occurs_check_soundness(), [{numtests, 100}, {to_file, user}]))}
    ]}.
