%%%-------------------------------------------------------------------
%%% @doc Type Substitution Operations
%%%
%%% Implements substitutions (mappings from type variables to types)
%%% and operations for composing and applying them. Essential for
%%% Robinson's unification algorithm and Algorithm W.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_subst).

-compile({no_auto_import,[apply/2]}).

-export([
    empty/0,
    singleton/2,
    lookup/2,
    extend/3,
    compose/2,
    apply/2,
    apply_effects/2,
    domain/1,
    range/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Substitution: mapping from type variable IDs to types
-type subst() :: #{topos_types:type_var_id() => topos_types:ty()}.

-export_type([subst/0]).

%%====================================================================
%% Substitution Construction
%%====================================================================

-spec empty() -> subst().
empty() ->
    #{}.

-spec singleton(topos_types:type_var_id(), topos_types:ty()) -> subst().
singleton(VarId, Type) when is_integer(VarId), VarId > 0 ->
    #{VarId => Type}.

-spec lookup(subst(), topos_types:type_var_id()) -> {ok, topos_types:ty()} | none.
lookup(Subst, VarId) ->
    case maps:find(VarId, Subst) of
        {ok, Type} -> {ok, Type};
        error -> none
    end.

-spec extend(subst(), topos_types:type_var_id(), topos_types:ty()) -> subst().
extend(Subst, VarId, Type) ->
    maps:put(VarId, Type, Subst).

%%====================================================================
%% Substitution Composition
%%====================================================================

-spec compose(subst(), subst()) -> subst().
%% @doc Compose two substitutions: compose(S2, S1) = S2 ∘ S1
%% This means: first apply S1, then apply S2
%% Result: a substitution that has the same effect as applying S1 then S2
compose(S2, S1) ->
    % Apply S2 to all types in the range of S1
    S1Applied = maps:map(fun(_VarId, Type) -> apply(S2, Type) end, S1),
    % Merge: S2's bindings take precedence, but include S1's bindings too
    maps:merge(S1Applied, S2).

%%====================================================================
%% Substitution Application
%%====================================================================

-spec apply(subst(), topos_types:ty()) -> topos_types:ty().
%% @doc Apply a substitution to a type, replacing type variables
apply(Subst, {tvar, VarId}) ->
    case lookup(Subst, VarId) of
        {ok, Type} -> Type;  % Replace with substituted type
        none -> {tvar, VarId}  % Variable not in substitution, keep as is
    end;

apply(_Subst, {tcon, Name}) ->
    {tcon, Name};  % Constants don't change

apply(Subst, {tapp, Constructor, Args}) ->
    {tapp, apply(Subst, Constructor), [apply(Subst, Arg) || Arg <- Args]};

apply(Subst, {tfun, From, To, Effects}) ->
    {tfun, apply(Subst, From), apply(Subst, To), apply_effects(Subst, Effects)};

apply(Subst, {trecord, Fields, RowVar}) ->
    NewFields = [{Name, apply(Subst, FieldType)} || {Name, FieldType} <- Fields],
    NewRowVar = case RowVar of
                    closed -> closed;
                    VarId when is_integer(VarId) ->
                        case lookup(Subst, VarId) of
                            {ok, {tvar, NewVarId}} -> NewVarId;
                            {ok, _Other} -> closed;  % Row variable bound to non-var
                            none -> VarId
                        end
                end,
    {trecord, NewFields, NewRowVar};

apply(Subst, {ttuple, Elements}) ->
    {ttuple, [apply(Subst, Elem) || Elem <- Elements]};

apply(Subst, {tvariant, Constructors}) ->
    NewConstructors = [{Name, [apply(Subst, ArgType) || ArgType <- ArgTypes]}
                      || {Name, ArgTypes} <- Constructors],
    {tvariant, NewConstructors}.

-spec apply_effects(subst(), topos_types:effect_set()) -> topos_types:effect_set().
%% @doc Apply substitution to effect set
%% Effect sets don't contain type variables currently, so this is identity
%% But included for future effect polymorphism
apply_effects(_Subst, Effects) ->
    Effects.

%%====================================================================
%% Substitution Properties
%%====================================================================

-spec domain(subst()) -> [topos_types:type_var_id()].
%% @doc Get the domain (set of variables) of a substitution
domain(Subst) ->
    maps:keys(Subst).

-spec range(subst()) -> [topos_types:ty()].
%% @doc Get the range (set of types) of a substitution
range(Subst) ->
    maps:values(Subst).

%%====================================================================
%% Internal Functions
%%====================================================================

% (None yet)
