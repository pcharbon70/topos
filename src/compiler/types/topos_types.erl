%%%-------------------------------------------------------------------
%%% @doc Topos Type Representation
%%%
%%% Internal type representation for the Topos type inference engine.
%%% This module provides the core type constructors, fresh variable
%%% generation, and effect set operations.
%%%
%%% This is separate from AST type expressions to optimize for
%%% Algorithm W operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_types).

%% Type construction
-export([
    tvar/1,
    tcon/1,
    tapp/2,
    tfun/3,
    trecord/2,
    ttuple/1,
    tvariant/1
]).

%% Fresh variable generation (stateless - explicit state threading)
-export([
    fresh_var/1,
    fresh_var_id/1
]).

%% Effect set operations
-export([
    empty_effects/0,
    singleton_effect/1,
    union_effects/2,
    normalize_effects/1,
    is_pure/1,
    effects_equal/2
]).

%% Type operations
-export([
    is_function_type/1,
    is_type_var/1,
    extract_function_effects/1,
    type_vars/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type type_var_id() :: pos_integer().

-type base_type() :: integer | float | string | atom | boolean | unit.

-type ty() :: {tvar, type_var_id()}                     % Type variable
            | {tcon, atom()}                            % Type constructor
            | {tapp, ty(), [ty()]}                      % Type application
            | {tfun, ty(), ty(), effect_set()}          % Function with effects
            | {trecord, [{atom(), ty()}], row_var()}    % Record type
            | {ttuple, [ty()]}                          % Tuple type
            | {tvariant, [{atom(), [ty()]}]}.           % Variant type

-type effect_set() :: {effect_set, [atom()]}.  % Normalized: sorted, no dups
-type row_var() :: type_var_id() | closed.     % Row variable or closed

-export_type([ty/0, type_var_id/0, effect_set/0, row_var/0, base_type/0]).

%%====================================================================
%% Type Construction Functions
%%====================================================================

-spec tvar(type_var_id()) -> ty().
tvar(Id) when is_integer(Id), Id > 0 ->
    {tvar, Id}.

-spec tcon(atom()) -> ty().
tcon(Name) when is_atom(Name) ->
    {tcon, Name}.

-spec tapp(ty(), [ty()]) -> ty().
tapp(Constructor, Args) when is_list(Args) ->
    {tapp, Constructor, Args}.

-spec tfun(ty(), ty(), effect_set()) -> ty().
tfun(From, To, Effects) ->
    {tfun, From, To, Effects}.

-spec trecord([{atom(), ty()}], row_var()) -> ty().
trecord(Fields, RowVar) when is_list(Fields) ->
    % Validate no duplicate field names
    FieldNames = [Name || {Name, _Type} <- Fields],
    case length(FieldNames) =:= length(lists:usort(FieldNames)) of
        true -> {trecord, Fields, RowVar};
        false ->
            Duplicates = find_duplicates(FieldNames),
            error({duplicate_record_fields, Duplicates})
    end.

-spec ttuple([ty()]) -> ty().
ttuple(Elements) when is_list(Elements) ->
    {ttuple, Elements}.

-spec tvariant([{atom(), [ty()]}]) -> ty().
tvariant(Constructors) when is_list(Constructors) ->
    % Validate no duplicate constructor names
    ConstructorNames = [Name || {Name, _Args} <- Constructors],
    case length(ConstructorNames) =:= length(lists:usort(ConstructorNames)) of
        true -> {tvariant, Constructors};
        false ->
            Duplicates = find_duplicates(ConstructorNames),
            error({duplicate_variant_constructors, Duplicates})
    end.

%%====================================================================
%% Fresh Variable Generation
%%====================================================================

%% These functions explicitly thread state through computations,
%% making them functional, testable, and thread-safe.
%% State is managed by topos_type_state module.

%% @doc Generate fresh type variable with explicit state
%% Returns {TypeVariable, NewState}
%%
%% This function threads state through computations, making it functional,
%% testable, and thread-safe. State must be explicitly passed and returned.
%%
%% Example:
%%   State0 = topos_type_state:new(),
%%   {Var1, State1} = topos_types:fresh_var(State0),
%%   {Var2, State2} = topos_types:fresh_var(State1)
-spec fresh_var(topos_type_state:state()) -> {ty(), topos_type_state:state()}.
fresh_var(State) ->
    topos_type_state:fresh_var(State).

%% @doc Generate fresh type variable ID with explicit state
%% Returns {VarId, NewState}
%%
%% Example:
%%   State0 = topos_type_state:new(),
%%   {Id1, State1} = topos_types:fresh_var_id(State0),
%%   {Id2, State2} = topos_types:fresh_var_id(State1)
-spec fresh_var_id(topos_type_state:state()) -> {type_var_id(), topos_type_state:state()}.
fresh_var_id(State) ->
    topos_type_state:fresh_var_id(State).

%%====================================================================
%% Effect Set Operations
%%====================================================================

-spec empty_effects() -> effect_set().
empty_effects() ->
    {effect_set, []}.

-spec singleton_effect(atom()) -> effect_set().
singleton_effect(Effect) when is_atom(Effect) ->
    {effect_set, [Effect]}.

-spec union_effects(effect_set(), effect_set()) -> effect_set().
union_effects({effect_set, E1}, {effect_set, E2}) ->
    % Merge and normalize (sort + deduplicate)
    normalize_effects(E1 ++ E2).

-spec normalize_effects([atom()]) -> effect_set().
normalize_effects(Effects) when is_list(Effects) ->
    % Sort and remove duplicates in one pass
    % lists:usort already sorts, so no need for separate lists:sort
    {effect_set, lists:usort(Effects)}.

-spec is_pure(effect_set()) -> boolean().
is_pure({effect_set, []}) -> true;
is_pure({effect_set, _}) -> false.

-spec effects_equal(effect_set(), effect_set()) -> boolean().
effects_equal({effect_set, E1}, {effect_set, E2}) ->
    E1 =:= E2.  % Already normalized, so direct comparison

%%====================================================================
%% Type Operations
%%====================================================================

-spec is_function_type(ty()) -> boolean().
is_function_type({tfun, _, _, _}) -> true;
is_function_type(_) -> false.

-spec is_type_var(ty()) -> boolean().
is_type_var({tvar, _}) -> true;
is_type_var(_) -> false.

-spec extract_function_effects(ty()) -> {ok, effect_set()} | error.
extract_function_effects({tfun, _, _, Effects}) ->
    {ok, Effects};
extract_function_effects(_) ->
    error.

-spec type_vars(ty()) -> sets:set(type_var_id()).
type_vars(Type) ->
    type_vars_acc(Type, sets:new()).

-spec type_vars_acc(ty(), sets:set(type_var_id())) -> sets:set(type_var_id()).
type_vars_acc({tvar, Id}, Acc) ->
    sets:add_element(Id, Acc);
type_vars_acc({tcon, _}, Acc) ->
    Acc;
type_vars_acc({tapp, Con, Args}, Acc) ->
    Acc1 = type_vars_acc(Con, Acc),
    lists:foldl(fun type_vars_acc/2, Acc1, Args);
type_vars_acc({tfun, From, To, _Effects}, Acc) ->
    Acc1 = type_vars_acc(From, Acc),
    type_vars_acc(To, Acc1);
type_vars_acc({trecord, Fields, RowVar}, Acc) ->
    Acc1 = lists:foldl(
        fun({_Name, FieldType}, A) -> type_vars_acc(FieldType, A) end,
        Acc,
        Fields
    ),
    case RowVar of
        closed -> Acc1;
        VarId when is_integer(VarId) -> sets:add_element(VarId, Acc1)
    end;
type_vars_acc({ttuple, Elements}, Acc) ->
    lists:foldl(fun type_vars_acc/2, Acc, Elements);
type_vars_acc({tvariant, Constructors}, Acc) ->
    lists:foldl(
        fun({_Name, ArgTypes}, A) ->
            lists:foldl(fun type_vars_acc/2, A, ArgTypes)
        end,
        Acc,
        Constructors
    ).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Find duplicate elements in a list
%% Returns a list of elements that appear more than once
-spec find_duplicates([atom()]) -> [atom()].
find_duplicates(List) ->
    find_duplicates(List, #{}, []).

find_duplicates([], _Seen, Duplicates) ->
    lists:usort(Duplicates);
find_duplicates([Item | Rest], Seen, Duplicates) ->
    case maps:is_key(Item, Seen) of
        true ->
            % Already seen, it's a duplicate
            find_duplicates(Rest, Seen, [Item | Duplicates]);
        false ->
            % First occurrence
            find_duplicates(Rest, maps:put(Item, true, Seen), Duplicates)
    end.
