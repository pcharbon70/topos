%%%-------------------------------------------------------------------
%%% @doc Pattern Type Inference
%%%
%%% This module implements type inference for patterns in pattern matching.
%%% Patterns introduce new bindings in the environment and generate constraints.
%%%
%%% Pattern inference returns:
%%% - The inferred type of the pattern
%%% - Bindings introduced by the pattern (variable names → types)
%%% - Updated inference state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_infer_pattern).

-export([
    infer/3
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Infer the type of a pattern
%% Returns the pattern's type and bindings it introduces
-spec infer(topos_ast:pattern(), topos_type_env:env(), topos_infer_state:infer_state()) ->
    {topos_types:type(), topos_type_env:env(), topos_infer_state:infer_state()}.

% Pattern: Wildcard
% _ : α  (fresh type variable, no bindings)
infer({pwild}, _Env, State) ->
    {Type, State1} = topos_infer_state:fresh_var(State),
    EmptyBindings = topos_type_env:empty(),
    {Type, EmptyBindings, State1};

% Pattern: Literal
% 42 : Int
% true : Bool
% "hello" : String
infer({plit, Lit}, _Env, State) ->
    Type = literal_type(Lit),
    EmptyBindings = topos_type_env:empty(),
    {Type, EmptyBindings, State};

% Pattern: Variable
% x : α  (fresh type variable, binds x : α)
infer({pvar, Name}, _Env, State) ->
    {Type, State1} = topos_infer_state:fresh_var(State),

    % Create binding for variable
    Scheme = topos_type_scheme:mono(Type),
    Bindings = topos_type_env:extend(topos_type_env:empty(), Name, Scheme),

    {Type, Bindings, State1};

% Pattern: Tuple
% (p1, ..., pn) : (T1, ..., Tn)
% where pi : Ti
infer({ptuple, PatternList}, Env, State) ->
    {Types, Bindings, State1} = infer_patterns(PatternList, Env, State),
    Type = {ttuple, Types},
    {Type, Bindings, State1};

% Pattern: Record
% {field1: p1, ..., fieldn: pn} : {field1: T1, ..., fieldn: Tn | closed}
% where pi : Ti
infer({precord, Fields}, Env, State) ->
    {FieldTypes, Bindings, State1} = infer_record_fields(Fields, Env, State),
    Type = {trecord, FieldTypes, closed},
    {Type, Bindings, State1};

% Pattern: Variant Constructor
% C p1 ... pn : [... | C T1 ... Tn | ...]
% where pi : Ti
% For PoC, we create a fresh variant type with this constructor
infer({pvariant, Constructor, ArgPatterns}, Env, State) ->
    {ArgTypes, Bindings, State1} = infer_patterns(ArgPatterns, Env, State),

    % Create variant type with this constructor
    Type = {tvariant, [{Constructor, ArgTypes}]},

    {Type, Bindings, State1};

% Pattern: As-pattern
% (p as x) : T where p : T
% Binds both the pattern's bindings and x : T
infer({pas, Name, Pattern}, Env, State) ->
    {Type, PatBindings, State1} = infer(Pattern, Env, State),

    % Add binding for the name
    Scheme = topos_type_scheme:mono(Type),
    NameBinding = topos_type_env:extend(topos_type_env:empty(), Name, Scheme),

    % Merge bindings
    CombinedBindings = merge_bindings(NameBinding, PatBindings),

    {Type, CombinedBindings, State1}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Infer types for multiple patterns
%% Collects all bindings from all patterns
-spec infer_patterns([topos_ast:pattern()], topos_type_env:env(), topos_infer_state:infer_state()) ->
    {[topos_types:type()], topos_type_env:env(), topos_infer_state:infer_state()}.
infer_patterns(Patterns, Env, State) ->
    infer_patterns_acc(Patterns, Env, State, [], topos_type_env:empty()).

infer_patterns_acc([], _Env, State, TypesAcc, BindingsAcc) ->
    {lists:reverse(TypesAcc), BindingsAcc, State};
infer_patterns_acc([P | Rest], Env, State, TypesAcc, BindingsAcc) ->
    {Type, Bindings, State1} = infer(P, Env, State),

    % Merge bindings (checking for duplicates)
    CombinedBindings = merge_bindings(BindingsAcc, Bindings),

    infer_patterns_acc(Rest, Env, State1, [Type | TypesAcc], CombinedBindings).

%% @doc Infer types for record pattern fields
-spec infer_record_fields([{atom(), topos_ast:pattern()}], topos_type_env:env(),
                         topos_infer_state:infer_state()) ->
    {[{atom(), topos_types:type()}], topos_type_env:env(), topos_infer_state:infer_state()}.
infer_record_fields(Fields, Env, State) ->
    infer_record_fields_acc(Fields, Env, State, [], topos_type_env:empty()).

infer_record_fields_acc([], _Env, State, FieldsAcc, BindingsAcc) ->
    {lists:reverse(FieldsAcc), BindingsAcc, State};
infer_record_fields_acc([{Label, Pattern} | Rest], Env, State, FieldsAcc, BindingsAcc) ->
    {Type, Bindings, State1} = infer(Pattern, Env, State),

    % Merge bindings
    CombinedBindings = merge_bindings(BindingsAcc, Bindings),

    infer_record_fields_acc(Rest, Env, State1, [{Label, Type} | FieldsAcc], CombinedBindings).

%% @doc Merge two binding environments, detecting duplicate bindings
%% In patterns, duplicate variable names are not allowed
-spec merge_bindings(topos_type_env:env(), topos_type_env:env()) -> topos_type_env:env().
merge_bindings(Env1, Env2) ->
    % For PoC, simple merge (assuming no duplicates)
    % In full implementation, would check for duplicate variable names
    topos_type_env:merge(Env1, Env2).

%% @doc Get the type of a literal value
-spec literal_type(topos_ast:literal()) -> topos_types:type().
literal_type({int, _}) ->
    {tcon, int};
literal_type({float, _}) ->
    {tcon, float};
literal_type({bool, _}) ->
    {tcon, bool};
literal_type({string, _}) ->
    {tcon, string};
literal_type({atom, _}) ->
    {tcon, atom};
literal_type({unit}) ->
    {tcon, unit}.
