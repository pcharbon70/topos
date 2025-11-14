%%%-------------------------------------------------------------------
%%% @doc Type Error Formatting
%%%
%%% Provides standardized error types and user-friendly error messages
%%% for type system operations. Integrates with the compiler's error
%%% reporting infrastructure.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_error).

-export([
    format_error/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type errors that can occur during type checking and inference
-type type_error() ::
    % Substitution errors
    {circular_substitution, topos_types:type_var_id()} |
    {substitution_depth_exceeded, non_neg_integer(), non_neg_integer()} |

    % Construction errors
    {duplicate_record_fields, [atom()]} |
    {duplicate_variant_constructors, [atom()]} |

    % Unification errors (for future use)
    {unification_failure, topos_types:ty(), topos_types:ty()} |
    {occurs_check_failure, topos_types:type_var_id(), topos_types:ty()} |

    % Type depth errors
    {type_depth_exceeded, non_neg_integer(), non_neg_integer()} |

    % Environment errors
    {unbound_variable, atom()} |
    {environment_too_large, non_neg_integer(), non_neg_integer()}.

-export_type([type_error/0]).

%%====================================================================
%% Error Formatting
%%====================================================================

-spec format_error(type_error()) -> string().
%% @doc Format a type error into a human-readable error message

%% Substitution errors
format_error({circular_substitution, VarId}) ->
    io_lib:format(
        "Circular type substitution detected: type variable α~p occurs in its own definition~n"
        "This would create an infinite type. Check your type definitions for cycles.",
        [VarId]
    );

format_error({substitution_depth_exceeded, Depth, Max}) ->
    io_lib:format(
        "Type substitution depth limit exceeded: ~p levels (maximum: ~p)~n"
        "This may indicate a very complex type or a circular dependency.~n"
        "Consider simplifying your type definitions.",
        [Depth, Max]
    );

%% Construction errors
format_error({duplicate_record_fields, Fields}) ->
    FieldList = string:join([atom_to_list(F) || F <- Fields], ", "),
    io_lib:format(
        "Duplicate field names in record type: ~s~n"
        "Each field name must be unique within a record.",
        [FieldList]
    );

format_error({duplicate_variant_constructors, Constructors}) ->
    ConstructorList = string:join([atom_to_list(C) || C <- Constructors], ", "),
    io_lib:format(
        "Duplicate constructor names in variant type: ~s~n"
        "Each constructor name must be unique within a variant.",
        [ConstructorList]
    );

%% Unification errors
format_error({unification_failure, Type1, Type2}) ->
    Type1Str = topos_type_pp:pp_type(Type1),
    Type2Str = topos_type_pp:pp_type(Type2),
    io_lib:format(
        "Type unification failed:~n"
        "  Expected: ~s~n"
        "  Got:      ~s~n"
        "These types are incompatible.",
        [Type1Str, Type2Str]
    );

format_error({occurs_check_failure, VarId, Type}) ->
    TypeStr = topos_type_pp:pp_type(Type),
    io_lib:format(
        "Occurs check failed: type variable α~p occurs in type ~s~n"
        "This would create an infinite type. Cannot unify a variable with a type containing itself.",
        [VarId, TypeStr]
    );

%% Type depth errors
format_error({type_depth_exceeded, Depth, Max}) ->
    io_lib:format(
        "Type nesting depth exceeded: ~p levels (maximum: ~p)~n"
        "Your type is too deeply nested. Consider breaking it into smaller, named types.",
        [Depth, Max]
    );

%% Environment errors
format_error({unbound_variable, VarName}) ->
    io_lib:format(
        "Unbound variable: ~s~n"
        "This variable is not defined in the current scope.",
        [atom_to_list(VarName)]
    );

format_error({environment_too_large, Size, Max}) ->
    io_lib:format(
        "Type environment too large: ~p bindings (maximum: ~p)~n"
        "Consider reducing the number of variables in scope or splitting into smaller functions.",
        [Size, Max]
    );

%% Catch-all for unknown errors
format_error(Error) ->
    io_lib:format("Unknown type error: ~p", [Error]).

%%====================================================================
%% Helper Functions
%%====================================================================

% (None yet)
