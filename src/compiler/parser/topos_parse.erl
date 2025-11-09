-module(topos_parse).
-export([parse/1, parse_file/1, format_error/1]).
-export([
    get_max_ast_depth/0, get_max_ast_nodes/0,
    get_max_token_count/0, get_max_parse_time/0,
    get_max_pattern_depth/0, get_max_type_depth/0
]).

%%% @doc High-level parser wrapper for the Topos language.
%%%
%%% This module provides a convenient high-level API that combines lexical
%%% analysis (tokenization) and syntactic analysis (parsing) into a single
%%% operation with comprehensive resource protection and error handling.
%%%
%%% == Key Features ==
%%%
%%% <ul>
%%%   <li><b>Unified API</b> - Parse from strings or files with a single call</li>
%%%   <li><b>Resource Limits</b> - Six categories of protection against malicious input:
%%%     <ul>
%%%       <li>Token count limit (500k default)</li>
%%%       <li>Parse time limit (30s default)</li>
%%%       <li>AST depth limit (500 levels)</li>
%%%       <li>AST node count limit (100k nodes)</li>
%%%       <li>Pattern depth limit (100 levels)</li>
%%%       <li>Type depth limit (100 levels)</li>
%%%     </ul>
%%%   </li>
%%%   <li><b>Error Formatting</b> - Human-readable error messages with location info</li>
%%%   <li><b>Configurable Limits</b> - All limits adjustable via application environment</li>
%%% </ul>
%%%
%%% == Usage Example ==
%%%
%%% ```
%%% %% Parse from string
%%% {ok, AST} = topos_parse:parse("shape Maybe a = Some a | None"),
%%%
%%% %% Parse from file
%%% {ok, AST} = topos_parse:parse_file("example.topos"),
%%%
%%% %% Handle errors
%%% case topos_parse:parse(BadInput) of
%%%     {ok, AST} -> process(AST);
%%%     {error, Reason} ->
%%%         Msg = topos_parse:format_error(Reason),
%%%         io:format("Error: ~s~n", [Msg])
%%% end.
%%%
%%% %% Configure limits (in application environment)
%%% application:set_env(topos, max_ast_depth, 1000),
%%% application:set_env(topos, max_parse_time, 60000).
%%% '''
%%%
%%% @author Topos Contributors
%%% @version 1.0.0
%%% @since 2025-11-08

%% Default resource limits to prevent DoS attacks
%% These can be overridden via application environment:
%%   application:set_env(topos, max_ast_depth, NewDepth)
-define(DEFAULT_MAX_AST_DEPTH, 500).        % 500 levels - prevents stack overflow
-define(DEFAULT_MAX_AST_NODES, 100000).     % 100k nodes - reasonable for large files
-define(DEFAULT_MAX_TOKEN_COUNT, 500000).   % 500k tokens - reasonable upper bound
-define(DEFAULT_MAX_PARSE_TIME, 30000).     % 30 seconds - prevent algorithmic attacks
-define(DEFAULT_MAX_PATTERN_DEPTH, 100).    % 100 levels - deeply nested patterns
-define(DEFAULT_MAX_TYPE_DEPTH, 100).       % 100 levels - deeply nested types

%% @doc Parse Topos source code into an Abstract Syntax Tree.
%%
%% Performs complete lexical and syntactic analysis in a single operation,
%% with comprehensive resource limit checking to prevent denial-of-service
%% attacks and resource exhaustion.
%%
%% The parsing process consists of 4 steps:
%% <ol>
%%%   <li>Tokenization using {@link topos_lexer}</li>
%%%   <li>Token count validation</li>
%%%   <li>Parsing with timeout protection using {@link topos_parser}</li>
%%%   <li>AST validation (depth, node count, pattern depth, type depth)</li>
%% </ol>
%%
%% All resource limits are configurable via application environment.
%%
%% @param String Topos source code as a string
%% @returns `{ok, AST}' on success, `{error, Reason}' on failure
%%
%% @see parse_file/1
%% @see format_error/1
%% @see topos_lexer:tokenize/1
%% @see topos_parser:parse/1
%%
%% @example
%% ```
%% %% Success case
%% {ok, AST} = parse("shape Maybe a = Some a | None").
%%%
%% %% Lexer error (invalid syntax)
%% {error, {lex_error, 1, "Illegal character: @"}} = parse("shape @Invalid").
%%%
%% %% Parser error (syntax error)
%% {error, {parse_error, 1, "unexpected token"}} = parse("shape =").
%%%
%% %% Resource limit exceeded
%% BigInput = string:copies("(", 10000),
%% {error, {ast_too_deep, _, _}} = parse(BigInput).
%% '''
-spec parse(string()) -> {ok, term()} | {error, term()}.
parse(String) ->
    StartTime = erlang:monotonic_time(millisecond),
    MaxParseTime = get_max_parse_time(),

    %% Step 1: Tokenize
    case topos_lexer:tokenize(String) of
        {ok, Tokens} ->
            %% Check token count limit
            TokenCount = length(Tokens),
            MaxTokens = get_max_token_count(),

            case TokenCount > MaxTokens of
                true ->
                    {error, {too_many_tokens, TokenCount, MaxTokens}};
                false ->
                    %% Step 2: Parse with timeout
                    case parse_with_timeout(Tokens, MaxParseTime, StartTime) of
                        {ok, AST} ->
                            %% Step 3: Validate AST resource limits
                            case validate_ast(AST) of
                                {ok, ValidatedAST} ->
                                    %% Step 4: Validate structural limits
                                    validate_structure(ValidatedAST);
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end
            end;
        {error, {Line, Module, Reason}} ->
            %% Standardize lexer errors
            {error, {lex_error, Line, format_lex_error(Module, Reason)}}
    end.

%% @doc Parse with timeout protection
-spec parse_with_timeout(list(), pos_integer(), integer()) -> {ok, term()} | {error, term()}.
parse_with_timeout(Tokens, MaxTime, StartTime) ->
    case topos_parser:parse(Tokens) of
        {ok, AST} ->
            ElapsedTime = erlang:monotonic_time(millisecond) - StartTime,
            case ElapsedTime > MaxTime of
                true ->
                    {error, {parse_timeout, ElapsedTime, MaxTime}};
                false ->
                    {ok, AST}
            end;
        {error, {Line, Module, Reason}} ->
            %% Check if we timed out
            ElapsedTime = erlang:monotonic_time(millisecond) - StartTime,
            case ElapsedTime > MaxTime of
                true ->
                    {error, {parse_timeout, ElapsedTime, MaxTime}};
                false ->
                    %% Normal parse error
                    {error, {parse_error, Line, format_parse_error(Module, Reason)}}
            end
    end.

%% @doc Parse a file containing Topos source code.
%%
%% Reads a file from disk and parses its contents using {@link parse/1}.
%% All resource limits and validation apply to the file contents.
%%
%% @param Filename Path to the file to parse (absolute or relative)
%% @returns `{ok, AST}' on success, `{error, Reason}' on failure
%%
%% @see parse/1
%% @see format_error/1
%%
%% @example
%% ```
%% %% Success case
%% {ok, AST} = parse_file("examples/maybe.topos").
%%%
%% %% File not found
%% {error, {file_error, "missing.topos", enoent}} =
%%     parse_file("missing.topos").
%%%
%% %% Parse error in file
%% {error, {parse_error, _, _}} = parse_file("invalid.topos").
%% '''
-spec parse_file(string()) -> {ok, term()} | {error, term()}.
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            parse(binary_to_list(Binary));
        {error, Reason} ->
            {error, {file_error, Filename, Reason}}
    end.

%% @doc Format error reason into human-readable string.
%%
%% Converts error tuples returned by {@link parse/1} and {@link parse_file/1}
%% into user-friendly error messages suitable for display.
%%
%% Handles all error types:
%% <ul>
%%%   <li>`{lex_error, Location, Reason}' - Lexical errors</li>
%%%   <li>`{parse_error, Location, Reason}' - Syntax errors</li>
%%%   <li>`{ast_too_deep, Depth, Max}' - AST depth exceeded</li>
%%%   <li>`{ast_too_large, Count, Max}' - AST node count exceeded</li>
%%%   <li>`{too_many_tokens, Count, Max}' - Token count exceeded</li>
%%%   <li>`{parse_timeout, Elapsed, Max}' - Parse timeout</li>
%%%   <li>`{pattern_too_deep, Depth, Max}' - Pattern nesting exceeded</li>
%%%   <li>`{type_too_deep, Depth, Max}' - Type nesting exceeded</li>
%%%   <li>`{file_error, Filename, Reason}' - File I/O errors</li>
%% </ul>
%%
%% @param Reason Error tuple from parse/1 or parse_file/1
%% @returns Formatted error message string
%%
%% @see parse/1
%% @see parse_file/1
%%
%% @example
%% ```
%% %% Format lexer error
%% "Lexical error at 5:10: Illegal character" =
%%     format_error({lex_error, {location, 5, 10}, "Illegal character"}).
%%%
%% %% Format parser error
%% "Parse error at line 3: unexpected token" =
%%     format_error({parse_error, 3, "unexpected token"}).
%%%
%% %% Format resource limit error
%% "AST too deep: 600 levels exceeds maximum of 500" =
%%     format_error({ast_too_deep, 600, 500}).
%% '''
-spec format_error(term()) -> string().
format_error({lex_error, Line, Reason}) when is_integer(Line) ->
    io_lib:format("Lexical error at line ~p: ~s", [Line, Reason]);
format_error({lex_error, Location, Reason}) ->
    LocStr = topos_location:format(Location),
    io_lib:format("Lexical error at ~s: ~s", [LocStr, Reason]);
format_error({parse_error, Line, Reason}) when is_integer(Line) ->
    io_lib:format("Parse error at line ~p: ~s", [Line, Reason]);
format_error({parse_error, Location, Reason}) ->
    LocStr = topos_location:format(Location),
    io_lib:format("Parse error at ~s: ~s", [LocStr, Reason]);
format_error({ast_too_deep, Depth, MaxDepth}) ->
    io_lib:format("AST too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({ast_too_large, NodeCount, MaxNodes}) ->
    io_lib:format("AST too large: ~p nodes exceeds maximum of ~p",
                  [NodeCount, MaxNodes]);
format_error({too_many_tokens, Count, MaxCount}) ->
    io_lib:format("Too many tokens: ~p exceeds maximum of ~p",
                  [Count, MaxCount]);
format_error({parse_timeout, ElapsedTime, MaxTime}) ->
    io_lib:format("Parse timeout: took ~p ms, maximum is ~p ms",
                  [ElapsedTime, MaxTime]);
format_error({pattern_too_deep, Depth, MaxDepth}) ->
    io_lib:format("Pattern nesting too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({type_too_deep, Depth, MaxDepth}) ->
    io_lib:format("Type expression too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({file_error, Filename, Reason}) ->
    io_lib:format("Error reading file ~s: ~p", [Filename, Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc Get maximum AST depth limit.
%%
%% Returns the maximum allowed nesting depth for Abstract Syntax Trees.
%% Deeply nested ASTs can cause stack overflow; this limit prevents DoS attacks.
%% Default: 500 levels. Configurable via `application:set_env(topos, max_ast_depth, N)'.
%%
%% @returns Maximum AST depth in levels
%% @see topos_compiler_utils:get_max_ast_depth/0
-spec get_max_ast_depth() -> pos_integer().
get_max_ast_depth() ->
    application:get_env(topos, max_ast_depth, ?DEFAULT_MAX_AST_DEPTH).

%% @doc Get maximum AST node count limit.
%%
%% Returns the maximum allowed number of nodes in Abstract Syntax Trees.
%% Prevents memory exhaustion from pathologically large source files.
%% Default: 100,000 nodes. Configurable via `application:set_env(topos, max_ast_nodes, N)'.
%%
%% @returns Maximum AST node count
%% @see topos_compiler_utils:get_max_ast_nodes/0
-spec get_max_ast_nodes() -> pos_integer().
get_max_ast_nodes() ->
    application:get_env(topos, max_ast_nodes, ?DEFAULT_MAX_AST_NODES).

%% @doc Get maximum token count limit.
%%
%% Returns the maximum allowed number of tokens in a single parse operation.
%% Prevents excessive memory usage from very large token streams.
%% Default: 500,000 tokens. Configurable via `application:set_env(topos, max_token_count, N)'.
%%
%% @returns Maximum token count
%% @see topos_compiler_utils:get_max_token_count/0
-spec get_max_token_count() -> pos_integer().
get_max_token_count() ->
    application:get_env(topos, max_token_count, ?DEFAULT_MAX_TOKEN_COUNT).

%% @doc Get maximum parse time limit.
%%
%% Returns the maximum time allowed for a single parse operation in milliseconds.
%% Prevents algorithmic complexity attacks and runaway parsing.
%% Default: 30,000 ms (30 seconds). Configurable via `application:set_env(topos, max_parse_time, N)'.
%%
%% @returns Maximum parse time in milliseconds
%% @see topos_compiler_utils:get_max_parse_time/0
-spec get_max_parse_time() -> pos_integer().
get_max_parse_time() ->
    application:get_env(topos, max_parse_time, ?DEFAULT_MAX_PARSE_TIME).

%% @doc Get maximum pattern nesting depth limit.
%%
%% Returns the maximum allowed nesting depth for pattern matching expressions.
%% Deeply nested patterns can cause stack overflow during pattern compilation.
%% Default: 100 levels. Configurable via `application:set_env(topos, max_pattern_depth, N)'.
%%
%% @returns Maximum pattern depth in levels
%% @see topos_compiler_utils:get_max_pattern_depth/0
-spec get_max_pattern_depth() -> pos_integer().
get_max_pattern_depth() ->
    application:get_env(topos, max_pattern_depth, ?DEFAULT_MAX_PATTERN_DEPTH).

%% @doc Get maximum type expression depth limit.
%%
%% Returns the maximum allowed nesting depth for type expressions.
%% Deeply nested types can cause issues during type checking.
%% Default: 100 levels. Configurable via `application:set_env(topos, max_type_depth, N)'.
%%
%% @returns Maximum type depth in levels
%% @see topos_compiler_utils:get_max_type_depth/0
-spec get_max_type_depth() -> pos_integer().
get_max_type_depth() ->
    application:get_env(topos, max_type_depth, ?DEFAULT_MAX_TYPE_DEPTH).

%%============================================================================
%% Internal Functions
%%============================================================================

%% @doc Validate AST doesn't exceed resource limits
-spec validate_ast(term()) -> {ok, term()} | {error, term()}.
validate_ast(AST) ->
    MaxDepth = get_max_ast_depth(),
    MaxNodes = get_max_ast_nodes(),

    %% Calculate AST metrics
    Depth = calculate_ast_depth(AST),
    NodeCount = count_ast_nodes(AST),

    %% Check limits
    if
        Depth > MaxDepth ->
            {error, {ast_too_deep, Depth, MaxDepth}};
        NodeCount > MaxNodes ->
            {error, {ast_too_large, NodeCount, MaxNodes}};
        true ->
            {ok, AST}
    end.

%% @doc Validate structural limits (pattern depth, type depth)
-spec validate_structure(term()) -> {ok, term()} | {error, term()}.
validate_structure(AST) ->
    MaxPatternDepth = get_max_pattern_depth(),
    MaxTypeDepth = get_max_type_depth(),

    %% Check pattern depth
    PatternDepth = calculate_max_pattern_depth(AST),
    if
        PatternDepth > MaxPatternDepth ->
            {error, {pattern_too_deep, PatternDepth, MaxPatternDepth}};
        true ->
            %% Check type depth
            TypeDepth = calculate_max_type_depth(AST),
            if
                TypeDepth > MaxTypeDepth ->
                    {error, {type_too_deep, TypeDepth, MaxTypeDepth}};
                true ->
                    {ok, AST}
            end
    end.

%% @doc Calculate the maximum depth of an AST
-spec calculate_ast_depth(term()) -> non_neg_integer().
calculate_ast_depth(AST) ->
    calculate_depth(AST, 0).

calculate_depth(Tuple, CurrentDepth) when is_tuple(Tuple) ->
    %% For each element in the tuple, calculate depth and take max
    Elements = tuple_to_list(Tuple),
    ChildDepths = [calculate_depth(E, CurrentDepth + 1) || E <- Elements],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_depth(List, CurrentDepth) when is_list(List) ->
    %% For each element in the list, calculate depth and take max
    ChildDepths = [calculate_depth(E, CurrentDepth + 1) || E <- List],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_depth(_Atom, CurrentDepth) ->
    %% Atoms, integers, floats are leaves
    CurrentDepth.

%% @doc Count total number of nodes in AST
-spec count_ast_nodes(term()) -> non_neg_integer().
count_ast_nodes(Tuple) when is_tuple(Tuple) ->
    %% Count this node plus all children
    Elements = tuple_to_list(Tuple),
    1 + lists:sum([count_ast_nodes(E) || E <- Elements]);
count_ast_nodes(List) when is_list(List) ->
    %% Lists aren't nodes themselves, just containers
    lists:sum([count_ast_nodes(E) || E <- List]);
count_ast_nodes(_Leaf) ->
    %% Atoms, integers, floats, etc. are single nodes
    1.

%% @doc Calculate maximum pattern nesting depth in AST
-spec calculate_max_pattern_depth(term()) -> non_neg_integer().
calculate_max_pattern_depth(AST) ->
    calculate_pattern_depth(AST, 0).

calculate_pattern_depth({pat_constructor, _Name, Args, _Loc}, CurrentDepth) ->
    %% Nested pattern in constructor
    NewDepth = CurrentDepth + 1,
    ArgDepths = [calculate_pattern_depth(Arg, NewDepth) || Arg <- Args],
    case ArgDepths of
        [] -> NewDepth;
        _ -> lists:max(ArgDepths)
    end;
calculate_pattern_depth({pat_tuple, Elements, _Loc}, CurrentDepth) ->
    %% Nested patterns in tuple
    NewDepth = CurrentDepth + 1,
    ElemDepths = [calculate_pattern_depth(E, NewDepth) || E <- Elements],
    case ElemDepths of
        [] -> NewDepth;
        _ -> lists:max(ElemDepths)
    end;
calculate_pattern_depth({pat_list, Elements, _Loc}, CurrentDepth) ->
    %% Nested patterns in list
    NewDepth = CurrentDepth + 1,
    ElemDepths = [calculate_pattern_depth(E, NewDepth) || E <- Elements],
    case ElemDepths of
        [] -> NewDepth;
        _ -> lists:max(ElemDepths)
    end;
calculate_pattern_depth({pat_record, Fields, _Loc}, CurrentDepth) ->
    %% Nested patterns in record fields
    NewDepth = CurrentDepth + 1,
    FieldDepths = [calculate_pattern_depth(P, NewDepth) || {_, P} <- Fields],
    case FieldDepths of
        [] -> NewDepth;
        _ -> lists:max(FieldDepths)
    end;
calculate_pattern_depth({pat_var, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_pattern_depth({pat_wildcard, _}, CurrentDepth) ->
    CurrentDepth;
calculate_pattern_depth({pat_literal, _, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_pattern_depth(Tuple, CurrentDepth) when is_tuple(Tuple) ->
    %% Recursively check all tuple elements for patterns
    Elements = tuple_to_list(Tuple),
    ChildDepths = [calculate_pattern_depth(E, CurrentDepth) || E <- Elements],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_pattern_depth(List, CurrentDepth) when is_list(List) ->
    %% Recursively check all list elements for patterns
    ChildDepths = [calculate_pattern_depth(E, CurrentDepth) || E <- List],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_pattern_depth(_Other, CurrentDepth) ->
    CurrentDepth.

%% @doc Calculate maximum type expression depth in AST
-spec calculate_max_type_depth(term()) -> non_neg_integer().
calculate_max_type_depth(AST) ->
    calculate_type_depth(AST, 0).

calculate_type_depth({type_fun, From, To, _Loc}, CurrentDepth) ->
    %% Nested type in function type
    NewDepth = CurrentDepth + 1,
    FromDepth = calculate_type_depth(From, NewDepth),
    ToDepth = calculate_type_depth(To, NewDepth),
    max(FromDepth, ToDepth);
calculate_type_depth({type_app, Con, Args, _Loc}, CurrentDepth) ->
    %% Nested types in type application
    NewDepth = CurrentDepth + 1,
    ConDepth = calculate_type_depth(Con, NewDepth),
    ArgDepths = [calculate_type_depth(Arg, NewDepth) || Arg <- Args],
    AllDepths = [ConDepth | ArgDepths],
    lists:max(AllDepths);
calculate_type_depth({type_forall, _Vars, Type, _Loc}, CurrentDepth) ->
    %% Nested type in forall
    NewDepth = CurrentDepth + 1,
    calculate_type_depth(Type, NewDepth);
calculate_type_depth({type_tuple, Elements, _Loc}, CurrentDepth) ->
    %% Nested types in tuple
    NewDepth = CurrentDepth + 1,
    ElemDepths = [calculate_type_depth(E, NewDepth) || E <- Elements],
    case ElemDepths of
        [] -> NewDepth;
        _ -> lists:max(ElemDepths)
    end;
calculate_type_depth({type_record, Fields, _Extension, _Loc}, CurrentDepth) ->
    %% Nested types in record fields
    NewDepth = CurrentDepth + 1,
    FieldDepths = [calculate_type_depth(T, NewDepth) || {_, T} <- Fields],
    case FieldDepths of
        [] -> NewDepth;
        _ -> lists:max(FieldDepths)
    end;
calculate_type_depth({type_var, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_type_depth({type_con, _, _}, CurrentDepth) ->
    CurrentDepth;
calculate_type_depth(Tuple, CurrentDepth) when is_tuple(Tuple) ->
    %% Recursively check all tuple elements for types
    Elements = tuple_to_list(Tuple),
    ChildDepths = [calculate_type_depth(E, CurrentDepth) || E <- Elements],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_type_depth(List, CurrentDepth) when is_list(List) ->
    %% Recursively check all list elements for types
    ChildDepths = [calculate_type_depth(E, CurrentDepth) || E <- List],
    case ChildDepths of
        [] -> CurrentDepth;
        _ -> lists:max(ChildDepths)
    end;
calculate_type_depth(_Other, CurrentDepth) ->
    CurrentDepth.

%% @doc Format lexer error messages
-spec format_lex_error(atom(), term()) -> string().
format_lex_error(topos_lexer, Reason) ->
    topos_lexer:format_error(Reason);
format_lex_error(_Module, Reason) when is_list(Reason) ->
    Reason;
format_lex_error(_Module, Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc Format parser error messages
-spec format_parse_error(atom(), term()) -> string().
format_parse_error(topos_parser, Reason) when is_list(Reason) ->
    Reason;
format_parse_error(topos_parser, {unexpected, Token}) ->
    io_lib:format("unexpected token: ~p", [Token]);
format_parse_error(topos_parser, {expected, Expected, Got}) ->
    io_lib:format("expected ~p but got ~p", [Expected, Got]);
format_parse_error(_Module, Reason) when is_list(Reason) ->
    Reason;
format_parse_error(_Module, Reason) ->
    io_lib:format("~p", [Reason]).
