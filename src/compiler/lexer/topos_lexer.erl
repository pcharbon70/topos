-module(topos_lexer).
-export([tokenize/1, tokenize_file/1, format_error/1]).
-export([get_max_input_size/0, get_max_nesting_depth/0, get_max_identifier_length/0]).

%%% @doc Lexical analyzer (tokenizer) for the Topos language.
%%%
%%% This module provides lexical analysis functionality that converts Topos
%%% source code into a stream of tokens for consumption by the parser. It includes
%%% comprehensive resource protection and handles nested comments correctly.
%%%
%%% == Key Features ==
%%%
%%% <ul>
%%%   <li><b>Resource Limits</b> - Three categories of protection:
%%%     <ul>
%%%       <li>Input size limit (10MB default)</li>
%%%       <li>Comment nesting depth limit (100 levels)</li>
%%%       <li>Identifier length limit (255 characters)</li>
%%%     </ul>
%%%   </li>
%%%   <li><b>Nested Comments</b> - Proper handling of `{- ... -}' with arbitrary nesting</li>
%%%   <li><b>Error Formatting</b> - Human-readable error messages with line numbers</li>
%%%   <li><b>File and String API</b> - Tokenize from strings or files directly</li>
%%% </ul>
%%%
%%% == Usage Example ==
%%%
%%% ```
%%% %% Tokenize from string
%%% {ok, Tokens} = topos_lexer:tokenize("shape Maybe a = Some a | None"),
%%%
%%% %% Tokenize from file
%%% {ok, Tokens} = topos_lexer:tokenize_file("example.topos"),
%%%
%%% %% Handle errors
%%% case topos_lexer:tokenize(BadInput) of
%%%     {ok, Tokens} -> process(Tokens);
%%%     {error, {Line, topos_lexer, Reason}} ->
%%%         Msg = topos_lexer:format_error(Reason),
%%%         io:format("Lexical error at line ~p: ~s~n", [Line, Msg])
%%% end.
%%% '''
%%%
%%% == Token Format ==
%%%
%%% Tokens are tuples with the format:
%%% <ul>
%%%   <li>`{TokenType, Line}' - Simple tokens like keywords and operators</li>
%%%   <li>`{TokenType, Line, Value}' - Tokens with values like identifiers and literals</li>
%%% </ul>
%%%
%%% @author Topos Contributors
%%% @version 1.0.0
%%% @since 2025-11-08

%% Default resource limits to prevent DoS attacks
%% These can be overridden via application environment:
%%   application:set_env(topos, max_input_size, NewSize)
-define(DEFAULT_MAX_INPUT_SIZE, 10000000).     % 10 MB - reasonable for most source files
-define(DEFAULT_MAX_NESTING_DEPTH, 100).       % 100 levels - prevents stack overflow
-define(DEFAULT_MAX_IDENT_LENGTH, 255).        % 255 chars - standard for many languages

%% @doc Tokenize Topos source code into a token stream.
%%
%% Performs lexical analysis on the input string, converting it into a sequence
%% of tokens suitable for parsing. Includes comprehensive resource limit checking
%% and proper handling of nested comments.
%%
%% Steps:
%% <ol>
%%%   <li>Check input size limit</li>
%%%   <li>Run Leex-generated lexer ({@link topos_lexer_gen})</li>
%%%   <li>Filter and validate nested comments</li>
%%%   <li>Return token stream or error</li>
%% </ol>
%%
%% @param String Topos source code as a string
%% @returns `{ok, [Token]}' on success, `{error, {Line, Module, Reason}}' on failure
%%
%% @see tokenize_file/1
%% @see format_error/1
%% @see topos_parse:parse/1
%%
%% @example
%% ```
%% %% Success case
%% {ok, Tokens} = tokenize("shape Maybe a = Some a | None").
%%%
%% %% Input too large
%% BigInput = lists:duplicate(20000000, $x),
%% {error, {0, topos_lexer, {input_too_large, _, _}}} = tokenize(BigInput).
%%%
%% %% Unclosed comment
%% {error, {0, topos_lexer, {unclosed_comment, 1}}} = tokenize("{- comment").
%% '''
-spec tokenize(string()) -> {ok, [tuple()]} | {error, term()}.
tokenize(String) ->
    InputSize = length(String),
    MaxSize = get_max_input_size(),
    case InputSize > MaxSize of
        true ->
            {error, {0, topos_lexer, {input_too_large, InputSize, MaxSize}}};
        false ->
            tokenize_internal(String)
    end.

tokenize_internal(String) ->
    case topos_lexer_gen:string(String) of
        {ok, RawTokens, _} ->
            %% Post-process to handle nested comments
            case filter_comments(RawTokens) of
                {ok, Tokens} ->
                    {ok, Tokens};
                {error, {Line, topos_lexer, Reason}} ->
                    %% Already standardized from filter_comments
                    {error, {Line, topos_lexer, Reason}}
            end;
        {error, {Line, _Module, {user, {identifier_too_long, _ErrorLine, ActualLen, MaxLen}}}, _} ->
            %% Custom error from validate_identifier
            {error, {Line, topos_lexer, {identifier_too_long, ActualLen, MaxLen}}};
        {error, {Line, Module, Error}, _} ->
            %% Standard leex error
            {error, {Line, Module, Error}}
    end.

%% @doc Tokenize a file containing Topos source code.
%%
%% Reads a file from disk and tokenizes its contents using {@link tokenize/1}.
%% All resource limits and validation apply to the file contents.
%%
%% @param Filename Path to the file to tokenize (absolute or relative)
%% @returns `{ok, [Token]}' on success, `{error, {Line, Module, Reason}}' on failure
%%
%% @see tokenize/1
%% @see format_error/1
%%
%% @example
%% ```
%% %% Success case
%% {ok, Tokens} = tokenize_file("examples/maybe.topos").
%%%
%% %% File not found
%% {error, {0, topos_lexer, {file_error, enoent}}} =
%%     tokenize_file("missing.topos").
%% '''
-spec tokenize_file(string()) -> {ok, [tuple()]} | {error, term()}.
tokenize_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            tokenize(binary_to_list(Binary));
        {error, Reason} ->
            {error, {0, topos_lexer, {file_error, Reason}}}
    end.

%% @doc Format lexer error into human-readable string.
%%
%% Converts error reasons returned by {@link tokenize/1} and {@link tokenize_file/1}
%% into user-friendly error messages suitable for display.
%%
%% Handles all error types:
%% <ul>
%%%   <li>`{input_too_large, Actual, Max}' - Input size exceeded</li>
%%%   <li>`{unclosed_comment, Depth}' - Comment not closed</li>
%%%   <li>`{nesting_too_deep, Depth, Max}' - Comment nesting exceeded</li>
%%%   <li>`{identifier_too_long, Len, Max}' - Identifier length exceeded</li>
%%%   <li>`{file_error, Reason}' - File I/O errors</li>
%%%   <li>`unmatched_comment_end' - Comment end without start</li>
%% </ul>
%%
%% @param Reason Error reason from tokenize/1 or tokenize_file/1
%% @returns Formatted error message string
%%
%% @see tokenize/1
%% @see tokenize_file/1
%%
%% @example
%% ```
%% %% Format size error
%% "input too large: 20000000 bytes exceeds maximum of 10000000 bytes" =
%%     format_error({input_too_large, 20000000, 10000000}).
%%%
%% %% Format nesting error
%% "comment nesting too deep: 101 levels exceeds maximum of 100" =
%%     format_error({nesting_too_deep, 101, 100}).
%% '''
-spec format_error(term()) -> string().
format_error({input_too_large, ActualSize, MaxSize}) ->
    io_lib:format("input too large: ~p bytes exceeds maximum of ~p bytes",
                  [ActualSize, MaxSize]);
format_error({unclosed_comment, Depth}) ->
    io_lib:format("unclosed comment at depth ~p", [Depth]);
format_error({nesting_too_deep, Depth, MaxDepth}) ->
    io_lib:format("comment nesting too deep: ~p levels exceeds maximum of ~p",
                  [Depth, MaxDepth]);
format_error({identifier_too_long, ActualLen, MaxLen}) ->
    io_lib:format("identifier too long: ~p characters exceeds maximum of ~p",
                  [ActualLen, MaxLen]);
format_error({file_error, Reason}) ->
    io_lib:format("file error: ~p", [Reason]);
format_error(unmatched_comment_end) ->
    "unmatched comment end marker -}";
format_error(Reason) when is_list(Reason) ->
    Reason;
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc Get maximum input size limit.
%%
%% Returns the maximum allowed size (in bytes) for source code input.
%% Large enough for substantial source files, small enough to prevent memory exhaustion.
%% Default: 10,000,000 bytes (10 MB). Configurable via `application:set_env(topos, max_input_size, N)'.
%%
%% @returns Maximum input size in bytes
%% @see topos_compiler_utils:get_max_input_size/0
-spec get_max_input_size() -> pos_integer().
get_max_input_size() ->
    application:get_env(topos, max_input_size, ?DEFAULT_MAX_INPUT_SIZE).

%% @doc Get maximum comment nesting depth limit.
%%
%% Returns the maximum allowed nesting depth for comments `{- ... -}'.
%% Deeply nested comments are rare in practice; limit prevents stack overflow attacks.
%% Default: 100 levels. Configurable via `application:set_env(topos, max_nesting_depth, N)'.
%%
%% @returns Maximum nesting depth in levels
%% @see topos_compiler_utils:get_max_nesting_depth/0
-spec get_max_nesting_depth() -> pos_integer().
get_max_nesting_depth() ->
    application:get_env(topos, max_nesting_depth, ?DEFAULT_MAX_NESTING_DEPTH).

%% @doc Get maximum identifier length limit.
%%
%% Returns the maximum allowed length for identifiers (variable names, function names, etc.).
%% Matches common language limits (Java, C#); prevents resource exhaustion.
%% Default: 255 characters. Configurable via `application:set_env(topos, max_identifier_length, N)'.
%%
%% @returns Maximum identifier length in characters
%% @see topos_compiler_utils:get_max_identifier_length/0
-spec get_max_identifier_length() -> pos_integer().
get_max_identifier_length() ->
    application:get_env(topos, max_identifier_length, ?DEFAULT_MAX_IDENT_LENGTH).

%% @doc Filter out nested comments from token stream
%% Handles {- ... -} with arbitrary nesting depth
-spec filter_comments([tuple()]) -> {ok, [tuple()]} | {error, term()}.
filter_comments(Tokens) ->
    MaxDepth = get_max_nesting_depth(),
    filter_comments(Tokens, [], 0, MaxDepth).

filter_comments([], Acc, 0, _MaxDepth) ->
    {ok, lists:reverse(Acc)};
filter_comments([], _Acc, Depth, _MaxDepth) when Depth > 0 ->
    {error, {0, topos_lexer, {unclosed_comment, Depth}}};
filter_comments([{comment_start, Line} | _Rest], _Acc, Depth, MaxDepth) when Depth >= MaxDepth ->
    %% Nesting depth limit exceeded
    {error, {Line, topos_lexer, {nesting_too_deep, Depth, MaxDepth}}};
filter_comments([{comment_start, _Line} | Rest], Acc, Depth, MaxDepth) ->
    %% Entering a comment block
    filter_comments(Rest, Acc, Depth + 1, MaxDepth);
filter_comments([{comment_end, Line} | _Rest], _Acc, 0, _MaxDepth) ->
    %% Comment end without matching start
    {error, {Line, topos_lexer, unmatched_comment_end}};
filter_comments([{comment_end, _Line} | Rest], Acc, Depth, MaxDepth) when Depth > 0 ->
    %% Exiting a comment block
    filter_comments(Rest, Acc, Depth - 1, MaxDepth);
filter_comments([Token | Rest], Acc, 0, MaxDepth) ->
    %% Not inside a comment - keep the token
    filter_comments(Rest, [Token | Acc], 0, MaxDepth);
filter_comments([_Token | Rest], Acc, Depth, MaxDepth) when Depth > 0 ->
    %% Inside a comment - discard the token
    filter_comments(Rest, Acc, Depth, MaxDepth).
