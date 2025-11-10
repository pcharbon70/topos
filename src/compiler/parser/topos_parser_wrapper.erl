%% Topos Parser Wrapper with Error Reporting
%% Phase 1, Task 1.1.4: Error Recovery and Reporting
%%
%% This module wraps the yecc-generated parser and provides
%% enhanced error reporting with source context and suggestions.

-module(topos_parser_wrapper).

-include("../error/topos_error.hrl").

-export([
    parse_file/1,
    parse_tokens/1,
    parse_tokens_with_file/2
]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Parse a Topos source file
-spec parse_file(string()) -> {ok, term()} | {error, [#error{}]}.
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Content = unicode:characters_to_list(Binary),
            case topos_lexer:tokenize(Content) of
                {ok, Tokens} ->
                    parse_tokens_with_file(Tokens, Filename);
                {error, {Line, _Module, ErrorDesc}} ->
                    % Handle errors from any lexer module (topos_lexer, topos_lexer_gen, etc.)
                    Err = make_lexer_error(ErrorDesc, Line, Filename),
                    {error, [Err]}
            end;
        {error, Reason} ->
            Err = topos_error:new_error(
                'E000_file_error',
                io_lib:format("Could not read file: ~p", [Reason]),
                {1, undefined},
                Filename
            ),
            {error, [Err]}
    end.

%% @doc Parse tokens without file context
-spec parse_tokens([term()]) -> {ok, term()} | {error, [#error{}]}.
parse_tokens(Tokens) ->
    parse_tokens_with_file(Tokens, undefined).

%% @doc Parse tokens with file context for better error messages
-spec parse_tokens_with_file([term()], string() | undefined) -> {ok, term()} | {error, [#error{}]}.
parse_tokens_with_file(Tokens, File) ->
    case topos_parser:parse(Tokens) of
        {ok, AST} ->
            {ok, AST};
        {error, {Line, topos_parser, ErrorDesc}} ->
            Err = make_parser_error(ErrorDesc, Line, File),
            Err2 = add_source_context(Err, File),
            Err3 = add_parser_suggestion(Err2, ErrorDesc),
            {error, [Err3]}
    end.

%%====================================================================
%% Error Creation
%%====================================================================

%% @doc Create a lexer error
make_lexer_error(ErrorDesc, Line, File) ->
    Message = format_lexer_error(ErrorDesc),
    topos_error:new_error(
        'E100_lexer_error',
        Message,
        {Line, undefined},
        File
    ).

%% @doc Create a parser error
make_parser_error(ErrorDesc, Line, File) ->
    Message = format_parser_error(ErrorDesc),
    topos_error:new_error(
        'E200_syntax_error',
        Message,
        {Line, undefined},
        File
    ).

%%====================================================================
%% Error Formatting
%%====================================================================

%% @doc Format lexer error description
format_lexer_error({string, Quote, _Text}) ->
    io_lib:format("Unterminated string starting with ~s", [[Quote]]);
format_lexer_error({illegal, Text}) ->
    io_lib:format("Illegal character sequence: ~s", [Text]);
format_lexer_error(ErrorDesc) when is_list(ErrorDesc) ->
    ErrorDesc;
format_lexer_error(ErrorDesc) ->
    lists:flatten(io_lib:format("~p", [ErrorDesc])).

%% @doc Format parser error description
format_parser_error(["syntax error before: ", Token]) ->
    io_lib:format("Unexpected token: ~s", [Token]);
format_parser_error(ErrorDesc) when is_list(ErrorDesc) ->
    lists:flatten(ErrorDesc);
format_parser_error(ErrorDesc) ->
    lists:flatten(io_lib:format("~p", [ErrorDesc])).

%%====================================================================
%% Source Context
%%====================================================================

%% @doc Add source context to error if file is available
add_source_context(Err, undefined) ->
    Err;
add_source_context(#error{file = File, line = Line} = Err, File) when File =/= undefined ->
    case topos_error:read_source_context(File, Line, 2) of
        {ok, Context} ->
            Before = maps:get(before, Context),
            ErrorLine = maps:get(error_line, Context),
            After = maps:get('after', Context),
            topos_error:add_context(Err, Before, ErrorLine, After);
        {error, _} ->
            Err
    end;
add_source_context(Err, _) ->
    Err.

%%====================================================================
%% Suggestions
%%====================================================================

%% @doc Add helpful suggestion based on error pattern
add_parser_suggestion(Err, ErrorDesc) ->
    Suggestion = detect_suggestion(ErrorDesc),
    case Suggestion of
        undefined -> Err;
        _ -> topos_error:add_suggestion(Err, Suggestion)
    end.

%% @doc Detect suggestion from error description
detect_suggestion(["syntax error before: ", "'end'"]) ->
    "This 'end' keyword appears unexpected. Check if there's an extra 'end' or a missing opening keyword.";
detect_suggestion(["syntax error before: ", Token]) ->
    case Token of
        "'}'" -> "Check for missing opening '{' or extra '}'";
        "']'" -> "Check for missing opening '[' or extra ']'";
        "')'" -> "Check for missing opening '(' or extra ')'";
        "'|'" -> "In shape declarations, use '|' to separate constructors";
        _ ->
            % Check if it looks like end of file
            case string:find(Token, "end of input") of
                nomatch -> undefined;
                _ -> "File ended unexpectedly. Check for missing 'end' keywords or unclosed delimiters."
            end
    end;
detect_suggestion(_) ->
    undefined.
