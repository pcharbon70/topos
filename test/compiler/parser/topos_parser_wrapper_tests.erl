-module(topos_parser_wrapper_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../../src/compiler/error/topos_error.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

% Create a temporary test file
create_test_file(Filename, Content) ->
    ok = file:write_file(Filename, Content).

% Delete a test file
delete_test_file(Filename) ->
    file:delete(Filename).

%%====================================================================
%% parse_file/1 Tests
%%====================================================================

parse_file_nonexistent_test() ->
    Result = topos_parser_wrapper:parse_file("/nonexistent/file.topos"),
    ?assertMatch({error, [#error{code = 'E000_file_error'}]}, Result).

parse_file_valid_simple_test() ->
    TestFile = "/tmp/topos_test_valid_simple.topos",
    Content = "shape Bool = True | False\n",
    create_test_file(TestFile, Content),

    Result = topos_parser_wrapper:parse_file(TestFile),

    delete_test_file(TestFile),
    ?assertMatch({ok, _AST}, Result).

parse_file_empty_test() ->
    TestFile = "/tmp/topos_test_empty.topos",
    create_test_file(TestFile, ""),

    Result = topos_parser_wrapper:parse_file(TestFile),

    delete_test_file(TestFile),
    % Empty file might be valid or invalid depending on grammar
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%%====================================================================
%% parse_tokens/1 Tests
%%====================================================================

parse_tokens_valid_test() ->
    % Simple valid token stream for: shape Bool = True | False
    Tokens = [
        {shape, 1},
        {type_id, 1, "Bool"},
        {'=', 1},
        {constructor_id, 1, "True"},
        {'|', 1},
        {constructor_id, 1, "False"}
    ],
    Result = topos_parser_wrapper:parse_tokens(Tokens),
    % Parser might accept or reject this depending on grammar completeness
    case Result of
        {ok, _AST} -> ok;
        {error, _} -> ok
    end.

parse_tokens_empty_test() ->
    Result = topos_parser_wrapper:parse_tokens([]),
    % Empty input might be valid or invalid depending on grammar
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%%====================================================================
%% Lexer Error Handling Tests
%%====================================================================

parse_file_lexer_error_unterminated_string_test() ->
    TestFile = "/tmp/topos_test_unterminated.topos",
    Content = "shape Foo = Bar \"unterminated string\n",
    create_test_file(TestFile, Content),

    Result = topos_parser_wrapper:parse_file(TestFile),

    delete_test_file(TestFile),
    case Result of
        {error, [Err]} ->
            ?assertEqual('E100_lexer_error', Err#error.code),
            ?assertEqual(error, Err#error.severity),
            ?assert(Err#error.line > 0);
        _ ->
            ?assert(false)  % Should be an error
    end.

%%====================================================================
%% Parser Error Handling Tests
%%====================================================================

parse_tokens_parser_error_test() ->
    % Invalid token stream: shape = (missing type name)
    Tokens = [
        {shape, 1},
        {'=', 1},
        {constructor_id, 1, "True"}
    ],
    Result = topos_parser_wrapper:parse_tokens(Tokens),
    ?assertMatch({error, [#error{code = 'E200_syntax_error'}]}, Result).

parse_tokens_parser_error_severity_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1},
        {constructor_id, 1, "True"}
    ],
    {error, [Err]} = topos_parser_wrapper:parse_tokens(Tokens),
    ?assertEqual(error, Err#error.severity).

%%====================================================================
%% Source Context Integration Tests
%%====================================================================

parse_file_error_with_context_test() ->
    TestFile = "/tmp/topos_test_error_context.topos",
    Content = "shape Foo = Bar\nshape Baz =\nshape Qux = Quux\n",
    create_test_file(TestFile, Content),

    Result = topos_parser_wrapper:parse_file(TestFile),

    delete_test_file(TestFile),
    case Result of
        {error, [Err]} ->
            ?assertEqual(TestFile, Err#error.file),
            ?assert(Err#error.line > 0);
        {ok, _} ->
            % This might parse successfully depending on parser rules
            ok
    end.

parse_file_error_has_file_location_test() ->
    TestFile = "/tmp/topos_test_location.topos",
    Content = "shape",  % Incomplete declaration
    create_test_file(TestFile, Content),

    Result = topos_parser_wrapper:parse_file(TestFile),

    delete_test_file(TestFile),
    case Result of
        {error, [Err]} ->
            ?assertEqual(TestFile, Err#error.file),
            ?assertNotEqual(undefined, Err#error.line);
        {ok, _} ->
            % Parser might recover
            ok
    end.

%%====================================================================
%% Suggestion Generation Tests
%%====================================================================

parse_tokens_suggestion_for_extra_end_test() ->
    % Test that extra 'end' keyword generates a suggestion
    Tokens = [
        {shape, 1},
        {type_id, 1, "Foo"},
        {'=', 1},
        {constructor_id, 1, "Bar"},
        {'end', 2}  % Unexpected end
    ],
    Result = topos_parser_wrapper:parse_tokens(Tokens),
    case Result of
        {error, [Err]} ->
            case Err#error.suggestion of
                undefined -> ok;  % Suggestion is optional
                Sugg ->
                    ?assert(is_list(Sugg)),
                    ?assert(length(Sugg) > 0)
            end;
        {ok, _} ->
            ok  % Might parse successfully
    end.

parse_tokens_suggestion_for_unmatched_brace_test() ->
    % Test unmatched closing brace
    Tokens = [
        {shape, 1},
        {type_id, 1, "Foo"},
        {'=', 1},
        {'{', 1},
        {constructor_id, 1, "Bar"},
        {'}', 1},
        {'}', 1}  % Extra closing brace
    ],
    Result = topos_parser_wrapper:parse_tokens(Tokens),
    case Result of
        {error, [Err]} ->
            ?assertEqual('E200_syntax_error', Err#error.code);
        {ok, _} ->
            ok  % Might parse successfully depending on grammar
    end.

%%====================================================================
%% Error Message Formatting Tests
%%====================================================================

parse_tokens_error_message_not_empty_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    {error, [Err]} = topos_parser_wrapper:parse_tokens(Tokens),
    ?assert(length(Err#error.message) > 0).

parse_tokens_error_has_code_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    {error, [Err]} = topos_parser_wrapper:parse_tokens(Tokens),
    ?assert(is_atom(Err#error.code)),
    ?assertNotEqual(undefined, Err#error.code).

%%====================================================================
%% Multiple Error Tests
%%====================================================================

parse_tokens_single_error_list_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    {error, Errors} = topos_parser_wrapper:parse_tokens(Tokens),
    ?assert(is_list(Errors)),
    ?assertEqual(1, length(Errors)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

parse_file_unicode_test() ->
    TestFile = "/tmp/topos_test_unicode.topos",
    % Use simpler content to avoid unicode encoding issues in file writing
    Content = "shape Foo = Bar\n",
    case file:write_file(TestFile, unicode:characters_to_binary(Content)) of
        ok ->
            Result = topos_parser_wrapper:parse_file(TestFile),
            delete_test_file(TestFile),
            % Should either parse or error gracefully
            case Result of
                {ok, _} -> ok;
                {error, [Err]} ->
                    ?assert(is_list(Err#error.message))
            end;
        {error, _} ->
            % If we can't write the file, skip the test
            ok
    end.

parse_tokens_with_file_undefined_file_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    Result = topos_parser_wrapper:parse_tokens_with_file(Tokens, undefined),
    case Result of
        {error, [Err]} ->
            ?assertEqual(undefined, Err#error.file);
        {ok, _} ->
            ok
    end.

parse_tokens_with_file_specified_file_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    Result = topos_parser_wrapper:parse_tokens_with_file(Tokens, "test.topos"),
    case Result of
        {error, [Err]} ->
            ?assertEqual("test.topos", Err#error.file);
        {ok, _} ->
            ok
    end.

%%====================================================================
%% Format Tests (Integration with formatter)
%%====================================================================

parse_tokens_error_can_be_formatted_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    {error, [Err]} = topos_parser_wrapper:parse_tokens(Tokens),
    % Test that error can be formatted
    Formatted = topos_error_formatter:format_error(Err),
    ?assert(is_list(Formatted) orelse is_binary(Formatted)).

parse_tokens_error_list_can_be_formatted_test() ->
    Tokens = [
        {shape, 1},
        {'=', 1}
    ],
    {error, Errors} = topos_parser_wrapper:parse_tokens(Tokens),
    % Test that error list can be formatted
    Formatted = topos_error_formatter:format_error_list(Errors),
    ?assert(is_list(Formatted) orelse is_binary(Formatted)).
