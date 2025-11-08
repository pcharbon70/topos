-module(topos_lexer_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Helper Functions
%%====================================================================

%% @doc Extract token types from token list
%% Converts [{Type, Line}, ...] or [{Type, Line, Value}, ...] to [Type, ...]
-spec token_types([tuple()]) -> [atom()].
token_types(Tokens) ->
    [Type || {Type, _Line} <- Tokens].

%%====================================================================
%% Test 1.1.1.1: Keywords, Operators, and Delimiters
%%====================================================================

keywords_test() ->
    %% Test all Topos keywords
    {ok, Tokens} = topos_lexer:tokenize("shape flow match where let in do end"),
    ?assertEqual([
        {shape, 1},
        {flow, 1},
        {match, 1},
        {where, 1},
        {'let', 1},
        {'in', 1},
        {'do', 1},
        {'end', 1}
    ], Tokens).

all_keywords_test() ->
    %% Test comprehensive keyword list
    Input = "shape flow match where let in do end if then else case of when "
            "module import export exports as qualified private trait instance forall actor supervisor",
    {ok, Tokens} = topos_lexer:tokenize(Input),
    Expected = [shape, flow, match, where, 'let', 'in', 'do', 'end', 'if', 'then', 'else',
                'case', 'of', 'when', 'module', 'import', 'export', exports, as, qualified,
                private, trait, instance, forall, actor, supervisor],
    ?assertEqual(Expected, token_types(Tokens)).

operators_two_char_test() ->
    %% Test two-character operators
    {ok, Tokens} = topos_lexer:tokenize("|> >>= -> => <> == /= <= >= || && :: <- .."),
    Expected = [pipe_right, bind, arrow, double_arrow, concat, eq, neq, lte, gte,
                'or', 'and', cons, left_arrow, range],
    ?assertEqual(Expected, token_types(Tokens)).

operators_single_char_test() ->
    %% Test single-character operators
    {ok, Tokens} = topos_lexer:tokenize(": = | < > + - * / ."),
    Expected = [colon, equals, pipe, lt, gt, plus, minus, star, slash, dot],
    ?assertEqual(Expected, token_types(Tokens)).

delimiters_test() ->
    %% Test all delimiters
    {ok, Tokens} = topos_lexer:tokenize("{ } [ ] ( ) , ; _"),
    Expected = [lbrace, rbrace, lbracket, rbracket, lparen, rparen, comma, semicolon, underscore],
    ?assertEqual(Expected, token_types(Tokens)).

operator_precedence_in_expression_test() ->
    %% Test that operators are recognized in expressions
    {ok, Tokens} = topos_lexer:tokenize("x |> f |> g"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {pipe_right, 1},
        {lower_ident, 1, "f"},
        {pipe_right, 1},
        {lower_ident, 1, "g"}
    ], Tokens).

%%====================================================================
%% Test 1.1.1.2: Number Literal Recognition
%%====================================================================

integer_literals_test() ->
    {ok, Tokens} = topos_lexer:tokenize("0 1 42 123456789"),
    ?assertMatch([
        {integer, 1, 0},
        {integer, 1, 1},
        {integer, 1, 42},
        {integer, 1, 123456789}
    ], Tokens).

float_literals_test() ->
    {ok, Tokens} = topos_lexer:tokenize("0.0 1.5 3.14159 99.99"),
    ?assertMatch([
        {float, 1, 0.0},
        {float, 1, 1.5},
        {float, 1, _},
        {float, 1, 99.99}
    ], Tokens),
    %% Check pi approximation
    {float, 1, Pi} = lists:nth(3, Tokens),
    ?assert(abs(Pi - 3.14159) < 0.00001).

scientific_notation_positive_exp_test() ->
    {ok, Tokens} = topos_lexer:tokenize("1e10 1.5e10 2.5E5"),
    ?assertMatch([
        {float, 1, _},
        {float, 1, _},
        {float, 1, _}
    ], Tokens),
    %% Verify values
    {float, 1, V1} = lists:nth(1, Tokens),
    ?assertEqual(1.0e10, V1),
    {float, 1, V2} = lists:nth(2, Tokens),
    ?assertEqual(1.5e10, V2),
    {float, 1, V3} = lists:nth(3, Tokens),
    ?assertEqual(2.5e5, V3).

scientific_notation_negative_exp_test() ->
    {ok, Tokens} = topos_lexer:tokenize("1e-10 1.5e-10 2.5E-5"),
    ?assertMatch([
        {float, 1, _},
        {float, 1, _},
        {float, 1, _}
    ], Tokens),
    %% Verify values
    {float, 1, V1} = lists:nth(1, Tokens),
    ?assertEqual(1.0e-10, V1),
    {float, 1, V2} = lists:nth(2, Tokens),
    ?assertEqual(1.5e-10, V2),
    {float, 1, V3} = lists:nth(3, Tokens),
    ?assertEqual(2.5e-5, V3).

scientific_notation_explicit_plus_test() ->
    {ok, Tokens} = topos_lexer:tokenize("1e+5 2.5E+10"),
    ?assertMatch([
        {float, 1, _},
        {float, 1, _}
    ], Tokens),
    {float, 1, V1} = lists:nth(1, Tokens),
    ?assertEqual(1.0e5, V1),
    {float, 1, V2} = lists:nth(2, Tokens),
    ?assertEqual(2.5e10, V2).

mixed_number_literals_test() ->
    %% Test integers, floats, and scientific notation together
    {ok, Tokens} = topos_lexer:tokenize("42 3.14 1e10 100 0.5 2e-3"),
    ?assertEqual(6, length(Tokens)),
    ?assertMatch([
        {integer, 1, 42},
        {float, 1, 3.14},
        {float, 1, _},
        {integer, 1, 100},
        {float, 1, 0.5},
        {float, 1, _}
    ], Tokens).

%%====================================================================
%% Test 1.1.1.3: String Literal Recognition
%%====================================================================

simple_string_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"hello\""),
    ?assertMatch([{string, 1, "hello"}], Tokens).

string_with_spaces_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"hello world\""),
    ?assertMatch([{string, 1, "hello world"}], Tokens).

empty_string_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"\""),
    ?assertMatch([{string, 1, ""}], Tokens).

string_escape_newline_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"line1\\nline2\""),
    ?assertMatch([{string, 1, "line1\nline2"}], Tokens).

string_escape_tab_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"col1\\tcol2\""),
    ?assertMatch([{string, 1, "col1\tcol2"}], Tokens).

string_escape_carriage_return_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"text\\rmore\""),
    ?assertMatch([{string, 1, "text\rmore"}], Tokens).

string_escape_backslash_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"path\\\\file\""),
    ?assertMatch([{string, 1, "path\\file"}], Tokens).

string_escape_quote_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"say \\\"hello\\\"\""),
    ?assertMatch([{string, 1, "say \"hello\""}], Tokens).

string_escape_single_quote_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"can\\'t\""),
    ?assertMatch([{string, 1, "can't"}], Tokens).

string_all_escapes_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"\\n\\r\\t\\\\\\\"'\""),
    ?assertMatch([{string, 1, "\n\r\t\\\"'"}], Tokens).

multiple_strings_test() ->
    {ok, Tokens} = topos_lexer:tokenize("\"first\" \"second\" \"third\""),
    ?assertEqual(3, length(Tokens)),
    ?assertMatch([
        {string, 1, "first"},
        {string, 1, "second"},
        {string, 1, "third"}
    ], Tokens).

%%====================================================================
%% Test 1.1.1.4: Comment Recognition
%%====================================================================

single_line_comment_test() ->
    {ok, Tokens} = topos_lexer:tokenize("x -- this is a comment\ny"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 2, "y"}
    ], Tokens).

single_line_comment_at_end_test() ->
    {ok, Tokens} = topos_lexer:tokenize("x -- comment at end"),
    ?assertMatch([{lower_ident, 1, "x"}], Tokens).

single_line_comment_entire_line_test() ->
    {ok, Tokens} = topos_lexer:tokenize("-- entire line is a comment\nx"),
    ?assertMatch([{lower_ident, 2, "x"}], Tokens).

multi_line_comment_test() ->
    {ok, Tokens} = topos_lexer:tokenize("x {- comment -} y"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 1, "y"}
    ], Tokens).

multi_line_comment_multiline_test() ->
    Input = "x {- this is\n"
            "a multi-line\n"
            "comment -} y",
    {ok, Tokens} = topos_lexer:tokenize(Input),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 3, "y"}
    ], Tokens).

nested_comment_single_level_test() ->
    {ok, Tokens} = topos_lexer:tokenize("x {- outer {- inner -} -} y"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 1, "y"}
    ], Tokens).

nested_comment_multiple_levels_test() ->
    {ok, Tokens} = topos_lexer:tokenize("a {- L1 {- L2 {- L3 -} -} -} b"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {lower_ident, 1, "b"}
    ], Tokens).

unclosed_comment_error_test() ->
    Result = topos_lexer:tokenize("x {- unclosed comment"),
    ?assertMatch({error, {0, topos_lexer, {unclosed_comment, 1}}}, Result).

unmatched_comment_end_error_test() ->
    Result = topos_lexer:tokenize("x -} y"),
    ?assertMatch({error, {1, topos_lexer, unmatched_comment_end}}, Result).

mixed_comments_test() ->
    Input = "x -- single line\n"
            "y {- multi\n"
            "line -}\n"
            "z",
    {ok, Tokens} = topos_lexer:tokenize(Input),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 2, "y"},
        {lower_ident, 4, "z"}
    ], Tokens).

%%====================================================================
%% Integration Tests
%%====================================================================

realistic_topos_code_test() ->
    %% Test a realistic piece of Topos code
    Code = "flow factorial : Natural -> Natural\n"
           "flow factorial n = match n\n"
           "  | 0 -> 1\n"
           "  | n -> n * factorial (n - 1)\n"
           "end",
    {ok, Tokens} = topos_lexer:tokenize(Code),
    %% Verify it tokenizes without errors
    ?assert(length(Tokens) > 0),
    %% Check that flow keyword appears twice
    FlowCount = length([T || T = {flow, _} <- Tokens]),
    ?assertEqual(2, FlowCount).

shape_definition_test() ->
    Code = "shape Maybe a = Some a | None",
    {ok, Tokens} = topos_lexer:tokenize(Code),
    ?assertMatch([
        {shape, 1},
        {upper_ident, 1, "Maybe"},
        {lower_ident, 1, "a"},
        {equals, 1},
        {upper_ident, 1, "Some"},
        {lower_ident, 1, "a"},
        {pipe, 1},
        {upper_ident, 1, "None"}
    ], Tokens).

composition_operators_test() ->
    Code = "validate |> transform >>= persist",
    {ok, Tokens} = topos_lexer:tokenize(Code),
    ?assertMatch([
        {lower_ident, 1, "validate"},
        {pipe_right, 1},
        {lower_ident, 1, "transform"},
        {bind, 1},
        {lower_ident, 1, "persist"}
    ], Tokens).

identifiers_test() ->
    %% Test lowercase and uppercase identifiers
    {ok, Tokens} = topos_lexer:tokenize("foo Bar baz'"),
    ?assertMatch([
        {lower_ident, 1, "foo"},
        {upper_ident, 1, "Bar"},
        {lower_ident, 1, "baz'"}
    ], Tokens).

whitespace_handling_test() ->
    %% Test that various whitespace is handled correctly
    {ok, Tokens} = topos_lexer:tokenize("a   \t  b\n\nc"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {lower_ident, 1, "b"},
        {lower_ident, 3, "c"}
    ], Tokens).

line_tracking_test() ->
    %% Test that line numbers are tracked correctly
    Code = "a\nb\n\nc",
    {ok, Tokens} = topos_lexer:tokenize(Code),
    Lines = [Line || {_, Line, _} <- Tokens] ++ [Line || {_, Line} <- Tokens],
    ?assertEqual([1, 2, 4], Lines).

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

consecutive_operators_test() ->
    %% Test that consecutive operators are handled correctly
    {ok, Tokens} = topos_lexer:tokenize("a|>b"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {pipe_right, 1},
        {lower_ident, 1, "b"}
    ], Tokens).

operator_lookahead_test() ->
    %% Test that '-' is not confused with comment start
    {ok, Tokens} = topos_lexer:tokenize("a - b"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {minus, 1},
        {lower_ident, 1, "b"}
    ], Tokens).

comment_in_string_test() ->
    %% Test that comment markers in strings are not treated as comments
    {ok, Tokens} = topos_lexer:tokenize("\"-- not a comment\""),
    ?assertMatch([{string, 1, "-- not a comment"}], Tokens).

empty_input_test() ->
    {ok, Tokens} = topos_lexer:tokenize(""),
    ?assertEqual([], Tokens).

only_whitespace_test() ->
    {ok, Tokens} = topos_lexer:tokenize("   \n\t\r\n   "),
    ?assertEqual([], Tokens).

only_comments_test() ->
    {ok, Tokens} = topos_lexer:tokenize("-- just comments\n{- more comments -}"),
    ?assertEqual([], Tokens).

%%====================================================================
%% Test 1.1.1.5: Error Handling
%%====================================================================

%% Unterminated string tests
unterminated_string_simple_test() ->
    Result = topos_lexer:tokenize("\"hello"),
    ?assertMatch({error, {_, _, _}}, Result).

unterminated_string_with_newline_test() ->
    Result = topos_lexer:tokenize("\"line1\nline2"),
    ?assertMatch({error, {_, _, _}}, Result).

unterminated_string_ending_with_backslash_test() ->
    Result = topos_lexer:tokenize("\"escape\\"),
    ?assertMatch({error, {_, _, _}}, Result).

%% Illegal character tests
illegal_character_at_sign_test() ->
    Result = topos_lexer:tokenize("@symbol"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_hash_test() ->
    Result = topos_lexer:tokenize("#directive"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_dollar_test() ->
    Result = topos_lexer:tokenize("$var"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_percent_test() ->
    Result = topos_lexer:tokenize("%percent"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_caret_test() ->
    Result = topos_lexer:tokenize("^caret"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_backtick_test() ->
    Result = topos_lexer:tokenize("`template`"),
    ?assertMatch({error, {_, _, _}}, Result).

%% Invalid escape sequence tests
invalid_escape_unknown_letter_test() ->
    Result = topos_lexer:tokenize("\"invalid \\z escape\""),
    ?assertMatch({error, {_, _, _}}, Result).

invalid_escape_hex_not_supported_test() ->
    Result = topos_lexer:tokenize("\"hex \\x41\""),
    ?assertMatch({error, {_, _, _}}, Result).

%%====================================================================
%% Test 1.1.1.6: Resource Limits (DoS Prevention)
%%====================================================================

input_too_large_test() ->
    %% Create a string larger than MAX_INPUT_SIZE (10MB)
    LargeString = lists:duplicate(10000001, $a),
    Result = topos_lexer:tokenize(LargeString),
    ?assertMatch({error, {0, topos_lexer, {input_too_large, 10000001, 10000000}}}, Result).

nesting_too_deep_test() ->
    %% Create 101 levels of nested comments (exceeds MAX_NESTING_DEPTH of 100)
    OpenComments = lists:duplicate(101, "{-"),
    CloseComments = lists:duplicate(101, "-}"),
    Input = lists:flatten(OpenComments ++ CloseComments),
    Result = topos_lexer:tokenize(Input),
    ?assertMatch({error, {_Line, topos_lexer, {nesting_too_deep, 100, 100}}}, Result).

identifier_too_long_test() ->
    %% Create an identifier longer than MAX_IDENT_LENGTH (255)
    LongIdent = lists:duplicate(256, $a),
    Result = topos_lexer:tokenize(LongIdent),
    ?assertMatch({error, {1, topos_lexer, {identifier_too_long, 256, 255}}}, Result).

%%====================================================================
%% Test 1.1.1.7: Unicode and Special Characters
%%====================================================================

%% Unicode in strings tests (unicode IS supported in strings)
unicode_string_russian_test() ->
    %% UTF-8 Russian text in string - should work
    {ok, Tokens} = topos_lexer:tokenize("\"Привет мир\""),
    ?assertMatch([{string, 1, _}], Tokens),
    %% Verify it contains unicode codepoints
    [{string, 1, Content}] = Tokens,
    ?assert(lists:any(fun(C) -> C > 127 end, Content)).

unicode_string_emoji_test() ->
    %% Emoji in string - should work
    {ok, Tokens} = topos_lexer:tokenize("\"Hello 👋 World\""),
    ?assertMatch([{string, 1, _}], Tokens),
    %% Verify it contains emoji codepoint (128075 is wave emoji)
    [{string, 1, Content}] = Tokens,
    ?assert(lists:member(128075, Content)).

unicode_string_cjk_test() ->
    %% Chinese characters in string - should work
    {ok, Tokens} = topos_lexer:tokenize("\"你好世界\""),
    ?assertMatch([{string, 1, _}], Tokens),
    %% Verify it contains CJK codepoints (> 127)
    [{string, 1, Content}] = Tokens,
    ?assert(lists:all(fun(C) -> C > 127 end, Content)).

%% Unicode in identifiers tests (should fail - only ASCII allowed)
unicode_identifier_russian_test() ->
    %% UTF-8 identifier should fail
    Result = topos_lexer:tokenize("привет"),
    ?assertMatch({error, {_, _, _}}, Result).

unicode_identifier_emoji_test() ->
    %% Emoji identifier should fail
    Result = topos_lexer:tokenize("hello👋"),
    ?assertMatch({error, {_, _, _}}, Result).

unicode_identifier_cjk_test() ->
    %% CJK identifier should fail
    Result = topos_lexer:tokenize("变量名"),
    ?assertMatch({error, {_, _, _}}, Result).

%% Special whitespace tests (should be rejected as illegal characters)
special_whitespace_bom_test() ->
    %% Byte Order Mark (BOM) should be rejected as illegal character
    BOM = [16#EF, 16#BB, 16#BF],
    Result = topos_lexer:tokenize(BOM ++ "x"),
    ?assertMatch({error, {1, _, _}}, Result).

special_whitespace_zero_width_test() ->
    %% Zero-width space (U+200B) should be rejected
    ZeroWidth = "x" ++ [16#E2, 16#80, 16#8B] ++ "y",
    Result = topos_lexer:tokenize(ZeroWidth),
    ?assertMatch({error, {1, _, _}}, Result).

special_whitespace_nbsp_test() ->
    %% Non-breaking space (U+00A0) should be rejected
    NBSP = "x" ++ [16#C2, 16#A0] ++ "y",
    Result = topos_lexer:tokenize(NBSP),
    ?assertMatch({error, {1, _, _}}, Result).
