-module(topos_error_formatter_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../../src/compiler/error/topos_error.hrl").

%%====================================================================
%% ANSI Color Tests
%%====================================================================

red_text_test() ->
    topos_error_formatter:set_color_mode(always),
    Result = lists:flatten(topos_error_formatter:red("error")),
    ?assertEqual("\e[31merror\e[0m", Result).

yellow_text_test() ->
    topos_error_formatter:set_color_mode(always),
    Result = lists:flatten(topos_error_formatter:yellow("warning")),
    ?assertEqual("\e[33mwarning\e[0m", Result).

blue_text_test() ->
    topos_error_formatter:set_color_mode(always),
    Result = lists:flatten(topos_error_formatter:blue("note")),
    ?assertEqual("\e[34mnote\e[0m", Result).

cyan_text_test() ->
    topos_error_formatter:set_color_mode(always),
    Result = lists:flatten(topos_error_formatter:cyan("help")),
    ?assertEqual("\e[36mhelp\e[0m", Result).

bold_text_test() ->
    topos_error_formatter:set_color_mode(always),
    Result = lists:flatten(topos_error_formatter:bold("important")),
    ?assertEqual("\e[1mimportant\e[0m", Result).

dim_text_test() ->
    topos_error_formatter:set_color_mode(always),
    Result = lists:flatten(topos_error_formatter:dim("context")),
    ?assertEqual("\e[2mcontext\e[0m", Result).

reset_test() ->
    Result = topos_error_formatter:reset(),
    ?assertEqual("\e[0m", Result).

%%====================================================================
%% Color Mode Tests
%%====================================================================

color_mode_always_test() ->
    topos_error_formatter:set_color_mode(always),
    ?assertEqual(true, topos_error_formatter:supports_color()).

color_mode_never_test() ->
    topos_error_formatter:set_color_mode(never),
    ?assertEqual(false, topos_error_formatter:supports_color()).

color_mode_never_no_ansi_test() ->
    topos_error_formatter:set_color_mode(never),
    Result = lists:flatten(topos_error_formatter:red("text")),
    ?assertEqual("text", Result).

set_color_mode_test() ->
    ?assertEqual(ok, topos_error_formatter:set_color_mode(always)),
    ?assertEqual(ok, topos_error_formatter:set_color_mode(never)),
    ?assertEqual(ok, topos_error_formatter:set_color_mode(auto)).

%%====================================================================
%% Simple Error Formatting Tests
%%====================================================================

format_error_simple_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Syntax error", {5, 10}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_simple(Err)),
    ?assert(string:find(Result, "error") /= nomatch),
    ?assert(string:find(Result, "E001") /= nomatch),
    ?assert(string:find(Result, "Syntax error") /= nomatch),
    ?assert(string:find(Result, "test.topos:5:10") /= nomatch).

format_error_simple_no_file_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {10, undefined}, undefined),
    Result = lists:flatten(topos_error_formatter:format_error_simple(Err)),
    ?assert(string:find(Result, "10") /= nomatch).

format_error_simple_warning_test() ->
    topos_error_formatter:set_color_mode(never),
    Warn = topos_error:new_warning('W001', "Unused variable", {3, 5}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_simple(Warn)),
    ?assert(string:find(Result, "warning") /= nomatch),
    ?assert(string:find(Result, "W001") /= nomatch).

format_error_simple_note_test() ->
    topos_error_formatter:set_color_mode(never),
    Note = topos_error:new_note('N001', "Consider refactoring", {7, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_simple(Note)),
    ?assert(string:find(Result, "note") /= nomatch),
    ?assert(string:find(Result, "N001") /= nomatch).

%%====================================================================
%% Full Error Formatting Tests
%%====================================================================

format_error_basic_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Syntax error", {5, 10}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "error[E001]") /= nomatch),
    ?assert(string:find(Result, "Syntax error") /= nomatch),
    ?assert(string:find(Result, "test.topos:5:10") /= nomatch).

format_error_with_context_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Missing end", {3, 1}, "test.topos"),
    Err2 = topos_error:add_context(Err, ["line 1", "line 2"], "line 3 error", ["line 4"]),
    Result = lists:flatten(topos_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "line 1") /= nomatch),
    ?assert(string:find(Result, "line 2") /= nomatch),
    ?assert(string:find(Result, "line 3 error") /= nomatch),
    ?assert(string:find(Result, "line 4") /= nomatch).

format_error_with_suggestion_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Missing end", {3, 1}, "test.topos"),
    Err2 = topos_error:add_suggestion(Err, "Add 'end' to close match expression"),
    Result = lists:flatten(topos_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "help:") /= nomatch),
    ?assert(string:find(Result, "Add 'end'") /= nomatch).

format_error_with_related_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Main error", {5, 1}, "test.topos"),
    Note = topos_error:new_note('N001', "Related note", {3, 1}, "test.topos"),
    Err2 = topos_error:add_related(Err, Note),
    Result = lists:flatten(topos_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "Main error") /= nomatch),
    ?assert(string:find(Result, "Related note") /= nomatch).

format_error_no_file_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "error[E001]") /= nomatch).

format_error_no_column_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {5, undefined}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "test.topos:5") /= nomatch).

format_error_with_column_highlight_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {3, 5}, "test.topos"),
    Err2 = topos_error:add_context(Err, [], "line text", []),
    Result = lists:flatten(topos_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "^") /= nomatch).

%%====================================================================
%% Error List Formatting Tests
%%====================================================================

format_error_list_empty_test() ->
    topos_error_formatter:set_color_mode(never),
    Result = lists:flatten(topos_error_formatter:format_error_list([])),
    ?assertEqual("\n\n", Result).

format_error_list_single_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {1, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_list([Err])),
    ?assert(string:find(Result, "error[E001]") /= nomatch),
    ?assert(string:find(Result, "1 error") /= nomatch).

format_error_list_multiple_test() ->
    topos_error_formatter:set_color_mode(never),
    Err1 = topos_error:new_error('E001', "Error 1", {1, 1}, "test.topos"),
    Err2 = topos_error:new_error('E002', "Error 2", {2, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_list([Err1, Err2])),
    ?assert(string:find(Result, "Error 1") /= nomatch),
    ?assert(string:find(Result, "Error 2") /= nomatch),
    ?assert(string:find(Result, "2 errors") /= nomatch).

format_error_list_mixed_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {1, 1}, "test.topos"),
    Warn = topos_error:new_warning('W001', "Warning", {2, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_list([Err, Warn])),
    ?assert(string:find(Result, "1 error") /= nomatch),
    ?assert(string:find(Result, "1 warning") /= nomatch).

format_error_list_warnings_only_test() ->
    topos_error_formatter:set_color_mode(never),
    Warn1 = topos_error:new_warning('W001', "Warning 1", {1, 1}, "test.topos"),
    Warn2 = topos_error:new_warning('W002', "Warning 2", {2, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error_list([Warn1, Warn2])),
    ?assert(string:find(Result, "2 warnings") /= nomatch),
    ?assert(string:find(Result, "error") == nomatch).  % Should not contain "error" word

%%====================================================================
%% Color Formatting Tests
%%====================================================================

format_error_with_colors_test() ->
    topos_error_formatter:set_color_mode(always),
    Err = topos_error:new_error('E001', "Syntax error", {5, 10}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "\e[31m") /= nomatch),  % Red for error
    ?assert(string:find(Result, "\e[0m") /= nomatch).   % Reset code

format_warning_with_colors_test() ->
    topos_error_formatter:set_color_mode(always),
    Warn = topos_error:new_warning('W001', "Warning", {3, 5}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Warn)),
    ?assert(string:find(Result, "\e[33m") /= nomatch).  % Yellow for warning

format_note_with_colors_test() ->
    topos_error_formatter:set_color_mode(always),
    Note = topos_error:new_note('N001', "Note", {7, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Note)),
    ?assert(string:find(Result, "\e[34m") /= nomatch).  % Blue for note

%%====================================================================
%% Edge Case Tests
%%====================================================================

format_error_empty_context_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {1, 1}, "test.topos"),
    Err2 = topos_error:add_context(Err, [], "", []),
    Result = lists:flatten(topos_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "error[E001]") /= nomatch).

format_error_long_message_test() ->
    topos_error_formatter:set_color_mode(never),
    LongMsg = lists:duplicate(200, $x),
    Err = topos_error:new_error('E001', LongMsg, {1, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(length(Result) > 200).

format_error_unicode_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error: λ → ∀", {1, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "λ") /= nomatch).

format_error_zero_column_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {3, 0}, "test.topos"),
    Err2 = topos_error:add_context(Err, [], "line text", []),
    Result = lists:flatten(topos_error_formatter:format_error(Err2)),
    % Should handle column 0 gracefully (no highlight)
    ?assert(is_list(Result)).

format_error_large_line_number_test() ->
    topos_error_formatter:set_color_mode(never),
    Err = topos_error:new_error('E001', "Error", {99999, 1}, "test.topos"),
    Result = lists:flatten(topos_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "99999") /= nomatch).
