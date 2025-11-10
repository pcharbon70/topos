-module(topos_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../../src/compiler/error/topos_error.hrl").

%%====================================================================
%% Error Creation Tests
%%====================================================================

new_error_test() ->
    Err = topos_error:new_error('E001', "Syntax error", {5, 10}, "test.topos"),
    ?assertEqual(error, Err#error.severity),
    ?assertEqual('E001', Err#error.code),
    ?assertEqual("Syntax error", Err#error.message),
    ?assertEqual("test.topos", Err#error.file),
    ?assertEqual(5, Err#error.line),
    ?assertEqual(10, Err#error.column),
    ?assertEqual(undefined, Err#error.source_line),
    ?assertEqual([], Err#error.context_before),
    ?assertEqual([], Err#error.context_after),
    ?assertEqual(undefined, Err#error.suggestion),
    ?assertEqual([], Err#error.related).

new_error_no_file_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    ?assertEqual(undefined, Err#error.file).

new_error_no_column_test() ->
    Err = topos_error:new_error('E002', "Error", {10, undefined}, "test.topos"),
    ?assertEqual(undefined, Err#error.column).

new_warning_test() ->
    Warn = topos_error:new_warning('W001', "Unused variable", {3, 5}, "test.topos"),
    ?assertEqual(warning, Warn#error.severity),
    ?assertEqual('W001', Warn#error.code),
    ?assertEqual("Unused variable", Warn#error.message).

new_note_test() ->
    Note = topos_error:new_note('N001', "Consider refactoring", {7, 1}, "test.topos"),
    ?assertEqual(note, Note#error.severity),
    ?assertEqual('N001', Note#error.code).

%%====================================================================
%% Error Manipulation Tests
%%====================================================================

add_suggestion_test() ->
    Err = topos_error:new_error('E001', "Missing end", {10, 1}, undefined),
    Err2 = topos_error:add_suggestion(Err, "Add 'end' to close match expression"),
    ?assertEqual("Add 'end' to close match expression", Err2#error.suggestion).

add_suggestion_empty_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Err2 = topos_error:add_suggestion(Err, ""),
    ?assertEqual("", Err2#error.suggestion).

add_context_test() ->
    Err = topos_error:new_error('E001', "Error", {3, 1}, undefined),
    Err2 = topos_error:add_context(Err, ["line 1", "line 2"], "line 3 error", ["line 4"]),
    ?assertEqual(["line 1", "line 2"], Err2#error.context_before),
    ?assertEqual("line 3 error", Err2#error.source_line),
    ?assertEqual(["line 4"], Err2#error.context_after).

add_context_empty_before_after_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Err2 = topos_error:add_context(Err, [], "only line", []),
    ?assertEqual([], Err2#error.context_before),
    ?assertEqual("only line", Err2#error.source_line),
    ?assertEqual([], Err2#error.context_after).

add_related_single_test() ->
    Err1 = topos_error:new_error('E001', "Main error", {5, 1}, undefined),
    Err2 = topos_error:new_note('N001', "Related note", {3, 1}, undefined),
    Err3 = topos_error:add_related(Err1, Err2),
    ?assertEqual([Err2], Err3#error.related).

add_related_multiple_test() ->
    Err1 = topos_error:new_error('E001', "Main error", {5, 1}, undefined),
    Note1 = topos_error:new_note('N001', "Note 1", {3, 1}, undefined),
    Note2 = topos_error:new_note('N002', "Note 2", {7, 1}, undefined),
    Err2 = topos_error:add_related(Err1, [Note1, Note2]),
    ?assertEqual([Note1, Note2], Err2#error.related).

add_related_accumulate_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Note1 = topos_error:new_note('N001', "Note 1", {1, 1}, undefined),
    Note2 = topos_error:new_note('N002', "Note 2", {1, 1}, undefined),
    Err2 = topos_error:add_related(Err, Note1),
    Err3 = topos_error:add_related(Err2, Note2),
    ?assertEqual(2, length(Err3#error.related)).

set_source_line_test() ->
    Err = topos_error:new_error('E001', "Error", {5, 1}, undefined),
    Err2 = topos_error:set_source_line(Err, "flow foo = bar"),
    ?assertEqual("flow foo = bar", Err2#error.source_line).

%%====================================================================
%% Error Accumulation Tests
%%====================================================================

accumulate_empty_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Errors = topos_error:accumulate([], Err),
    ?assertEqual([Err], Errors).

accumulate_multiple_test() ->
    Err1 = topos_error:new_error('E001', "Error 1", {1, 1}, undefined),
    Err2 = topos_error:new_error('E002', "Error 2", {2, 1}, undefined),
    Err3 = topos_error:new_error('E003', "Error 3", {3, 1}, undefined),
    Errors1 = topos_error:accumulate([], Err1),
    Errors2 = topos_error:accumulate(Errors1, Err2),
    Errors3 = topos_error:accumulate(Errors2, Err3),
    ?assertEqual(3, length(Errors3)),
    ?assertEqual([Err1, Err2, Err3], Errors3).

has_errors_empty_test() ->
    ?assertEqual(false, topos_error:has_errors([])).

has_errors_with_errors_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    ?assertEqual(true, topos_error:has_errors([Err])).

has_errors_warnings_only_test() ->
    Warn = topos_error:new_warning('W001', "Warning", {1, 1}, undefined),
    ?assertEqual(false, topos_error:has_errors([Warn])).

has_errors_mixed_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Warn = topos_error:new_warning('W001', "Warning", {2, 1}, undefined),
    ?assertEqual(true, topos_error:has_errors([Warn, Err])).

get_errors_empty_test() ->
    ?assertEqual([], topos_error:get_errors([])).

get_errors_only_errors_test() ->
    Err1 = topos_error:new_error('E001', "Error 1", {1, 1}, undefined),
    Err2 = topos_error:new_error('E002', "Error 2", {2, 1}, undefined),
    ?assertEqual([Err1, Err2], topos_error:get_errors([Err1, Err2])).

get_errors_mixed_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Warn = topos_error:new_warning('W001', "Warning", {2, 1}, undefined),
    Note = topos_error:new_note('N001', "Note", {3, 1}, undefined),
    ?assertEqual([Err], topos_error:get_errors([Err, Warn, Note])).

get_warnings_empty_test() ->
    ?assertEqual([], topos_error:get_warnings([])).

get_warnings_only_warnings_test() ->
    Warn1 = topos_error:new_warning('W001', "Warning 1", {1, 1}, undefined),
    Warn2 = topos_error:new_warning('W002', "Warning 2", {2, 1}, undefined),
    ?assertEqual([Warn1, Warn2], topos_error:get_warnings([Warn1, Warn2])).

get_warnings_mixed_test() ->
    Err = topos_error:new_error('E001', "Error", {1, 1}, undefined),
    Warn = topos_error:new_warning('W001', "Warning", {2, 1}, undefined),
    Note = topos_error:new_note('N001', "Note", {3, 1}, undefined),
    ?assertEqual([Warn], topos_error:get_warnings([Err, Warn, Note])).

%%====================================================================
%% Source Context Extraction Tests
%%====================================================================

read_source_context_test() ->
    % Create temporary test file
    TestFile = "/tmp/topos_test_source.topos",
    Content = "line 1\nline 2\nline 3\nline 4\nline 5\n",
    ok = file:write_file(TestFile, Content),

    % Read context around line 3
    {ok, Context} = topos_error:read_source_context(TestFile, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual(["line 4"], maps:get('after', Context)),

    % Cleanup
    file:delete(TestFile).

read_source_context_first_line_test() ->
    TestFile = "/tmp/topos_test_first.topos",
    Content = "line 1\nline 2\nline 3\n",
    ok = file:write_file(TestFile, Content),

    {ok, Context} = topos_error:read_source_context(TestFile, 1, 2),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("line 1", maps:get(error_line, Context)),
    ?assertEqual(["line 2", "line 3"], maps:get('after', Context)),

    file:delete(TestFile).

read_source_context_last_line_test() ->
    TestFile = "/tmp/topos_test_last.topos",
    Content = "line 1\nline 2\nline 3\n",
    ok = file:write_file(TestFile, Content),

    {ok, Context} = topos_error:read_source_context(TestFile, 3, 2),
    ?assertEqual(["line 1", "line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)),

    file:delete(TestFile).

read_source_context_zero_context_test() ->
    TestFile = "/tmp/topos_test_zero.topos",
    Content = "line 1\nline 2\nline 3\n",
    ok = file:write_file(TestFile, Content),

    {ok, Context} = topos_error:read_source_context(TestFile, 2, 0),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("line 2", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)),

    file:delete(TestFile).

read_source_context_file_not_found_test() ->
    {error, {file_read_error, enoent}} = topos_error:read_source_context("/nonexistent/file.topos", 1, 1).

read_source_context_line_out_of_bounds_test() ->
    TestFile = "/tmp/topos_test_bounds.topos",
    Content = "line 1\nline 2\n",
    ok = file:write_file(TestFile, Content),

    {error, line_out_of_bounds} = topos_error:read_source_context(TestFile, 10, 1),
    {error, line_out_of_bounds} = topos_error:read_source_context(TestFile, 0, 1),

    file:delete(TestFile).

extract_context_from_file_test() ->
    Lines = ["line 1", "line 2", "line 3", "line 4", "line 5"],
    {ok, Context} = topos_error:extract_context_from_file(Lines, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual(["line 4"], maps:get('after', Context)).

extract_context_from_file_first_line_test() ->
    Lines = ["line 1", "line 2", "line 3"],
    {ok, Context} = topos_error:extract_context_from_file(Lines, 1, 1),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("line 1", maps:get(error_line, Context)),
    ?assertEqual(["line 2"], maps:get('after', Context)).

extract_context_from_file_last_line_test() ->
    Lines = ["line 1", "line 2", "line 3"],
    {ok, Context} = topos_error:extract_context_from_file(Lines, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)).

extract_context_from_file_out_of_bounds_test() ->
    Lines = ["line 1", "line 2"],
    {error, line_out_of_bounds} = topos_error:extract_context_from_file(Lines, 10, 1),
    {error, line_out_of_bounds} = topos_error:extract_context_from_file(Lines, 0, 1).

extract_context_large_context_test() ->
    Lines = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"],
    {ok, Context} = topos_error:extract_context_from_file(Lines, 5, 3),
    ?assertEqual(["2", "3", "4"], maps:get(before, Context)),
    ?assertEqual("5", maps:get(error_line, Context)),
    ?assertEqual(["6", "7", "8"], maps:get('after', Context)).

extract_context_single_line_file_test() ->
    Lines = ["only line"],
    {ok, Context} = topos_error:extract_context_from_file(Lines, 1, 5),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("only line", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)).
