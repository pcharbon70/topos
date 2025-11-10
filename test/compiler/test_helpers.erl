%% Test Helper Module
%% Shared utilities for compiler test suites
%%
%% This module provides common test utilities to reduce code duplication
%% across test files. All test files can use these helpers for file
%% management and other common testing tasks.

-module(test_helpers).

-export([
    % File management
    with_temp_file/2,
    with_temp_file/3,
    create_test_file/2,
    delete_test_file/1,
    generate_temp_filename/0,
    generate_temp_filename/1
]).

%%====================================================================
%% Temporary File Management
%%====================================================================

%% @doc Execute function with temporary file, ensuring cleanup
%% Creates a temporary file with the given content, executes the function,
%% and guarantees cleanup even if the function fails.
-spec with_temp_file(iodata(), fun((string()) -> term())) -> term().
with_temp_file(Content, Fun) ->
    TestFile = generate_temp_filename(),
    create_test_file(TestFile, Content),
    try
        Fun(TestFile)
    after
        delete_test_file(TestFile)
    end.

%% @doc Execute function with temporary file using custom filename
%% Allows specifying a custom filename pattern while still ensuring cleanup.
-spec with_temp_file(string(), iodata(), fun((string()) -> term())) -> term().
with_temp_file(Suffix, Content, Fun) ->
    TestFile = generate_temp_filename(Suffix),
    create_test_file(TestFile, Content),
    try
        Fun(TestFile)
    after
        delete_test_file(TestFile)
    end.

%% @doc Generate a unique temporary filename
%% Creates a filename based on process ID and timestamp to avoid collisions.
-spec generate_temp_filename() -> string().
generate_temp_filename() ->
    generate_temp_filename("test").

%% @doc Generate a unique temporary filename with custom suffix
%% Useful for creating descriptive test filenames.
-spec generate_temp_filename(string()) -> string().
generate_temp_filename(Suffix) ->
    % Use process ID and unique reference for uniqueness
    Pid = pid_to_list(self()),
    Ref = ref_to_list(make_ref()),
    % Clean up the ref string (remove < > characters)
    CleanRef = lists:filter(fun(C) -> C =/= $< andalso C =/= $> end, Ref),
    "/tmp/topos_test_" ++ Suffix ++ "_" ++ Pid ++ "_" ++ CleanRef ++ ".topos".

%% @doc Create a test file with given content
%% Writes content to a file, failing if the write fails.
-spec create_test_file(string(), iodata()) -> ok.
create_test_file(Filename, Content) ->
    ok = file:write_file(Filename, Content).

%% @doc Delete a test file
%% Removes the test file from the filesystem.
-spec delete_test_file(string()) -> ok | {error, term()}.
delete_test_file(Filename) ->
    file:delete(Filename).
