-module(topos_lexer).
-export([tokenize/1, tokenize_file/1, format_error/1]).
-export([get_max_input_size/0, get_max_nesting_depth/0, get_max_identifier_length/0]).

%% Default resource limits to prevent DoS attacks
%% These can be overridden via application environment:
%%   application:set_env(topos, max_input_size, NewSize)
-define(DEFAULT_MAX_INPUT_SIZE, 10000000).     % 10 MB - reasonable for most source files
-define(DEFAULT_MAX_NESTING_DEPTH, 100).       % 100 levels - prevents stack overflow
-define(DEFAULT_MAX_IDENT_LENGTH, 255).        % 255 chars - standard for many languages

%% @doc Tokenize a string of Topos source code
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

%% @doc Tokenize a file containing Topos source code
-spec tokenize_file(string()) -> {ok, [tuple()]} | {error, term()}.
tokenize_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            tokenize(binary_to_list(Binary));
        {error, Reason} ->
            {error, {0, topos_lexer, {file_error, Reason}}}
    end.

%% @doc Format error reason into human-readable string
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

%% @doc Get maximum input size (configurable via application environment)
%% Default: 10,000,000 bytes (10 MB)
%% Rationale: Large enough for substantial source files, small enough to prevent memory exhaustion
-spec get_max_input_size() -> pos_integer().
get_max_input_size() ->
    application:get_env(topos, max_input_size, ?DEFAULT_MAX_INPUT_SIZE).

%% @doc Get maximum comment nesting depth (configurable via application environment)
%% Default: 100 levels
%% Rationale: Deeply nested comments are rare in practice; limit prevents stack overflow attacks
-spec get_max_nesting_depth() -> pos_integer().
get_max_nesting_depth() ->
    application:get_env(topos, max_nesting_depth, ?DEFAULT_MAX_NESTING_DEPTH).

%% @doc Get maximum identifier length (configurable via application environment)
%% Default: 255 characters
%% Rationale: Matches common language limits (Java, C#); prevents resource exhaustion
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
