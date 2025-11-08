-module(topos_lexer).
-export([tokenize/1, tokenize_file/1]).

%% @doc Tokenize a string of Topos source code
-spec tokenize(string()) -> {ok, [tuple()]} | {error, term()}.
tokenize(String) ->
    case topos_lexer_gen:string(String) of
        {ok, RawTokens, _} ->
            %% Post-process to handle nested comments
            case filter_comments(RawTokens) of
                {ok, Tokens} ->
                    {ok, Tokens};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, {Line, Module, Error}, _} ->
            {error, {Line, lists:flatten(Module:format_error(Error))}}
    end.

%% @doc Tokenize a file containing Topos source code
-spec tokenize_file(string()) -> {ok, [tuple()]} | {error, term()}.
tokenize_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            tokenize(binary_to_list(Binary));
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc Filter out nested comments from token stream
%% Handles {- ... -} with arbitrary nesting depth
-spec filter_comments([tuple()]) -> {ok, [tuple()]} | {error, term()}.
filter_comments(Tokens) ->
    filter_comments(Tokens, [], 0).

filter_comments([], Acc, 0) ->
    {ok, lists:reverse(Acc)};
filter_comments([], _Acc, Depth) when Depth > 0 ->
    {error, {unclosed_comment, Depth}};
filter_comments([{comment_start, _Line} | Rest], Acc, Depth) ->
    %% Entering a comment block
    filter_comments(Rest, Acc, Depth + 1);
filter_comments([{comment_end, Line} | _Rest], _Acc, 0) ->
    %% Comment end without matching start
    {error, {Line, "unmatched comment end marker -}"}};
filter_comments([{comment_end, _Line} | Rest], Acc, Depth) when Depth > 0 ->
    %% Exiting a comment block
    filter_comments(Rest, Acc, Depth - 1);
filter_comments([Token | Rest], Acc, 0) ->
    %% Not inside a comment - keep the token
    filter_comments(Rest, [Token | Acc], 0);
filter_comments([_Token | Rest], Acc, Depth) when Depth > 0 ->
    %% Inside a comment - discard the token
    filter_comments(Rest, Acc, Depth).
