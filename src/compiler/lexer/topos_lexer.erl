-module(topos_lexer).

%% @doc Wrapper module for the generated lexer to provide the expected API
%% The actual lexer is generated in topos_lexer_gen.erl from topos_lexer.xrl

-export([tokenize/1]).

%% @doc Tokenize input string and return {ok, Tokens}
%% This is a wrapper around the generated lexer's string/1 function
tokenize(String) ->
    case topos_lexer_gen:string(String) of
        {ok, Tokens, _Location} ->
            {ok, Tokens};
        {error, _Reason} = Error ->
            Error;
        Other ->
            {error, Other}
    end.