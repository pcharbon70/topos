%% Topos Lexer Definition
%% Implements Task 1.1.1: Token Recognition
%%
%% This lexer recognizes all Topos language tokens including:
%% - Keywords (shape, flow, match, etc.)
%% - Operators (|>, ->, :, =, etc.)
%% - Delimiters ({, }, [, ], etc.)
%% - Literals (integers, floats, strings)
%% - Identifiers
%% - Comments (single-line and nested multi-line)

Definitions.

%% Whitespace
WHITESPACE = [\s\t\r\n]+

%% Identifiers and Keywords
LOWER = [a-z]
UPPER = [A-Z]
DIGIT = [0-9]
IDENT_CHAR = [a-zA-Z0-9_']
LOWER_IDENT = {LOWER}{IDENT_CHAR}*
UPPER_IDENT = {UPPER}{IDENT_CHAR}*

%% Numbers
INTEGER = {DIGIT}+
FLOAT = {DIGIT}+\.{DIGIT}+
EXPONENT = [eE][+-]?{DIGIT}+
SCIENTIFIC = ({INTEGER}|{FLOAT}){EXPONENT}

%% String literals
STRING_CHAR = [^"\\]
ESCAPE_SEQ = \\[nrt\\"']
STRING_CONTENT = ({STRING_CHAR}|{ESCAPE_SEQ})*

%% Single-line comment
SINGLE_COMMENT = --[^\n]*

Rules.

%% Whitespace (ignored)
{WHITESPACE} : skip_token.

%% Single-line comments (ignored)
{SINGLE_COMMENT} : skip_token.

%% Multi-line comments with nesting support
\{- : {token, {comment_start, TokenLine}}.
-\} : {token, {comment_end, TokenLine}}.

%% Keywords
shape : {token, {shape, TokenLine}}.
flow : {token, {flow, TokenLine}}.
match : {token, {match, TokenLine}}.
where : {token, {where, TokenLine}}.
let : {token, {'let', TokenLine}}.
in : {token, {'in', TokenLine}}.
do : {token, {'do', TokenLine}}.
end : {token, {'end', TokenLine}}.
if : {token, {'if', TokenLine}}.
then : {token, {'then', TokenLine}}.
else : {token, {'else', TokenLine}}.
case : {token, {'case', TokenLine}}.
of : {token, {'of', TokenLine}}.
when : {token, {'when', TokenLine}}.
module : {token, {'module', TokenLine}}.
import : {token, {'import', TokenLine}}.
export : {token, {'export', TokenLine}}.
exports : {token, {exports, TokenLine}}.
as : {token, {as, TokenLine}}.
qualified : {token, {qualified, TokenLine}}.
private : {token, {private, TokenLine}}.
trait : {token, {trait, TokenLine}}.
instance : {token, {instance, TokenLine}}.
forall : {token, {forall, TokenLine}}.
actor : {token, {actor, TokenLine}}.
supervisor : {token, {supervisor, TokenLine}}.

%% Effect system keywords (Task 1.1.5)
effect : {token, {effect, TokenLine}}.
operation : {token, {operation, TokenLine}}.
perform : {token, {perform, TokenLine}}.
try : {token, {'try', TokenLine}}.
with : {token, {with, TokenLine}}.

%% Two-character operators (must come before single-character)
\|> : {token, {pipe_right, TokenLine}}.
>>= : {token, {bind, TokenLine}}.
-> : {token, {arrow, TokenLine}}.
=> : {token, {double_arrow, TokenLine}}.
<> : {token, {concat, TokenLine}}.
== : {token, {eq, TokenLine}}.
/= : {token, {neq, TokenLine}}.
<= : {token, {lte, TokenLine}}.
>= : {token, {gte, TokenLine}}.
\|\| : {token, {'or', TokenLine}}.
&& : {token, {'and', TokenLine}}.
:: : {token, {cons, TokenLine}}.
<- : {token, {left_arrow, TokenLine}}.
\.\. : {token, {range, TokenLine}}.

%% Single-character operators
: : {token, {colon, TokenLine}}.
= : {token, {equals, TokenLine}}.
\| : {token, {pipe, TokenLine}}.
< : {token, {lt, TokenLine}}.
> : {token, {gt, TokenLine}}.
\+ : {token, {plus, TokenLine}}.
- : {token, {minus, TokenLine}}.
\* : {token, {star, TokenLine}}.
/ : {token, {slash, TokenLine}}.
\. : {token, {dot, TokenLine}}.

%% Delimiters
\{ : {token, {lbrace, TokenLine}}.
\} : {token, {rbrace, TokenLine}}.
\[ : {token, {lbracket, TokenLine}}.
\] : {token, {rbracket, TokenLine}}.
\( : {token, {lparen, TokenLine}}.
\) : {token, {rparen, TokenLine}}.
, : {token, {comma, TokenLine}}.
; : {token, {semicolon, TokenLine}}.
_ : {token, {underscore, TokenLine}}.

%% Numeric literals (scientific notation must come first)
{SCIENTIFIC} : {token, {float, TokenLine, parse_scientific(TokenChars)}}.
{FLOAT} : {token, {float, TokenLine, parse_float(TokenChars)}}.
{INTEGER} : {token, {integer, TokenLine, parse_integer(TokenChars)}}.

%% String literals
"{STRING_CONTENT}" : {token, {string, TokenLine, parse_string(TokenChars)}}.

%% Identifiers (uppercase and lowercase)
{UPPER_IDENT} : validate_identifier(TokenLine, TokenChars, upper_ident).
{LOWER_IDENT} : validate_identifier(TokenLine, TokenChars, lower_ident).

Erlang code.

%% Helper functions for parsing literals

validate_identifier(Line, Chars, Type) ->
    %% Use a fixed maximum identifier length
    MaxLen = 255,
    ActualLen = length(Chars),
    case ActualLen > MaxLen of
        true ->
            {error, {identifier_too_long, Line, ActualLen, MaxLen}};
        false ->
            {token, {Type, Line, Chars}}
    end.

parse_integer(Chars) ->
    list_to_integer(Chars).

parse_float(Chars) ->
    list_to_float(Chars).

parse_scientific(Chars) ->
    %% Erlang's list_to_float requires a decimal point
    %% If the input is like "1e10", convert to "1.0e10"
    case string:chr(Chars, $.) of
        0 ->
            %% No decimal point - add one before 'e' or 'E'
            case lists:splitwith(fun(C) -> C /= $e andalso C /= $E end, Chars) of
                {Base, [$e | Exp]} -> list_to_float(Base ++ ".0e" ++ Exp);
                {Base, [$E | Exp]} -> list_to_float(Base ++ ".0E" ++ Exp);
                _ -> list_to_float(Chars)
            end;
        _ ->
            %% Already has decimal point
            list_to_float(Chars)
    end.

parse_string(Chars) ->
    %% Remove surrounding quotes
    String = lists:sublist(Chars, 2, length(Chars) - 2),
    %% Process escape sequences
    process_escapes(String).

process_escapes(String) ->
    process_escapes(String, []).

process_escapes([], Acc) ->
    lists:reverse(Acc);
process_escapes([$\\, $n | Rest], Acc) ->
    process_escapes(Rest, [$\n | Acc]);
process_escapes([$\\, $r | Rest], Acc) ->
    process_escapes(Rest, [$\r | Acc]);
process_escapes([$\\, $t | Rest], Acc) ->
    process_escapes(Rest, [$\t | Acc]);
process_escapes([$\\, $\\ | Rest], Acc) ->
    process_escapes(Rest, [$\\ | Acc]);
process_escapes([$\\, $\" | Rest], Acc) ->
    process_escapes(Rest, [$\" | Acc]);
process_escapes([$\\, $' | Rest], Acc) ->
    process_escapes(Rest, [$' | Acc]);
process_escapes([C | Rest], Acc) ->
    process_escapes(Rest, [C | Acc]).
