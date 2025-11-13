%% This file is generated from topos_parser.yrl. Do not edit directly.
-file("src/compiler/parser/topos_parser.yrl", 0).
-module(topos_parser).
-file("src/compiler/parser/topos_parser.erl", 4).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/compiler/parser/topos_parser.yrl", 728).

%% @doc Extract atom from token
%% Delegates to topos_compiler_utils to avoid code duplication
extract_atom(Token) -> topos_compiler_utils:extract_atom(Token).

%% @doc Extract value from token
%% Delegates to topos_compiler_utils to avoid code duplication
extract_value(Token) -> topos_compiler_utils:extract_value(Token).

%% @doc Extract location from token or AST node
%% Delegates to topos_compiler_utils to avoid code duplication
%% Supports both legacy {line, N} format and enhanced {location, ...} format
extract_location(Node) -> topos_compiler_utils:extract_location(Node).

%% @doc Extract flow name from flow signature
extract_flow_name({flow_sig, Name, _Type, _Loc}) -> Name.

%% @doc Extract flow type from flow signature
extract_flow_type({flow_sig, _Name, Type, _Loc}) -> Type.

%% @doc Create an error declaration node for error recovery
%% Returns a special AST node that marks a parsing error
make_error_declaration(Location, Message, _ErrorInfo) ->
    {error_decl,
        Message,
        Location}.

-file("/home/ducky/.asdf/installs/erlang/27.3/lib/parsetools-2.6/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-ifdef (YECC_PARSE_DOC).
-doc ?YECC_PARSE_DOC.
-endif.
-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-ifdef (YECC_PARSE_AND_SCAN_DOC).
-doc ?YECC_PARSE_AND_SCAN_DOC.
-endif.
-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-ifdef (YECC_FORMAT_ERROR_DOC).
-doc ?YECC_FORMAT_ERROR_DOC.
-endif.
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(EndLocation), [],
              {no_func, EndLocation}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Location}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location}).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string1({integer,_,N}) -> io_lib:write(N);
yecctoken2string1({float,_,F}) -> io_lib:write(F);
yecctoken2string1({char,_,C}) -> io_lib:write_char(C);
yecctoken2string1({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string1({string,_,S}) -> io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) -> "'.'";
yecctoken2string1({'$end', _}) -> [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/compiler/parser/topos_parser.erl", 222).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function,  yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function,  yeccpars2_0/7}).
yeccpars2_0(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function,  yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function,  yeccpars2_2/7}).
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function,  yeccpars2_3/7}).
yeccpars2_3(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_flow_decl(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function,  yeccpars2_4/7}).
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function,  yeccpars2_5/7}).
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_topos_module(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function,  yeccpars2_7/7}).
yeccpars2_7(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function,  yeccpars2_10/7}).
yeccpars2_10(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function,  yeccpars2_11/7}).
yeccpars2_11(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_shape_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccpars2_15(15, Cat, [13 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_type_params(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function,  yeccpars2_15/7}).
yeccpars2_15(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_16(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_type_params_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_type_params_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_shape_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_shape_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function,  yeccpars2_21/7}).
yeccpars2_21(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_constructors(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_constructor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_constructor_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_constructor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_27(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_type_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_type_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function,  yeccpars2_32/7}).
yeccpars2_32(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2_36(36, Cat, [33 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
-compile({nowarn_unused_function,  yeccpars2_34/7}).
yeccpars2_34(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function,  yeccpars2_36/7}).
yeccpars2_36(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_37: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function,  yeccpars2_38/7}).
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_39: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_41: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_type_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function,  yeccpars2_44/7}).
yeccpars2_44(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_type_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function,  yeccpars2_45/7}).
yeccpars2_45(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function,  yeccpars2_48/7}).
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function,  yeccpars2_49/7}).
yeccpars2_49(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_effect_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function,  yeccpars2_50/7}).
yeccpars2_50(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function,  yeccpars2_51/7}).
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_effect_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function,  yeccpars2_52/7}).
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_53/7}).
-compile({nowarn_unused_function,  yeccpars2_53/7}).
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function,  yeccpars2_54/7}).
yeccpars2_54(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function,  yeccpars2_55/7}).
yeccpars2_55(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_type_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_56/7}).
-compile({nowarn_unused_function,  yeccpars2_56/7}).
yeccpars2_56(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_57/7}).
-compile({nowarn_unused_function,  yeccpars2_57/7}).
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_58: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_59/7}).
-compile({nowarn_unused_function,  yeccpars2_59/7}).
yeccpars2_59(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_type_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
-compile({nowarn_unused_function,  yeccpars2_60/7}).
yeccpars2_60(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_61/7}).
-compile({nowarn_unused_function,  yeccpars2_61/7}).
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_type_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function,  yeccpars2_62/7}).
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_63/7}).
-compile({nowarn_unused_function,  yeccpars2_63/7}).
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_constructor_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_64: see yeccpars2_18

-dialyzer({nowarn_function, yeccpars2_65/7}).
-compile({nowarn_unused_function,  yeccpars2_65/7}).
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_constructors(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_66: see yeccpars2_18

-dialyzer({nowarn_function, yeccpars2_67/7}).
-compile({nowarn_unused_function,  yeccpars2_67/7}).
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_shape_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function,  yeccpars2_68/7}).
yeccpars2_68(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_69/7}).
-compile({nowarn_unused_function,  yeccpars2_69/7}).
yeccpars2_69(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2_71(71, Cat, [69 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_70/7}).
-compile({nowarn_unused_function,  yeccpars2_70/7}).
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_pattern_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function,  yeccpars2_71/7}).
yeccpars2_71(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, 'error', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_72/7}).
-compile({nowarn_unused_function,  yeccpars2_72/7}).
yeccpars2_72(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_73: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function,  yeccpars2_74/7}).
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_75/7}).
-compile({nowarn_unused_function,  yeccpars2_75/7}).
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_76/7}).
-compile({nowarn_unused_function,  yeccpars2_76/7}).
yeccpars2_76(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_77(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_78/7}).
-compile({nowarn_unused_function,  yeccpars2_78/7}).
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function,  yeccpars2_79/7}).
yeccpars2_79(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_80/7}).
-compile({nowarn_unused_function,  yeccpars2_80/7}).
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).
-compile({nowarn_unused_function,  yeccpars2_81/7}).
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_82/7}).
-compile({nowarn_unused_function,  yeccpars2_82/7}).
yeccpars2_82(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_83/7}).
-compile({nowarn_unused_function,  yeccpars2_83/7}).
yeccpars2_83(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2_84(84, Cat, [83 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
-compile({nowarn_unused_function,  yeccpars2_84/7}).
yeccpars2_84(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_85/7}).
-compile({nowarn_unused_function,  yeccpars2_85/7}).
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).
-compile({nowarn_unused_function,  yeccpars2_86/7}).
yeccpars2_86(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_87/7}).
-compile({nowarn_unused_function,  yeccpars2_87/7}).
yeccpars2_87(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_88: see yeccpars2_79

-dialyzer({nowarn_function, yeccpars2_89/7}).
-compile({nowarn_unused_function,  yeccpars2_89/7}).
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_90/7}).
-compile({nowarn_unused_function,  yeccpars2_90/7}).
yeccpars2_90(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_91/7}).
-compile({nowarn_unused_function,  yeccpars2_91/7}).
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_92/7}).
-compile({nowarn_unused_function,  yeccpars2_92/7}).
yeccpars2_92(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_93/7}).
-compile({nowarn_unused_function,  yeccpars2_93/7}).
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function,  yeccpars2_94/7}).
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_95/7}).
-compile({nowarn_unused_function,  yeccpars2_95/7}).
yeccpars2_95(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function,  yeccpars2_96/7}).
yeccpars2_96(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_record_pattern_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
-compile({nowarn_unused_function,  yeccpars2_97/7}).
yeccpars2_97(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_98/7}).
-compile({nowarn_unused_function,  yeccpars2_98/7}).
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_79

-dialyzer({nowarn_function, yeccpars2_100/7}).
-compile({nowarn_unused_function,  yeccpars2_100/7}).
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_record_pattern_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
-compile({nowarn_unused_function,  yeccpars2_101/7}).
yeccpars2_101(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_102/7}).
-compile({nowarn_unused_function,  yeccpars2_102/7}).
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_record_pattern_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).
-compile({nowarn_unused_function,  yeccpars2_103/7}).
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_104/7}).
-compile({nowarn_unused_function,  yeccpars2_104/7}).
yeccpars2_104(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_flow_signature(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_105/7}).
-compile({nowarn_unused_function,  yeccpars2_105/7}).
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function,  yeccpars2_107/7}).
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
-compile({nowarn_unused_function,  yeccpars2_108/7}).
yeccpars2_108(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function,  yeccpars2_109/7}).
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).
-compile({nowarn_unused_function,  yeccpars2_110/7}).
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
-compile({nowarn_unused_function,  yeccpars2_111/7}).
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_112/7}).
-compile({nowarn_unused_function,  yeccpars2_112/7}).
yeccpars2_112(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_113/7}).
-compile({nowarn_unused_function,  yeccpars2_113/7}).
yeccpars2_113(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_guards(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_114/7}).
-compile({nowarn_unused_function,  yeccpars2_114/7}).
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_115/7}).
-compile({nowarn_unused_function,  yeccpars2_115/7}).
yeccpars2_115(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
-compile({nowarn_unused_function,  yeccpars2_116/7}).
yeccpars2_116(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_guard(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_117/7}).
-compile({nowarn_unused_function,  yeccpars2_117/7}).
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_118: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_119/7}).
-compile({nowarn_unused_function,  yeccpars2_119/7}).
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_120/7}).
-compile({nowarn_unused_function,  yeccpars2_120/7}).
yeccpars2_120(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_121(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function,  yeccpars2_122/7}).
yeccpars2_122(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_123/7}).
-compile({nowarn_unused_function,  yeccpars2_123/7}).
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_124: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_125/7}).
-compile({nowarn_unused_function,  yeccpars2_125/7}).
yeccpars2_125(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_126/7}).
-compile({nowarn_unused_function,  yeccpars2_126/7}).
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_127: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_128/7}).
-compile({nowarn_unused_function,  yeccpars2_128/7}).
yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_128_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_129(S, 'with', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_129(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_129/7}).
-compile({nowarn_unused_function,  yeccpars2_129/7}).
yeccpars2_cont_129(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_130: see yeccpars2_108

%% yeccpars2_131: see yeccpars2_108

%% yeccpars2_132: see yeccpars2_108

%% yeccpars2_133: see yeccpars2_108

%% yeccpars2_134: see yeccpars2_108

%% yeccpars2_135: see yeccpars2_108

%% yeccpars2_136: see yeccpars2_108

%% yeccpars2_137: see yeccpars2_108

%% yeccpars2_138: see yeccpars2_108

%% yeccpars2_139: see yeccpars2_108

%% yeccpars2_140: see yeccpars2_108

%% yeccpars2_141: see yeccpars2_108

%% yeccpars2_142: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_143/7}).
-compile({nowarn_unused_function,  yeccpars2_143/7}).
yeccpars2_143(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_144/7}).
-compile({nowarn_unused_function,  yeccpars2_144/7}).
yeccpars2_144(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_145/7}).
-compile({nowarn_unused_function,  yeccpars2_145/7}).
yeccpars2_145(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_handler_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function,  yeccpars2_146/7}).
yeccpars2_146(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_147/7}).
-compile({nowarn_unused_function,  yeccpars2_147/7}).
yeccpars2_147(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function,  yeccpars2_148/7}).
yeccpars2_148(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_149/7}).
-compile({nowarn_unused_function,  yeccpars2_149/7}).
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_operation_cases(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_150/7}).
-compile({nowarn_unused_function,  yeccpars2_150/7}).
yeccpars2_150(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_151: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function,  yeccpars2_152/7}).
yeccpars2_152(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccpars2_153(153, Cat, [152 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_153/7}).
-compile({nowarn_unused_function,  yeccpars2_153/7}).
yeccpars2_153(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function,  yeccpars2_154/7}).
yeccpars2_154(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_pattern_list_comma(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function,  yeccpars2_155/7}).
yeccpars2_155(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccpars2_156(_S, Cat, [155 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_156/7}).
-compile({nowarn_unused_function,  yeccpars2_156/7}).
yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_pattern_list_comma(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_157/7}).
-compile({nowarn_unused_function,  yeccpars2_157/7}).
yeccpars2_157(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_158: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_159/7}).
-compile({nowarn_unused_function,  yeccpars2_159/7}).
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_160/7}).
-compile({nowarn_unused_function,  yeccpars2_160/7}).
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_operation_case(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_161/7}).
-compile({nowarn_unused_function,  yeccpars2_161/7}).
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_operation_cases(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_162/7}).
-compile({nowarn_unused_function,  yeccpars2_162/7}).
yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_162_(Stack),
 yeccgoto_handler_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_163/7}).
-compile({nowarn_unused_function,  yeccpars2_163/7}).
yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_handler_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_164/7}).
-compile({nowarn_unused_function,  yeccpars2_164/7}).
yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_try_with_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_165/7}).
-compile({nowarn_unused_function,  yeccpars2_165/7}).
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
-compile({nowarn_unused_function,  yeccpars2_166/7}).
yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_167/7}).
-compile({nowarn_unused_function,  yeccpars2_167/7}).
yeccpars2_167(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_168/7}).
-compile({nowarn_unused_function,  yeccpars2_168/7}).
yeccpars2_168(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_169/7}).
-compile({nowarn_unused_function,  yeccpars2_169/7}).
yeccpars2_169(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_169_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_try(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_S, 'with', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_with(Stack),
 yeccgoto_expr(hd(Nss), 'with', Nss, NewStack, T, Ts, Tzr);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_170/7}).
-compile({nowarn_unused_function,  yeccpars2_170/7}).
yeccpars2_170(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_171/7}).
-compile({nowarn_unused_function,  yeccpars2_171/7}).
yeccpars2_171(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_171_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_try(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_S, 'with', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_with(Stack),
 yeccgoto_expr(hd(Nss), 'with', Nss, NewStack, T, Ts, Tzr);
yeccpars2_171(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_172/7}).
-compile({nowarn_unused_function,  yeccpars2_172/7}).
yeccpars2_172(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_172_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_try(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_S, 'with', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_with(Stack),
 yeccgoto_expr(hd(Nss), 'with', Nss, NewStack, T, Ts, Tzr);
yeccpars2_172(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_173/7}).
-compile({nowarn_unused_function,  yeccpars2_173/7}).
yeccpars2_173(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_173_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_try(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_S, 'with', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_with(Stack),
 yeccgoto_expr(hd(Nss), 'with', Nss, NewStack, T, Ts, Tzr);
yeccpars2_173(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_174/7}).
-compile({nowarn_unused_function,  yeccpars2_174/7}).
yeccpars2_174(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_174_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_try(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_S, 'with', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_174_with(Stack),
 yeccgoto_expr(hd(Nss), 'with', Nss, NewStack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_175/7}).
-compile({nowarn_unused_function,  yeccpars2_175/7}).
yeccpars2_175(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_175_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'effect', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_effect(Stack),
 yeccgoto_expr(hd(Nss), 'effect', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'error', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_error(Stack),
 yeccgoto_expr(hd(Nss), 'error', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_perform(Stack),
 yeccgoto_expr(hd(Nss), 'perform', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_try(Stack),
 yeccgoto_expr(hd(Nss), 'try', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_S, 'with', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_with(Stack),
 yeccgoto_expr(hd(Nss), 'with', Nss, NewStack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_176/7}).
-compile({nowarn_unused_function,  yeccpars2_176/7}).
yeccpars2_176(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_177/7}).
-compile({nowarn_unused_function,  yeccpars2_177/7}).
yeccpars2_177(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_177_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_178/7}).
-compile({nowarn_unused_function,  yeccpars2_178/7}).
yeccpars2_178(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_179/7}).
-compile({nowarn_unused_function,  yeccpars2_179/7}).
yeccpars2_179(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_180/7}).
-compile({nowarn_unused_function,  yeccpars2_180/7}).
yeccpars2_180(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_181/7}).
-compile({nowarn_unused_function,  yeccpars2_181/7}).
yeccpars2_181(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'perform', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccpars2_182(182, Cat, [181 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_182/7}).
-compile({nowarn_unused_function,  yeccpars2_182/7}).
yeccpars2_182(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_183/7}).
-compile({nowarn_unused_function,  yeccpars2_183/7}).
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_expr_list_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_184/7}).
-compile({nowarn_unused_function,  yeccpars2_184/7}).
yeccpars2_184(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_185: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_186/7}).
-compile({nowarn_unused_function,  yeccpars2_186/7}).
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_187/7}).
-compile({nowarn_unused_function,  yeccpars2_187/7}).
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_187_(Stack),
 yeccgoto_perform_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_188/7}).
-compile({nowarn_unused_function,  yeccpars2_188/7}).
yeccpars2_188(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_189(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_129(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_190: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_191/7}).
-compile({nowarn_unused_function,  yeccpars2_191/7}).
yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_191_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_192/7}).
-compile({nowarn_unused_function,  yeccpars2_192/7}).
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_193/7}).
-compile({nowarn_unused_function,  yeccpars2_193/7}).
yeccpars2_193(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_194/7}).
-compile({nowarn_unused_function,  yeccpars2_194/7}).
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_195/7}).
-compile({nowarn_unused_function,  yeccpars2_195/7}).
yeccpars2_195(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_196: see yeccpars2_108

yeccpars2_197(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_129(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_198: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_199/7}).
-compile({nowarn_unused_function,  yeccpars2_199/7}).
yeccpars2_199(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_200/7}).
-compile({nowarn_unused_function,  yeccpars2_200/7}).
yeccpars2_200(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_201/7}).
-compile({nowarn_unused_function,  yeccpars2_201/7}).
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_202/7}).
-compile({nowarn_unused_function,  yeccpars2_202/7}).
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_203/7}).
-compile({nowarn_unused_function,  yeccpars2_203/7}).
yeccpars2_203(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_204/7}).
-compile({nowarn_unused_function,  yeccpars2_204/7}).
yeccpars2_204(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_205/7}).
-compile({nowarn_unused_function,  yeccpars2_205/7}).
yeccpars2_205(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_206/7}).
-compile({nowarn_unused_function,  yeccpars2_206/7}).
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_207: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_208/7}).
-compile({nowarn_unused_function,  yeccpars2_208/7}).
yeccpars2_208(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_209/7}).
-compile({nowarn_unused_function,  yeccpars2_209/7}).
yeccpars2_209(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_210/7}).
-compile({nowarn_unused_function,  yeccpars2_210/7}).
yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_211/7}).
-compile({nowarn_unused_function,  yeccpars2_211/7}).
yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_129(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_213: see yeccpars2_108

yeccpars2_214(S, 'else', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_129(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_215: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_216/7}).
-compile({nowarn_unused_function,  yeccpars2_216/7}).
yeccpars2_216(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_217/7}).
-compile({nowarn_unused_function,  yeccpars2_217/7}).
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_218/7}).
-compile({nowarn_unused_function,  yeccpars2_218/7}).
yeccpars2_218(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_219/7}).
-compile({nowarn_unused_function,  yeccpars2_219/7}).
yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_220: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_221/7}).
-compile({nowarn_unused_function,  yeccpars2_221/7}).
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_guards(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_222: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_223/7}).
-compile({nowarn_unused_function,  yeccpars2_223/7}).
yeccpars2_223(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_224/7}).
-compile({nowarn_unused_function,  yeccpars2_224/7}).
yeccpars2_224(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
-compile({nowarn_unused_function,  yeccpars2_225/7}).
yeccpars2_225(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_226/7}).
-compile({nowarn_unused_function,  yeccpars2_226/7}).
yeccpars2_226(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_227/7}).
-compile({nowarn_unused_function,  yeccpars2_227/7}).
yeccpars2_227(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_match_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_228: see yeccpars2_79

-dialyzer({nowarn_function, yeccpars2_229/7}).
-compile({nowarn_unused_function,  yeccpars2_229/7}).
yeccpars2_229(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_230: see yeccpars2_108

%% yeccpars2_231: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_232/7}).
-compile({nowarn_unused_function,  yeccpars2_232/7}).
yeccpars2_232(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_233: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_234/7}).
-compile({nowarn_unused_function,  yeccpars2_234/7}).
yeccpars2_234(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_235/7}).
-compile({nowarn_unused_function,  yeccpars2_235/7}).
yeccpars2_235(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_236/7}).
-compile({nowarn_unused_function,  yeccpars2_236/7}).
yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_match_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_237/7}).
-compile({nowarn_unused_function,  yeccpars2_237/7}).
yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_238: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_239/7}).
-compile({nowarn_unused_function,  yeccpars2_239/7}).
yeccpars2_239(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_240/7}).
-compile({nowarn_unused_function,  yeccpars2_240/7}).
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_241/7}).
-compile({nowarn_unused_function,  yeccpars2_241/7}).
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_242/7}).
-compile({nowarn_unused_function,  yeccpars2_242/7}).
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_243/7}).
-compile({nowarn_unused_function,  yeccpars2_243/7}).
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_244/7}).
-compile({nowarn_unused_function,  yeccpars2_244/7}).
yeccpars2_244(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_244_(Stack),
 yeccpars2_245(245, Cat, [244 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_245/7}).
-compile({nowarn_unused_function,  yeccpars2_245/7}).
yeccpars2_245(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_246/7}).
-compile({nowarn_unused_function,  yeccpars2_246/7}).
yeccpars2_246(S, 'operation', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_246_(Stack),
 yeccpars2_251(_S, Cat, [246 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_247/7}).
-compile({nowarn_unused_function,  yeccpars2_247/7}).
yeccpars2_247(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_248/7}).
-compile({nowarn_unused_function,  yeccpars2_248/7}).
yeccpars2_248(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_248_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_249: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_250/7}).
-compile({nowarn_unused_function,  yeccpars2_250/7}).
yeccpars2_250(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_effect_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_251/7}).
-compile({nowarn_unused_function,  yeccpars2_251/7}).
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_251_(Stack),
 yeccgoto_effect_operations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_252/7}).
-compile({nowarn_unused_function,  yeccpars2_252/7}).
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_252_(Stack),
 yeccgoto_effect_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_253/7}).
-compile({nowarn_unused_function,  yeccpars2_253/7}).
yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_254/7}).
-compile({nowarn_unused_function,  yeccpars2_254/7}).
yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_255/7}).
-compile({nowarn_unused_function,  yeccpars2_255/7}).
yeccpars2_255(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_255_(Stack),
 yeccgoto_flow_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_256/7}).
-compile({nowarn_unused_function,  yeccpars2_256/7}).
yeccpars2_256(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_257/7}).
-compile({nowarn_unused_function,  yeccpars2_257/7}).
yeccpars2_257(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_257_(Stack),
 yeccpars2_258(258, Cat, [257 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_258/7}).
-compile({nowarn_unused_function,  yeccpars2_258/7}).
yeccpars2_258(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_259(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_260: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_261/7}).
-compile({nowarn_unused_function,  yeccpars2_261/7}).
yeccpars2_261(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_262: see yeccpars2_108

-dialyzer({nowarn_function, yeccpars2_263/7}).
-compile({nowarn_unused_function,  yeccpars2_263/7}).
yeccpars2_263(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_263_(Stack),
 yeccgoto_flow_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_264/7}).
-compile({nowarn_unused_function,  yeccpars2_264/7}).
yeccpars2_264(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_flow_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_265: see yeccpars2_225

-dialyzer({nowarn_function, yeccpars2_266/7}).
-compile({nowarn_unused_function,  yeccpars2_266/7}).
yeccpars2_266(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_267/7}).
-compile({nowarn_unused_function,  yeccpars2_267/7}).
yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_267_(Stack),
 yeccgoto_flow_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_268/7}).
-compile({nowarn_unused_function,  yeccpars2_268/7}).
yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_268_(Stack),
 yeccgoto_flow_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor/7}).
yeccgoto_constructor(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor_fields/7}).
yeccgoto_constructor_fields(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor_fields(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructors/7}).
-compile({nowarn_unused_function,  yeccgoto_constructors/7}).
yeccgoto_constructors(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declaration/7}).
-compile({nowarn_unused_function,  yeccgoto_declaration/7}).
yeccgoto_declaration(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarations/7}).
-compile({nowarn_unused_function,  yeccgoto_declarations/7}).
yeccgoto_declarations(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_decl/7}).
yeccgoto_effect_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_list_nonempty/7}).
yeccgoto_effect_list_nonempty(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_list_nonempty(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operation/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operation/7}).
yeccgoto_effect_operation(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operation(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_effect_operations/7}).
-compile({nowarn_unused_function,  yeccgoto_effect_operations/7}).
yeccgoto_effect_operations(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(245, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_effect_operations(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_expr/7}).
yeccgoto_expr(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(224, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(172, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(171, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(169, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(167, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(197, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(199, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(222, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(233, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(259, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(260, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_app/7}).
yeccgoto_expr_app(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(133, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(142, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(215, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(222, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(233, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(259, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(260, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list/7}).
yeccgoto_expr_list(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list_opt/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list_opt/7}).
yeccgoto_expr_list_opt(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(182, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_primary/7}).
yeccgoto_expr_primary(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(233=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_clause/7}).
yeccgoto_flow_clause(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(255, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_clause(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(255, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_clauses/7}).
yeccgoto_flow_clauses(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_clauses(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_decl/7}).
yeccgoto_flow_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_signature/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_signature/7}).
yeccgoto_flow_signature(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_signature(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guard/7}).
-compile({nowarn_unused_function,  yeccgoto_guard/7}).
yeccgoto_guard(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(260, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guards/7}).
-compile({nowarn_unused_function,  yeccgoto_guards/7}).
yeccgoto_guards(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(260, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(261, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clause/7}).
yeccgoto_handler_clause(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clause(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_handler_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_handler_clauses/7}).
yeccgoto_handler_clauses(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_handler_clauses(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
-compile({nowarn_unused_function,  yeccgoto_literal/7}).
yeccgoto_literal(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(233=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clause/7}).
yeccgoto_match_clause(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clauses/7}).
yeccgoto_match_clauses(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(227=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(266, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_case/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_case/7}).
yeccgoto_operation_case(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_operation_case(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_cases/7}).
-compile({nowarn_unused_function,  yeccgoto_operation_cases/7}).
yeccgoto_operation_cases(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern/7}).
yeccgoto_pattern(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(155, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(228, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list/7}).
yeccgoto_pattern_list(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(258, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_comma/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_comma/7}).
yeccgoto_pattern_list_comma(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_comma(155=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_nonempty/7}).
yeccgoto_pattern_list_nonempty(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_perform_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_perform_expr/7}).
yeccgoto_perform_expr(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(233=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_perform_expr(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_field/7}).
yeccgoto_record_field(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_field(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_fields/7}).
yeccgoto_record_fields(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_fields(209=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_field/7}).
yeccgoto_record_pattern_field(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_field(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_fields/7}).
yeccgoto_record_pattern_fields(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_fields(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_shape_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_shape_decl/7}).
yeccgoto_shape_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shape_decl(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_topos_module/7}).
-compile({nowarn_unused_function,  yeccgoto_topos_module/7}).
yeccgoto_topos_module(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_try_with_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_try_with_expr/7}).
yeccgoto_try_with_expr(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(133=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(135=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(139=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(151=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(233=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_with_expr(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_expr_list/7}).
yeccgoto_tuple_expr_list(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_expr_list(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_pattern_list/7}).
yeccgoto_tuple_pattern_list(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_pattern_list(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr/7}).
yeccgoto_type_expr(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(250, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_app/7}).
yeccgoto_type_expr_app(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary/7}).
yeccgoto_type_expr_primary(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(249=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_list/7}).
yeccgoto_type_list(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_list(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params/7}).
yeccgoto_type_params(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params_nonempty/7}).
yeccgoto_type_params_nonempty(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_field/7}).
yeccgoto_type_record_field(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_field(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_fields/7}).
yeccgoto_type_record_fields(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_fields(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("src/compiler/parser/topos_parser.yrl", 179).
yeccpars2_2_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            ___1
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-dialyzer({nowarn_function, yeccpars2_3_/1}).
-compile({nowarn_unused_function,  yeccpars2_3_/1}).
-file("src/compiler/parser/topos_parser.yrl", 286).
yeccpars2_3_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {flow_decl,
        extract_flow_name(___1),
        extract_flow_type(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("src/compiler/parser/topos_parser.yrl", 180).
yeccpars2_4_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function,  yeccpars2_5_/1}).
-file("src/compiler/parser/topos_parser.yrl", 181).
yeccpars2_5_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("src/compiler/parser/topos_parser.yrl", 171).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              
    {module, undefined, [], [], ___1, {line, 1}}
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-dialyzer({nowarn_function, yeccpars2_7_/1}).
-compile({nowarn_unused_function,  yeccpars2_7_/1}).
-file("src/compiler/parser/topos_parser.yrl", 174).
yeccpars2_7_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function,  yeccpars2_12_/1}).
-file("src/compiler/parser/topos_parser.yrl", 208).
yeccpars2_12_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___1), "Incomplete shape declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function,  yeccpars2_13_/1}).
-file("src/compiler/parser/topos_parser.yrl", 211).
yeccpars2_13_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_14_/1}).
-dialyzer({nowarn_function, yeccpars2_14_/1}).
-compile({nowarn_unused_function,  yeccpars2_14_/1}).
-file("src/compiler/parser/topos_parser.yrl", 213).
yeccpars2_14_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    ___1
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-dialyzer({nowarn_function, yeccpars2_16_/1}).
-compile({nowarn_unused_function,  yeccpars2_16_/1}).
-file("src/compiler/parser/topos_parser.yrl", 216).
yeccpars2_16_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-dialyzer({nowarn_function, yeccpars2_17_/1}).
-compile({nowarn_unused_function,  yeccpars2_17_/1}).
-file("src/compiler/parser/topos_parser.yrl", 218).
yeccpars2_17_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    [extract_atom(___1) | ___2]
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-dialyzer({nowarn_function, yeccpars2_19_/1}).
-compile({nowarn_unused_function,  yeccpars2_19_/1}).
-file("src/compiler/parser/topos_parser.yrl", 206).
yeccpars2_19_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    make_error_declaration(extract_location(___1), "Missing '=' or constructors in shape declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-dialyzer({nowarn_function, yeccpars2_20_/1}).
-compile({nowarn_unused_function,  yeccpars2_20_/1}).
-file("src/compiler/parser/topos_parser.yrl", 195).
yeccpars2_20_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    {shape_decl,
        extract_atom(___2),
        ___3,
        ___5,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-dialyzer({nowarn_function, yeccpars2_21_/1}).
-compile({nowarn_unused_function,  yeccpars2_21_/1}).
-file("src/compiler/parser/topos_parser.yrl", 221).
yeccpars2_21_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-dialyzer({nowarn_function, yeccpars2_22_/1}).
-compile({nowarn_unused_function,  yeccpars2_22_/1}).
-file("src/compiler/parser/topos_parser.yrl", 226).
yeccpars2_22_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {constructor,
        extract_atom(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("src/compiler/parser/topos_parser.yrl", 238).
yeccpars2_23_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-dialyzer({nowarn_function, yeccpars2_24_/1}).
-compile({nowarn_unused_function,  yeccpars2_24_/1}).
-file("src/compiler/parser/topos_parser.yrl", 232).
yeccpars2_24_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {constructor,
        extract_atom(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function,  yeccpars2_26_/1}).
-file("src/compiler/parser/topos_parser.yrl", 677).
yeccpars2_26_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-dialyzer({nowarn_function, yeccpars2_28_/1}).
-compile({nowarn_unused_function,  yeccpars2_28_/1}).
-file("src/compiler/parser/topos_parser.yrl", 680).
yeccpars2_28_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("src/compiler/parser/topos_parser.yrl", 673).
yeccpars2_30_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                    
    ___1
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-dialyzer({nowarn_function, yeccpars2_31_/1}).
-compile({nowarn_unused_function,  yeccpars2_31_/1}).
-file("src/compiler/parser/topos_parser.yrl", 664).
yeccpars2_31_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function,  yeccpars2_33_/1}).
-file("src/compiler/parser/topos_parser.yrl", 211).
yeccpars2_33_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_34_/1}).
-dialyzer({nowarn_function, yeccpars2_34_/1}).
-compile({nowarn_unused_function,  yeccpars2_34_/1}).
-file("src/compiler/parser/topos_parser.yrl", 680).
yeccpars2_34_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("src/compiler/parser/topos_parser.yrl", 667).
yeccpars2_35_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {type_app,
        {type_con, extract_atom(___1), extract_location(___1)},
        [___2],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-dialyzer({nowarn_function, yeccpars2_38_/1}).
-compile({nowarn_unused_function,  yeccpars2_38_/1}).
-file("src/compiler/parser/topos_parser.yrl", 655).
yeccpars2_38_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {type_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function,  yeccpars2_40_/1}).
-file("src/compiler/parser/topos_parser.yrl", 652).
yeccpars2_40_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {type_fun, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function,  yeccpars2_42_/1}).
-file("src/compiler/parser/topos_parser.yrl", 683).
yeccpars2_42_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    ___2
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("src/compiler/parser/topos_parser.yrl", 697).
yeccpars2_43_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function,  yeccpars2_44_/1}).
-file("src/compiler/parser/topos_parser.yrl", 695).
yeccpars2_44_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function,  yeccpars2_48_/1}).
-file("src/compiler/parser/topos_parser.yrl", 658).
yeccpars2_48_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {type_effect, ___1, [], extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function,  yeccpars2_49_/1}).
-file("src/compiler/parser/topos_parser.yrl", 715).
yeccpars2_49_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-dialyzer({nowarn_function, yeccpars2_51_/1}).
-compile({nowarn_unused_function,  yeccpars2_51_/1}).
-file("src/compiler/parser/topos_parser.yrl", 717).
yeccpars2_51_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                
    [extract_atom(___1) | ___3]
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-dialyzer({nowarn_function, yeccpars2_52_/1}).
-compile({nowarn_unused_function,  yeccpars2_52_/1}).
-file("src/compiler/parser/topos_parser.yrl", 661).
yeccpars2_52_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     
    {type_effect, ___1, ___4, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-dialyzer({nowarn_function, yeccpars2_53_/1}).
-compile({nowarn_unused_function,  yeccpars2_53_/1}).
-file("src/compiler/parser/topos_parser.yrl", 686).
yeccpars2_53_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    {type_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-dialyzer({nowarn_function, yeccpars2_55_/1}).
-compile({nowarn_unused_function,  yeccpars2_55_/1}).
-file("src/compiler/parser/topos_parser.yrl", 701).
yeccpars2_55_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-dialyzer({nowarn_function, yeccpars2_57_/1}).
-compile({nowarn_unused_function,  yeccpars2_57_/1}).
-file("src/compiler/parser/topos_parser.yrl", 689).
yeccpars2_57_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    {type_record, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-dialyzer({nowarn_function, yeccpars2_59_/1}).
-compile({nowarn_unused_function,  yeccpars2_59_/1}).
-file("src/compiler/parser/topos_parser.yrl", 706).
yeccpars2_59_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-dialyzer({nowarn_function, yeccpars2_61_/1}).
-compile({nowarn_unused_function,  yeccpars2_61_/1}).
-file("src/compiler/parser/topos_parser.yrl", 703).
yeccpars2_61_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function,  yeccpars2_62_/1}).
-file("src/compiler/parser/topos_parser.yrl", 692).
yeccpars2_62_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {type_record, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-dialyzer({nowarn_function, yeccpars2_63_/1}).
-compile({nowarn_unused_function,  yeccpars2_63_/1}).
-file("src/compiler/parser/topos_parser.yrl", 240).
yeccpars2_63_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-dialyzer({nowarn_function, yeccpars2_65_/1}).
-compile({nowarn_unused_function,  yeccpars2_65_/1}).
-file("src/compiler/parser/topos_parser.yrl", 223).
yeccpars2_65_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-dialyzer({nowarn_function, yeccpars2_67_/1}).
-compile({nowarn_unused_function,  yeccpars2_67_/1}).
-file("src/compiler/parser/topos_parser.yrl", 204).
yeccpars2_67_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    make_error_declaration(extract_location(___1), "Invalid shape name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-dialyzer({nowarn_function, yeccpars2_68_/1}).
-compile({nowarn_unused_function,  yeccpars2_68_/1}).
-file("src/compiler/parser/topos_parser.yrl", 320).
yeccpars2_68_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                         
    make_error_declaration(extract_location(___1), "Incomplete flow declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-dialyzer({nowarn_function, yeccpars2_69_/1}).
-compile({nowarn_unused_function,  yeccpars2_69_/1}).
-file("src/compiler/parser/topos_parser.yrl", 379).
yeccpars2_69_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_70_/1}).
-dialyzer({nowarn_function, yeccpars2_70_/1}).
-compile({nowarn_unused_function,  yeccpars2_70_/1}).
-file("src/compiler/parser/topos_parser.yrl", 381).
yeccpars2_70_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-dialyzer({nowarn_function, yeccpars2_72_/1}).
-compile({nowarn_unused_function,  yeccpars2_72_/1}).
-file("src/compiler/parser/topos_parser.yrl", 384).
yeccpars2_72_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-dialyzer({nowarn_function, yeccpars2_74_/1}).
-compile({nowarn_unused_function,  yeccpars2_74_/1}).
-file("src/compiler/parser/topos_parser.yrl", 412).
yeccpars2_74_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-dialyzer({nowarn_function, yeccpars2_75_/1}).
-compile({nowarn_unused_function,  yeccpars2_75_/1}).
-file("src/compiler/parser/topos_parser.yrl", 409).
yeccpars2_75_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-dialyzer({nowarn_function, yeccpars2_78_/1}).
-compile({nowarn_unused_function,  yeccpars2_78_/1}).
-file("src/compiler/parser/topos_parser.yrl", 397).
yeccpars2_78_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-dialyzer({nowarn_function, yeccpars2_80_/1}).
-compile({nowarn_unused_function,  yeccpars2_80_/1}).
-file("src/compiler/parser/topos_parser.yrl", 415).
yeccpars2_80_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-dialyzer({nowarn_function, yeccpars2_81_/1}).
-compile({nowarn_unused_function,  yeccpars2_81_/1}).
-file("src/compiler/parser/topos_parser.yrl", 400).
yeccpars2_81_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-dialyzer({nowarn_function, yeccpars2_82_/1}).
-compile({nowarn_unused_function,  yeccpars2_82_/1}).
-file("src/compiler/parser/topos_parser.yrl", 403).
yeccpars2_82_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_constructor, extract_atom(___1), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-dialyzer({nowarn_function, yeccpars2_83_/1}).
-compile({nowarn_unused_function,  yeccpars2_83_/1}).
-file("src/compiler/parser/topos_parser.yrl", 379).
yeccpars2_83_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_85_/1}).
-dialyzer({nowarn_function, yeccpars2_85_/1}).
-compile({nowarn_unused_function,  yeccpars2_85_/1}).
-file("src/compiler/parser/topos_parser.yrl", 406).
yeccpars2_85_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_constructor, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-dialyzer({nowarn_function, yeccpars2_89_/1}).
-compile({nowarn_unused_function,  yeccpars2_89_/1}).
-file("src/compiler/parser/topos_parser.yrl", 436).
yeccpars2_89_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-dialyzer({nowarn_function, yeccpars2_90_/1}).
-compile({nowarn_unused_function,  yeccpars2_90_/1}).
-file("src/compiler/parser/topos_parser.yrl", 434).
yeccpars2_90_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-dialyzer({nowarn_function, yeccpars2_91_/1}).
-compile({nowarn_unused_function,  yeccpars2_91_/1}).
-file("src/compiler/parser/topos_parser.yrl", 424).
yeccpars2_91_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {pat_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-dialyzer({nowarn_function, yeccpars2_93_/1}).
-compile({nowarn_unused_function,  yeccpars2_93_/1}).
-file("src/compiler/parser/topos_parser.yrl", 418).
yeccpars2_93_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-dialyzer({nowarn_function, yeccpars2_94_/1}).
-compile({nowarn_unused_function,  yeccpars2_94_/1}).
-file("src/compiler/parser/topos_parser.yrl", 421).
yeccpars2_94_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function,  yeccpars2_96_/1}).
-file("src/compiler/parser/topos_parser.yrl", 440).
yeccpars2_96_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-dialyzer({nowarn_function, yeccpars2_98_/1}).
-compile({nowarn_unused_function,  yeccpars2_98_/1}).
-file("src/compiler/parser/topos_parser.yrl", 427).
yeccpars2_98_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_record, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-dialyzer({nowarn_function, yeccpars2_100_/1}).
-compile({nowarn_unused_function,  yeccpars2_100_/1}).
-file("src/compiler/parser/topos_parser.yrl", 445).
yeccpars2_100_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-dialyzer({nowarn_function, yeccpars2_102_/1}).
-compile({nowarn_unused_function,  yeccpars2_102_/1}).
-file("src/compiler/parser/topos_parser.yrl", 442).
yeccpars2_102_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                           
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-dialyzer({nowarn_function, yeccpars2_103_/1}).
-compile({nowarn_unused_function,  yeccpars2_103_/1}).
-file("src/compiler/parser/topos_parser.yrl", 430).
yeccpars2_103_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {pat_record, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-dialyzer({nowarn_function, yeccpars2_104_/1}).
-compile({nowarn_unused_function,  yeccpars2_104_/1}).
-file("src/compiler/parser/topos_parser.yrl", 323).
yeccpars2_104_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    
    {flow_sig, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-dialyzer({nowarn_function, yeccpars2_105_/1}).
-compile({nowarn_unused_function,  yeccpars2_105_/1}).
-file("src/compiler/parser/topos_parser.yrl", 386).
yeccpars2_105_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function,  yeccpars2_107_/1}).
-file("src/compiler/parser/topos_parser.yrl", 318).
yeccpars2_107_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    make_error_declaration(extract_location(___1), "Missing '=' or expression in flow declaration", ___4)
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-dialyzer({nowarn_function, yeccpars2_109_/1}).
-compile({nowarn_unused_function,  yeccpars2_109_/1}).
-file("src/compiler/parser/topos_parser.yrl", 547).
yeccpars2_109_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                ___1
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-dialyzer({nowarn_function, yeccpars2_110_/1}).
-compile({nowarn_unused_function,  yeccpars2_110_/1}).
-file("src/compiler/parser/topos_parser.yrl", 545).
yeccpars2_110_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               ___1
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-dialyzer({nowarn_function, yeccpars2_111_/1}).
-compile({nowarn_unused_function,  yeccpars2_111_/1}).
-file("src/compiler/parser/topos_parser.yrl", 516).
yeccpars2_111_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-dialyzer({nowarn_function, yeccpars2_113_/1}).
-compile({nowarn_unused_function,  yeccpars2_113_/1}).
-file("src/compiler/parser/topos_parser.yrl", 452).
yeccpars2_113_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                 
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-dialyzer({nowarn_function, yeccpars2_114_/1}).
-compile({nowarn_unused_function,  yeccpars2_114_/1}).
-file("src/compiler/parser/topos_parser.yrl", 513).
yeccpars2_114_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-dialyzer({nowarn_function, yeccpars2_115_/1}).
-compile({nowarn_unused_function,  yeccpars2_115_/1}).
-file("src/compiler/parser/topos_parser.yrl", 504).
yeccpars2_115_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-dialyzer({nowarn_function, yeccpars2_116_/1}).
-compile({nowarn_unused_function,  yeccpars2_116_/1}).
-file("src/compiler/parser/topos_parser.yrl", 457).
yeccpars2_116_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                ___1
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-dialyzer({nowarn_function, yeccpars2_117_/1}).
-compile({nowarn_unused_function,  yeccpars2_117_/1}).
-file("src/compiler/parser/topos_parser.yrl", 589).
yeccpars2_117_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-dialyzer({nowarn_function, yeccpars2_119_/1}).
-compile({nowarn_unused_function,  yeccpars2_119_/1}).
-file("src/compiler/parser/topos_parser.yrl", 586).
yeccpars2_119_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-dialyzer({nowarn_function, yeccpars2_123_/1}).
-compile({nowarn_unused_function,  yeccpars2_123_/1}).
-file("src/compiler/parser/topos_parser.yrl", 518).
yeccpars2_123_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-dialyzer({nowarn_function, yeccpars2_126_/1}).
-compile({nowarn_unused_function,  yeccpars2_126_/1}).
-file("src/compiler/parser/topos_parser.yrl", 592).
yeccpars2_126_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-dialyzer({nowarn_function, yeccpars2_128_/1}).
-compile({nowarn_unused_function,  yeccpars2_128_/1}).
-file("src/compiler/parser/topos_parser.yrl", 521).
yeccpars2_128_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-dialyzer({nowarn_function, yeccpars2_145_/1}).
-compile({nowarn_unused_function,  yeccpars2_145_/1}).
-file("src/compiler/parser/topos_parser.yrl", 616).
yeccpars2_145_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-dialyzer({nowarn_function, yeccpars2_149_/1}).
-compile({nowarn_unused_function,  yeccpars2_149_/1}).
-file("src/compiler/parser/topos_parser.yrl", 631).
yeccpars2_149_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-dialyzer({nowarn_function, yeccpars2_152_/1}).
-compile({nowarn_unused_function,  yeccpars2_152_/1}).
-file("src/compiler/parser/topos_parser.yrl", 390).
yeccpars2_152_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_154_/1}).
-dialyzer({nowarn_function, yeccpars2_154_/1}).
-compile({nowarn_unused_function,  yeccpars2_154_/1}).
-file("src/compiler/parser/topos_parser.yrl", 392).
yeccpars2_154_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-dialyzer({nowarn_function, yeccpars2_155_/1}).
-compile({nowarn_unused_function,  yeccpars2_155_/1}).
-file("src/compiler/parser/topos_parser.yrl", 390).
yeccpars2_155_(__Stack0) ->
 [begin
                                
    []
  end | __Stack0].

-compile({inline,yeccpars2_156_/1}).
-dialyzer({nowarn_function, yeccpars2_156_/1}).
-compile({nowarn_unused_function,  yeccpars2_156_/1}).
-file("src/compiler/parser/topos_parser.yrl", 394).
yeccpars2_156_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-dialyzer({nowarn_function, yeccpars2_159_/1}).
-compile({nowarn_unused_function,  yeccpars2_159_/1}).
-file("src/compiler/parser/topos_parser.yrl", 634).
yeccpars2_159_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                   
    {operation_case,
        extract_atom(___1),
        ___3,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-dialyzer({nowarn_function, yeccpars2_160_/1}).
-compile({nowarn_unused_function,  yeccpars2_160_/1}).
-file("src/compiler/parser/topos_parser.yrl", 641).
yeccpars2_160_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {operation_case,
        extract_atom(___1),
        [],
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-dialyzer({nowarn_function, yeccpars2_161_/1}).
-compile({nowarn_unused_function,  yeccpars2_161_/1}).
-file("src/compiler/parser/topos_parser.yrl", 629).
yeccpars2_161_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_162_/1}).
-dialyzer({nowarn_function, yeccpars2_162_/1}).
-compile({nowarn_unused_function,  yeccpars2_162_/1}).
-file("src/compiler/parser/topos_parser.yrl", 621).
yeccpars2_162_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             
    {handler_clause,
        extract_atom(___1),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-dialyzer({nowarn_function, yeccpars2_163_/1}).
-compile({nowarn_unused_function,  yeccpars2_163_/1}).
-file("src/compiler/parser/topos_parser.yrl", 618).
yeccpars2_163_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-dialyzer({nowarn_function, yeccpars2_164_/1}).
-compile({nowarn_unused_function,  yeccpars2_164_/1}).
-file("src/compiler/parser/topos_parser.yrl", 609).
yeccpars2_164_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    {try_with_expr,
        ___2,
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-dialyzer({nowarn_function, yeccpars2_165_/1}).
-compile({nowarn_unused_function,  yeccpars2_165_/1}).
-file("src/compiler/parser/topos_parser.yrl", 477).
yeccpars2_165_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, star, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-dialyzer({nowarn_function, yeccpars2_166_/1}).
-compile({nowarn_unused_function,  yeccpars2_166_/1}).
-file("src/compiler/parser/topos_parser.yrl", 480).
yeccpars2_166_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, slash, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-dialyzer({nowarn_function, yeccpars2_167_/1}).
-compile({nowarn_unused_function,  yeccpars2_167_/1}).
-file("src/compiler/parser/topos_parser.yrl", 471).
yeccpars2_167_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-dialyzer({nowarn_function, yeccpars2_168_/1}).
-compile({nowarn_unused_function,  yeccpars2_168_/1}).
-file("src/compiler/parser/topos_parser.yrl", 465).
yeccpars2_168_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, pipe_right, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_169_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_169_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_169_$end'/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
'yeccpars2_169_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_169_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_169_arrow/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_bind/1}).
-dialyzer({nowarn_function, yeccpars2_169_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_169_bind/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_comma/1}).
-dialyzer({nowarn_function, yeccpars2_169_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_169_comma/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_dot/1}).
-dialyzer({nowarn_function, yeccpars2_169_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_169_dot/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_effect/1}).
-dialyzer({nowarn_function, yeccpars2_169_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_169_effect/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_else/1}).
-dialyzer({nowarn_function, yeccpars2_169_else/1}).
-compile({nowarn_unused_function,  yeccpars2_169_else/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_end/1}).
-dialyzer({nowarn_function, yeccpars2_169_end/1}).
-compile({nowarn_unused_function,  yeccpars2_169_end/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_equals/1}).
-dialyzer({nowarn_function, yeccpars2_169_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_169_equals/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_error/1}).
-dialyzer({nowarn_function, yeccpars2_169_error/1}).
-compile({nowarn_unused_function,  yeccpars2_169_error/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_float/1}).
-dialyzer({nowarn_function, yeccpars2_169_float/1}).
-compile({nowarn_unused_function,  yeccpars2_169_float/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_flow/1}).
-dialyzer({nowarn_function, yeccpars2_169_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_169_flow/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_if/1}).
-dialyzer({nowarn_function, yeccpars2_169_if/1}).
-compile({nowarn_unused_function,  yeccpars2_169_if/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_in/1}).
-dialyzer({nowarn_function, yeccpars2_169_in/1}).
-compile({nowarn_unused_function,  yeccpars2_169_in/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_integer/1}).
-dialyzer({nowarn_function, yeccpars2_169_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_169_integer/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_169_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_169_lbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_169_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_169_lbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_let/1}).
-dialyzer({nowarn_function, yeccpars2_169_let/1}).
-compile({nowarn_unused_function,  yeccpars2_169_let/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_169_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_169_lower_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_169_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_169_lparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_perform/1}).
-dialyzer({nowarn_function, yeccpars2_169_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_169_perform/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_169_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_169_pipe/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_169_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_169_pipe_right/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_169_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_169_rbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_169_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_169_rbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_169_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_169_rparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_shape/1}).
-dialyzer({nowarn_function, yeccpars2_169_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_169_shape/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_string/1}).
-dialyzer({nowarn_function, yeccpars2_169_string/1}).
-compile({nowarn_unused_function,  yeccpars2_169_string/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_then/1}).
-dialyzer({nowarn_function, yeccpars2_169_then/1}).
-compile({nowarn_unused_function,  yeccpars2_169_then/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_try/1}).
-dialyzer({nowarn_function, yeccpars2_169_try/1}).
-compile({nowarn_unused_function,  yeccpars2_169_try/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_try(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_169_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_169_upper_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_169_with/1}).
-dialyzer({nowarn_function, yeccpars2_169_with/1}).
-compile({nowarn_unused_function,  yeccpars2_169_with/1}).
-file("src/compiler/parser/topos_parser.yrl", 489).
yeccpars2_169_with(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-dialyzer({nowarn_function, yeccpars2_170_/1}).
-compile({nowarn_unused_function,  yeccpars2_170_/1}).
-file("src/compiler/parser/topos_parser.yrl", 474).
yeccpars2_170_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, minus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_171_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_171_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_171_$end'/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
'yeccpars2_171_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_171_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_171_arrow/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_bind/1}).
-dialyzer({nowarn_function, yeccpars2_171_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_171_bind/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_comma/1}).
-dialyzer({nowarn_function, yeccpars2_171_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_171_comma/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_dot/1}).
-dialyzer({nowarn_function, yeccpars2_171_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_171_dot/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_effect/1}).
-dialyzer({nowarn_function, yeccpars2_171_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_171_effect/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_else/1}).
-dialyzer({nowarn_function, yeccpars2_171_else/1}).
-compile({nowarn_unused_function,  yeccpars2_171_else/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_end/1}).
-dialyzer({nowarn_function, yeccpars2_171_end/1}).
-compile({nowarn_unused_function,  yeccpars2_171_end/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_eq/1}).
-dialyzer({nowarn_function, yeccpars2_171_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_171_eq/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_equals/1}).
-dialyzer({nowarn_function, yeccpars2_171_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_171_equals/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_error/1}).
-dialyzer({nowarn_function, yeccpars2_171_error/1}).
-compile({nowarn_unused_function,  yeccpars2_171_error/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_float/1}).
-dialyzer({nowarn_function, yeccpars2_171_float/1}).
-compile({nowarn_unused_function,  yeccpars2_171_float/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_flow/1}).
-dialyzer({nowarn_function, yeccpars2_171_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_171_flow/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_if/1}).
-dialyzer({nowarn_function, yeccpars2_171_if/1}).
-compile({nowarn_unused_function,  yeccpars2_171_if/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_in/1}).
-dialyzer({nowarn_function, yeccpars2_171_in/1}).
-compile({nowarn_unused_function,  yeccpars2_171_in/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_integer/1}).
-dialyzer({nowarn_function, yeccpars2_171_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_171_integer/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_171_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_171_lbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_171_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_171_lbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_let/1}).
-dialyzer({nowarn_function, yeccpars2_171_let/1}).
-compile({nowarn_unused_function,  yeccpars2_171_let/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_171_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_171_lower_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_171_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_171_lparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_neq/1}).
-dialyzer({nowarn_function, yeccpars2_171_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_171_neq/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_perform/1}).
-dialyzer({nowarn_function, yeccpars2_171_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_171_perform/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_171_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_171_pipe/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_171_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_171_pipe_right/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_171_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_171_rbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_171_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_171_rbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_171_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_171_rparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_shape/1}).
-dialyzer({nowarn_function, yeccpars2_171_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_171_shape/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_string/1}).
-dialyzer({nowarn_function, yeccpars2_171_string/1}).
-compile({nowarn_unused_function,  yeccpars2_171_string/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_then/1}).
-dialyzer({nowarn_function, yeccpars2_171_then/1}).
-compile({nowarn_unused_function,  yeccpars2_171_then/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_try/1}).
-dialyzer({nowarn_function, yeccpars2_171_try/1}).
-compile({nowarn_unused_function,  yeccpars2_171_try/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_try(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_171_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_171_upper_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_171_with/1}).
-dialyzer({nowarn_function, yeccpars2_171_with/1}).
-compile({nowarn_unused_function,  yeccpars2_171_with/1}).
-file("src/compiler/parser/topos_parser.yrl", 498).
yeccpars2_171_with(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_172_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_172_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_172_$end'/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
'yeccpars2_172_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_172_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_172_arrow/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_bind/1}).
-dialyzer({nowarn_function, yeccpars2_172_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_172_bind/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_comma/1}).
-dialyzer({nowarn_function, yeccpars2_172_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_172_comma/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_dot/1}).
-dialyzer({nowarn_function, yeccpars2_172_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_172_dot/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_effect/1}).
-dialyzer({nowarn_function, yeccpars2_172_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_172_effect/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_else/1}).
-dialyzer({nowarn_function, yeccpars2_172_else/1}).
-compile({nowarn_unused_function,  yeccpars2_172_else/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_end/1}).
-dialyzer({nowarn_function, yeccpars2_172_end/1}).
-compile({nowarn_unused_function,  yeccpars2_172_end/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_eq/1}).
-dialyzer({nowarn_function, yeccpars2_172_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_172_eq/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_equals/1}).
-dialyzer({nowarn_function, yeccpars2_172_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_172_equals/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_error/1}).
-dialyzer({nowarn_function, yeccpars2_172_error/1}).
-compile({nowarn_unused_function,  yeccpars2_172_error/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_float/1}).
-dialyzer({nowarn_function, yeccpars2_172_float/1}).
-compile({nowarn_unused_function,  yeccpars2_172_float/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_flow/1}).
-dialyzer({nowarn_function, yeccpars2_172_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_172_flow/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_if/1}).
-dialyzer({nowarn_function, yeccpars2_172_if/1}).
-compile({nowarn_unused_function,  yeccpars2_172_if/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_in/1}).
-dialyzer({nowarn_function, yeccpars2_172_in/1}).
-compile({nowarn_unused_function,  yeccpars2_172_in/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_integer/1}).
-dialyzer({nowarn_function, yeccpars2_172_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_172_integer/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_172_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_172_lbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_172_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_172_lbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_let/1}).
-dialyzer({nowarn_function, yeccpars2_172_let/1}).
-compile({nowarn_unused_function,  yeccpars2_172_let/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_172_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_172_lower_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_172_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_172_lparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_neq/1}).
-dialyzer({nowarn_function, yeccpars2_172_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_172_neq/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_perform/1}).
-dialyzer({nowarn_function, yeccpars2_172_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_172_perform/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_172_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_172_pipe/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_172_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_172_pipe_right/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_172_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_172_rbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_172_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_172_rbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_172_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_172_rparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_shape/1}).
-dialyzer({nowarn_function, yeccpars2_172_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_172_shape/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_string/1}).
-dialyzer({nowarn_function, yeccpars2_172_string/1}).
-compile({nowarn_unused_function,  yeccpars2_172_string/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_then/1}).
-dialyzer({nowarn_function, yeccpars2_172_then/1}).
-compile({nowarn_unused_function,  yeccpars2_172_then/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_try/1}).
-dialyzer({nowarn_function, yeccpars2_172_try/1}).
-compile({nowarn_unused_function,  yeccpars2_172_try/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_try(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_172_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_172_upper_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_172_with/1}).
-dialyzer({nowarn_function, yeccpars2_172_with/1}).
-compile({nowarn_unused_function,  yeccpars2_172_with/1}).
-file("src/compiler/parser/topos_parser.yrl", 492).
yeccpars2_172_with(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_173_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_173_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_173_$end'/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
'yeccpars2_173_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_173_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_173_arrow/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_bind/1}).
-dialyzer({nowarn_function, yeccpars2_173_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_173_bind/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_comma/1}).
-dialyzer({nowarn_function, yeccpars2_173_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_173_comma/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_dot/1}).
-dialyzer({nowarn_function, yeccpars2_173_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_173_dot/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_effect/1}).
-dialyzer({nowarn_function, yeccpars2_173_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_173_effect/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_else/1}).
-dialyzer({nowarn_function, yeccpars2_173_else/1}).
-compile({nowarn_unused_function,  yeccpars2_173_else/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_end/1}).
-dialyzer({nowarn_function, yeccpars2_173_end/1}).
-compile({nowarn_unused_function,  yeccpars2_173_end/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_eq/1}).
-dialyzer({nowarn_function, yeccpars2_173_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_173_eq/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_equals/1}).
-dialyzer({nowarn_function, yeccpars2_173_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_173_equals/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_error/1}).
-dialyzer({nowarn_function, yeccpars2_173_error/1}).
-compile({nowarn_unused_function,  yeccpars2_173_error/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_float/1}).
-dialyzer({nowarn_function, yeccpars2_173_float/1}).
-compile({nowarn_unused_function,  yeccpars2_173_float/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_flow/1}).
-dialyzer({nowarn_function, yeccpars2_173_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_173_flow/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_if/1}).
-dialyzer({nowarn_function, yeccpars2_173_if/1}).
-compile({nowarn_unused_function,  yeccpars2_173_if/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_in/1}).
-dialyzer({nowarn_function, yeccpars2_173_in/1}).
-compile({nowarn_unused_function,  yeccpars2_173_in/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_integer/1}).
-dialyzer({nowarn_function, yeccpars2_173_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_173_integer/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_173_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_173_lbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_173_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_173_lbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_let/1}).
-dialyzer({nowarn_function, yeccpars2_173_let/1}).
-compile({nowarn_unused_function,  yeccpars2_173_let/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_173_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_173_lower_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_173_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_173_lparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_neq/1}).
-dialyzer({nowarn_function, yeccpars2_173_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_173_neq/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_perform/1}).
-dialyzer({nowarn_function, yeccpars2_173_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_173_perform/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_173_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_173_pipe/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_173_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_173_pipe_right/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_173_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_173_rbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_173_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_173_rbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_173_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_173_rparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_shape/1}).
-dialyzer({nowarn_function, yeccpars2_173_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_173_shape/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_string/1}).
-dialyzer({nowarn_function, yeccpars2_173_string/1}).
-compile({nowarn_unused_function,  yeccpars2_173_string/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_then/1}).
-dialyzer({nowarn_function, yeccpars2_173_then/1}).
-compile({nowarn_unused_function,  yeccpars2_173_then/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_try/1}).
-dialyzer({nowarn_function, yeccpars2_173_try/1}).
-compile({nowarn_unused_function,  yeccpars2_173_try/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_try(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_173_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_173_upper_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_173_with/1}).
-dialyzer({nowarn_function, yeccpars2_173_with/1}).
-compile({nowarn_unused_function,  yeccpars2_173_with/1}).
-file("src/compiler/parser/topos_parser.yrl", 501).
yeccpars2_173_with(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_174_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_174_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_174_$end'/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
'yeccpars2_174_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_174_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_174_arrow/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_bind/1}).
-dialyzer({nowarn_function, yeccpars2_174_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_174_bind/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_comma/1}).
-dialyzer({nowarn_function, yeccpars2_174_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_174_comma/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_dot/1}).
-dialyzer({nowarn_function, yeccpars2_174_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_174_dot/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_effect/1}).
-dialyzer({nowarn_function, yeccpars2_174_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_174_effect/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_else/1}).
-dialyzer({nowarn_function, yeccpars2_174_else/1}).
-compile({nowarn_unused_function,  yeccpars2_174_else/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_end/1}).
-dialyzer({nowarn_function, yeccpars2_174_end/1}).
-compile({nowarn_unused_function,  yeccpars2_174_end/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_eq/1}).
-dialyzer({nowarn_function, yeccpars2_174_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_174_eq/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_equals/1}).
-dialyzer({nowarn_function, yeccpars2_174_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_174_equals/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_error/1}).
-dialyzer({nowarn_function, yeccpars2_174_error/1}).
-compile({nowarn_unused_function,  yeccpars2_174_error/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_float/1}).
-dialyzer({nowarn_function, yeccpars2_174_float/1}).
-compile({nowarn_unused_function,  yeccpars2_174_float/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_flow/1}).
-dialyzer({nowarn_function, yeccpars2_174_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_174_flow/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_if/1}).
-dialyzer({nowarn_function, yeccpars2_174_if/1}).
-compile({nowarn_unused_function,  yeccpars2_174_if/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_in/1}).
-dialyzer({nowarn_function, yeccpars2_174_in/1}).
-compile({nowarn_unused_function,  yeccpars2_174_in/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_integer/1}).
-dialyzer({nowarn_function, yeccpars2_174_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_174_integer/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_174_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_174_lbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_174_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_174_lbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_let/1}).
-dialyzer({nowarn_function, yeccpars2_174_let/1}).
-compile({nowarn_unused_function,  yeccpars2_174_let/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_174_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_174_lower_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_174_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_174_lparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_neq/1}).
-dialyzer({nowarn_function, yeccpars2_174_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_174_neq/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_perform/1}).
-dialyzer({nowarn_function, yeccpars2_174_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_174_perform/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_174_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_174_pipe/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_174_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_174_pipe_right/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_174_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_174_rbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_174_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_174_rbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_174_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_174_rparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_shape/1}).
-dialyzer({nowarn_function, yeccpars2_174_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_174_shape/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_string/1}).
-dialyzer({nowarn_function, yeccpars2_174_string/1}).
-compile({nowarn_unused_function,  yeccpars2_174_string/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_then/1}).
-dialyzer({nowarn_function, yeccpars2_174_then/1}).
-compile({nowarn_unused_function,  yeccpars2_174_then/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_try/1}).
-dialyzer({nowarn_function, yeccpars2_174_try/1}).
-compile({nowarn_unused_function,  yeccpars2_174_try/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_try(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_174_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_174_upper_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_174_with/1}).
-dialyzer({nowarn_function, yeccpars2_174_with/1}).
-compile({nowarn_unused_function,  yeccpars2_174_with/1}).
-file("src/compiler/parser/topos_parser.yrl", 495).
yeccpars2_174_with(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_175_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_175_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_175_$end'/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
'yeccpars2_175_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_175_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_175_arrow/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_bind/1}).
-dialyzer({nowarn_function, yeccpars2_175_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_175_bind/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_comma/1}).
-dialyzer({nowarn_function, yeccpars2_175_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_175_comma/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_dot/1}).
-dialyzer({nowarn_function, yeccpars2_175_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_175_dot/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_effect/1}).
-dialyzer({nowarn_function, yeccpars2_175_effect/1}).
-compile({nowarn_unused_function,  yeccpars2_175_effect/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_effect(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_else/1}).
-dialyzer({nowarn_function, yeccpars2_175_else/1}).
-compile({nowarn_unused_function,  yeccpars2_175_else/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_end/1}).
-dialyzer({nowarn_function, yeccpars2_175_end/1}).
-compile({nowarn_unused_function,  yeccpars2_175_end/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_equals/1}).
-dialyzer({nowarn_function, yeccpars2_175_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_175_equals/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_error/1}).
-dialyzer({nowarn_function, yeccpars2_175_error/1}).
-compile({nowarn_unused_function,  yeccpars2_175_error/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_error(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_float/1}).
-dialyzer({nowarn_function, yeccpars2_175_float/1}).
-compile({nowarn_unused_function,  yeccpars2_175_float/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_flow/1}).
-dialyzer({nowarn_function, yeccpars2_175_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_175_flow/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_if/1}).
-dialyzer({nowarn_function, yeccpars2_175_if/1}).
-compile({nowarn_unused_function,  yeccpars2_175_if/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_in/1}).
-dialyzer({nowarn_function, yeccpars2_175_in/1}).
-compile({nowarn_unused_function,  yeccpars2_175_in/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_integer/1}).
-dialyzer({nowarn_function, yeccpars2_175_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_175_integer/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_175_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_175_lbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_175_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_175_lbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_let/1}).
-dialyzer({nowarn_function, yeccpars2_175_let/1}).
-compile({nowarn_unused_function,  yeccpars2_175_let/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_175_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_175_lower_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_175_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_175_lparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_perform/1}).
-dialyzer({nowarn_function, yeccpars2_175_perform/1}).
-compile({nowarn_unused_function,  yeccpars2_175_perform/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_perform(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_175_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_175_pipe/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_175_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_175_pipe_right/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_175_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_175_rbrace/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_175_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_175_rbracket/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_175_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_175_rparen/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_shape/1}).
-dialyzer({nowarn_function, yeccpars2_175_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_175_shape/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_string/1}).
-dialyzer({nowarn_function, yeccpars2_175_string/1}).
-compile({nowarn_unused_function,  yeccpars2_175_string/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_then/1}).
-dialyzer({nowarn_function, yeccpars2_175_then/1}).
-compile({nowarn_unused_function,  yeccpars2_175_then/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_try/1}).
-dialyzer({nowarn_function, yeccpars2_175_try/1}).
-compile({nowarn_unused_function,  yeccpars2_175_try/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_try(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_175_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_175_upper_ident/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_175_with/1}).
-dialyzer({nowarn_function, yeccpars2_175_with/1}).
-compile({nowarn_unused_function,  yeccpars2_175_with/1}).
-file("src/compiler/parser/topos_parser.yrl", 486).
yeccpars2_175_with(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-dialyzer({nowarn_function, yeccpars2_176_/1}).
-compile({nowarn_unused_function,  yeccpars2_176_/1}).
-file("src/compiler/parser/topos_parser.yrl", 483).
yeccpars2_176_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {binary_op, concat, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-dialyzer({nowarn_function, yeccpars2_177_/1}).
-compile({nowarn_unused_function,  yeccpars2_177_/1}).
-file("src/compiler/parser/topos_parser.yrl", 468).
yeccpars2_177_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, bind, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-dialyzer({nowarn_function, yeccpars2_181_/1}).
-compile({nowarn_unused_function,  yeccpars2_181_/1}).
-file("src/compiler/parser/topos_parser.yrl", 577).
yeccpars2_181_(__Stack0) ->
 [begin
                           
    []
  end | __Stack0].

-compile({inline,yeccpars2_183_/1}).
-dialyzer({nowarn_function, yeccpars2_183_/1}).
-compile({nowarn_unused_function,  yeccpars2_183_/1}).
-file("src/compiler/parser/topos_parser.yrl", 579).
yeccpars2_183_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    ___1
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-dialyzer({nowarn_function, yeccpars2_184_/1}).
-compile({nowarn_unused_function,  yeccpars2_184_/1}).
-file("src/compiler/parser/topos_parser.yrl", 571).
yeccpars2_184_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-dialyzer({nowarn_function, yeccpars2_186_/1}).
-compile({nowarn_unused_function,  yeccpars2_186_/1}).
-file("src/compiler/parser/topos_parser.yrl", 573).
yeccpars2_186_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_187_/1}).
-dialyzer({nowarn_function, yeccpars2_187_/1}).
-compile({nowarn_unused_function,  yeccpars2_187_/1}).
-file("src/compiler/parser/topos_parser.yrl", 601).
yeccpars2_187_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                 
    {perform_expr,
        extract_atom(___2),
        extract_atom(___4),
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_191_/1}).
-dialyzer({nowarn_function, yeccpars2_191_/1}).
-compile({nowarn_unused_function,  yeccpars2_191_/1}).
-file("src/compiler/parser/topos_parser.yrl", 524).
yeccpars2_191_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    ___2
  end | __Stack].

-compile({inline,yeccpars2_192_/1}).
-dialyzer({nowarn_function, yeccpars2_192_/1}).
-compile({nowarn_unused_function,  yeccpars2_192_/1}).
-file("src/compiler/parser/topos_parser.yrl", 533).
yeccpars2_192_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_193_/1}).
-dialyzer({nowarn_function, yeccpars2_193_/1}).
-compile({nowarn_unused_function,  yeccpars2_193_/1}).
-file("src/compiler/parser/topos_parser.yrl", 531).
yeccpars2_193_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_194_/1}).
-dialyzer({nowarn_function, yeccpars2_194_/1}).
-compile({nowarn_unused_function,  yeccpars2_194_/1}).
-file("src/compiler/parser/topos_parser.yrl", 527).
yeccpars2_194_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {tuple_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-dialyzer({nowarn_function, yeccpars2_199_/1}).
-compile({nowarn_unused_function,  yeccpars2_199_/1}).
-file("src/compiler/parser/topos_parser.yrl", 536).
yeccpars2_199_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {let_expr,
        [{pat_var, extract_atom(___2), extract_location(___2)}, ___4],
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-dialyzer({nowarn_function, yeccpars2_201_/1}).
-compile({nowarn_unused_function,  yeccpars2_201_/1}).
-file("src/compiler/parser/topos_parser.yrl", 549).
yeccpars2_201_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {list_expr, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-dialyzer({nowarn_function, yeccpars2_202_/1}).
-compile({nowarn_unused_function,  yeccpars2_202_/1}).
-file("src/compiler/parser/topos_parser.yrl", 552).
yeccpars2_202_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {list_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_204_/1}).
-dialyzer({nowarn_function, yeccpars2_204_/1}).
-compile({nowarn_unused_function,  yeccpars2_204_/1}).
-file("src/compiler/parser/topos_parser.yrl", 562).
yeccpars2_204_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-dialyzer({nowarn_function, yeccpars2_206_/1}).
-compile({nowarn_unused_function,  yeccpars2_206_/1}).
-file("src/compiler/parser/topos_parser.yrl", 555).
yeccpars2_206_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    {record_expr, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-dialyzer({nowarn_function, yeccpars2_208_/1}).
-compile({nowarn_unused_function,  yeccpars2_208_/1}).
-file("src/compiler/parser/topos_parser.yrl", 567).
yeccpars2_208_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-dialyzer({nowarn_function, yeccpars2_210_/1}).
-compile({nowarn_unused_function,  yeccpars2_210_/1}).
-file("src/compiler/parser/topos_parser.yrl", 564).
yeccpars2_210_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-dialyzer({nowarn_function, yeccpars2_211_/1}).
-compile({nowarn_unused_function,  yeccpars2_211_/1}).
-file("src/compiler/parser/topos_parser.yrl", 558).
yeccpars2_211_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {record_expr, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-dialyzer({nowarn_function, yeccpars2_216_/1}).
-compile({nowarn_unused_function,  yeccpars2_216_/1}).
-file("src/compiler/parser/topos_parser.yrl", 542).
yeccpars2_216_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {if_expr, ___2, ___4, ___6, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_217_/1}).
-dialyzer({nowarn_function, yeccpars2_217_/1}).
-compile({nowarn_unused_function,  yeccpars2_217_/1}).
-file("src/compiler/parser/topos_parser.yrl", 507).
yeccpars2_217_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {app, ___1, [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_219_/1}).
-dialyzer({nowarn_function, yeccpars2_219_/1}).
-compile({nowarn_unused_function,  yeccpars2_219_/1}).
-file("src/compiler/parser/topos_parser.yrl", 510).
yeccpars2_219_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {record_access, ___1, extract_atom(___3), extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-dialyzer({nowarn_function, yeccpars2_221_/1}).
-compile({nowarn_unused_function,  yeccpars2_221_/1}).
-file("src/compiler/parser/topos_parser.yrl", 454).
yeccpars2_221_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_223_/1}).
-dialyzer({nowarn_function, yeccpars2_223_/1}).
-compile({nowarn_unused_function,  yeccpars2_223_/1}).
-file("src/compiler/parser/topos_parser.yrl", 301).
yeccpars2_223_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                      
    {flow_decl,
        extract_atom(___2),
        undefined,
        [{flow_clause, ___3, ___5, ___7, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-dialyzer({nowarn_function, yeccpars2_224_/1}).
-compile({nowarn_unused_function,  yeccpars2_224_/1}).
-file("src/compiler/parser/topos_parser.yrl", 294).
yeccpars2_224_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    {flow_decl,
        extract_atom(___2),
        undefined,
        [{flow_clause, ___3, undefined, ___5, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-dialyzer({nowarn_function, yeccpars2_227_/1}).
-compile({nowarn_unused_function,  yeccpars2_227_/1}).
-file("src/compiler/parser/topos_parser.yrl", 356).
yeccpars2_227_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_234_/1}).
-dialyzer({nowarn_function, yeccpars2_234_/1}).
-compile({nowarn_unused_function,  yeccpars2_234_/1}).
-file("src/compiler/parser/topos_parser.yrl", 368).
yeccpars2_234_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {match_clause,
        ___2,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-dialyzer({nowarn_function, yeccpars2_235_/1}).
-compile({nowarn_unused_function,  yeccpars2_235_/1}).
-file("src/compiler/parser/topos_parser.yrl", 361).
yeccpars2_235_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    {match_clause,
        ___2,
        undefined,
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-dialyzer({nowarn_function, yeccpars2_236_/1}).
-compile({nowarn_unused_function,  yeccpars2_236_/1}).
-file("src/compiler/parser/topos_parser.yrl", 358).
yeccpars2_236_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-dialyzer({nowarn_function, yeccpars2_237_/1}).
-compile({nowarn_unused_function,  yeccpars2_237_/1}).
-file("src/compiler/parser/topos_parser.yrl", 308).
yeccpars2_237_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                             
    {flow_decl,
        extract_atom(___2),
        undefined,
        [{flow_clause, ___3, undefined, {match_expr, ___6, extract_location(___5)}, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-dialyzer({nowarn_function, yeccpars2_239_/1}).
-compile({nowarn_unused_function,  yeccpars2_239_/1}).
-file("src/compiler/parser/topos_parser.yrl", 316).
yeccpars2_239_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                     
    make_error_declaration(extract_location(___1), "Invalid flow name", ___2)
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-dialyzer({nowarn_function, yeccpars2_240_/1}).
-compile({nowarn_unused_function,  yeccpars2_240_/1}).
-file("src/compiler/parser/topos_parser.yrl", 188).
yeccpars2_240_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___2), "Malformed declaration before 'effect'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-dialyzer({nowarn_function, yeccpars2_241_/1}).
-compile({nowarn_unused_function,  yeccpars2_241_/1}).
-file("src/compiler/parser/topos_parser.yrl", 186).
yeccpars2_241_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           
    make_error_declaration(extract_location(___2), "Malformed declaration before 'flow'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-dialyzer({nowarn_function, yeccpars2_242_/1}).
-compile({nowarn_unused_function,  yeccpars2_242_/1}).
-file("src/compiler/parser/topos_parser.yrl", 184).
yeccpars2_242_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                            
    make_error_declaration(extract_location(___2), "Malformed declaration before 'shape'", ___1)
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-dialyzer({nowarn_function, yeccpars2_243_/1}).
-compile({nowarn_unused_function,  yeccpars2_243_/1}).
-file("src/compiler/parser/topos_parser.yrl", 254).
yeccpars2_243_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             
    make_error_declaration(extract_location(___1), "Incomplete effect declaration", ___2)
  end | __Stack].

-compile({inline,yeccpars2_244_/1}).
-dialyzer({nowarn_function, yeccpars2_244_/1}).
-compile({nowarn_unused_function,  yeccpars2_244_/1}).
-file("src/compiler/parser/topos_parser.yrl", 257).
yeccpars2_244_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_246_/1}).
-dialyzer({nowarn_function, yeccpars2_246_/1}).
-compile({nowarn_unused_function,  yeccpars2_246_/1}).
-file("src/compiler/parser/topos_parser.yrl", 257).
yeccpars2_246_(__Stack0) ->
 [begin
                               
    []
  end | __Stack0].

-compile({inline,yeccpars2_248_/1}).
-dialyzer({nowarn_function, yeccpars2_248_/1}).
-compile({nowarn_unused_function,  yeccpars2_248_/1}).
-file("src/compiler/parser/topos_parser.yrl", 262).
yeccpars2_248_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {effect_operation,
        extract_atom(___2),
        undefined,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_250_/1}).
-dialyzer({nowarn_function, yeccpars2_250_/1}).
-compile({nowarn_unused_function,  yeccpars2_250_/1}).
-file("src/compiler/parser/topos_parser.yrl", 268).
yeccpars2_250_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_operation,
        extract_atom(___2),
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_251_/1}).
-dialyzer({nowarn_function, yeccpars2_251_/1}).
-compile({nowarn_unused_function,  yeccpars2_251_/1}).
-file("src/compiler/parser/topos_parser.yrl", 259).
yeccpars2_251_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_252_/1}).
-dialyzer({nowarn_function, yeccpars2_252_/1}).
-compile({nowarn_unused_function,  yeccpars2_252_/1}).
-file("src/compiler/parser/topos_parser.yrl", 247).
yeccpars2_252_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           
    {effect_decl,
        extract_atom(___2),
        ___3,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_253_/1}).
-dialyzer({nowarn_function, yeccpars2_253_/1}).
-compile({nowarn_unused_function,  yeccpars2_253_/1}).
-file("src/compiler/parser/topos_parser.yrl", 176).
yeccpars2_253_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_254_/1}).
-dialyzer({nowarn_function, yeccpars2_254_/1}).
-compile({nowarn_unused_function,  yeccpars2_254_/1}).
-file("src/compiler/parser/topos_parser.yrl", 278).
yeccpars2_254_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {flow_decl,
        extract_flow_name(___1),
        extract_flow_type(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_255_/1}).
-dialyzer({nowarn_function, yeccpars2_255_/1}).
-compile({nowarn_unused_function,  yeccpars2_255_/1}).
-file("src/compiler/parser/topos_parser.yrl", 326).
yeccpars2_255_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_257_/1}).
-dialyzer({nowarn_function, yeccpars2_257_/1}).
-compile({nowarn_unused_function,  yeccpars2_257_/1}).
-file("src/compiler/parser/topos_parser.yrl", 379).
yeccpars2_257_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_263_/1}).
-dialyzer({nowarn_function, yeccpars2_263_/1}).
-compile({nowarn_unused_function,  yeccpars2_263_/1}).
-file("src/compiler/parser/topos_parser.yrl", 338).
yeccpars2_263_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                        
    {flow_clause,
        ___3,
        ___5,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-dialyzer({nowarn_function, yeccpars2_264_/1}).
-compile({nowarn_unused_function,  yeccpars2_264_/1}).
-file("src/compiler/parser/topos_parser.yrl", 331).
yeccpars2_264_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    {flow_clause,
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_267_/1}).
-dialyzer({nowarn_function, yeccpars2_267_/1}).
-compile({nowarn_unused_function,  yeccpars2_267_/1}).
-file("src/compiler/parser/topos_parser.yrl", 345).
yeccpars2_267_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                               
    {flow_clause,
        ___3,
        undefined,
        {match_expr, ___6, extract_location(___5)},
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_268_/1}).
-dialyzer({nowarn_function, yeccpars2_268_/1}).
-compile({nowarn_unused_function,  yeccpars2_268_/1}).
-file("src/compiler/parser/topos_parser.yrl", 328).
yeccpars2_268_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].


-file("src/compiler/parser/topos_parser.yrl", 755).
