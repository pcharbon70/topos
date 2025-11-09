%% This file is generated from topos_parser.yrl. Do not edit directly.
-file("topos_parser.yrl", 0).
-module(topos_parser).
-file("topos_parser.erl", 4).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("topos_parser.yrl", 493).

%% @doc Extract atom from token
extract_atom({_Tag, _Line, Atom}) when is_atom(Atom) -> Atom;
extract_atom({_Tag, _Line, String}) when is_list(String) -> list_to_atom(String);
extract_atom({Tag, _Line}) when is_atom(Tag) -> Tag.

%% @doc Extract value from token
extract_value({_Tag, _Line, Value}) -> Value.

%% @doc Extract location from token or AST node
%% Supports both legacy {line, N} format and enhanced {location, ...} format
extract_location({_Tag, Line}) when is_integer(Line) ->
    %% Convert token to enhanced location format
    topos_location:from_token({_Tag, Line});
extract_location({_Tag, Line, _Value}) when is_integer(Line) ->
    %% Convert token with value to enhanced location format
    topos_location:from_token({_Tag, Line, _Value});
extract_location({flow_sig, _Name, _Type, Loc}) -> Loc;
extract_location({var, _Name, Loc}) -> Loc;
extract_location({literal, _Value, _Type, Loc}) -> Loc;
extract_location({record_access, _Expr, _Field, Loc}) -> Loc;
extract_location({app, _Fun, _Args, Loc}) -> Loc;
extract_location({binary_op, _Op, _Left, _Right, Loc}) -> Loc;
extract_location({tuple_expr, _Elements, Loc}) -> Loc;
extract_location({list_expr, _Elements, Loc}) -> Loc;
extract_location({match_expr, _Clauses, Loc}) -> Loc;
extract_location({if_expr, _Cond, _Then, _Else, Loc}) -> Loc;
extract_location({let_expr, _Bindings, _Body, Loc}) -> Loc;
extract_location({match_clause, _Pattern, _Guard, _Body, Loc}) -> Loc;
extract_location({flow_clause, _Patterns, _Guard, _Body, Loc}) -> Loc;
extract_location({type_fun, _From, _To, Loc}) -> Loc;
extract_location({type_forall, _Vars, _Type, Loc}) -> Loc;
extract_location({type_app, _Con, _Args, Loc}) -> Loc;
extract_location({type_var, _Name, Loc}) -> Loc;
extract_location({type_con, _Name, Loc}) -> Loc;
extract_location({type_tuple, _Elements, Loc}) -> Loc;
extract_location({type_record, _Fields, _Extension, Loc}) -> Loc;
extract_location({pat_var, _Name, Loc}) -> Loc;
extract_location({pat_wildcard, Loc}) -> Loc;
extract_location({pat_constructor, _Name, _Args, Loc}) -> Loc;
extract_location({pat_literal, _Value, _Type, Loc}) -> Loc;
extract_location({pat_list, _Elements, Loc}) -> Loc;
extract_location({pat_tuple, _Elements, Loc}) -> Loc;
extract_location({pat_record, _Fields, Loc}) -> Loc;
extract_location({record_expr, _Fields, _Base, Loc}) -> Loc;
extract_location({shape_decl, _Name, _Params, _Constructors, _Traits, Loc}) -> Loc;
extract_location({constructor, _Name, _Fields, Loc}) -> Loc;
extract_location({flow_decl, _Name, _Type, _Clauses, Loc}) -> Loc;
extract_location(Tuple) when is_tuple(Tuple) ->
    %% Generic case: location is usually the last element
    Loc = element(tuple_size(Tuple), Tuple),
    %% Support both old and new format
    case Loc of
        {line, _} -> Loc;
        {location, _, _} -> Loc;
        {location, _, _, _, _} -> Loc;
        _ when is_integer(Loc) -> topos_location:new(Loc, 0);
        _ -> {location, 1, 0}  % Fallback
    end.

%% @doc Extract flow name from flow signature
extract_flow_name({flow_sig, Name, _Type, _Loc}) -> Name.

%% @doc Extract flow type from flow signature
extract_flow_type({flow_sig, _Name, Type, _Loc}) -> Type.

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



-file("topos_parser.erl", 261).

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
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function,  yeccpars2_0/7}).
yeccpars2_0(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function,  yeccpars2_4/7}).
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_declaration(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function,  yeccpars2_5/7}).
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_topos_module(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function,  yeccpars2_7/7}).
yeccpars2_7(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2_11(11, Cat, [9 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function,  yeccpars2_10/7}).
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_type_params(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function,  yeccpars2_11/7}).
yeccpars2_11(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_type_params_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_type_params_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_14(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function,  yeccpars2_15/7}).
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_shape_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_16(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_constructors(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_constructor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_constructor_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_constructor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function,  yeccpars2_21/7}).
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, 'forall', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_type_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_type_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_27(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccpars2_31(31, Cat, [28 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_type_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_type_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_32: see yeccpars2_22

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_34: see yeccpars2_22

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_type_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_36: see yeccpars2_22

-dialyzer({nowarn_function, yeccpars2_37/7}).
-compile({nowarn_unused_function,  yeccpars2_37/7}).
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function,  yeccpars2_38/7}).
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_type_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function,  yeccpars2_39/7}).
yeccpars2_39(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_type_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_type_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function,  yeccpars2_44/7}).
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_45: see yeccpars2_22

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_type_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function,  yeccpars2_48/7}).
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_type_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function,  yeccpars2_49/7}).
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_type_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function,  yeccpars2_50/7}).
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_constructor_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_51: see yeccpars2_14

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function,  yeccpars2_52/7}).
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_constructors(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_53/7}).
-compile({nowarn_unused_function,  yeccpars2_53/7}).
yeccpars2_53(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2_55(55, Cat, [53 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function,  yeccpars2_54/7}).
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_pattern_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function,  yeccpars2_55/7}).
yeccpars2_55(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_56/7}).
-compile({nowarn_unused_function,  yeccpars2_56/7}).
yeccpars2_56(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_57: see yeccpars2_22

-dialyzer({nowarn_function, yeccpars2_58/7}).
-compile({nowarn_unused_function,  yeccpars2_58/7}).
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).
-compile({nowarn_unused_function,  yeccpars2_59/7}).
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
-compile({nowarn_unused_function,  yeccpars2_60/7}).
yeccpars2_60(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function,  yeccpars2_62/7}).
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_63/7}).
-compile({nowarn_unused_function,  yeccpars2_63/7}).
yeccpars2_63(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_64/7}).
-compile({nowarn_unused_function,  yeccpars2_64/7}).
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
-compile({nowarn_unused_function,  yeccpars2_65/7}).
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_66/7}).
-compile({nowarn_unused_function,  yeccpars2_66/7}).
yeccpars2_66(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_67/7}).
-compile({nowarn_unused_function,  yeccpars2_67/7}).
yeccpars2_67(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccpars2_68(68, Cat, [67 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function,  yeccpars2_68/7}).
yeccpars2_68(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_69/7}).
-compile({nowarn_unused_function,  yeccpars2_69/7}).
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_70/7}).
-compile({nowarn_unused_function,  yeccpars2_70/7}).
yeccpars2_70(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function,  yeccpars2_71/7}).
yeccpars2_71(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_72: see yeccpars2_63

-dialyzer({nowarn_function, yeccpars2_73/7}).
-compile({nowarn_unused_function,  yeccpars2_73/7}).
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function,  yeccpars2_74/7}).
yeccpars2_74(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_tuple_pattern_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_75/7}).
-compile({nowarn_unused_function,  yeccpars2_75/7}).
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_76/7}).
-compile({nowarn_unused_function,  yeccpars2_76/7}).
yeccpars2_76(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_77/7}).
-compile({nowarn_unused_function,  yeccpars2_77/7}).
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_78/7}).
-compile({nowarn_unused_function,  yeccpars2_78/7}).
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function,  yeccpars2_79/7}).
yeccpars2_79(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_80/7}).
-compile({nowarn_unused_function,  yeccpars2_80/7}).
yeccpars2_80(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_record_pattern_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).
-compile({nowarn_unused_function,  yeccpars2_81/7}).
yeccpars2_81(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_82/7}).
-compile({nowarn_unused_function,  yeccpars2_82/7}).
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_83: see yeccpars2_63

-dialyzer({nowarn_function, yeccpars2_84/7}).
-compile({nowarn_unused_function,  yeccpars2_84/7}).
yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_record_pattern_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_85/7}).
-compile({nowarn_unused_function,  yeccpars2_85/7}).
yeccpars2_85(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_86/7}).
-compile({nowarn_unused_function,  yeccpars2_86/7}).
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_record_pattern_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_87/7}).
-compile({nowarn_unused_function,  yeccpars2_87/7}).
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_88/7}).
-compile({nowarn_unused_function,  yeccpars2_88/7}).
yeccpars2_88(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_flow_signature(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_89/7}).
-compile({nowarn_unused_function,  yeccpars2_89/7}).
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_pattern_list_nonempty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_90(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_91/7}).
-compile({nowarn_unused_function,  yeccpars2_91/7}).
yeccpars2_91(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_92/7}).
-compile({nowarn_unused_function,  yeccpars2_92/7}).
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_93/7}).
-compile({nowarn_unused_function,  yeccpars2_93/7}).
yeccpars2_93(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function,  yeccpars2_94/7}).
yeccpars2_94(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_guards(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_95/7}).
-compile({nowarn_unused_function,  yeccpars2_95/7}).
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_expr_app(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function,  yeccpars2_96/7}).
yeccpars2_96(S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
-compile({nowarn_unused_function,  yeccpars2_97/7}).
yeccpars2_97(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_guard(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_98/7}).
-compile({nowarn_unused_function,  yeccpars2_98/7}).
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_100/7}).
-compile({nowarn_unused_function,  yeccpars2_100/7}).
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
-compile({nowarn_unused_function,  yeccpars2_101/7}).
yeccpars2_101(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_102(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).
-compile({nowarn_unused_function,  yeccpars2_103/7}).
yeccpars2_103(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_104/7}).
-compile({nowarn_unused_function,  yeccpars2_104/7}).
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_105: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_106/7}).
-compile({nowarn_unused_function,  yeccpars2_106/7}).
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function,  yeccpars2_107/7}).
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_expr_primary(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
-compile({nowarn_unused_function,  yeccpars2_108/7}).
yeccpars2_108(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_109(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_109(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function,  yeccpars2_109/7}).
yeccpars2_cont_109(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_110: see yeccpars2_91

%% yeccpars2_111: see yeccpars2_91

%% yeccpars2_112: see yeccpars2_91

%% yeccpars2_113: see yeccpars2_91

%% yeccpars2_114: see yeccpars2_91

%% yeccpars2_115: see yeccpars2_91

%% yeccpars2_116: see yeccpars2_91

%% yeccpars2_117: see yeccpars2_91

%% yeccpars2_118: see yeccpars2_91

%% yeccpars2_119: see yeccpars2_91

%% yeccpars2_120: see yeccpars2_91

%% yeccpars2_121: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function,  yeccpars2_122/7}).
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_123: see yeccpars2_91

%% yeccpars2_124: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_125/7}).
-compile({nowarn_unused_function,  yeccpars2_125/7}).
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_126/7}).
-compile({nowarn_unused_function,  yeccpars2_126/7}).
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_127/7}).
-compile({nowarn_unused_function,  yeccpars2_127/7}).
yeccpars2_127(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_128/7}).
-compile({nowarn_unused_function,  yeccpars2_128/7}).
yeccpars2_128(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_128_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_129/7}).
-compile({nowarn_unused_function,  yeccpars2_129/7}).
yeccpars2_129(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_129_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_129_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_130/7}).
-compile({nowarn_unused_function,  yeccpars2_130/7}).
yeccpars2_130(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_131/7}).
-compile({nowarn_unused_function,  yeccpars2_131/7}).
yeccpars2_131(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_131_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_132/7}).
-compile({nowarn_unused_function,  yeccpars2_132/7}).
yeccpars2_132(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_132_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_132_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_132(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_133/7}).
-compile({nowarn_unused_function,  yeccpars2_133/7}).
yeccpars2_133(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_133_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_134/7}).
-compile({nowarn_unused_function,  yeccpars2_134/7}).
yeccpars2_134(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_134_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_eq(Stack),
 yeccgoto_expr(hd(Nss), 'eq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_neq(Stack),
 yeccgoto_expr(hd(Nss), 'neq', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_134_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_135/7}).
-compile({nowarn_unused_function,  yeccpars2_135/7}).
yeccpars2_135(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_135_$end'(Stack),
 yeccgoto_expr(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_arrow(Stack),
 yeccgoto_expr(hd(Nss), 'arrow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_bind(Stack),
 yeccgoto_expr(hd(Nss), 'bind', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_comma(Stack),
 yeccgoto_expr(hd(Nss), 'comma', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'dot', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_dot(Stack),
 yeccgoto_expr(hd(Nss), 'dot', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_else(Stack),
 yeccgoto_expr(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_end(Stack),
 yeccgoto_expr(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_equals(Stack),
 yeccgoto_expr(hd(Nss), 'equals', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'float', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_float(Stack),
 yeccgoto_expr(hd(Nss), 'float', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_flow(Stack),
 yeccgoto_expr(hd(Nss), 'flow', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_if(Stack),
 yeccgoto_expr(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_in(Stack),
 yeccgoto_expr(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_integer(Stack),
 yeccgoto_expr(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_lbrace(Stack),
 yeccgoto_expr(hd(Nss), 'lbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_lbracket(Stack),
 yeccgoto_expr(hd(Nss), 'lbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_let(Stack),
 yeccgoto_expr(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_lower_ident(Stack),
 yeccgoto_expr(hd(Nss), 'lower_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_lparen(Stack),
 yeccgoto_expr(hd(Nss), 'lparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_pipe(Stack),
 yeccgoto_expr(hd(Nss), 'pipe', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_pipe_right(Stack),
 yeccgoto_expr(hd(Nss), 'pipe_right', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_rbrace(Stack),
 yeccgoto_expr(hd(Nss), 'rbrace', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_rbracket(Stack),
 yeccgoto_expr(hd(Nss), 'rbracket', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'rparen', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_rparen(Stack),
 yeccgoto_expr(hd(Nss), 'rparen', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'shape', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_shape(Stack),
 yeccgoto_expr(hd(Nss), 'shape', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'string', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_string(Stack),
 yeccgoto_expr(hd(Nss), 'string', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_then(Stack),
 yeccgoto_expr(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_135_upper_ident(Stack),
 yeccgoto_expr(hd(Nss), 'upper_ident', Nss, NewStack, T, Ts, Tzr);
yeccpars2_135(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_136/7}).
-compile({nowarn_unused_function,  yeccpars2_136/7}).
yeccpars2_136(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_137/7}).
-compile({nowarn_unused_function,  yeccpars2_137/7}).
yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_138/7}).
-compile({nowarn_unused_function,  yeccpars2_138/7}).
yeccpars2_138(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_tuple_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_139/7}).
-compile({nowarn_unused_function,  yeccpars2_139/7}).
yeccpars2_139(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_140/7}).
-compile({nowarn_unused_function,  yeccpars2_140/7}).
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_141/7}).
-compile({nowarn_unused_function,  yeccpars2_141/7}).
yeccpars2_141(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_142: see yeccpars2_91

yeccpars2_143(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_109(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_144: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_145/7}).
-compile({nowarn_unused_function,  yeccpars2_145/7}).
yeccpars2_145(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function,  yeccpars2_146/7}).
yeccpars2_146(S, 'rbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_147/7}).
-compile({nowarn_unused_function,  yeccpars2_147/7}).
yeccpars2_147(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_expr_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function,  yeccpars2_148/7}).
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_149: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_150/7}).
-compile({nowarn_unused_function,  yeccpars2_150/7}).
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_expr_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_151/7}).
-compile({nowarn_unused_function,  yeccpars2_151/7}).
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function,  yeccpars2_152/7}).
yeccpars2_152(S, 'rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_153/7}).
-compile({nowarn_unused_function,  yeccpars2_153/7}).
yeccpars2_153(S, 'comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function,  yeccpars2_154/7}).
yeccpars2_154(S, 'colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function,  yeccpars2_155/7}).
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_156: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_157/7}).
-compile({nowarn_unused_function,  yeccpars2_157/7}).
yeccpars2_157(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_158/7}).
-compile({nowarn_unused_function,  yeccpars2_158/7}).
yeccpars2_158(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_159/7}).
-compile({nowarn_unused_function,  yeccpars2_159/7}).
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_160/7}).
-compile({nowarn_unused_function,  yeccpars2_160/7}).
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_161(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_109(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_162: see yeccpars2_91

yeccpars2_163(S, 'else', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_109(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_164: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_165/7}).
-compile({nowarn_unused_function,  yeccpars2_165/7}).
yeccpars2_165(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_expr_primary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
-compile({nowarn_unused_function,  yeccpars2_166/7}).
yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_167/7}).
-compile({nowarn_unused_function,  yeccpars2_167/7}).
yeccpars2_167(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_168/7}).
-compile({nowarn_unused_function,  yeccpars2_168/7}).
yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_expr_app(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_169: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_170/7}).
-compile({nowarn_unused_function,  yeccpars2_170/7}).
yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_guards(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_171: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_172/7}).
-compile({nowarn_unused_function,  yeccpars2_172/7}).
yeccpars2_172(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_173/7}).
-compile({nowarn_unused_function,  yeccpars2_173/7}).
yeccpars2_173(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_174/7}).
-compile({nowarn_unused_function,  yeccpars2_174/7}).
yeccpars2_174(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_175/7}).
-compile({nowarn_unused_function,  yeccpars2_175/7}).
yeccpars2_175(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_176/7}).
-compile({nowarn_unused_function,  yeccpars2_176/7}).
yeccpars2_176(S, 'pipe', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_match_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_177: see yeccpars2_63

-dialyzer({nowarn_function, yeccpars2_178/7}).
-compile({nowarn_unused_function,  yeccpars2_178/7}).
yeccpars2_178(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_179: see yeccpars2_91

%% yeccpars2_180: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_181/7}).
-compile({nowarn_unused_function,  yeccpars2_181/7}).
yeccpars2_181(S, 'arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_182: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_183/7}).
-compile({nowarn_unused_function,  yeccpars2_183/7}).
yeccpars2_183(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_184/7}).
-compile({nowarn_unused_function,  yeccpars2_184/7}).
yeccpars2_184(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_match_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_185/7}).
-compile({nowarn_unused_function,  yeccpars2_185/7}).
yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_match_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_186/7}).
-compile({nowarn_unused_function,  yeccpars2_186/7}).
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_187/7}).
-compile({nowarn_unused_function,  yeccpars2_187/7}).
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_187_(Stack),
 yeccgoto_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_188/7}).
-compile({nowarn_unused_function,  yeccpars2_188/7}).
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_188_(Stack),
 yeccgoto_flow_decl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_189/7}).
-compile({nowarn_unused_function,  yeccpars2_189/7}).
yeccpars2_189(S, 'flow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 yeccgoto_flow_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_190/7}).
-compile({nowarn_unused_function,  yeccpars2_190/7}).
yeccpars2_190(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_191/7}).
-compile({nowarn_unused_function,  yeccpars2_191/7}).
yeccpars2_191(S, 'float', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'lbracket', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'lower_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'lparen', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'underscore', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, 'upper_ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_191_(Stack),
 yeccpars2_192(192, Cat, [191 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_192/7}).
-compile({nowarn_unused_function,  yeccpars2_192/7}).
yeccpars2_192(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_193(S, 'match', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_194: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_195/7}).
-compile({nowarn_unused_function,  yeccpars2_195/7}).
yeccpars2_195(S, 'equals', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_196: see yeccpars2_91

-dialyzer({nowarn_function, yeccpars2_197/7}).
-compile({nowarn_unused_function,  yeccpars2_197/7}).
yeccpars2_197(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_flow_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_198/7}).
-compile({nowarn_unused_function,  yeccpars2_198/7}).
yeccpars2_198(S, 'bind', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'concat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'eq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'gt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'gte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'lt', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'lte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'neq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'pipe_right', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'slash', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 'star', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_flow_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_199: see yeccpars2_174

-dialyzer({nowarn_function, yeccpars2_200/7}).
-compile({nowarn_unused_function,  yeccpars2_200/7}).
yeccpars2_200(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_201/7}).
-compile({nowarn_unused_function,  yeccpars2_201/7}).
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_flow_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_202/7}).
-compile({nowarn_unused_function,  yeccpars2_202/7}).
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_flow_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor/7}).
yeccgoto_constructor(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor(51, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructor_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_constructor_fields/7}).
yeccgoto_constructor_fields(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructor_fields(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constructors/7}).
-compile({nowarn_unused_function,  yeccgoto_constructors/7}).
yeccgoto_constructors(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constructors(51=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declaration/7}).
-compile({nowarn_unused_function,  yeccgoto_declaration/7}).
yeccgoto_declaration(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declaration(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarations/7}).
-compile({nowarn_unused_function,  yeccgoto_declarations/7}).
yeccgoto_declarations(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarations(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_expr/7}).
yeccgoto_expr(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(135, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(133, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(142, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(172, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(183, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(197, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_app/7}).
yeccgoto_expr_app(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(121, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(142, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_app(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_list/7}).
yeccgoto_expr_list(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_list(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_expr_primary/7}).
yeccgoto_expr_primary(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_primary(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_clause/7}).
yeccgoto_flow_clause(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_clause(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_clauses/7}).
yeccgoto_flow_clauses(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_clauses(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_decl/7}).
yeccgoto_flow_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_decl(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_flow_signature/7}).
-compile({nowarn_unused_function,  yeccgoto_flow_signature/7}).
yeccgoto_flow_signature(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_flow_signature(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guard/7}).
-compile({nowarn_unused_function,  yeccgoto_guard/7}).
yeccgoto_guard(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_guards/7}).
-compile({nowarn_unused_function,  yeccgoto_guards/7}).
yeccgoto_guards(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(180, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guards(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(195, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_literal/7}).
-compile({nowarn_unused_function,  yeccgoto_literal/7}).
yeccgoto_literal(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(142=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clause/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clause/7}).
yeccgoto_match_clause(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(176, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clause(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_clauses/7}).
-compile({nowarn_unused_function,  yeccgoto_match_clauses/7}).
yeccgoto_match_clauses(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_clauses(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern/7}).
yeccgoto_pattern(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list/7}).
yeccgoto_pattern_list(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern_list_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_pattern_list_nonempty/7}).
yeccgoto_pattern_list_nonempty(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_list_nonempty(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_field/7}).
yeccgoto_record_field(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_field(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_fields/7}).
yeccgoto_record_fields(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(152, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_fields(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_field/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_field/7}).
yeccgoto_record_pattern_field(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_field(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_record_pattern_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_record_pattern_fields/7}).
yeccgoto_record_pattern_fields(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_pattern_fields(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_shape_decl/7}).
-compile({nowarn_unused_function,  yeccgoto_shape_decl/7}).
yeccgoto_shape_decl(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shape_decl(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_topos_module/7}).
-compile({nowarn_unused_function,  yeccgoto_topos_module/7}).
yeccgoto_topos_module(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_expr_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_expr_list/7}).
yeccgoto_tuple_expr_list(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_expr_list(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_pattern_list/7}).
-compile({nowarn_unused_function,  yeccgoto_tuple_pattern_list/7}).
yeccgoto_tuple_pattern_list(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple_pattern_list(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr/7}).
yeccgoto_type_expr(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(27, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_app/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_app/7}).
yeccgoto_type_expr_app(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_app(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_expr_primary/7}).
-compile({nowarn_unused_function,  yeccgoto_type_expr_primary/7}).
yeccgoto_type_expr_primary(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_expr_primary(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_list/7}).
-compile({nowarn_unused_function,  yeccgoto_type_list/7}).
yeccgoto_type_list(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_list(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params/7}).
yeccgoto_type_params(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_params_nonempty/7}).
-compile({nowarn_unused_function,  yeccgoto_type_params_nonempty/7}).
yeccgoto_type_params_nonempty(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_params_nonempty(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_field/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_field/7}).
yeccgoto_type_record_field(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_field(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_record_fields/7}).
-compile({nowarn_unused_function,  yeccgoto_type_record_fields/7}).
yeccgoto_type_record_fields(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_record_fields(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("topos_parser.yrl", 96).
yeccpars2_2_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            ___1
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("topos_parser.yrl", 97).
yeccpars2_4_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function,  yeccpars2_5_/1}).
-file("topos_parser.yrl", 88).
yeccpars2_5_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              
    {module, undefined, [], [], ___1, {line, 1}}
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("topos_parser.yrl", 91).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function,  yeccpars2_9_/1}).
-file("topos_parser.yrl", 111).
yeccpars2_9_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function,  yeccpars2_10_/1}).
-file("topos_parser.yrl", 113).
yeccpars2_10_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    ___1
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function,  yeccpars2_12_/1}).
-file("topos_parser.yrl", 116).
yeccpars2_12_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                     
    [extract_atom(___1)]
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function,  yeccpars2_13_/1}).
-file("topos_parser.yrl", 118).
yeccpars2_13_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    [extract_atom(___1) | ___2]
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-dialyzer({nowarn_function, yeccpars2_15_/1}).
-compile({nowarn_unused_function,  yeccpars2_15_/1}).
-file("topos_parser.yrl", 103).
yeccpars2_15_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                 
    {shape_decl,
        extract_atom(___2),
        ___3,
        ___5,
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-dialyzer({nowarn_function, yeccpars2_16_/1}).
-compile({nowarn_unused_function,  yeccpars2_16_/1}).
-file("topos_parser.yrl", 121).
yeccpars2_16_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-dialyzer({nowarn_function, yeccpars2_17_/1}).
-compile({nowarn_unused_function,  yeccpars2_17_/1}).
-file("topos_parser.yrl", 126).
yeccpars2_17_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            
    {constructor,
        extract_atom(___1),
        [],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-dialyzer({nowarn_function, yeccpars2_18_/1}).
-compile({nowarn_unused_function,  yeccpars2_18_/1}).
-file("topos_parser.yrl", 138).
yeccpars2_18_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-dialyzer({nowarn_function, yeccpars2_19_/1}).
-compile({nowarn_unused_function,  yeccpars2_19_/1}).
-file("topos_parser.yrl", 132).
yeccpars2_19_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {constructor,
        extract_atom(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-dialyzer({nowarn_function, yeccpars2_21_/1}).
-compile({nowarn_unused_function,  yeccpars2_21_/1}).
-file("topos_parser.yrl", 453).
yeccpars2_21_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("topos_parser.yrl", 456).
yeccpars2_23_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function,  yeccpars2_25_/1}).
-file("topos_parser.yrl", 449).
yeccpars2_25_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                    
    ___1
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function,  yeccpars2_26_/1}).
-file("topos_parser.yrl", 440).
yeccpars2_26_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             ___1
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-dialyzer({nowarn_function, yeccpars2_28_/1}).
-compile({nowarn_unused_function,  yeccpars2_28_/1}).
-file("topos_parser.yrl", 111).
yeccpars2_28_(__Stack0) ->
 [begin
                         
    []
  end | __Stack0].

-compile({inline,yeccpars2_29_/1}).
-dialyzer({nowarn_function, yeccpars2_29_/1}).
-compile({nowarn_unused_function,  yeccpars2_29_/1}).
-file("topos_parser.yrl", 456).
yeccpars2_29_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    {type_con, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("topos_parser.yrl", 443).
yeccpars2_30_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {type_app,
        {type_con, extract_atom(___1), extract_location(___1)},
        [___2],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function,  yeccpars2_33_/1}).
-file("topos_parser.yrl", 437).
yeccpars2_33_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {type_forall, ___2, ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("topos_parser.yrl", 434).
yeccpars2_35_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {type_fun, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-dialyzer({nowarn_function, yeccpars2_37_/1}).
-compile({nowarn_unused_function,  yeccpars2_37_/1}).
-file("topos_parser.yrl", 459).
yeccpars2_37_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    ___2
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-dialyzer({nowarn_function, yeccpars2_38_/1}).
-compile({nowarn_unused_function,  yeccpars2_38_/1}).
-file("topos_parser.yrl", 473).
yeccpars2_38_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function,  yeccpars2_39_/1}).
-file("topos_parser.yrl", 471).
yeccpars2_39_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function,  yeccpars2_40_/1}).
-file("topos_parser.yrl", 462).
yeccpars2_40_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              
    {type_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function,  yeccpars2_42_/1}).
-file("topos_parser.yrl", 477).
yeccpars2_42_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                         
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function,  yeccpars2_44_/1}).
-file("topos_parser.yrl", 465).
yeccpars2_44_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    {type_record, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function,  yeccpars2_46_/1}).
-file("topos_parser.yrl", 482).
yeccpars2_46_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                  
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function,  yeccpars2_48_/1}).
-file("topos_parser.yrl", 479).
yeccpars2_48_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                  
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function,  yeccpars2_49_/1}).
-file("topos_parser.yrl", 468).
yeccpars2_49_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {type_record, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-dialyzer({nowarn_function, yeccpars2_50_/1}).
-compile({nowarn_unused_function,  yeccpars2_50_/1}).
-file("topos_parser.yrl", 140).
yeccpars2_50_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                            
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-dialyzer({nowarn_function, yeccpars2_52_/1}).
-compile({nowarn_unused_function,  yeccpars2_52_/1}).
-file("topos_parser.yrl", 123).
yeccpars2_52_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-dialyzer({nowarn_function, yeccpars2_53_/1}).
-compile({nowarn_unused_function,  yeccpars2_53_/1}).
-file("topos_parser.yrl", 232).
yeccpars2_53_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_54_/1}).
-dialyzer({nowarn_function, yeccpars2_54_/1}).
-compile({nowarn_unused_function,  yeccpars2_54_/1}).
-file("topos_parser.yrl", 234).
yeccpars2_54_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                       
    ___1
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-dialyzer({nowarn_function, yeccpars2_56_/1}).
-compile({nowarn_unused_function,  yeccpars2_56_/1}).
-file("topos_parser.yrl", 237).
yeccpars2_56_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-dialyzer({nowarn_function, yeccpars2_58_/1}).
-compile({nowarn_unused_function,  yeccpars2_58_/1}).
-file("topos_parser.yrl", 257).
yeccpars2_58_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {pat_literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-dialyzer({nowarn_function, yeccpars2_59_/1}).
-compile({nowarn_unused_function,  yeccpars2_59_/1}).
-file("topos_parser.yrl", 254).
yeccpars2_59_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {pat_literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function,  yeccpars2_62_/1}).
-file("topos_parser.yrl", 242).
yeccpars2_62_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-dialyzer({nowarn_function, yeccpars2_64_/1}).
-compile({nowarn_unused_function,  yeccpars2_64_/1}).
-file("topos_parser.yrl", 260).
yeccpars2_64_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {pat_literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-dialyzer({nowarn_function, yeccpars2_65_/1}).
-compile({nowarn_unused_function,  yeccpars2_65_/1}).
-file("topos_parser.yrl", 245).
yeccpars2_65_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       
    {pat_wildcard, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-dialyzer({nowarn_function, yeccpars2_66_/1}).
-compile({nowarn_unused_function,  yeccpars2_66_/1}).
-file("topos_parser.yrl", 248).
yeccpars2_66_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        
    {pat_constructor, extract_atom(___1), [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-dialyzer({nowarn_function, yeccpars2_67_/1}).
-compile({nowarn_unused_function,  yeccpars2_67_/1}).
-file("topos_parser.yrl", 232).
yeccpars2_67_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_69_/1}).
-dialyzer({nowarn_function, yeccpars2_69_/1}).
-compile({nowarn_unused_function,  yeccpars2_69_/1}).
-file("topos_parser.yrl", 251).
yeccpars2_69_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {pat_constructor, extract_atom(___1), ___3, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-dialyzer({nowarn_function, yeccpars2_73_/1}).
-compile({nowarn_unused_function,  yeccpars2_73_/1}).
-file("topos_parser.yrl", 281).
yeccpars2_73_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-dialyzer({nowarn_function, yeccpars2_74_/1}).
-compile({nowarn_unused_function,  yeccpars2_74_/1}).
-file("topos_parser.yrl", 279).
yeccpars2_74_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-dialyzer({nowarn_function, yeccpars2_75_/1}).
-compile({nowarn_unused_function,  yeccpars2_75_/1}).
-file("topos_parser.yrl", 269).
yeccpars2_75_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {pat_tuple, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-dialyzer({nowarn_function, yeccpars2_77_/1}).
-compile({nowarn_unused_function,  yeccpars2_77_/1}).
-file("topos_parser.yrl", 263).
yeccpars2_77_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {pat_list, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-dialyzer({nowarn_function, yeccpars2_78_/1}).
-compile({nowarn_unused_function,  yeccpars2_78_/1}).
-file("topos_parser.yrl", 266).
yeccpars2_78_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           
    {pat_list, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-dialyzer({nowarn_function, yeccpars2_80_/1}).
-compile({nowarn_unused_function,  yeccpars2_80_/1}).
-file("topos_parser.yrl", 285).
yeccpars2_80_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-dialyzer({nowarn_function, yeccpars2_82_/1}).
-compile({nowarn_unused_function,  yeccpars2_82_/1}).
-file("topos_parser.yrl", 272).
yeccpars2_82_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {pat_record, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-dialyzer({nowarn_function, yeccpars2_84_/1}).
-compile({nowarn_unused_function,  yeccpars2_84_/1}).
-file("topos_parser.yrl", 290).
yeccpars2_84_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-dialyzer({nowarn_function, yeccpars2_86_/1}).
-compile({nowarn_unused_function,  yeccpars2_86_/1}).
-file("topos_parser.yrl", 287).
yeccpars2_86_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                           
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-dialyzer({nowarn_function, yeccpars2_87_/1}).
-compile({nowarn_unused_function,  yeccpars2_87_/1}).
-file("topos_parser.yrl", 275).
yeccpars2_87_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                
    {pat_record, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-dialyzer({nowarn_function, yeccpars2_88_/1}).
-compile({nowarn_unused_function,  yeccpars2_88_/1}).
-file("topos_parser.yrl", 176).
yeccpars2_88_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    
    {flow_sig, extract_atom(___2), ___4, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-dialyzer({nowarn_function, yeccpars2_89_/1}).
-compile({nowarn_unused_function,  yeccpars2_89_/1}).
-file("topos_parser.yrl", 239).
yeccpars2_89_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-dialyzer({nowarn_function, yeccpars2_92_/1}).
-compile({nowarn_unused_function,  yeccpars2_92_/1}).
-file("topos_parser.yrl", 361).
yeccpars2_92_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-dialyzer({nowarn_function, yeccpars2_94_/1}).
-compile({nowarn_unused_function,  yeccpars2_94_/1}).
-file("topos_parser.yrl", 297).
yeccpars2_94_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                 
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-dialyzer({nowarn_function, yeccpars2_95_/1}).
-compile({nowarn_unused_function,  yeccpars2_95_/1}).
-file("topos_parser.yrl", 358).
yeccpars2_95_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           ___1
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function,  yeccpars2_96_/1}).
-file("topos_parser.yrl", 349).
yeccpars2_96_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-dialyzer({nowarn_function, yeccpars2_97_/1}).
-compile({nowarn_unused_function,  yeccpars2_97_/1}).
-file("topos_parser.yrl", 302).
yeccpars2_97_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                ___1
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-dialyzer({nowarn_function, yeccpars2_98_/1}).
-compile({nowarn_unused_function,  yeccpars2_98_/1}).
-file("topos_parser.yrl", 424).
yeccpars2_98_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  
    {literal, extract_value(___1), float, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-dialyzer({nowarn_function, yeccpars2_100_/1}).
-compile({nowarn_unused_function,  yeccpars2_100_/1}).
-file("topos_parser.yrl", 421).
yeccpars2_100_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    
    {literal, extract_value(___1), integer, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-dialyzer({nowarn_function, yeccpars2_104_/1}).
-compile({nowarn_unused_function,  yeccpars2_104_/1}).
-file("topos_parser.yrl", 363).
yeccpars2_104_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-dialyzer({nowarn_function, yeccpars2_106_/1}).
-compile({nowarn_unused_function,  yeccpars2_106_/1}).
-file("topos_parser.yrl", 427).
yeccpars2_106_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    {literal, extract_value(___1), string, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function,  yeccpars2_107_/1}).
-file("topos_parser.yrl", 366).
yeccpars2_107_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    {var, extract_atom(___1), extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-dialyzer({nowarn_function, yeccpars2_122_/1}).
-compile({nowarn_unused_function,  yeccpars2_122_/1}).
-file("topos_parser.yrl", 369).
yeccpars2_122_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    ___2
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-dialyzer({nowarn_function, yeccpars2_125_/1}).
-compile({nowarn_unused_function,  yeccpars2_125_/1}).
-file("topos_parser.yrl", 322).
yeccpars2_125_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, star, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-dialyzer({nowarn_function, yeccpars2_126_/1}).
-compile({nowarn_unused_function,  yeccpars2_126_/1}).
-file("topos_parser.yrl", 325).
yeccpars2_126_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, slash, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-dialyzer({nowarn_function, yeccpars2_127_/1}).
-compile({nowarn_unused_function,  yeccpars2_127_/1}).
-file("topos_parser.yrl", 316).
yeccpars2_127_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, plus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-dialyzer({nowarn_function, yeccpars2_128_/1}).
-compile({nowarn_unused_function,  yeccpars2_128_/1}).
-file("topos_parser.yrl", 310).
yeccpars2_128_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    {binary_op, pipe_right, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_129_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_129_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_129_$end'/1}).
-file("topos_parser.yrl", 334).
'yeccpars2_129_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_129_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_129_arrow/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_bind/1}).
-dialyzer({nowarn_function, yeccpars2_129_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_129_bind/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_comma/1}).
-dialyzer({nowarn_function, yeccpars2_129_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_129_comma/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_dot/1}).
-dialyzer({nowarn_function, yeccpars2_129_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_129_dot/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_else/1}).
-dialyzer({nowarn_function, yeccpars2_129_else/1}).
-compile({nowarn_unused_function,  yeccpars2_129_else/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_end/1}).
-dialyzer({nowarn_function, yeccpars2_129_end/1}).
-compile({nowarn_unused_function,  yeccpars2_129_end/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_equals/1}).
-dialyzer({nowarn_function, yeccpars2_129_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_129_equals/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_float/1}).
-dialyzer({nowarn_function, yeccpars2_129_float/1}).
-compile({nowarn_unused_function,  yeccpars2_129_float/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_flow/1}).
-dialyzer({nowarn_function, yeccpars2_129_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_129_flow/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_if/1}).
-dialyzer({nowarn_function, yeccpars2_129_if/1}).
-compile({nowarn_unused_function,  yeccpars2_129_if/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_in/1}).
-dialyzer({nowarn_function, yeccpars2_129_in/1}).
-compile({nowarn_unused_function,  yeccpars2_129_in/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_integer/1}).
-dialyzer({nowarn_function, yeccpars2_129_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_129_integer/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_129_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_129_lbrace/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_129_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_129_lbracket/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_let/1}).
-dialyzer({nowarn_function, yeccpars2_129_let/1}).
-compile({nowarn_unused_function,  yeccpars2_129_let/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_129_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_129_lower_ident/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_129_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_129_lparen/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_129_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_129_pipe/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_129_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_129_pipe_right/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_129_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_129_rbrace/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_129_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_129_rbracket/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_129_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_129_rparen/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_shape/1}).
-dialyzer({nowarn_function, yeccpars2_129_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_129_shape/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_string/1}).
-dialyzer({nowarn_function, yeccpars2_129_string/1}).
-compile({nowarn_unused_function,  yeccpars2_129_string/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_then/1}).
-dialyzer({nowarn_function, yeccpars2_129_then/1}).
-compile({nowarn_unused_function,  yeccpars2_129_then/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_129_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_129_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_129_upper_ident/1}).
-file("topos_parser.yrl", 334).
yeccpars2_129_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, neq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-dialyzer({nowarn_function, yeccpars2_130_/1}).
-compile({nowarn_unused_function,  yeccpars2_130_/1}).
-file("topos_parser.yrl", 319).
yeccpars2_130_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                         
    {binary_op, minus, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_131_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_131_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_131_$end'/1}).
-file("topos_parser.yrl", 343).
'yeccpars2_131_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_131_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_131_arrow/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_bind/1}).
-dialyzer({nowarn_function, yeccpars2_131_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_131_bind/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_comma/1}).
-dialyzer({nowarn_function, yeccpars2_131_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_131_comma/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_dot/1}).
-dialyzer({nowarn_function, yeccpars2_131_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_131_dot/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_else/1}).
-dialyzer({nowarn_function, yeccpars2_131_else/1}).
-compile({nowarn_unused_function,  yeccpars2_131_else/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_end/1}).
-dialyzer({nowarn_function, yeccpars2_131_end/1}).
-compile({nowarn_unused_function,  yeccpars2_131_end/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_eq/1}).
-dialyzer({nowarn_function, yeccpars2_131_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_131_eq/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_equals/1}).
-dialyzer({nowarn_function, yeccpars2_131_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_131_equals/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_float/1}).
-dialyzer({nowarn_function, yeccpars2_131_float/1}).
-compile({nowarn_unused_function,  yeccpars2_131_float/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_flow/1}).
-dialyzer({nowarn_function, yeccpars2_131_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_131_flow/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_if/1}).
-dialyzer({nowarn_function, yeccpars2_131_if/1}).
-compile({nowarn_unused_function,  yeccpars2_131_if/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_in/1}).
-dialyzer({nowarn_function, yeccpars2_131_in/1}).
-compile({nowarn_unused_function,  yeccpars2_131_in/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_integer/1}).
-dialyzer({nowarn_function, yeccpars2_131_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_131_integer/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_131_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_131_lbrace/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_131_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_131_lbracket/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_let/1}).
-dialyzer({nowarn_function, yeccpars2_131_let/1}).
-compile({nowarn_unused_function,  yeccpars2_131_let/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_131_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_131_lower_ident/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_131_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_131_lparen/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_neq/1}).
-dialyzer({nowarn_function, yeccpars2_131_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_131_neq/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_131_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_131_pipe/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_131_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_131_pipe_right/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_131_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_131_rbrace/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_131_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_131_rbracket/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_131_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_131_rparen/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_shape/1}).
-dialyzer({nowarn_function, yeccpars2_131_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_131_shape/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_string/1}).
-dialyzer({nowarn_function, yeccpars2_131_string/1}).
-compile({nowarn_unused_function,  yeccpars2_131_string/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_then/1}).
-dialyzer({nowarn_function, yeccpars2_131_then/1}).
-compile({nowarn_unused_function,  yeccpars2_131_then/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_131_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_131_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_131_upper_ident/1}).
-file("topos_parser.yrl", 343).
yeccpars2_131_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, lte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_132_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_132_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_132_$end'/1}).
-file("topos_parser.yrl", 337).
'yeccpars2_132_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_132_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_132_arrow/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_bind/1}).
-dialyzer({nowarn_function, yeccpars2_132_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_132_bind/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_comma/1}).
-dialyzer({nowarn_function, yeccpars2_132_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_132_comma/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_dot/1}).
-dialyzer({nowarn_function, yeccpars2_132_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_132_dot/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_else/1}).
-dialyzer({nowarn_function, yeccpars2_132_else/1}).
-compile({nowarn_unused_function,  yeccpars2_132_else/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_end/1}).
-dialyzer({nowarn_function, yeccpars2_132_end/1}).
-compile({nowarn_unused_function,  yeccpars2_132_end/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_eq/1}).
-dialyzer({nowarn_function, yeccpars2_132_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_132_eq/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_equals/1}).
-dialyzer({nowarn_function, yeccpars2_132_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_132_equals/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_float/1}).
-dialyzer({nowarn_function, yeccpars2_132_float/1}).
-compile({nowarn_unused_function,  yeccpars2_132_float/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_flow/1}).
-dialyzer({nowarn_function, yeccpars2_132_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_132_flow/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_if/1}).
-dialyzer({nowarn_function, yeccpars2_132_if/1}).
-compile({nowarn_unused_function,  yeccpars2_132_if/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_in/1}).
-dialyzer({nowarn_function, yeccpars2_132_in/1}).
-compile({nowarn_unused_function,  yeccpars2_132_in/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_integer/1}).
-dialyzer({nowarn_function, yeccpars2_132_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_132_integer/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_132_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_132_lbrace/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_132_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_132_lbracket/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_let/1}).
-dialyzer({nowarn_function, yeccpars2_132_let/1}).
-compile({nowarn_unused_function,  yeccpars2_132_let/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_132_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_132_lower_ident/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_132_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_132_lparen/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_neq/1}).
-dialyzer({nowarn_function, yeccpars2_132_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_132_neq/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_132_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_132_pipe/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_132_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_132_pipe_right/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_132_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_132_rbrace/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_132_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_132_rbracket/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_132_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_132_rparen/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_shape/1}).
-dialyzer({nowarn_function, yeccpars2_132_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_132_shape/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_string/1}).
-dialyzer({nowarn_function, yeccpars2_132_string/1}).
-compile({nowarn_unused_function,  yeccpars2_132_string/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_then/1}).
-dialyzer({nowarn_function, yeccpars2_132_then/1}).
-compile({nowarn_unused_function,  yeccpars2_132_then/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_132_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_132_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_132_upper_ident/1}).
-file("topos_parser.yrl", 337).
yeccpars2_132_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, lt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_133_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_133_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_133_$end'/1}).
-file("topos_parser.yrl", 346).
'yeccpars2_133_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_133_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_133_arrow/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_bind/1}).
-dialyzer({nowarn_function, yeccpars2_133_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_133_bind/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_comma/1}).
-dialyzer({nowarn_function, yeccpars2_133_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_133_comma/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_dot/1}).
-dialyzer({nowarn_function, yeccpars2_133_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_133_dot/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_else/1}).
-dialyzer({nowarn_function, yeccpars2_133_else/1}).
-compile({nowarn_unused_function,  yeccpars2_133_else/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_end/1}).
-dialyzer({nowarn_function, yeccpars2_133_end/1}).
-compile({nowarn_unused_function,  yeccpars2_133_end/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_eq/1}).
-dialyzer({nowarn_function, yeccpars2_133_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_133_eq/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_equals/1}).
-dialyzer({nowarn_function, yeccpars2_133_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_133_equals/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_float/1}).
-dialyzer({nowarn_function, yeccpars2_133_float/1}).
-compile({nowarn_unused_function,  yeccpars2_133_float/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_flow/1}).
-dialyzer({nowarn_function, yeccpars2_133_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_133_flow/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_if/1}).
-dialyzer({nowarn_function, yeccpars2_133_if/1}).
-compile({nowarn_unused_function,  yeccpars2_133_if/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_in/1}).
-dialyzer({nowarn_function, yeccpars2_133_in/1}).
-compile({nowarn_unused_function,  yeccpars2_133_in/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_integer/1}).
-dialyzer({nowarn_function, yeccpars2_133_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_133_integer/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_133_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_133_lbrace/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_133_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_133_lbracket/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_let/1}).
-dialyzer({nowarn_function, yeccpars2_133_let/1}).
-compile({nowarn_unused_function,  yeccpars2_133_let/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_133_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_133_lower_ident/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_133_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_133_lparen/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_neq/1}).
-dialyzer({nowarn_function, yeccpars2_133_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_133_neq/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_133_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_133_pipe/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_133_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_133_pipe_right/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_133_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_133_rbrace/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_133_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_133_rbracket/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_133_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_133_rparen/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_shape/1}).
-dialyzer({nowarn_function, yeccpars2_133_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_133_shape/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_string/1}).
-dialyzer({nowarn_function, yeccpars2_133_string/1}).
-compile({nowarn_unused_function,  yeccpars2_133_string/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_then/1}).
-dialyzer({nowarn_function, yeccpars2_133_then/1}).
-compile({nowarn_unused_function,  yeccpars2_133_then/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_133_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_133_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_133_upper_ident/1}).
-file("topos_parser.yrl", 346).
yeccpars2_133_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                       
    {binary_op, gte, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_134_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_134_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_134_$end'/1}).
-file("topos_parser.yrl", 340).
'yeccpars2_134_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_134_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_134_arrow/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_bind/1}).
-dialyzer({nowarn_function, yeccpars2_134_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_134_bind/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_comma/1}).
-dialyzer({nowarn_function, yeccpars2_134_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_134_comma/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_dot/1}).
-dialyzer({nowarn_function, yeccpars2_134_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_134_dot/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_else/1}).
-dialyzer({nowarn_function, yeccpars2_134_else/1}).
-compile({nowarn_unused_function,  yeccpars2_134_else/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_end/1}).
-dialyzer({nowarn_function, yeccpars2_134_end/1}).
-compile({nowarn_unused_function,  yeccpars2_134_end/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_eq/1}).
-dialyzer({nowarn_function, yeccpars2_134_eq/1}).
-compile({nowarn_unused_function,  yeccpars2_134_eq/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_eq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_equals/1}).
-dialyzer({nowarn_function, yeccpars2_134_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_134_equals/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_float/1}).
-dialyzer({nowarn_function, yeccpars2_134_float/1}).
-compile({nowarn_unused_function,  yeccpars2_134_float/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_flow/1}).
-dialyzer({nowarn_function, yeccpars2_134_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_134_flow/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_if/1}).
-dialyzer({nowarn_function, yeccpars2_134_if/1}).
-compile({nowarn_unused_function,  yeccpars2_134_if/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_in/1}).
-dialyzer({nowarn_function, yeccpars2_134_in/1}).
-compile({nowarn_unused_function,  yeccpars2_134_in/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_integer/1}).
-dialyzer({nowarn_function, yeccpars2_134_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_134_integer/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_134_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_134_lbrace/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_134_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_134_lbracket/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_let/1}).
-dialyzer({nowarn_function, yeccpars2_134_let/1}).
-compile({nowarn_unused_function,  yeccpars2_134_let/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_134_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_134_lower_ident/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_134_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_134_lparen/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_neq/1}).
-dialyzer({nowarn_function, yeccpars2_134_neq/1}).
-compile({nowarn_unused_function,  yeccpars2_134_neq/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_neq(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_134_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_134_pipe/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_134_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_134_pipe_right/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_134_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_134_rbrace/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_134_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_134_rbracket/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_134_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_134_rparen/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_shape/1}).
-dialyzer({nowarn_function, yeccpars2_134_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_134_shape/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_string/1}).
-dialyzer({nowarn_function, yeccpars2_134_string/1}).
-compile({nowarn_unused_function,  yeccpars2_134_string/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_then/1}).
-dialyzer({nowarn_function, yeccpars2_134_then/1}).
-compile({nowarn_unused_function,  yeccpars2_134_then/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_134_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_134_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_134_upper_ident/1}).
-file("topos_parser.yrl", 340).
yeccpars2_134_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, gt, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,'yeccpars2_135_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_135_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_135_$end'/1}).
-file("topos_parser.yrl", 331).
'yeccpars2_135_$end'(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_arrow/1}).
-dialyzer({nowarn_function, yeccpars2_135_arrow/1}).
-compile({nowarn_unused_function,  yeccpars2_135_arrow/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_arrow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_bind/1}).
-dialyzer({nowarn_function, yeccpars2_135_bind/1}).
-compile({nowarn_unused_function,  yeccpars2_135_bind/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_bind(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_comma/1}).
-dialyzer({nowarn_function, yeccpars2_135_comma/1}).
-compile({nowarn_unused_function,  yeccpars2_135_comma/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_comma(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_dot/1}).
-dialyzer({nowarn_function, yeccpars2_135_dot/1}).
-compile({nowarn_unused_function,  yeccpars2_135_dot/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_dot(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_else/1}).
-dialyzer({nowarn_function, yeccpars2_135_else/1}).
-compile({nowarn_unused_function,  yeccpars2_135_else/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_else(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_end/1}).
-dialyzer({nowarn_function, yeccpars2_135_end/1}).
-compile({nowarn_unused_function,  yeccpars2_135_end/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_end(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_equals/1}).
-dialyzer({nowarn_function, yeccpars2_135_equals/1}).
-compile({nowarn_unused_function,  yeccpars2_135_equals/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_equals(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_float/1}).
-dialyzer({nowarn_function, yeccpars2_135_float/1}).
-compile({nowarn_unused_function,  yeccpars2_135_float/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_float(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_flow/1}).
-dialyzer({nowarn_function, yeccpars2_135_flow/1}).
-compile({nowarn_unused_function,  yeccpars2_135_flow/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_flow(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_if/1}).
-dialyzer({nowarn_function, yeccpars2_135_if/1}).
-compile({nowarn_unused_function,  yeccpars2_135_if/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_if(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_in/1}).
-dialyzer({nowarn_function, yeccpars2_135_in/1}).
-compile({nowarn_unused_function,  yeccpars2_135_in/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_in(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_integer/1}).
-dialyzer({nowarn_function, yeccpars2_135_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_135_integer/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_integer(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_lbrace/1}).
-dialyzer({nowarn_function, yeccpars2_135_lbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_135_lbrace/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_lbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_lbracket/1}).
-dialyzer({nowarn_function, yeccpars2_135_lbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_135_lbracket/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_lbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_let/1}).
-dialyzer({nowarn_function, yeccpars2_135_let/1}).
-compile({nowarn_unused_function,  yeccpars2_135_let/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_let(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_lower_ident/1}).
-dialyzer({nowarn_function, yeccpars2_135_lower_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_135_lower_ident/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_lower_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_lparen/1}).
-dialyzer({nowarn_function, yeccpars2_135_lparen/1}).
-compile({nowarn_unused_function,  yeccpars2_135_lparen/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_lparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_pipe/1}).
-dialyzer({nowarn_function, yeccpars2_135_pipe/1}).
-compile({nowarn_unused_function,  yeccpars2_135_pipe/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_pipe(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_pipe_right/1}).
-dialyzer({nowarn_function, yeccpars2_135_pipe_right/1}).
-compile({nowarn_unused_function,  yeccpars2_135_pipe_right/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_pipe_right(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_rbrace/1}).
-dialyzer({nowarn_function, yeccpars2_135_rbrace/1}).
-compile({nowarn_unused_function,  yeccpars2_135_rbrace/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_rbrace(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_rbracket/1}).
-dialyzer({nowarn_function, yeccpars2_135_rbracket/1}).
-compile({nowarn_unused_function,  yeccpars2_135_rbracket/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_rbracket(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_rparen/1}).
-dialyzer({nowarn_function, yeccpars2_135_rparen/1}).
-compile({nowarn_unused_function,  yeccpars2_135_rparen/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_rparen(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_shape/1}).
-dialyzer({nowarn_function, yeccpars2_135_shape/1}).
-compile({nowarn_unused_function,  yeccpars2_135_shape/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_shape(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_string/1}).
-dialyzer({nowarn_function, yeccpars2_135_string/1}).
-compile({nowarn_unused_function,  yeccpars2_135_string/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_string(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_then/1}).
-dialyzer({nowarn_function, yeccpars2_135_then/1}).
-compile({nowarn_unused_function,  yeccpars2_135_then/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_then(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_135_upper_ident/1}).
-dialyzer({nowarn_function, yeccpars2_135_upper_ident/1}).
-compile({nowarn_unused_function,  yeccpars2_135_upper_ident/1}).
-file("topos_parser.yrl", 331).
yeccpars2_135_upper_ident(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                      
    {binary_op, eq, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-dialyzer({nowarn_function, yeccpars2_136_/1}).
-compile({nowarn_unused_function,  yeccpars2_136_/1}).
-file("topos_parser.yrl", 328).
yeccpars2_136_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                          
    {binary_op, concat, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-dialyzer({nowarn_function, yeccpars2_137_/1}).
-compile({nowarn_unused_function,  yeccpars2_137_/1}).
-file("topos_parser.yrl", 378).
yeccpars2_137_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-dialyzer({nowarn_function, yeccpars2_138_/1}).
-compile({nowarn_unused_function,  yeccpars2_138_/1}).
-file("topos_parser.yrl", 376).
yeccpars2_138_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    
    [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-dialyzer({nowarn_function, yeccpars2_139_/1}).
-compile({nowarn_unused_function,  yeccpars2_139_/1}).
-file("topos_parser.yrl", 313).
yeccpars2_139_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                        
    {binary_op, bind, ___1, ___3, extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-dialyzer({nowarn_function, yeccpars2_140_/1}).
-compile({nowarn_unused_function,  yeccpars2_140_/1}).
-file("topos_parser.yrl", 372).
yeccpars2_140_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               
    {tuple_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-dialyzer({nowarn_function, yeccpars2_145_/1}).
-compile({nowarn_unused_function,  yeccpars2_145_/1}).
-file("topos_parser.yrl", 381).
yeccpars2_145_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                         
    {let_expr,
        [{pat_var, extract_atom(___2), extract_location(___2)}, ___4],
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-dialyzer({nowarn_function, yeccpars2_147_/1}).
-compile({nowarn_unused_function,  yeccpars2_147_/1}).
-file("topos_parser.yrl", 412).
yeccpars2_147_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-dialyzer({nowarn_function, yeccpars2_148_/1}).
-compile({nowarn_unused_function,  yeccpars2_148_/1}).
-file("topos_parser.yrl", 390).
yeccpars2_148_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {list_expr, [], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-dialyzer({nowarn_function, yeccpars2_150_/1}).
-compile({nowarn_unused_function,  yeccpars2_150_/1}).
-file("topos_parser.yrl", 414).
yeccpars2_150_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-dialyzer({nowarn_function, yeccpars2_151_/1}).
-compile({nowarn_unused_function,  yeccpars2_151_/1}).
-file("topos_parser.yrl", 393).
yeccpars2_151_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {list_expr, ___2, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-dialyzer({nowarn_function, yeccpars2_153_/1}).
-compile({nowarn_unused_function,  yeccpars2_153_/1}).
-file("topos_parser.yrl", 403).
yeccpars2_153_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-dialyzer({nowarn_function, yeccpars2_155_/1}).
-compile({nowarn_unused_function,  yeccpars2_155_/1}).
-file("topos_parser.yrl", 396).
yeccpars2_155_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               
    {record_expr, [], undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-dialyzer({nowarn_function, yeccpars2_157_/1}).
-compile({nowarn_unused_function,  yeccpars2_157_/1}).
-file("topos_parser.yrl", 408).
yeccpars2_157_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        
    {extract_atom(___1), ___3}
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-dialyzer({nowarn_function, yeccpars2_159_/1}).
-compile({nowarn_unused_function,  yeccpars2_159_/1}).
-file("topos_parser.yrl", 405).
yeccpars2_159_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-dialyzer({nowarn_function, yeccpars2_160_/1}).
-compile({nowarn_unused_function,  yeccpars2_160_/1}).
-file("topos_parser.yrl", 399).
yeccpars2_160_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    {record_expr, ___2, undefined, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-dialyzer({nowarn_function, yeccpars2_165_/1}).
-compile({nowarn_unused_function,  yeccpars2_165_/1}).
-file("topos_parser.yrl", 387).
yeccpars2_165_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                   
    {if_expr, ___2, ___4, ___6, extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-dialyzer({nowarn_function, yeccpars2_166_/1}).
-compile({nowarn_unused_function,  yeccpars2_166_/1}).
-file("topos_parser.yrl", 352).
yeccpars2_166_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   
    {app, ___1, [___2], extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-dialyzer({nowarn_function, yeccpars2_168_/1}).
-compile({nowarn_unused_function,  yeccpars2_168_/1}).
-file("topos_parser.yrl", 355).
yeccpars2_168_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      
    {record_access, ___1, extract_atom(___3), extract_location(___2)}
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-dialyzer({nowarn_function, yeccpars2_170_/1}).
-compile({nowarn_unused_function,  yeccpars2_170_/1}).
-file("topos_parser.yrl", 299).
yeccpars2_170_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              
    [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-dialyzer({nowarn_function, yeccpars2_172_/1}).
-compile({nowarn_unused_function,  yeccpars2_172_/1}).
-file("topos_parser.yrl", 162).
yeccpars2_172_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                      
    {flow_decl,
        extract_atom(___2),
        undefined,
        [{flow_clause, ___3, ___5, ___7, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-dialyzer({nowarn_function, yeccpars2_173_/1}).
-compile({nowarn_unused_function,  yeccpars2_173_/1}).
-file("topos_parser.yrl", 155).
yeccpars2_173_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        
    {flow_decl,
        extract_atom(___2),
        undefined,
        [{flow_clause, ___3, undefined, ___5, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-dialyzer({nowarn_function, yeccpars2_176_/1}).
-compile({nowarn_unused_function,  yeccpars2_176_/1}).
-file("topos_parser.yrl", 209).
yeccpars2_176_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_183_/1}).
-dialyzer({nowarn_function, yeccpars2_183_/1}).
-compile({nowarn_unused_function,  yeccpars2_183_/1}).
-file("topos_parser.yrl", 221).
yeccpars2_183_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                       
    {match_clause,
        ___2,
        ___4,
        ___6,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-dialyzer({nowarn_function, yeccpars2_184_/1}).
-compile({nowarn_unused_function,  yeccpars2_184_/1}).
-file("topos_parser.yrl", 214).
yeccpars2_184_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         
    {match_clause,
        ___2,
        undefined,
        ___4,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-dialyzer({nowarn_function, yeccpars2_185_/1}).
-compile({nowarn_unused_function,  yeccpars2_185_/1}).
-file("topos_parser.yrl", 211).
yeccpars2_185_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-dialyzer({nowarn_function, yeccpars2_186_/1}).
-compile({nowarn_unused_function,  yeccpars2_186_/1}).
-file("topos_parser.yrl", 169).
yeccpars2_186_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                             
    {flow_decl,
        extract_atom(___2),
        undefined,
        [{flow_clause, ___3, undefined, {match_expr, ___6, extract_location(___5)}, extract_location(___1)}],
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_187_/1}).
-dialyzer({nowarn_function, yeccpars2_187_/1}).
-compile({nowarn_unused_function,  yeccpars2_187_/1}).
-file("topos_parser.yrl", 93).
yeccpars2_187_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].

-compile({inline,yeccpars2_188_/1}).
-dialyzer({nowarn_function, yeccpars2_188_/1}).
-compile({nowarn_unused_function,  yeccpars2_188_/1}).
-file("topos_parser.yrl", 147).
yeccpars2_188_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    {flow_decl,
        extract_flow_name(___1),
        extract_flow_type(___1),
        ___2,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_189_/1}).
-dialyzer({nowarn_function, yeccpars2_189_/1}).
-compile({nowarn_unused_function,  yeccpars2_189_/1}).
-file("topos_parser.yrl", 179).
yeccpars2_189_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             
    [___1]
  end | __Stack].

-compile({inline,yeccpars2_191_/1}).
-dialyzer({nowarn_function, yeccpars2_191_/1}).
-compile({nowarn_unused_function,  yeccpars2_191_/1}).
-file("topos_parser.yrl", 232).
yeccpars2_191_(__Stack0) ->
 [begin
                          
    []
  end | __Stack0].

-compile({inline,yeccpars2_197_/1}).
-dialyzer({nowarn_function, yeccpars2_197_/1}).
-compile({nowarn_unused_function,  yeccpars2_197_/1}).
-file("topos_parser.yrl", 191).
yeccpars2_197_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                        
    {flow_clause,
        ___3,
        ___5,
        ___7,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-dialyzer({nowarn_function, yeccpars2_198_/1}).
-compile({nowarn_unused_function,  yeccpars2_198_/1}).
-file("topos_parser.yrl", 184).
yeccpars2_198_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          
    {flow_clause,
        ___3,
        undefined,
        ___5,
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-dialyzer({nowarn_function, yeccpars2_201_/1}).
-compile({nowarn_unused_function,  yeccpars2_201_/1}).
-file("topos_parser.yrl", 198).
yeccpars2_201_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                               
    {flow_clause,
        ___3,
        undefined,
        {match_expr, ___6, extract_location(___5)},
        extract_location(___1)}
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-dialyzer({nowarn_function, yeccpars2_202_/1}).
-compile({nowarn_unused_function,  yeccpars2_202_/1}).
-file("topos_parser.yrl", 181).
yeccpars2_202_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                          
    [___1 | ___2]
  end | __Stack].


-file("topos_parser.yrl", 559).
