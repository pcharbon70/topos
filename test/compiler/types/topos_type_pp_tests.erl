%%%-------------------------------------------------------------------
%%% @doc Unit Tests for topos_type_pp module
%%% @end
%%%-------------------------------------------------------------------
-module(topos_type_pp_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    topos_types:init_fresh_counter(),
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Basic Type Pretty-Printing Tests
%%====================================================================

basic_types_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_var()),
      ?_test(test_pp_con()),
      ?_test(test_pp_tuple()),
      ?_test(test_pp_empty_tuple())
     ]}.

test_pp_var() ->
    Var = topos_types:tvar(1),
    ?assertEqual("α1", topos_type_pp:pp_type(Var)),

    Var2 = topos_types:tvar(42),
    ?assertEqual("α42", topos_type_pp:pp_type(Var2)).

test_pp_con() ->
    IntType = topos_types:tcon(integer),
    ?assertEqual("integer", topos_type_pp:pp_type(IntType)),

    StringType = topos_types:tcon(string),
    ?assertEqual("string", topos_type_pp:pp_type(StringType)).

test_pp_tuple() ->
    % (Int, String)
    Tuple = topos_types:ttuple([
        topos_types:tcon(integer),
        topos_types:tcon(string)
    ]),
    ?assertEqual("(integer, string)", topos_type_pp:pp_type(Tuple)).

test_pp_empty_tuple() ->
    % ()
    EmptyTuple = topos_types:ttuple([]),
    ?assertEqual("()", topos_type_pp:pp_type(EmptyTuple)).

%%====================================================================
%% Type Application Tests
%%====================================================================

type_app_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_simple_app()),
      ?_test(test_pp_multiple_args())
     ]}.

test_pp_simple_app() ->
    % List<Int>
    ListInt = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tcon(integer)]
    ),
    ?assertEqual("List<integer>", topos_type_pp:pp_type(ListInt)).

test_pp_multiple_args() ->
    % Map<String, Int>
    MapStringInt = topos_types:tapp(
        topos_types:tcon('Map'),
        [topos_types:tcon(string), topos_types:tcon(integer)]
    ),
    ?assertEqual("Map<string, integer>", topos_type_pp:pp_type(MapStringInt)).

%%====================================================================
%% Function Type Tests
%%====================================================================

function_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_pure_function()),
      ?_test(test_pp_effectful_function()),
      ?_test(test_pp_higher_order_function()),
      ?_test(test_pp_multiple_effects())
     ]}.

test_pp_pure_function() ->
    % Int -> String
    Fun = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        topos_types:empty_effects()
    ),
    ?assertEqual("integer -> string", topos_type_pp:pp_type(Fun)).

test_pp_effectful_function() ->
    % String -> Unit / {io}
    Fun = topos_types:tfun(
        topos_types:tcon(string),
        topos_types:tcon(unit),
        topos_types:singleton_effect(io)
    ),
    ?assertEqual("string -> unit / {io}", topos_type_pp:pp_type(Fun)).

test_pp_higher_order_function() ->
    % (Int -> String) -> List<Int> -> List<String>
    InnerFun = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        topos_types:empty_effects()
    ),

    ListInt = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tcon(integer)]
    ),

    ListString = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tcon(string)]
    ),

    MapFun = topos_types:tfun(
        InnerFun,
        topos_types:tfun(
            ListInt,
            ListString,
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),

    Expected = "(integer -> string) -> List<integer> -> List<string>",
    ?assertEqual(Expected, topos_type_pp:pp_type(MapFun)).

test_pp_multiple_effects() ->
    % Int -> String / {file, io}
    Fun = topos_types:tfun(
        topos_types:tcon(integer),
        topos_types:tcon(string),
        topos_types:normalize_effects([io, file])  % Will be sorted
    ),
    ?assertEqual("integer -> string / {file, io}", topos_type_pp:pp_type(Fun)).

%%====================================================================
%% Record Type Tests
%%====================================================================

record_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_closed_record()),
      ?_test(test_pp_open_record()),
      ?_test(test_pp_empty_open_record())
     ]}.

test_pp_closed_record() ->
    % {x: Int, y: Float}
    Record = topos_types:trecord([
        {x, topos_types:tcon(integer)},
        {y, topos_types:tcon(float)}
    ], closed),
    ?assertEqual("{x: integer, y: float}", topos_type_pp:pp_type(Record)).

test_pp_open_record() ->
    % {name: String | ρ1}
    Record = topos_types:trecord([
        {name, topos_types:tcon(string)}
    ], 1),
    ?assertEqual("{name: string | ρ1}", topos_type_pp:pp_type(Record)).

test_pp_empty_open_record() ->
    % {| ρ1}
    Record = topos_types:trecord([], 1),
    ?assertEqual("{| ρ1}", topos_type_pp:pp_type(Record)).

%%====================================================================
%% Variant Type Tests
%%====================================================================

variant_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_simple_variant()),
      ?_test(test_pp_variant_with_args())
     ]}.

test_pp_simple_variant() ->
    % None | Some
    Variant = topos_types:tvariant([
        {'None', []},
        {'Some', [topos_types:tvar(1)]}
    ]),
    ?assertEqual("None | Some α1", topos_type_pp:pp_type(Variant)).

test_pp_variant_with_args() ->
    % Red | Green | Blue Int String
    Variant = topos_types:tvariant([
        {'Red', []},
        {'Green', []},
        {'Blue', [topos_types:tcon(integer), topos_types:tcon(string)]}
    ]),
    ?assertEqual("Red | Green | Blue integer string",
                topos_type_pp:pp_type(Variant)).

%%====================================================================
%% Type Scheme Tests
%%====================================================================

scheme_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_mono_scheme()),
      ?_test(test_pp_poly_scheme()),
      ?_test(test_pp_complex_poly_scheme())
     ]}.

test_pp_mono_scheme() ->
    Scheme = topos_type_scheme:mono(topos_types:tcon(integer)),
    ?assertEqual("integer", topos_type_pp:pp_scheme(Scheme)).

test_pp_poly_scheme() ->
    % ∀α1. α1 -> α1
    Type = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(1),
        topos_types:empty_effects()
    ),
    Scheme = topos_type_scheme:poly([1], Type),
    ?assertEqual("∀α1. α1 -> α1", topos_type_pp:pp_scheme(Scheme)).

test_pp_complex_poly_scheme() ->
    % ∀α1 α2. (α1 -> α2) -> List<α1> -> List<α2>
    InnerFun = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:empty_effects()
    ),

    ListAlpha1 = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    ),

    ListAlpha2 = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(2)]
    ),

    MapType = topos_types:tfun(
        InnerFun,
        topos_types:tfun(
            ListAlpha1,
            ListAlpha2,
            topos_types:empty_effects()
        ),
        topos_types:empty_effects()
    ),

    Scheme = topos_type_scheme:poly([1, 2], MapType),
    Expected = "∀α1 α2. (α1 -> α2) -> List<α1> -> List<α2>",
    ?assertEqual(Expected, topos_type_pp:pp_scheme(Scheme)).

%%====================================================================
%% Effect Set Tests
%%====================================================================

effects_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_pp_empty_effects()),
      ?_test(test_pp_single_effect()),
      ?_test(test_pp_multiple_effects_sorted())
     ]}.

test_pp_empty_effects() ->
    Effects = topos_types:empty_effects(),
    ?assertEqual("", topos_type_pp:pp_effects(Effects)).

test_pp_single_effect() ->
    Effects = topos_types:singleton_effect(io),
    ?assertEqual("{io}", topos_type_pp:pp_effects(Effects)).

test_pp_multiple_effects_sorted() ->
    Effects = topos_types:normalize_effects([process, io, file]),
    % Should be sorted: file, io, process
    ?assertEqual("{file, io, process}", topos_type_pp:pp_effects(Effects)).

%%====================================================================
%% Complex Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_complex_nested_type())
     ]}.

test_complex_nested_type() ->
    % ∀α β. (α -> β / {io}) -> List<α> -> List<β> / {io}
    InnerFun = topos_types:tfun(
        topos_types:tvar(1),
        topos_types:tvar(2),
        topos_types:singleton_effect(io)
    ),

    ListAlpha = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(1)]
    ),

    ListBeta = topos_types:tapp(
        topos_types:tcon('List'),
        [topos_types:tvar(2)]
    ),

    MapType = topos_types:tfun(
        InnerFun,
        topos_types:tfun(
            ListAlpha,
            ListBeta,
            topos_types:singleton_effect(io)
        ),
        topos_types:empty_effects()
    ),

    Scheme = topos_type_scheme:poly([1, 2], MapType),
    Expected = "∀α1 α2. (α1 -> α2 / {io}) -> List<α1> -> List<α2> / {io}",
    ?assertEqual(Expected, topos_type_pp:pp_scheme(Scheme)).
