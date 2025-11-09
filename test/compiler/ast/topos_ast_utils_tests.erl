-module(topos_ast_utils_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Traversal Tests
%%====================================================================

map_expr_literal_test() ->
    %% Test that map_expr preserves literals
    Expr = {literal, 42, integer, {line, 1}},
    Result = topos_ast_utils:map_expr(fun(E) -> E end, Expr),
    ?assertEqual(Expr, Result).

map_expr_binary_op_test() ->
    %% Test mapping over binary operations
    Left = {literal, 1, integer, {line, 1}},
    Right = {literal, 2, integer, {line, 1}},
    Expr = {binary_op, plus, Left, Right, {line, 1}},

    %% Map function that increments all literals
    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 10, Type, Loc};
             (E) -> E
          end,

    Result = topos_ast_utils:map_expr(Fun, Expr),
    Expected = {binary_op, plus,
        {literal, 11, integer, {line, 1}},
        {literal, 12, integer, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

map_expr_nested_test() ->
    %% Test mapping over nested expressions
    Expr = {binary_op, plus,
        {binary_op, star,
            {literal, 2, integer, {line, 1}},
            {literal, 3, integer, {line, 1}},
            {line, 1}},
        {literal, 5, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = topos_ast_utils:map_expr(Fun, Expr),
    Expected = {binary_op, plus,
        {binary_op, star,
            {literal, 4, integer, {line, 1}},
            {literal, 6, integer, {line, 1}},
            {line, 1}},
        {literal, 10, integer, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

map_expr_list_test() ->
    %% Test mapping over list expressions
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
             (E) -> E
          end,

    Result = topos_ast_utils:map_expr(Fun, Expr),
    Expected = {list_expr, [
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}},
        {literal, 4, integer, {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

map_expr_perform_test() ->
    %% Test mapping over perform expressions
    Expr = {perform_expr, 'FileIO', read, [
        {literal, 42, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = topos_ast_utils:map_expr(Fun, Expr),
    Expected = {perform_expr, 'FileIO', read, [
        {literal, 84, integer, {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_count_literals_test() ->
    %% Test folding to count literals
    Expr = {binary_op, plus,
        {binary_op, star,
            {literal, 2, integer, {line, 1}},
            {literal, 3, integer, {line, 1}},
            {line, 1}},
        {literal, 5, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = topos_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(3, Result).

fold_expr_sum_literals_test() ->
    %% Test folding to sum literal values
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, _, _}, Acc) -> Acc + Val;
             (_, Acc) -> Acc
          end,

    Result = topos_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(6, Result).

walk_expr_side_effects_test() ->
    %% Test walking with side effects
    Expr = {binary_op, plus,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {line, 1}},

    %% Use ETS table to collect visited nodes
    ets:new(visited, [named_table, public]),
    ets:insert(visited, {count, 0}),

    Fun = fun(_) ->
        [{count, C}] = ets:lookup(visited, count),
        ets:insert(visited, {count, C + 1})
    end,

    topos_ast_utils:walk_expr(Fun, Expr),
    [{count, FinalCount}] = ets:lookup(visited, count),
    ets:delete(visited),

    %% Should visit: binary_op, literal, literal = 3 nodes
    ?assertEqual(3, FinalCount).

%%====================================================================
%% Validation Tests
%%====================================================================

validate_empty_ast_test() ->
    %% Test validation of empty module
    AST = {module, undefined, [], [], [], {line, 1}},
    ?assertEqual(ok, topos_ast_utils:validate_ast(AST)).

check_duplicate_names_no_duplicates_test() ->
    %% Test no duplicates
    AST = {module, undefined, [], [], [
        {flow_decl, foo, undefined, [], {line, 1}},
        {flow_decl, bar, undefined, [], {line, 2}},
        {shape_decl, baz, [], [], [], {line, 3}}
    ], {line, 1}},
    ?assertEqual(ok, topos_ast_utils:validate_ast(AST)).

check_duplicate_names_with_duplicates_test() ->
    %% Test duplicate detection
    AST = {module, undefined, [], [], [
        {flow_decl, foo, undefined, [], {line, 1}},
        {flow_decl, foo, undefined, [], {line, 2}}
    ], {line, 1}},
    Result = topos_ast_utils:validate_ast(AST),
    ?assertMatch({error, {duplicate_declaration, foo}}, Result).

%%====================================================================
%% Pretty-printing Tests
%%====================================================================

format_expr_literal_integer_test() ->
    Expr = {literal, 42, integer, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("42", Result).

format_expr_literal_float_test() ->
    Expr = {literal, 3.14, float, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("3.14000000000000012434e+00", Result).  % Erlang float formatting

format_expr_literal_string_test() ->
    Expr = {literal, "hello", string, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("\"hello\"", Result).

format_expr_variable_test() ->
    Expr = {var, x, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("x", Result).

format_expr_binary_op_test() ->
    Expr = {binary_op, plus,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("(1 plus 2)", Result).

format_expr_list_test() ->
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("[1, 2, 3]", Result).

format_expr_tuple_test() ->
    Expr = {tuple_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("(1, 2)", Result).

format_expr_perform_test() ->
    Expr = {perform_expr, 'FileIO', read, [
        {literal, 42, integer, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_expr(Expr)),
    ?assertEqual("perform FileIO.read(42)", Result).

format_pattern_var_test() ->
    Pattern = {pat_var, x, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_pattern(Pattern)),
    ?assertEqual("x", Result).

format_pattern_wildcard_test() ->
    Pattern = {pat_wildcard, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_pattern(Pattern)),
    ?assertEqual("_", Result).

format_pattern_literal_test() ->
    Pattern = {pat_literal, 42, integer, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_pattern(Pattern)),
    ?assertEqual("42", Result).

format_pattern_constructor_test() ->
    Pattern = {pat_constructor, 'Some', [
        {pat_var, x, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_pattern(Pattern)),
    ?assertEqual("Some x", Result).

format_type_var_test() ->
    Type = {type_var, a, {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_type(Type)),
    ?assertEqual("a", Result).

format_type_con_test() ->
    Type = {type_con, 'Int', {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_type(Type)),
    ?assertEqual("Int", Result).

format_type_fun_test() ->
    Type = {type_fun,
        {type_con, 'Int', {line, 1}},
        {type_con, 'String', {line, 1}},
        {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_type(Type)),
    ?assertEqual("Int -> String", Result).

format_type_effect_test() ->
    Type = {type_effect,
        {type_con, 'String', {line, 1}},
        ['FileIO', 'Network'],
        {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_type(Type)),
    ?assertEqual("String / {FileIO, Network}", Result).

format_decl_shape_test() ->
    Decl = {shape_decl, 'Maybe', [], [], [], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_decl(Decl)),
    ?assertEqual("shape Maybe", Result).

format_decl_flow_test() ->
    Decl = {flow_decl, foo, undefined, [], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_decl(Decl)),
    ?assertEqual("flow foo", Result).

format_decl_effect_test() ->
    Decl = {effect_decl, 'FileIO', [], {line, 1}},
    Result = lists:flatten(topos_ast_utils:format_decl(Decl)),
    ?assertEqual("effect FileIO", Result).

%%====================================================================
%% Location Utility Tests
%%====================================================================

get_location_from_node_test() ->
    Node = {var, x, {line, 42}},
    ?assertEqual({line, 42}, topos_ast_utils:get_location(Node)).

get_location_default_test() ->
    ?assertEqual({line, 1}, topos_ast_utils:get_location(not_a_node)).

default_location_test() ->
    ?assertEqual({line, 1}, topos_ast_utils:default_location()).
