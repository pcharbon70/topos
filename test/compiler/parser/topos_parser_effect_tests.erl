-module(topos_parser_effect_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Effect Declaration Tests
%%====================================================================

effect_decl_empty_test() ->
    %% Test empty effect declaration
    Code = "effect FileIO end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {effect_decl, 'FileIO', [], _}
        ], _},
        AST
    ).

effect_decl_single_operation_no_type_test() ->
    %% Test effect with single operation without type signature
    Code = "effect Console\n"
           "  operation print\n"
           "end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {effect_decl, 'Console', [
                {effect_operation, print, undefined, _}
            ], _}
        ], _},
        AST
    ).

effect_decl_single_operation_with_type_test() ->
    %% Test effect with single operation with type signature
    Code = "effect FileIO\n"
           "  operation readFile : String\n"
           "end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {effect_decl, 'FileIO', [
                {effect_operation, readFile, {type_con, 'String', _}, _}
            ], _}
        ], _},
        AST
    ).

effect_decl_multiple_operations_test() ->
    %% Test effect with multiple operations
    Code = "effect FileIO\n"
           "  operation readFile : String\n"
           "  operation writeFile : String\n"
           "  operation deleteFile\n"
           "end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {effect_decl, 'FileIO', [
                {effect_operation, readFile, {type_con, 'String', _}, _},
                {effect_operation, writeFile, {type_con, 'String', _}, _},
                {effect_operation, deleteFile, undefined, _}
            ], _}
        ], _},
        AST
    ).

effect_decl_function_type_test() ->
    %% Test effect operation with function type
    Code = "effect State\n"
           "  operation get : Unit\n"
           "  operation put : Int\n"
           "end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {effect_decl, 'State', [
                {effect_operation, get, {type_con, 'Unit', _}, _},
                {effect_operation, put, {type_con, 'Int', _}, _}
            ], _}
        ], _},
        AST
    ).

%%====================================================================
%% Perform Expression Tests
%%====================================================================

perform_expr_no_args_no_parens_test() ->
    %% Test perform expression without arguments (parentheses required in current implementation)
    Code = "flow main = perform Console.print()",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, main, undefined, [
                {flow_clause, [], undefined,
                    {perform_expr, 'Console', print, [], _},
                _}
            ], _}
        ], _},
        AST
    ).

perform_expr_no_args_with_parens_test() ->
    %% Test perform expression with empty parentheses
    Code = "flow main = perform FileIO.readFile()",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, main, undefined, [
                {flow_clause, [], undefined,
                    {perform_expr, 'FileIO', readFile, [], _},
                _}
            ], _}
        ], _},
        AST
    ).

perform_expr_single_arg_test() ->
    %% Test perform expression with single argument
    Code = "flow writeLog msg = perform FileIO.writeFile(msg)",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, writeLog, undefined, [
                {flow_clause, [{pat_var, msg, _}], undefined,
                    {perform_expr, 'FileIO', writeFile, [
                        {var, msg, _}
                    ], _},
                _}
            ], _}
        ], _},
        AST
    ).

perform_expr_multiple_args_test() ->
    %% Test perform expression with multiple arguments
    Code = "flow copyFile src dst = perform FileIO.copy(src, dst)",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, copyFile, undefined, [
                {flow_clause, [{pat_var, src, _}, {pat_var, dst, _}], undefined,
                    {perform_expr, 'FileIO', copy, [
                        {var, src, _},
                        {var, dst, _}
                    ], _},
                _}
            ], _}
        ], _},
        AST
    ).

%%====================================================================
%% Try-With Handler Tests
%%====================================================================

try_with_single_handler_single_operation_test() ->
    %% Test try-with with single handler and single operation
    Code = "flow safe =\n"
           "  try\n"
           "    perform FileIO.readFile()\n"
           "  with FileIO {\n"
           "    readFile -> 42\n"
           "  }\n"
           "  end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, safe, undefined, [
                {flow_clause, [], undefined,
                    {try_with_expr,
                        {perform_expr, 'FileIO', readFile, [], _},
                        [
                            {handler_clause, 'FileIO', [
                                {operation_case, readFile, [],
                                    {literal, 42, integer, _},
                                _}
                            ], _}
                        ],
                    _},
                _}
            ], _}
        ], _},
        AST
    ).

try_with_single_handler_multiple_operations_test() ->
    %% Test try-with with single handler and multiple operations
    Code = "flow safeIO =\n"
           "  try\n"
           "    perform FileIO.readFile()\n"
           "  with FileIO {\n"
           "    readFile -> 0\n"
           "    writeFile -> 1\n"
           "  }\n"
           "  end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, safeIO, undefined, [
                {flow_clause, [], undefined,
                    {try_with_expr,
                        {perform_expr, 'FileIO', readFile, [], _},
                        [
                            {handler_clause, 'FileIO', [
                                {operation_case, readFile, [],
                                    {literal, 0, integer, _},
                                _},
                                {operation_case, writeFile, [],
                                    {literal, 1, integer, _},
                                _}
                            ], _}
                        ],
                    _},
                _}
            ], _}
        ], _},
        AST
    ).

try_with_operation_with_params_test() ->
    %% Test try-with with operation that has parameters
    Code = "flow handleWrite =\n"
           "  try\n"
           "    perform FileIO.writeFile()\n"
           "  with FileIO {\n"
           "    writeFile(path, content) -> path\n"
           "  }\n"
           "  end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, handleWrite, undefined, [
                {flow_clause, [], undefined,
                    {try_with_expr,
                        {perform_expr, 'FileIO', writeFile, [], _},
                        [
                            {handler_clause, 'FileIO', [
                                {operation_case, writeFile, [
                                    {pat_var, path, _},
                                    {pat_var, content, _}
                                ],
                                    {var, path, _},
                                _}
                            ], _}
                        ],
                    _},
                _}
            ], _}
        ], _},
        AST
    ).

try_with_multiple_handlers_test() ->
    %% Test try-with with multiple handlers
    Code = "flow multiEffect =\n"
           "  try\n"
           "    perform FileIO.readFile()\n"
           "  with FileIO {\n"
           "    readFile -> 0\n"
           "  }\n"
           "  Console {\n"
           "    print(msg) -> msg\n"
           "  }\n"
           "  end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, multiEffect, undefined, [
                {flow_clause, [], undefined,
                    {try_with_expr,
                        {perform_expr, 'FileIO', readFile, [], _},
                        [
                            {handler_clause, 'FileIO', [
                                {operation_case, readFile, [],
                                    {literal, 0, integer, _},
                                _}
                            ], _},
                            {handler_clause, 'Console', [
                                {operation_case, print, [
                                    {pat_var, msg, _}
                                ],
                                    {var, msg, _},
                                _}
                            ], _}
                        ],
                    _},
                _}
            ], _}
        ], _},
        AST
    ).

%%====================================================================
%% Effect Annotation Tests
%%====================================================================

effect_annotation_single_effect_test() ->
    %% Test type with single effect annotation
    Code = "flow readConfig : String / {FileIO}",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, readConfig,
                {type_effect,
                    {type_con, 'String', _},
                    ['FileIO'],
                _},
            [], _}
        ], _},
        AST
    ).

effect_annotation_multiple_effects_test() ->
    %% Test type with multiple effect annotations
    Code = "flow interactive : String / {FileIO, Console, Network}",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {flow_decl, interactive,
                {type_effect,
                    {type_con, 'String', _},
                    ['FileIO', 'Console', 'Network'],
                _},
            [], _}
        ], _},
        AST
    ).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_effect_complete_program_test() ->
    %% Test complete program with effect declaration, perform, and try-with
    Code = "effect FileIO\n"
           "  operation readFile : String\n"
           "end\n"
           "\n"
           "flow loadConfig =\n"
           "  perform FileIO.readFile()\n"
           "\n"
           "flow main =\n"
           "  try\n"
           "    loadConfig\n"
           "  with FileIO {\n"
           "    readFile -> 0\n"
           "  }\n"
           "  end",
    Tokens = tokenize(Code),
    {ok, AST} = topos_parser:parse(Tokens),
    ?assertMatch(
        {module, undefined, [], [], [
            {effect_decl, 'FileIO', [
                {effect_operation, readFile, {type_con, 'String', _}, _}
            ], _},
            {flow_decl, loadConfig, undefined, [
                {flow_clause, [], undefined,
                    {perform_expr, 'FileIO', readFile, [], _},
                _}
            ], _},
            {flow_decl, main, undefined, [
                {flow_clause, [], undefined,
                    {try_with_expr,
                        {var, loadConfig, _},
                        [
                            {handler_clause, 'FileIO', [
                                {operation_case, readFile, [],
                                    {literal, 0, integer, _},
                                _}
                            ], _}
                        ],
                    _},
                _}
            ], _}
        ], _},
        AST
    ).

%%====================================================================
%% Helper Functions
%%====================================================================

tokenize(Code) ->
    {ok, Tokens, _EndLine} = topos_lexer:string(Code),
    Tokens.
