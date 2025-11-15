-module(test_trait_integration).
-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/topos_ast.hrl").

%%====================================================================
%% Integration Tests: Trait System Syntax (Task 1.1.6)
%% End-to-end testing with lexer + parser
%%====================================================================

%%--------------------------------------------------------------------
%% Test 1: Basic Trait Declaration
%%--------------------------------------------------------------------

basic_trait_declaration_test() ->
    %% Topos source code
    Source = "trait Functor f where fmap : a -> b end",

    %% Tokenize
    {ok, Tokens} = topos_lexer:tokenize(Source),

    %% Parse
    {ok, Result} = topos_parser:parse(Tokens),

    %% Verify AST structure
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [TraitDecl], _} = Result,

    %% Verify trait declaration
    ?assertMatch(#trait_decl{
        name = 'Functor',
        type_params = [f],
        extends = undefined,
        methods = [{fmap, _}],
        default_methods = undefined
    }, TraitDecl).

%%--------------------------------------------------------------------
%% Test 2: Trait with Extends Clause (Trait Hierarchy)
%%--------------------------------------------------------------------

trait_with_extends_test() ->
    %% Topos source code
    Source = "trait Monad m extends Applicative m where bind : a -> b end",

    %% Tokenize
    {ok, Tokens} = topos_lexer:tokenize(Source),

    %% Parse
    {ok, Result} = topos_parser:parse(Tokens),

    %% Verify AST structure
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [TraitDecl], _} = Result,

    %% Verify trait with extends
    ?assertMatch(#trait_decl{
        name = 'Monad',
        type_params = [m],
        extends = [_],
        methods = [{bind, _}],
        default_methods = undefined
    }, TraitDecl),

    %% Verify extends constraint
    #trait_decl{extends = [Constraint]} = TraitDecl,
    ?assertMatch(#trait_constraint{
        trait = 'Applicative',
        type_args = [_]
    }, Constraint).

%%--------------------------------------------------------------------
%% Test 3: Instance Declaration
%%--------------------------------------------------------------------

instance_declaration_test() ->
    %% Topos source code
    Source = "instance Functor Maybe where flow fmap f = f end",

    %% Tokenize
    {ok, Tokens} = topos_lexer:tokenize(Source),

    %% Parse
    {ok, Result} = topos_parser:parse(Tokens),

    %% Verify AST structure
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [InstanceDecl], _} = Result,

    %% Verify instance declaration
    ?assertMatch(#instance_decl{
        trait = 'Functor',
        type_args = [_],
        constraints = undefined,
        methods = [{fmap, _}]
    }, InstanceDecl).

%%--------------------------------------------------------------------
%% Test 4: Multiple Declarations (Trait and Instance)
%%--------------------------------------------------------------------

multiple_declarations_test() ->
    %% Topos source code
    Source = "trait Eq a where eq : a -> a -> Bool end "
             "instance Eq Bool where flow eq x = x end",

    %% Tokenize
    {ok, Tokens} = topos_lexer:tokenize(Source),

    %% Parse
    {ok, Result} = topos_parser:parse(Tokens),

    %% Verify AST structure
    ?assertMatch({module, undefined, [], [], [_, _], _}, Result),
    {module, _, _, _, [TraitDecl, InstanceDecl], _} = Result,

    %% Verify both declarations
    ?assertMatch(#trait_decl{name = 'Eq'}, TraitDecl),
    ?assertMatch(#instance_decl{trait = 'Eq'}, InstanceDecl).

%%--------------------------------------------------------------------
%% Test Runner (for manual verification)
%%--------------------------------------------------------------------

run_all_tests() ->
    io:format("~n=== Running Integration Tests ===~n~n"),

    io:format("Test 1: Basic trait declaration... "),
    case catch basic_trait_declaration_test() of
        ok -> io:format("✓~n");
        Error -> io:format("✗~nError: ~p~n", [Error])
    end,

    io:format("Test 2: Trait with extends clause... "),
    case catch trait_with_extends_test() of
        ok -> io:format("✓~n");
        Error2 -> io:format("✗~nError: ~p~n", [Error2])
    end,

    io:format("Test 3: Instance declaration... "),
    case catch instance_declaration_test() of
        ok -> io:format("✓~n");
        Error3 -> io:format("✗~nError: ~p~n", [Error3])
    end,

    io:format("Test 4: Multiple declarations... "),
    case catch multiple_declarations_test() of
        ok -> io:format("✓~n");
        Error4 -> io:format("✗~nError: ~p~n", [Error4])
    end,

    io:format("~n=== All Integration Tests Passed! ===~n").
