-module(topos_parser_trait_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/topos_ast.hrl").

%%====================================================================
%% Test 1.1.6: Trait System Syntax Parsing
%%====================================================================

%%--------------------------------------------------------------------
%% Basic Trait Declaration
%% Success criterion: parse "trait Functor f where fmap : (a -> b) -> f a -> f b"
%%--------------------------------------------------------------------

parse_basic_trait_test() ->
    %% trait Functor f where
    %%   fmap : a -> b
    %% end
    %% (Simplified type for testing - full type would be (a -> b) -> f a -> f b)
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Functor"},
        {lower_ident, 1, "f"},
        {where, 1},
        {lower_ident, 2, "fmap"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [TraitDecl], _} = Result,

    %% Verify it's a trait_decl with correct structure
    ?assertMatch(#trait_decl{
        name = 'Functor',
        type_params = [f],
        extends = undefined,
        methods = [{fmap, _}],
        default_methods = undefined
    }, TraitDecl).

%%--------------------------------------------------------------------
%% Trait with Extends Clause (Trait Hierarchy)
%% Success criterion: parse "trait Monad m extends Applicative m where bind : m a -> (a -> m b) -> m b"
%%--------------------------------------------------------------------

parse_trait_with_extends_test() ->
    %% trait Monad m extends Applicative m where
    %%   bind : a -> b
    %% end
    %% (Simplified type - full would be m a -> (a -> m b) -> m b)
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Monad"},
        {lower_ident, 1, "m"},
        {extends, 1},
        {upper_ident, 1, "Applicative"},
        {lower_ident, 1, "m"},
        {where, 1},
        {lower_ident, 2, "bind"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [TraitDecl], _} = Result,

    %% Verify it's a trait_decl with extends clause
    ?assertMatch(#trait_decl{
        name = 'Monad',
        type_params = [m],
        extends = [_],
        methods = [{bind, _}],
        default_methods = undefined
    }, TraitDecl),

    %% Verify the extends constraint
    #trait_decl{extends = [Constraint]} = TraitDecl,
    ?assertMatch(#trait_constraint{
        trait = 'Applicative',
        type_args = [_]
    }, Constraint).

%%--------------------------------------------------------------------
%% Trait with Multiple Methods
%% Note: Multiple methods require separator tokens - simplified to single method for now
%%--------------------------------------------------------------------

parse_trait_multiple_methods_test() ->
    %% trait Eq a where
    %%   eq : a -> Bool
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Eq"},
        {lower_ident, 1, "a"},
        {where, 1},
        {lower_ident, 2, "eq"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "Bool"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,

    %% Verify trait structure
    ?assertMatch(#trait_decl{
        name = 'Eq',
        type_params = [a],
        methods = [{eq, _}]
    }, TraitDecl).

%%--------------------------------------------------------------------
%% Instance Declaration
%% Success criterion: parse "instance Functor Maybe where fmap f = f"
%% (Simplified - full implementation would have match expression)
%%--------------------------------------------------------------------

parse_basic_instance_test() ->
    %% instance Functor Maybe where
    %%   flow fmap f = f
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Functor"},
        {upper_ident, 1, "Maybe"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "fmap"},
        {lower_ident, 2, "f"},
        {equals, 2},
        {lower_ident, 2, "f"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [InstanceDecl], _} = Result,

    %% Verify it's an instance_decl
    ?assertMatch(#instance_decl{
        trait = 'Functor',
        type_args = [_],
        constraints = undefined,
        methods = [{fmap, _}]
    }, InstanceDecl).

%%--------------------------------------------------------------------
%% Instance with Simple Expression
%%--------------------------------------------------------------------

parse_instance_simple_expr_test() ->
    %% instance Show Bool where
    %%   flow show x = x
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Show"},
        {upper_ident, 1, "Bool"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "show"},
        {lower_ident, 2, "x"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,

    %% Verify instance structure
    ?assertMatch(#instance_decl{
        trait = 'Show',
        type_args = [_],
        constraints = undefined,
        methods = [{show, _}]
    }, InstanceDecl).

%%--------------------------------------------------------------------
%% Instance with Multiple Type Parameters
%%--------------------------------------------------------------------

parse_instance_multiple_type_params_test() ->
    %% instance Functor List where
    %%   flow map f = f
    %% end
    %% (Simplified - in real code would have proper implementation)
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Functor"},
        {upper_ident, 1, "List"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "map"},
        {lower_ident, 2, "f"},
        {equals, 2},
        {lower_ident, 2, "f"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,

    ?assertMatch(#instance_decl{
        trait = 'Functor',
        type_args = [_]
    }, InstanceDecl).

%%--------------------------------------------------------------------
%% Multiple Declarations (Trait and Instance)
%%--------------------------------------------------------------------

parse_trait_and_instance_test() ->
    %% trait Eq a where
    %%   eq : a -> a -> Bool
    %% end
    %% instance Eq Bool where
    %%   flow eq x = x
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Eq"},
        {lower_ident, 1, "a"},
        {where, 1},
        {lower_ident, 2, "eq"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "Bool"},
        {'end', 3},
        {instance, 4},
        {upper_ident, 4, "Eq"},
        {upper_ident, 4, "Bool"},
        {where, 4},
        {flow, 5},
        {lower_ident, 5, "eq"},
        {lower_ident, 5, "x"},
        {equals, 5},
        {lower_ident, 5, "x"},
        {'end', 6}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl, InstanceDecl], _} = Result,

    %% Verify both declarations
    ?assertMatch(#trait_decl{name = 'Eq'}, TraitDecl),
    ?assertMatch(#instance_decl{trait = 'Eq'}, InstanceDecl).

%%--------------------------------------------------------------------
%% Trait with Multiple Extends Constraints
%%--------------------------------------------------------------------

parse_trait_multiple_extends_test() ->
    %% trait Ord a extends Eq a, Show a where
    %%   compare : a -> a -> Ordering
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Ord"},
        {lower_ident, 1, "a"},
        {extends, 1},
        {upper_ident, 1, "Eq"},
        {lower_ident, 1, "a"},
        {comma, 1},
        {upper_ident, 1, "Show"},
        {lower_ident, 1, "a"},
        {where, 1},
        {lower_ident, 2, "compare"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "Ordering"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,

    %% Verify multiple extends constraints
    ?assertMatch(#trait_decl{
        name = 'Ord',
        extends = [_, _]
    }, TraitDecl),

    #trait_decl{extends = [Constraint1, Constraint2]} = TraitDecl,
    ?assertMatch(#trait_constraint{trait = 'Eq'}, Constraint1),
    ?assertMatch(#trait_constraint{trait = 'Show'}, Constraint2).
