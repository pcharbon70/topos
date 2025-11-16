-module(topos_parser_operator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/topos_ast.hrl").

%%====================================================================
%% Operator Tests - Integration with Trait/Instance System
%% These tests verify that all new operators parse correctly in
%% realistic trait and instance declarations.
%%====================================================================

%%--------------------------------------------------------------------
%% Setoid Equality Operators
%%--------------------------------------------------------------------

parse_setoid_eq_in_trait_test() ->
    %% trait Setoid a where
    %%   eq : a -> a -> Bool
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Setoid"},
        {lower_ident, 1, "a"},
        {where, 1},
        {lower_ident, 2, "eq"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "Bool"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    ?assertMatch(#trait_decl{name = 'Setoid', methods = [{eq, _}]}, TraitDecl).

parse_instance_with_setoid_operators_test() ->
    %% instance Setoid Bool where
    %%   flow eq x y = x === y
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Setoid"},
        {upper_ident, 1, "Bool"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "eq"},
        {lower_ident, 2, "x"},
        {lower_ident, 2, "y"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{trait = 'Setoid', methods = [{eq, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, setoid_eq, _, _, _}, _}, MethodBody).

parse_instance_with_setoid_neq_test() ->
    %% instance Test T where
    %%   flow neq x y = x !== y
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Test"},
        {upper_ident, 1, "T"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "neq"},
        {lower_ident, 2, "x"},
        {lower_ident, 2, "y"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_neq, 2},
        {lower_ident, 2, "y"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{neq, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, setoid_neq, _, _, _}, _}, MethodBody).

%%--------------------------------------------------------------------
%% Functor Operator (<$>)
%%--------------------------------------------------------------------

parse_functor_trait_test() ->
    %% trait Functor f where
    %%   fmap : (a -> b) -> f a -> f b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Functor"},
        {lower_ident, 1, "f"},
        {where, 1},
        {lower_ident, 2, "fmap"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {upper_ident, 2, "f"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "f"},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    ?assertMatch(#trait_decl{name = 'Functor', methods = [{fmap, _}]}, TraitDecl).

parse_instance_with_fmap_operator_test() ->
    %% instance Functor Maybe where
    %%   flow fmap f mx = f <$> mx
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Functor"},
        {upper_ident, 1, "Maybe"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "fmap"},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "mx"},
        {equals, 2},
        {lower_ident, 2, "f"},
        {fmap, 2},
        {lower_ident, 2, "mx"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{fmap, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, fmap, _, _, _}, _}, MethodBody).

%%--------------------------------------------------------------------
%% Applicative Operator (<*>)
%%--------------------------------------------------------------------

parse_applicative_trait_test() ->
    %% trait Applicative f where
    %%   ap : f (a -> b) -> f a -> f b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Applicative"},
        {lower_ident, 1, "f"},
        {where, 1},
        {lower_ident, 2, "ap"},
        {colon, 2},
        {upper_ident, 2, "f"},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {upper_ident, 2, "f"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "f"},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    ?assertMatch(#trait_decl{name = 'Applicative', methods = [{ap, _}]}, TraitDecl).

parse_instance_with_ap_operator_test() ->
    %% instance Applicative Maybe where
    %%   flow ap mf mx = mf <*> mx
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Applicative"},
        {upper_ident, 1, "Maybe"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "ap"},
        {lower_ident, 2, "mf"},
        {lower_ident, 2, "mx"},
        {equals, 2},
        {lower_ident, 2, "mf"},
        {ap, 2},
        {lower_ident, 2, "mx"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{ap, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, ap, _, _, _}, _}, MethodBody).

%%--------------------------------------------------------------------
%% Semigroup Operator (<>)
%%--------------------------------------------------------------------

parse_semigroup_trait_test() ->
    %% trait Semigroup a where
    %%   append : a -> a -> a
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Semigroup"},
        {lower_ident, 1, "a"},
        {where, 1},
        {lower_ident, 2, "append"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    ?assertMatch(#trait_decl{name = 'Semigroup', methods = [{append, _}]}, TraitDecl).

parse_instance_with_append_operator_test() ->
    %% instance Semigroup String where
    %%   flow append x y = x <> y
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Semigroup"},
        {upper_ident, 1, "String"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "append"},
        {lower_ident, 2, "x"},
        {lower_ident, 2, "y"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {concat, 2},
        {lower_ident, 2, "y"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{append, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, concat, _, _, _}, _}, MethodBody).

%%--------------------------------------------------------------------
%% Monadic Operators (>>=, >>, =<<, >=>, <=<)
%%--------------------------------------------------------------------

parse_monad_trait_test() ->
    %% trait Monad m where
    %%   bind : m a -> (a -> m b) -> m b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Monad"},
        {lower_ident, 1, "m"},
        {where, 1},
        {lower_ident, 2, "bind"},
        {colon, 2},
        {upper_ident, 2, "m"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "m"},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {upper_ident, 2, "m"},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    ?assertMatch(#trait_decl{name = 'Monad', methods = [{bind, _}]}, TraitDecl).

parse_instance_with_bind_operator_test() ->
    %% instance Monad Maybe where
    %%   flow bind m f = m >>= f
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Monad"},
        {upper_ident, 1, "Maybe"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "bind"},
        {lower_ident, 2, "m"},
        {lower_ident, 2, "f"},
        {equals, 2},
        {lower_ident, 2, "m"},
        {bind, 2},
        {lower_ident, 2, "f"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{bind, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, bind, _, _, _}, _}, MethodBody).

parse_instance_with_then_operator_test() ->
    %% instance Test T where
    %%   flow then m n = m >> n
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Test"},
        {upper_ident, 1, "T"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "then"},
        {lower_ident, 2, "m"},
        {lower_ident, 2, "n"},
        {equals, 2},
        {lower_ident, 2, "m"},
        {then_op, 2},
        {lower_ident, 2, "n"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{'then', MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, then_op, _, _, _}, _}, MethodBody).

parse_instance_with_bind_flip_test() ->
    %% instance Test T where
    %%   flow bindFlip f m = f =<< m
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Test"},
        {upper_ident, 1, "T"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "bindFlip"},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "m"},
        {equals, 2},
        {lower_ident, 2, "f"},
        {bind_flip, 2},
        {lower_ident, 2, "m"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{bindFlip, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, bind_flip, _, _, _}, _}, MethodBody).

parse_instance_with_kleisli_lr_test() ->
    %% instance Test T where
    %%   flow compose f g = f >=> g
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Test"},
        {upper_ident, 1, "T"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "compose"},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "g"},
        {equals, 2},
        {lower_ident, 2, "f"},
        {kleisli_lr, 2},
        {lower_ident, 2, "g"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{compose, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, kleisli_lr, _, _, _}, _}, MethodBody).

parse_instance_with_kleisli_rl_test() ->
    %% instance Test T where
    %%   flow composeRev f g = f <=< g
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Test"},
        {upper_ident, 1, "T"},
        {where, 1},
        {flow, 2},
        {lower_ident, 2, "composeRev"},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "g"},
        {equals, 2},
        {lower_ident, 2, "f"},
        {kleisli_rl, 2},
        {lower_ident, 2, "g"},
        {'end', 3}
    ],
    {ok, Result} = topos_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    #instance_decl{methods = [{composeRev, MethodBody}]} = InstanceDecl,
    ?assertMatch({lambda, _, {binary_op, kleisli_rl, _, _, _}, _}, MethodBody).

%%--------------------------------------------------------------------
%% Comprehensive Tests - All Operators Together
%%--------------------------------------------------------------------

parse_all_operators_comprehensive_test() ->
    %% Verify all 8 new operators plus existing bind work in one test
    %% Test tokenization and parsing of all operators
    OpTests = [
        {"x === y", setoid_eq},
        {"x !== y", setoid_neq},
        {"f <$> x", fmap},
        {"f <*> x", ap},
        {"x <> y", concat},
        {"m >>= f", bind},
        {"m >> n", then_op},
        {"f =<< m", bind_flip},
        {"f >=> g", kleisli_lr},
        {"f <=< g", kleisli_rl}
    ],

    lists:foreach(
        fun({ExprStr, ExpectedOp}) ->
            {ok, Tokens, _} = topos_lexer:string(ExprStr),
            %% Verify the operator token is present
            ?assert(lists:any(fun({Op, _}) -> Op =:= ExpectedOp; (_) -> false end, Tokens),
                    "Operator " ++ atom_to_list(ExpectedOp) ++ " should tokenize in: " ++ ExprStr)
        end,
        OpTests
    ).
