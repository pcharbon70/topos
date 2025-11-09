-module(topos_ast_utils).

%% AST Traversal
-export([
    map_expr/2,
    fold_expr/3,
    walk_expr/2
]).

%% AST Validation
-export([
    validate_ast/1,
    check_duplicate_names/1
]).

%% AST Pretty-printing
-export([
    format_expr/1,
    format_pattern/1,
    format_type/1,
    format_decl/1
]).

%% Location utilities
-export([
    get_location/1,
    default_location/0
]).

%%====================================================================
%% Traversal Functions
%%====================================================================

%% @doc Map a function over all sub-expressions in an expression (bottom-up)
map_expr(Fun, Expr) ->
    %% First recursively map over children
    Mapped = case Expr of
        {binary_op, Op, Left, Right, Loc} ->
            {binary_op, Op, map_expr(Fun, Left), map_expr(Fun, Right), Loc};
        {unary_op, Op, Operand, Loc} ->
            {unary_op, Op, map_expr(Fun, Operand), Loc};
        {app, Func, Args, Loc} ->
            {app, map_expr(Fun, Func), [map_expr(Fun, Arg) || Arg <- Args], Loc};
        {lambda, Params, Body, Loc} ->
            {lambda, Params, map_expr(Fun, Body), Loc};
        {let_expr, Bindings, Body, Loc} ->
            NewBindings = [{Pat, map_expr(Fun, E)} || {Pat, E} <- Bindings],
            {let_expr, NewBindings, map_expr(Fun, Body), Loc};
        {if_expr, Cond, Then, Else, Loc} ->
            {if_expr, map_expr(Fun, Cond), map_expr(Fun, Then), map_expr(Fun, Else), Loc};
        {match_expr, Clauses, Loc} ->
            NewClauses = [map_match_clause(Fun, C) || C <- Clauses],
            {match_expr, NewClauses, Loc};
        {list_expr, Elements, Loc} ->
            {list_expr, [map_expr(Fun, E) || E <- Elements], Loc};
        {tuple_expr, Elements, Loc} ->
            {tuple_expr, [map_expr(Fun, E) || E <- Elements], Loc};
        {record_expr, Fields, Base, Loc} ->
            NewFields = [{Name, map_expr(Fun, E)} || {Name, E} <- Fields],
            NewBase = case Base of
                undefined -> undefined;
                B -> map_expr(Fun, B)
            end,
            {record_expr, NewFields, NewBase, Loc};
        {record_access, Record, Field, Loc} ->
            {record_access, map_expr(Fun, Record), Field, Loc};
        {perform_expr, Effect, Operation, Args, Loc} ->
            {perform_expr, Effect, Operation, [map_expr(Fun, Arg) || Arg <- Args], Loc};
        {try_with_expr, Body, Handlers, Loc} ->
            NewBody = map_expr(Fun, Body),
            NewHandlers = [map_handler(Fun, H) || H <- Handlers],
            {try_with_expr, NewBody, NewHandlers, Loc};
        _ ->
            %% Leaf nodes (literals, variables)
            Expr
    end,
    %% Then apply the function to the resulting node
    Fun(Mapped).

map_match_clause(Fun, {match_clause, Pattern, Guards, Body, Loc}) ->
    NewGuards = case Guards of
        undefined -> undefined;
        Gs -> [map_expr(Fun, G) || G <- Gs]
    end,
    {match_clause, Pattern, NewGuards, map_expr(Fun, Body), Loc}.

map_handler(Fun, {handler_clause, Effect, Operations, Loc}) ->
    NewOps = [map_operation_case(Fun, Op) || Op <- Operations],
    {handler_clause, Effect, NewOps, Loc}.

map_operation_case(Fun, {operation_case, Operation, Params, Body, Loc}) ->
    {operation_case, Operation, Params, map_expr(Fun, Body), Loc}.

%% @doc Fold over all sub-expressions in an expression
fold_expr(Fun, Acc, Expr) ->
    NewAcc = Fun(Expr, Acc),
    case Expr of
        {binary_op, _Op, Left, Right, _Loc} ->
            Acc2 = fold_expr(Fun, NewAcc, Left),
            fold_expr(Fun, Acc2, Right);
        {unary_op, _Op, Operand, _Loc} ->
            fold_expr(Fun, NewAcc, Operand);
        {app, Func, Args, _Loc} ->
            Acc2 = fold_expr(Fun, NewAcc, Func),
            lists:foldl(fun(Arg, A) -> fold_expr(Fun, A, Arg) end, Acc2, Args);
        {lambda, _Params, Body, _Loc} ->
            fold_expr(Fun, NewAcc, Body);
        {let_expr, Bindings, Body, _Loc} ->
            Acc2 = lists:foldl(fun({_Pat, E}, A) -> fold_expr(Fun, A, E) end, NewAcc, Bindings),
            fold_expr(Fun, Acc2, Body);
        {if_expr, Cond, Then, Else, _Loc} ->
            Acc2 = fold_expr(Fun, NewAcc, Cond),
            Acc3 = fold_expr(Fun, Acc2, Then),
            fold_expr(Fun, Acc3, Else);
        {match_expr, Clauses, _Loc} ->
            lists:foldl(fun(C, A) -> fold_match_clause(Fun, A, C) end, NewAcc, Clauses);
        {list_expr, Elements, _Loc} ->
            lists:foldl(fun(E, A) -> fold_expr(Fun, A, E) end, NewAcc, Elements);
        {tuple_expr, Elements, _Loc} ->
            lists:foldl(fun(E, A) -> fold_expr(Fun, A, E) end, NewAcc, Elements);
        {record_expr, Fields, Base, _Loc} ->
            Acc2 = lists:foldl(fun({_Name, E}, A) -> fold_expr(Fun, A, E) end, NewAcc, Fields),
            case Base of
                undefined -> Acc2;
                B -> fold_expr(Fun, Acc2, B)
            end;
        {record_access, Record, _Field, _Loc} ->
            fold_expr(Fun, NewAcc, Record);
        {perform_expr, _Effect, _Operation, Args, _Loc} ->
            lists:foldl(fun(Arg, A) -> fold_expr(Fun, A, Arg) end, NewAcc, Args);
        {try_with_expr, Body, Handlers, _Loc} ->
            Acc2 = fold_expr(Fun, NewAcc, Body),
            lists:foldl(fun(H, A) -> fold_handler(Fun, A, H) end, Acc2, Handlers);
        _ ->
            NewAcc
    end.

fold_match_clause(Fun, Acc, {match_clause, _Pattern, Guards, Body, _Loc}) ->
    Acc2 = case Guards of
        undefined -> Acc;
        Gs -> lists:foldl(fun(G, A) -> fold_expr(Fun, A, G) end, Acc, Gs)
    end,
    fold_expr(Fun, Acc2, Body).

fold_handler(Fun, Acc, {handler_clause, _Effect, Operations, _Loc}) ->
    lists:foldl(fun(Op, A) -> fold_operation_case(Fun, A, Op) end, Acc, Operations).

fold_operation_case(Fun, Acc, {operation_case, _Operation, _Params, Body, _Loc}) ->
    fold_expr(Fun, Acc, Body).

%% @doc Walk the AST, executing a function for each node
walk_expr(Fun, Expr) ->
    Fun(Expr),
    case Expr of
        {binary_op, _Op, Left, Right, _Loc} ->
            walk_expr(Fun, Left),
            walk_expr(Fun, Right);
        {unary_op, _Op, Operand, _Loc} ->
            walk_expr(Fun, Operand);
        {app, Func, Args, _Loc} ->
            walk_expr(Fun, Func),
            [walk_expr(Fun, Arg) || Arg <- Args];
        {lambda, _Params, Body, _Loc} ->
            walk_expr(Fun, Body);
        {let_expr, Bindings, Body, _Loc} ->
            [walk_expr(Fun, E) || {_Pat, E} <- Bindings],
            walk_expr(Fun, Body);
        {if_expr, Cond, Then, Else, _Loc} ->
            walk_expr(Fun, Cond),
            walk_expr(Fun, Then),
            walk_expr(Fun, Else);
        {match_expr, Clauses, _Loc} ->
            [walk_match_clause(Fun, C) || C <- Clauses];
        {list_expr, Elements, _Loc} ->
            [walk_expr(Fun, E) || E <- Elements];
        {tuple_expr, Elements, _Loc} ->
            [walk_expr(Fun, E) || E <- Elements];
        {record_expr, Fields, Base, _Loc} ->
            [walk_expr(Fun, E) || {_Name, E} <- Fields],
            case Base of
                undefined -> ok;
                B -> walk_expr(Fun, B)
            end;
        {record_access, Record, _Field, _Loc} ->
            walk_expr(Fun, Record);
        {perform_expr, _Effect, _Operation, Args, _Loc} ->
            [walk_expr(Fun, Arg) || Arg <- Args];
        {try_with_expr, Body, Handlers, _Loc} ->
            walk_expr(Fun, Body),
            [walk_handler(Fun, H) || H <- Handlers];
        _ ->
            ok
    end.

walk_match_clause(Fun, {match_clause, _Pattern, Guards, Body, _Loc}) ->
    case Guards of
        undefined -> ok;
        Gs -> [walk_expr(Fun, G) || G <- Gs]
    end,
    walk_expr(Fun, Body).

walk_handler(Fun, {handler_clause, _Effect, Operations, _Loc}) ->
    [walk_operation_case(Fun, Op) || Op <- Operations].

walk_operation_case(Fun, {operation_case, _Operation, _Params, Body, _Loc}) ->
    walk_expr(Fun, Body).

%%====================================================================
%% Validation Functions
%%====================================================================

%% @doc Validate an AST node
validate_ast(AST) ->
    try
        check_duplicate_names(AST),
        ok
    catch
        throw:{error, Reason} -> {error, Reason}
    end.

%% @doc Check for duplicate names in declarations
check_duplicate_names({module, _Name, _Exports, _Imports, Declarations, _Loc}) ->
    Names = [get_decl_name(D) || D <- Declarations],
    Duplicates = Names -- lists:usort(Names),
    case Duplicates of
        [] -> ok;
        [First | _] -> throw({error, {duplicate_declaration, First}})
    end;
check_duplicate_names(_) ->
    ok.

get_decl_name({shape_decl, Name, _, _, _, _}) -> Name;
get_decl_name({flow_decl, Name, _, _, _}) -> Name;
get_decl_name({effect_decl, Name, _, _}) -> Name;
get_decl_name({trait_decl, Name, _, _, _}) -> Name;
get_decl_name(_) -> undefined.

%%====================================================================
%% Pretty-printing Functions
%%====================================================================

%% @doc Format an expression as a string
format_expr({literal, Value, Type, _Loc}) ->
    format_literal(Value, Type);
format_expr({var, Name, _Loc}) ->
    atom_to_list(Name);
format_expr({binary_op, Op, Left, Right, _Loc}) ->
    io_lib:format("(~s ~s ~s)", [format_expr(Left), atom_to_list(Op), format_expr(Right)]);
format_expr({unary_op, Op, Operand, _Loc}) ->
    io_lib:format("(~s ~s)", [atom_to_list(Op), format_expr(Operand)]);
format_expr({app, Func, Args, _Loc}) ->
    ArgsStr = string:join([format_expr(Arg) || Arg <- Args], " "),
    io_lib:format("(~s ~s)", [format_expr(Func), ArgsStr]);
format_expr({lambda, Params, Body, _Loc}) ->
    ParamsStr = string:join([format_pattern(P) || P <- Params], " "),
    io_lib:format("(\\~s -> ~s)", [ParamsStr, format_expr(Body)]);
format_expr({let_expr, Bindings, Body, _Loc}) ->
    BindingsStr = string:join([io_lib:format("~s = ~s", [format_pattern(P), format_expr(E)]) || {P, E} <- Bindings], ", "),
    io_lib:format("let ~s in ~s", [BindingsStr, format_expr(Body)]);
format_expr({if_expr, Cond, Then, Else, _Loc}) ->
    io_lib:format("if ~s then ~s else ~s", [format_expr(Cond), format_expr(Then), format_expr(Else)]);
format_expr({list_expr, Elements, _Loc}) ->
    ElementsStr = string:join([format_expr(E) || E <- Elements], ", "),
    io_lib:format("[~s]", [ElementsStr]);
format_expr({tuple_expr, Elements, _Loc}) ->
    ElementsStr = string:join([format_expr(E) || E <- Elements], ", "),
    io_lib:format("(~s)", [ElementsStr]);
format_expr({perform_expr, Effect, Operation, Args, _Loc}) ->
    ArgsStr = string:join([format_expr(Arg) || Arg <- Args], ", "),
    io_lib:format("perform ~s.~s(~s)", [atom_to_list(Effect), atom_to_list(Operation), ArgsStr]);
format_expr(_) ->
    "<??>".

format_literal(Value, integer) -> integer_to_list(Value);
format_literal(Value, float) -> float_to_list(Value);
format_literal(Value, string) -> io_lib:format("\"~s\"", [Value]).

%% @doc Format a pattern as a string
format_pattern({pat_var, Name, _Loc}) ->
    atom_to_list(Name);
format_pattern({pat_wildcard, _Loc}) ->
    "_";
format_pattern({pat_literal, Value, Type, _Loc}) ->
    format_literal(Value, Type);
format_pattern({pat_constructor, Name, Args, _Loc}) ->
    ArgsStr = string:join([format_pattern(Arg) || Arg <- Args], " "),
    io_lib:format("~s ~s", [atom_to_list(Name), ArgsStr]);
format_pattern({pat_list, Elements, _Loc}) ->
    ElementsStr = string:join([format_pattern(E) || E <- Elements], ", "),
    io_lib:format("[~s]", [ElementsStr]);
format_pattern({pat_tuple, Elements, _Loc}) ->
    ElementsStr = string:join([format_pattern(E) || E <- Elements], ", "),
    io_lib:format("(~s)", [ElementsStr]);
format_pattern(_) ->
    "_".

%% @doc Format a type expression as a string
format_type({type_var, Name, _Loc}) ->
    atom_to_list(Name);
format_type({type_con, Name, _Loc}) ->
    atom_to_list(Name);
format_type({type_fun, From, To, _Loc}) ->
    io_lib:format("~s -> ~s", [format_type(From), format_type(To)]);
format_type({type_app, Constructor, Args, _Loc}) ->
    ArgsStr = string:join([format_type(Arg) || Arg <- Args], " "),
    io_lib:format("~s ~s", [format_type(Constructor), ArgsStr]);
format_type({type_effect, Type, Effects, _Loc}) ->
    EffectsStr = string:join([atom_to_list(E) || E <- Effects], ", "),
    io_lib:format("~s / {~s}", [format_type(Type), EffectsStr]);
format_type(_) ->
    "?".

%% @doc Format a declaration as a string
format_decl({shape_decl, Name, _TypeParams, _Constructors, _Derives, _Loc}) ->
    io_lib:format("shape ~s", [atom_to_list(Name)]);
format_decl({flow_decl, Name, _TypeSig, _Clauses, _Loc}) ->
    io_lib:format("flow ~s", [atom_to_list(Name)]);
format_decl({effect_decl, Name, _Operations, _Loc}) ->
    io_lib:format("effect ~s", [atom_to_list(Name)]);
format_decl(_) ->
    "?".

%%====================================================================
%% Location Utilities
%%====================================================================

%% @doc Get location from an AST node
get_location(Node) when is_tuple(Node) ->
    element(tuple_size(Node), Node);
get_location(_) ->
    default_location().

%% @doc Create a default location
default_location() ->
    {line, 1}.
