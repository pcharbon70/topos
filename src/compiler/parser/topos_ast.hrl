%% Topos AST - Abstract Syntax Tree Node Definitions
%% Phase 1, Task 1.1.2: Grammar Implementation
%%
%% This header file defines all AST node types for the Topos compiler.
%% All nodes include location metadata for error reporting.

%% Location metadata
-type location() :: {line, pos_integer()}.

%%====================================================================
%% Module Structure
%%====================================================================

-record(module, {
    name :: atom(),
    exports :: [export()],
    imports :: [import()],
    declarations :: [declaration()],
    location :: location()
}).

-record(export, {
    name :: atom(),
    arity :: non_neg_integer(),
    location :: location()
}).

-record(import, {
    module :: atom(),
    items :: [atom()] | all,
    qualified :: boolean(),
    alias :: atom() | undefined,
    location :: location()
}).

%%====================================================================
%% Declarations
%%====================================================================

-type declaration() :: #shape_decl{} | #flow_decl{} | #trait_decl{}.

%% Shape declaration (algebraic data type)
-record(shape_decl, {
    name :: atom(),
    type_params :: [atom()],
    constructors :: [#constructor{}],
    derives :: [atom()],
    location :: location()
}).

-record(constructor, {
    name :: atom(),
    fields :: [type_expr()],
    location :: location()
}).

%% Flow declaration (function definition)
-record(flow_decl, {
    name :: atom(),
    type_sig :: type_expr() | undefined,
    clauses :: [#flow_clause{}],
    location :: location()
}).

-record(flow_clause, {
    patterns :: [pattern()],
    guards :: [expr()] | undefined,
    body :: expr(),
    location :: location()
}).

%% Trait declaration (type class)
-record(trait_decl, {
    name :: atom(),
    type_params :: [atom()],
    methods :: [{atom(), type_expr()}],
    location :: location()
}).

%%====================================================================
%% Expressions
%%====================================================================

-type expr() :: #literal{} | #var{} | #app{} | #lambda{} | #let_expr{} |
                #match_expr{} | #if_expr{} | #do_expr{} | #record_expr{} |
                #record_access{} | #binary_op{} | #unary_op{} | #list_expr{} |
                #tuple_expr{}.

%% Literal values
-record(literal, {
    value :: integer() | float() | binary(),
    type :: integer | float | string,
    location :: location()
}).

%% Variable reference
-record(var, {
    name :: atom(),
    location :: location()
}).

%% Function application
-record(app, {
    func :: expr(),
    args :: [expr()],
    location :: location()
}).

%% Lambda expression
-record(lambda, {
    params :: [pattern()],
    body :: expr(),
    location :: location()
}).

%% Let binding
-record(let_expr, {
    bindings :: [{pattern(), expr()}],
    body :: expr(),
    location :: location()
}).

%% Match expression (pattern matching)
-record(match_expr, {
    clauses :: [#match_clause{}],
    location :: location()
}).

-record(match_clause, {
    pattern :: pattern(),
    guards :: [expr()] | undefined,
    body :: expr(),
    location :: location()
}).

%% If expression
-record(if_expr, {
    condition :: expr(),
    then_branch :: expr(),
    else_branch :: expr(),
    location :: location()
}).

%% Do expression (monadic sequencing)
-record(do_expr, {
    statements :: [do_statement()],
    location :: location()
}).

-type do_statement() :: {bind, pattern(), expr()} | {expr, expr()}.

%% Binary operation
-record(binary_op, {
    op :: atom(),
    left :: expr(),
    right :: expr(),
    location :: location()
}).

%% Unary operation
-record(unary_op, {
    op :: atom(),
    operand :: expr(),
    location :: location()
}).

%% Record expression
-record(record_expr, {
    fields :: [{atom(), expr()}],
    base :: expr() | undefined,  % For record update
    location :: location()
}).

%% Record field access
-record(record_access, {
    record :: expr(),
    field :: atom(),
    location :: location()
}).

%% List expression
-record(list_expr, {
    elements :: [expr()],
    location :: location()
}).

%% Tuple expression
-record(tuple_expr, {
    elements :: [expr()],
    location :: location()
}).

%%====================================================================
%% Patterns
%%====================================================================

-type pattern() :: #pat_var{} | #pat_constructor{} | #pat_record{} |
                   #pat_literal{} | #pat_wildcard{} | #pat_as{} |
                   #pat_list{} | #pat_tuple{}.

%% Variable pattern
-record(pat_var, {
    name :: atom(),
    location :: location()
}).

%% Constructor pattern
-record(pat_constructor, {
    name :: atom(),
    args :: [pattern()],
    location :: location()
}).

%% Record pattern
-record(pat_record, {
    fields :: [{atom(), pattern()}],
    location :: location()
}).

%% Literal pattern
-record(pat_literal, {
    value :: integer() | float() | binary(),
    type :: integer | float | string,
    location :: location()
}).

%% Wildcard pattern (_)
-record(pat_wildcard, {
    location :: location()
}).

%% As-pattern (x@pattern)
-record(pat_as, {
    name :: atom(),
    pattern :: pattern(),
    location :: location()
}).

%% List pattern
-record(pat_list, {
    elements :: [pattern()],
    location :: location()
}).

%% Tuple pattern
-record(pat_tuple, {
    elements :: [pattern()],
    location :: location()
}).

%%====================================================================
%% Type Expressions
%%====================================================================

-type type_expr() :: #type_var{} | #type_con{} | #type_app{} |
                     #type_fun{} | #type_record{} | #type_forall{} |
                     #type_tuple{}.

%% Type variable
-record(type_var, {
    name :: atom(),
    location :: location()
}).

%% Type constructor
-record(type_con, {
    name :: atom(),
    location :: location()
}).

%% Type application
-record(type_app, {
    constructor :: type_expr(),
    args :: [type_expr()],
    location :: location()
}).

%% Function type
-record(type_fun, {
    from :: type_expr(),
    to :: type_expr(),
    location :: location()
}).

%% Record type
-record(type_record, {
    fields :: [{atom(), type_expr()}],
    row_var :: atom() | undefined,
    location :: location()
}).

%% Forall quantification
-record(type_forall, {
    type_vars :: [atom()],
    type :: type_expr(),
    location :: location()
}).

%% Tuple type
-record(type_tuple, {
    elements :: [type_expr()],
    location :: location()
}).
