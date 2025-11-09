%% Topos Parser - Yecc Grammar Definition
%% Phase 1, Task 1.1.2: Grammar Implementation
%%
%% This parser transforms Topos token streams into Abstract Syntax Trees (ASTs).
%% Built using yecc (Erlang's LALR parser generator).

%%============================================================================
%% Header
%%============================================================================

Header
"%% This file is generated from topos_parser.yrl. Do not edit directly."
.

%%============================================================================
%% Nonterminals
%%============================================================================

Nonterminals
  topos_module
  declarations declaration
  shape_decl flow_decl effect_decl
  type_params type_params_nonempty constructors constructor constructor_fields
  effect_operations effect_operation
  flow_signature flow_clauses flow_clause
  match_clauses match_clause
  pattern_list pattern_list_nonempty pattern tuple_pattern_list
  guards guard
  expr expr_primary expr_app expr_list tuple_expr_list
  record_fields record_field
  record_pattern_fields record_pattern_field
  literal
  perform_expr try_with_expr
  handler_clauses handler_clause operation_cases operation_case
  effect_list effect_list_nonempty
  type_expr type_expr_primary type_expr_app
  type_list type_record_fields type_record_field
  .

%%============================================================================
%% Terminals
%%============================================================================

Terminals
  %% Keywords
  shape flow match where 'let' 'in' 'do' 'end'
  'if' 'then' 'else' 'case' 'of' 'when'
  module import export exports as qualified private
  trait instance forall
  actor supervisor
  effect operation perform 'try' with

  %% Operators
  pipe_right bind arrow double_arrow concat
  eq neq lte gte lt gt
  'or' 'and' cons left_arrow range
  colon equals pipe
  plus minus star slash dot

  %% Delimiters
  lbrace rbrace lbracket rbracket lparen rparen
  comma semicolon underscore

  %% Literals and identifiers
  integer float string
  lower_ident upper_ident
  .

%%============================================================================
%% Rootsymbol
%%============================================================================

Rootsymbol topos_module.

%%============================================================================
%% Precedence and Associativity
%%============================================================================

Right    100 arrow.           %% Type-level function arrow (right-assoc)
Right    150 pipe_right.      %% |> pipe operator
Right    160 bind.            %% >>= Kleisli composition

Nonassoc 300 eq neq.          %% == /=
Nonassoc 310 lt gt lte gte.   %% < > <= >=
Right    350 concat.          %% <> (right-assoc for strings)

Left     400 plus minus.      %% + -
Left     500 star slash.      %% * /

Left     600 dot.             %% Record field access

%%============================================================================
%% Grammar Rules
%%============================================================================

%% Module structure
topos_module -> declarations :
    {module, undefined, [], [], '$1', {line, 1}}.

declarations -> declaration :
    ['$1'].
declarations -> declaration declarations :
    ['$1' | '$2'].

declaration -> shape_decl : '$1'.
declaration -> flow_decl : '$1'.
declaration -> effect_decl : '$1'.

%%----------------------------------------------------------------------------
%% Shape Declarations (Algebraic Data Types)
%%----------------------------------------------------------------------------

shape_decl -> shape upper_ident type_params equals constructors :
    {shape_decl,
        extract_atom('$2'),
        '$3',
        '$5',
        [],
        extract_location('$1')}.

type_params -> '$empty' :
    [].
type_params -> type_params_nonempty :
    '$1'.

type_params_nonempty -> lower_ident :
    [extract_atom('$1')].
type_params_nonempty -> lower_ident type_params_nonempty :
    [extract_atom('$1') | '$2'].

constructors -> constructor :
    ['$1'].
constructors -> constructor pipe constructors :
    ['$1' | '$3'].

constructor -> upper_ident :
    {constructor,
        extract_atom('$1'),
        [],
        extract_location('$1')}.

constructor -> upper_ident constructor_fields :
    {constructor,
        extract_atom('$1'),
        '$2',
        extract_location('$1')}.

constructor_fields -> type_expr_primary :
    ['$1'].
constructor_fields -> type_expr_primary constructor_fields :
    ['$1' | '$2'].

%%----------------------------------------------------------------------------
%% Effect Declarations (Algebraic Effects)
%%----------------------------------------------------------------------------

effect_decl -> effect upper_ident effect_operations 'end' :
    {effect_decl,
        extract_atom('$2'),
        '$3',
        extract_location('$1')}.

effect_operations -> '$empty' :
    [].
effect_operations -> effect_operation effect_operations :
    ['$1' | '$2'].

effect_operation -> operation lower_ident :
    {effect_operation,
        extract_atom('$2'),
        undefined,
        extract_location('$1')}.

effect_operation -> operation lower_ident colon type_expr :
    {effect_operation,
        extract_atom('$2'),
        '$4',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Flow Declarations (Function Definitions)
%%----------------------------------------------------------------------------

flow_decl -> flow_signature flow_clauses :
    {flow_decl,
        extract_flow_name('$1'),
        extract_flow_type('$1'),
        '$2',
        extract_location('$1')}.

%% Simple flow without type signature (like minimal parser)
flow_decl -> flow lower_ident pattern_list equals expr :
    {flow_decl,
        extract_atom('$2'),
        undefined,
        [{flow_clause, '$3', undefined, '$5', extract_location('$1')}],
        extract_location('$1')}.

flow_decl -> flow lower_ident pattern_list 'when' guards equals expr :
    {flow_decl,
        extract_atom('$2'),
        undefined,
        [{flow_clause, '$3', '$5', '$7', extract_location('$1')}],
        extract_location('$1')}.

flow_decl -> flow lower_ident pattern_list equals match match_clauses 'end' :
    {flow_decl,
        extract_atom('$2'),
        undefined,
        [{flow_clause, '$3', undefined, {match_expr, '$6', extract_location('$5')}, extract_location('$1')}],
        extract_location('$1')}.

flow_signature -> flow lower_ident colon type_expr :
    {flow_sig, extract_atom('$2'), '$4', extract_location('$1')}.

flow_clauses -> flow_clause :
    ['$1'].
flow_clauses -> flow_clause flow_clauses :
    ['$1' | '$2'].

flow_clause -> flow lower_ident pattern_list equals expr :
    {flow_clause,
        '$3',
        undefined,
        '$5',
        extract_location('$1')}.

flow_clause -> flow lower_ident pattern_list 'when' guards equals expr :
    {flow_clause,
        '$3',
        '$5',
        '$7',
        extract_location('$1')}.

flow_clause -> flow lower_ident pattern_list equals match match_clauses 'end' :
    {flow_clause,
        '$3',
        undefined,
        {match_expr, '$6', extract_location('$5')},
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Match Expressions
%%----------------------------------------------------------------------------

match_clauses -> match_clause :
    ['$1'].
match_clauses -> match_clause match_clauses :
    ['$1' | '$2'].

match_clause -> pipe pattern arrow expr :
    {match_clause,
        '$2',
        undefined,
        '$4',
        extract_location('$1')}.

match_clause -> pipe pattern 'when' guards arrow expr :
    {match_clause,
        '$2',
        '$4',
        '$6',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Patterns
%%----------------------------------------------------------------------------

pattern_list -> '$empty' :
    [].
pattern_list -> pattern_list_nonempty :
    '$1'.

pattern_list_nonempty -> pattern :
    ['$1'].
pattern_list_nonempty -> pattern pattern_list_nonempty :
    ['$1' | '$2'].

pattern -> lower_ident :
    {pat_var, extract_atom('$1'), extract_location('$1')}.

pattern -> underscore :
    {pat_wildcard, extract_location('$1')}.

pattern -> upper_ident :
    {pat_constructor, extract_atom('$1'), [], extract_location('$1')}.

pattern -> upper_ident lparen pattern_list rparen :
    {pat_constructor, extract_atom('$1'), '$3', extract_location('$1')}.

pattern -> integer :
    {pat_literal, extract_value('$1'), integer, extract_location('$1')}.

pattern -> float :
    {pat_literal, extract_value('$1'), float, extract_location('$1')}.

pattern -> string :
    {pat_literal, extract_value('$1'), string, extract_location('$1')}.

pattern -> lbracket rbracket :
    {pat_list, [], extract_location('$1')}.

pattern -> lbracket pattern_list rbracket :
    {pat_list, '$2', extract_location('$1')}.

pattern -> lparen tuple_pattern_list rparen :
    {pat_tuple, '$2', extract_location('$1')}.

pattern -> lbrace rbrace :
    {pat_record, [], extract_location('$1')}.

pattern -> lbrace record_pattern_fields rbrace :
    {pat_record, '$2', extract_location('$1')}.

%% Tuple pattern lists (comma-separated patterns)
tuple_pattern_list -> pattern comma pattern :
    ['$1', '$3'].
tuple_pattern_list -> pattern comma tuple_pattern_list :
    ['$1' | '$3'].

%% Record pattern fields (field: pattern, field: pattern, ...)
record_pattern_fields -> record_pattern_field :
    ['$1'].
record_pattern_fields -> record_pattern_field comma record_pattern_fields :
    ['$1' | '$3'].

record_pattern_field -> lower_ident colon pattern :
    {extract_atom('$1'), '$3'}.

%%----------------------------------------------------------------------------
%% Guards
%%----------------------------------------------------------------------------

guards -> guard :
    ['$1'].
guards -> guard comma guards :
    ['$1' | '$3'].

guard -> expr : '$1'.

%%----------------------------------------------------------------------------
%% Expressions
%%----------------------------------------------------------------------------

%% Expression hierarchy: expr > expr_app > expr_primary

expr -> expr pipe_right expr :
    {binary_op, pipe_right, '$1', '$3', extract_location('$2')}.

expr -> expr bind expr :
    {binary_op, bind, '$1', '$3', extract_location('$2')}.

expr -> expr plus expr :
    {binary_op, plus, '$1', '$3', extract_location('$2')}.

expr -> expr minus expr :
    {binary_op, minus, '$1', '$3', extract_location('$2')}.

expr -> expr star expr :
    {binary_op, star, '$1', '$3', extract_location('$2')}.

expr -> expr slash expr :
    {binary_op, slash, '$1', '$3', extract_location('$2')}.

expr -> expr concat expr :
    {binary_op, concat, '$1', '$3', extract_location('$2')}.

expr -> expr eq expr :
    {binary_op, eq, '$1', '$3', extract_location('$2')}.

expr -> expr neq expr :
    {binary_op, neq, '$1', '$3', extract_location('$2')}.

expr -> expr lt expr :
    {binary_op, lt, '$1', '$3', extract_location('$2')}.

expr -> expr gt expr :
    {binary_op, gt, '$1', '$3', extract_location('$2')}.

expr -> expr lte expr :
    {binary_op, lte, '$1', '$3', extract_location('$2')}.

expr -> expr gte expr :
    {binary_op, gte, '$1', '$3', extract_location('$2')}.

expr -> expr_app : '$1'.

%% Function application (left-associative, juxtaposition)
expr_app -> expr_app expr_primary :
    {app, '$1', ['$2'], extract_location('$1')}.

expr_app -> expr_app dot lower_ident :
    {record_access, '$1', extract_atom('$3'), extract_location('$2')}.

expr_app -> expr_primary : '$1'.

%% Primary expressions (highest precedence, atomic)
expr_primary -> literal : '$1'.

expr_primary -> lower_ident :
    {var, extract_atom('$1'), extract_location('$1')}.

expr_primary -> upper_ident :
    {var, extract_atom('$1'), extract_location('$1')}.

expr_primary -> lparen expr rparen :
    '$2'.

expr_primary -> lparen tuple_expr_list rparen :
    {tuple_expr, '$2', extract_location('$1')}.

%% Tuple expression lists (comma-separated expressions)
tuple_expr_list -> expr comma expr :
    ['$1', '$3'].
tuple_expr_list -> expr comma tuple_expr_list :
    ['$1' | '$3'].

expr_primary -> 'let' lower_ident equals expr 'in' expr :
    {let_expr,
        [{pat_var, extract_atom('$2'), extract_location('$2')}, '$4'],
        '$6',
        extract_location('$1')}.

expr_primary -> 'if' expr 'then' expr 'else' expr :
    {if_expr, '$2', '$4', '$6', extract_location('$1')}.

expr_primary -> perform_expr : '$1'.

expr_primary -> try_with_expr : '$1'.

expr_primary -> lbracket rbracket :
    {list_expr, [], extract_location('$1')}.

expr_primary -> lbracket expr_list rbracket :
    {list_expr, '$2', extract_location('$1')}.

expr_primary -> lbrace rbrace :
    {record_expr, [], undefined, extract_location('$1')}.

expr_primary -> lbrace record_fields rbrace :
    {record_expr, '$2', undefined, extract_location('$1')}.

%% Record fields (field: expr, field: expr, ...)
record_fields -> record_field :
    ['$1'].
record_fields -> record_field comma record_fields :
    ['$1' | '$3'].

record_field -> lower_ident colon expr :
    {extract_atom('$1'), '$3'}.

%% Expression lists (for list literals, function arguments, etc.)
expr_list -> expr :
    ['$1'].
expr_list -> expr comma expr_list :
    ['$1' | '$3'].

%%----------------------------------------------------------------------------
%% Literals
%%----------------------------------------------------------------------------

literal -> integer :
    {literal, extract_value('$1'), integer, extract_location('$1')}.

literal -> float :
    {literal, extract_value('$1'), float, extract_location('$1')}.

literal -> string :
    {literal, extract_value('$1'), string, extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Effect Expressions
%%----------------------------------------------------------------------------

%% Perform expression: perform Effect.operation(args)
perform_expr -> perform upper_ident dot lower_ident lparen rparen :
    {perform_expr,
        extract_atom('$2'),
        extract_atom('$4'),
        [],
        extract_location('$1')}.

perform_expr -> perform upper_ident dot lower_ident lparen expr_list rparen :
    {perform_expr,
        extract_atom('$2'),
        extract_atom('$4'),
        '$6',
        extract_location('$1')}.

perform_expr -> perform upper_ident dot lower_ident :
    {perform_expr,
        extract_atom('$2'),
        extract_atom('$4'),
        [],
        extract_location('$1')}.

%% Try-with expression: try { expr } with handlers end
try_with_expr -> 'try' expr with handler_clauses 'end' :
    {try_with_expr,
        '$2',
        '$4',
        extract_location('$1')}.

%% Handler clauses: Effect { operation cases }
handler_clauses -> handler_clause :
    ['$1'].
handler_clauses -> handler_clause handler_clauses :
    ['$1' | '$2'].

handler_clause -> upper_ident lbrace operation_cases rbrace :
    {handler_clause,
        extract_atom('$1'),
        '$3',
        extract_location('$1')}.

%% Operation cases: operation(params) -> expr
operation_cases -> operation_case :
    ['$1'].
operation_cases -> operation_case operation_cases :
    ['$1' | '$2'].

operation_case -> lower_ident lparen pattern_list rparen arrow expr :
    {operation_case,
        extract_atom('$1'),
        '$3',
        '$6',
        extract_location('$1')}.

operation_case -> lower_ident arrow expr :
    {operation_case,
        extract_atom('$1'),
        [],
        '$3',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Type Expressions
%%----------------------------------------------------------------------------

type_expr -> type_expr arrow type_expr :
    {type_fun, '$1', '$3', extract_location('$2')}.

type_expr -> forall type_params dot type_expr :
    {type_forall, '$2', '$4', extract_location('$1')}.

type_expr -> type_expr_app slash lbrace effect_list_nonempty rbrace :
    {type_effect, '$1', '$4', extract_location('$2')}.

type_expr -> type_expr_app : '$1'.

%% Type application (higher precedence than function arrows)
type_expr_app -> upper_ident type_expr_primary :
    {type_app,
        {type_con, extract_atom('$1'), extract_location('$1')},
        ['$2'],
        extract_location('$1')}.

type_expr_app -> type_expr_primary :
    '$1'.

%% Primary type expressions (atomic)
type_expr_primary -> lower_ident :
    {type_var, extract_atom('$1'), extract_location('$1')}.

type_expr_primary -> upper_ident :
    {type_con, extract_atom('$1'), extract_location('$1')}.

type_expr_primary -> lparen type_expr rparen :
    '$2'.

type_expr_primary -> lparen type_list rparen :
    {type_tuple, '$2', extract_location('$1')}.

type_expr_primary -> lbrace rbrace :
    {type_record, [], undefined, extract_location('$1')}.

type_expr_primary -> lbrace type_record_fields rbrace :
    {type_record, '$2', undefined, extract_location('$1')}.

type_list -> type_expr :
    ['$1'].
type_list -> type_expr comma type_list :
    ['$1' | '$3'].

%% Record type fields (field: Type, field: Type, ...)
type_record_fields -> type_record_field :
    ['$1'].
type_record_fields -> type_record_field comma type_record_fields :
    ['$1' | '$3'].

type_record_field -> lower_ident colon type_expr :
    {extract_atom('$1'), '$3'}.

%% Effect lists (for effect annotations)
effect_list -> '$empty' :
    [].
effect_list -> effect_list_nonempty :
    '$1'.

effect_list_nonempty -> upper_ident :
    [extract_atom('$1')].
effect_list_nonempty -> upper_ident comma effect_list_nonempty :
    [extract_atom('$1') | '$3'].

%%============================================================================
%% Erlang Code - Helper Functions
%%============================================================================

Erlang code.

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
extract_location({effect_decl, _Name, _Operations, Loc}) -> Loc;
extract_location({effect_operation, _Name, _Type, Loc}) -> Loc;
extract_location({perform_expr, _Effect, _Operation, _Args, Loc}) -> Loc;
extract_location({try_with_expr, _Body, _Handlers, Loc}) -> Loc;
extract_location({handler_clause, _Effect, _Operations, Loc}) -> Loc;
extract_location({operation_case, _Operation, _Params, _Body, Loc}) -> Loc;
extract_location({type_effect, _Type, _Effects, Loc}) -> Loc;
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
