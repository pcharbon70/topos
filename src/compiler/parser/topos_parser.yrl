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
  shape_decl flow_decl
  type_params constructors constructor constructor_fields
  flow_signature flow_clauses flow_clause
  match_clauses match_clause
  pattern_list pattern
  guards guard
  expr expr_primary expr_app expr_list
  literal
  type_expr type_expr_primary type_expr_app
  type_list
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

Left     400 plus minus.      %% + -
Left     500 star slash.      %% * /
Right    550 concat.          %% <> (right-assoc for strings)

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
type_params -> lower_ident :
    [extract_atom('$1')].
type_params -> lower_ident type_params :
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
%% Flow Declarations (Function Definitions)
%%----------------------------------------------------------------------------

flow_decl -> flow_signature flow_clauses :
    {flow_decl,
        extract_flow_name('$1'),
        extract_flow_type('$1'),
        '$2',
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

flow_clause -> flow lower_ident equals match match_clauses 'end' :
    {flow_clause,
        [],
        undefined,
        {match_expr, '$5', extract_location('$4')},
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
pattern_list -> pattern :
    ['$1'].
pattern_list -> pattern pattern_list :
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

expr -> expr dot lower_ident :
    {record_access, '$1', extract_atom('$3'), extract_location('$2')}.

expr -> expr_app : '$1'.

%% Function application (left-associative, juxtaposition)
expr_app -> expr_app expr_primary :
    {app, '$1', ['$2'], extract_location('$1')}.

expr_app -> expr_primary : '$1'.

%% Primary expressions (highest precedence, atomic)
expr_primary -> literal : '$1'.

expr_primary -> lower_ident :
    {var, extract_atom('$1'), extract_location('$1')}.

expr_primary -> upper_ident :
    {var, extract_atom('$1'), extract_location('$1')}.

expr_primary -> lparen expr rparen :
    '$2'.

expr_primary -> 'let' lower_ident equals expr 'in' expr :
    {let_expr,
        [{pat_var, extract_atom('$2'), extract_location('$2')}, '$4'],
        '$6',
        extract_location('$1')}.

expr_primary -> 'if' expr 'then' expr 'else' expr :
    {if_expr, '$2', '$4', '$6', extract_location('$1')}.

expr_primary -> lbracket rbracket :
    {list_expr, [], extract_location('$1')}.

expr_primary -> lbracket expr_list rbracket :
    {list_expr, '$2', extract_location('$1')}.

expr_primary -> lbrace rbrace :
    {record_expr, [], undefined, extract_location('$1')}.

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
%% Type Expressions
%%----------------------------------------------------------------------------

type_expr -> type_expr arrow type_expr :
    {type_fun, '$1', '$3', extract_location('$2')}.

type_expr -> forall type_params dot type_expr :
    {type_forall, '$2', '$4', extract_location('$1')}.

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

type_list -> type_expr :
    ['$1'].
type_list -> type_expr comma type_list :
    ['$1' | '$3'].

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

%% @doc Extract location from token
extract_location({_Tag, Line}) -> {line, Line};
extract_location({_Tag, Line, _Value}) -> {line, Line}.

%% @doc Extract flow name from flow signature
extract_flow_name({flow_sig, Name, _Type, _Loc}) -> Name.

%% @doc Extract flow type from flow signature
extract_flow_type({flow_sig, _Name, Type, _Loc}) -> Type.
