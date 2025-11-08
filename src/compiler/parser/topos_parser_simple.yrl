%% Topos Parser - Minimal Viable Grammar
%% Phase 1, Task 1.1.2: Grammar Implementation (Simplified Initial Version)

Nonterminals
  topos_module declarations declaration
  shape_decl constructors constructor
  flow_decl flow_clause
  expr literal
  .

Terminals
  shape flow equals pipe
  lower_ident upper_ident
  integer float string
  .

Rootsymbol topos_module.

topos_module -> declarations :
    {module, undefined, [], [], '$1', {line, 1}}.

declarations -> declaration :
    ['$1'].
declarations -> declaration declarations :
    ['$1' | '$2'].

declaration -> shape_decl : '$1'.
declaration -> flow_decl : '$1'.

%% Shape declarations
shape_decl -> shape upper_ident equals constructors :
    {shape_decl,
        get_atom('$2'),
        [],
        '$4',
        [],
        get_line('$1')}.

constructors -> constructor :
    ['$1'].
constructors -> constructor pipe constructors :
    ['$1' | '$3'].

constructor -> upper_ident :
    {constructor, get_atom('$1'), [], get_line('$1')}.

%% Flow declarations
flow_decl -> flow lower_ident equals expr :
    {flow_decl,
        get_atom('$2'),
        undefined,
        [{flow_clause, [], undefined, '$4', get_line('$1')}],
        get_line('$1')}.

%% Expressions
expr -> literal : '$1'.
expr -> lower_ident : {var, get_atom('$1'), get_line('$1')}.

literal -> integer : {literal, get_value('$1'), integer, get_line('$1')}.
literal -> float : {literal, get_value('$1'), float, get_line('$1')}.
literal -> string : {literal, get_value('$1'), string, get_line('$1')}.

Erlang code.

get_atom({_Tag, _Line, Atom}) when is_atom(Atom) -> Atom;
get_atom({_Tag, _Line, String}) when is_list(String) -> list_to_atom(String);
get_atom({Tag, _Line}) when is_atom(Tag) -> Tag.

get_value({_Tag, _Line, Value}) -> Value.

get_line({_Tag, Line}) -> {line, Line};
get_line({_Tag, Line, _Value}) -> {line, Line}.
