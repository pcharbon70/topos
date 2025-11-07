# Topos Minimal Proof-of-Concept Implementation Plan

## Executive Overview

The minimal PoC will demonstrate Topos's core concepts: category-theory-inspired syntax (`shape` for types, `flow` for functions), immutability by default, basic pattern matching, and compilation to BEAM bytecode via Core Erlang. The implementation will be written in Elixir/Erlang to leverage existing BEAM infrastructure.

## Phase 1: Core Language Infrastructure (Weeks 1-3)

### 1.1 Lexer and Parser

**Goal**: Parse basic Topos syntax into an Abstract Syntax Tree (AST)

**Implementation**:
```elixir
# Use leex for lexical analysis
# Use yecc for parsing (LALR parser generator)
# Alternative: Use nimble_parsec for a pure Elixir solution
```

**Core Syntax to Support**:
```topos
-- Basic shapes (types)
shape Point = { x: Float, y: Float }
shape Maybe a = Some a | None

-- Basic flows (functions)  
flow add : Int -> Int -> Int
flow add x y = x + y

-- Pattern matching
flow describe : Maybe Int -> Text
flow describe = match
  | Some n -> "Value: " <> show n
  | None -> "No value"
```

**Deliverables**:
- `lexer.xrl` - Lexical rules for tokens
- `parser.yrl` - Grammar specification
- `ast.ex` - AST data structures
- Basic test suite for parsing

### 1.2 Core Type System

**Goal**: Implement Hindley-Milner type inference with basic extensions

**Components**:
```elixir
defmodule Topos.Types do
  # Type representation
  @type type :: 
    | {:var, atom()}           # Type variables
    | {:arrow, type(), type()} # Function types
    | {:shape, atom(), [type()]} # ADTs
    | {:record, [{atom(), type()}]} # Records
    
  # Type schemes for polymorphism
  @type scheme :: {:forall, [atom()], type()}
end
```

**Algorithm W Implementation**:
- Unification with occurs check
- Constraint generation
- Type scheme instantiation/generalization
- Basic type error reporting

**Deliverables**:
- `type_system.ex` - Core type definitions
- `inference.ex` - Type inference engine
- `unification.ex` - Unification algorithm
- Test suite with type checking examples

### 1.3 Core to Core Erlang Translator

**Goal**: Translate typed AST to Core Erlang for BEAM compilation

**Strategy**:
```erlang
% Topos source
flow factorial : Int -> Int
flow factorial n = match n
  | 0 -> 1
  | n -> n * factorial (n - 1)

% Core Erlang output
'factorial'/1 =
  fun (N) ->
    case N of
      0 -> 1;
      N -> call 'erlang':'*'(N, 
           call 'topos':'factorial'(
             call 'erlang':'-'(N, 1)))
    end
```

**Key Translations**:
- Shapes → Tagged tuples or records
- Flows → Core Erlang functions
- Pattern matching → Case expressions
- Module structure → Core Erlang modules

**Deliverables**:
- `core_erlang_gen.ex` - Core Erlang code generator
- `runtime.erl` - Minimal runtime support functions
- Integration tests compiling to `.beam` files

## Phase 2: REPL and Basic Runtime (Weeks 4-5)

### 2.1 Interactive REPL

**Architecture**:
```elixir
defmodule Topos.REPL do
  def start do
    # Initialize type environment
    # Start evaluation loop
    # Maintain history and state
  end
  
  def eval_line(input, env) do
    with {:ok, ast} <- parse(input),
         {:ok, typed_ast, type} <- typecheck(ast, env),
         {:ok, core_erlang} <- compile(typed_ast),
         {:ok, result} <- evaluate(core_erlang) do
      {:ok, result, type, updated_env}
    end
  end
end
```

**Features**:
- Expression evaluation
- Type inspection (`:t expression`)
- Multi-line input support
- Definition persistence within session
- Error recovery and helpful messages

**Deliverables**:
- `repl.ex` - REPL implementation
- `evaluator.ex` - Direct evaluation engine
- Command-line interface
- REPL user guide

### 2.2 Standard Prelude

**Core Types and Functions**:
```topos
-- Primitive types
shape Bool = True | False
shape List a = Nil | Cons a (List a)
shape Result a b = Ok a | Error b

-- Essential functions
flow compose : (b -> c) -> (a -> b) -> (a -> c)
flow compose f g x = f (g x)

flow map : (a -> b) -> List a -> List b
flow map f = match
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
```

**Deliverables**:
- `prelude.topos` - Standard library basics
- Compiled prelude module
- Documentation for standard functions

## Phase 3: Essential Language Features (Weeks 6-8)

### 3.1 Enhanced Pattern Matching

**Features to Implement**:
- Guards: `| pattern when condition ->`
- Nested patterns: `| Some (Cons x xs) ->`
- As-patterns: `| Some x as maybe ->`
- Wildcard: `| _ ->`

**Implementation Strategy**:
```elixir
defmodule Topos.PatternCompiler do
  # Compile patterns to decision trees
  # Generate optimal Core Erlang case expressions
  # Handle exhaustiveness checking
  # Report non-exhaustive/redundant patterns
end
```

### 3.2 Let Bindings and Where Clauses

```topos
flow calculate : Int -> Int
flow calculate x = 
  let y = x * 2
      z = y + 1
  in z * z

flow calculate2 : Int -> Int  
flow calculate2 x = result
  where result = z * z
        z = y + 1
        y = x * 2
```

### 3.3 Pipe Operator and Composition

```topos
flow process : List Int -> Int
flow process = 
  filter isPositive
  |> map double
  |> fold add 0
```

## Phase 4: Module System Foundation (Weeks 9-10)

### 4.1 Basic Modules

**Syntax**:
```topos
module Data.List exports (List, map, filter, fold) where
  
  shape List a = Nil | Cons a (List a)
  
  flow map : (a -> b) -> List a -> List b
  flow map f = match
    | Nil -> Nil
    | Cons x xs -> Cons (f x) (map f xs)
    
  -- Private helper (not exported)
  flow helper : List a -> Int
  flow helper = ...
end
```

**Compilation**:
- Each Topos module → BEAM module
- Export lists → `-export([...])` directives
- Module imports → fully qualified calls

### 4.2 Import System

```topos
import Data.List (List, map)
import qualified Data.Set as Set

flow process : List Int -> Set.Set Int
flow process xs = 
  xs |> map increment |> Set.fromList
```

## Phase 5: Actor Model Integration (Weeks 11-12)

### 5.1 Basic Actor Support

**Minimal Actor Syntax**:
```topos
actor Counter = {
  shape State = { count: Int }
  shape Message = Increment | Get
  
  flow init : () -> State
  flow init () = { count: 0 }
  
  flow handle : Message -> State -> (State, Maybe Reply)
  flow handle msg state = match msg
    | Increment -> ({ count: state.count + 1 }, None)
    | Get -> (state, Some state.count)
}
```

**Compilation to gen_server**:
```erlang
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

init([]) -> {ok, #{count => 0}}.

handle_cast(increment, State) ->
  {noreply, State#{count => maps:get(count, State) + 1}}.
  
handle_call(get, _From, State) ->
  {reply, maps:get(count, State), State}.
```

## Implementation Architecture

### Technology Stack

**Core Implementation** (Elixir/Erlang):
- Lexer: `leex` (Erlang's lex)
- Parser: `yecc` (Erlang's yacc)
- Type System: Pure Elixir/Erlang
- Code Generation: Generate Core Erlang AST
- REPL: Elixir GenServer

### Project Structure

```
topos/
├── lib/
│   ├── topos/
│   │   ├── lexer.xrl
│   │   ├── parser.yrl
│   │   ├── ast.ex
│   │   ├── types/
│   │   │   ├── inference.ex
│   │   │   ├── unification.ex
│   │   │   └── environment.ex
│   │   ├── compiler/
│   │   │   ├── core_erlang.ex
│   │   │   ├── pattern_compiler.ex
│   │   │   └── module_compiler.ex
│   │   ├── repl/
│   │   │   ├── server.ex
│   │   │   ├── evaluator.ex
│   │   │   └── commands.ex
│   │   └── runtime/
│   │       └── prelude.ex
├── priv/
│   └── stdlib/
│       ├── prelude.topos
│       └── data/
│           └── list.topos
├── test/
│   ├── lexer_test.exs
│   ├── parser_test.exs
│   ├── type_inference_test.exs
│   └── integration_test.exs
└── mix.exs
```

## Testing Strategy

### Unit Tests
- Lexer: Token generation for all syntax
- Parser: AST construction and error cases
- Type System: Inference, unification, error messages
- Code Generation: Core Erlang output verification

### Integration Tests
```topos
-- test/examples/factorial.topos
flow factorial : Int -> Int
flow factorial n = match n
  | 0 -> 1
  | n -> n * factorial (n - 1)

-- Compile, run, verify output
-- Check type inference
-- Verify BEAM bytecode generation
```

### REPL Tests
- Interactive session recording/playback
- Error recovery testing
- Multi-line input handling
- State management

## Development Milestones

### Milestone 1 (Week 3): "Hello, World!"
- Parse and compile: `flow main = "Hello, World!"`
- Generate working `.beam` file
- Execute on BEAM

### Milestone 2 (Week 5): Interactive REPL
- Basic arithmetic and functions in REPL
- Type inspection working
- Error messages helpful

### Milestone 3 (Week 8): Pattern Matching
- Compile recursive functions with pattern matching
- Exhaustiveness checking
- List processing working

### Milestone 4 (Week 10): Modules
- Multi-module compilation
- Import/export working
- Standard library usable

### Milestone 5 (Week 12): Actors
- Basic actor compilation to gen_server
- Message passing working
- Counter example running

## Performance Considerations

### Compilation Performance
- Cache type inference results
- Incremental compilation support
- Parallel module compilation

### Runtime Performance
- Direct Core Erlang generation (no intermediate representations)
- Leverage BEAM's pattern matching optimizations
- Minimal runtime overhead

## Documentation Requirements

### User Documentation
1. **Quick Start Guide**: Installation, first program, REPL basics
2. **Language Tutorial**: Types, functions, pattern matching
3. **Standard Library Reference**: Prelude functions
4. **REPL Guide**: Commands, shortcuts, debugging

### Developer Documentation
1. **Architecture Overview**: Component relationships
2. **Type System Internals**: Algorithm W implementation
3. **Core Erlang Mapping**: Translation strategies
4. **Extension Guide**: Adding new features

## Risk Mitigation

### Technical Risks

**Risk**: Type inference complexity
- **Mitigation**: Start with basic HM, add features incrementally

**Risk**: Core Erlang generation bugs  
- **Mitigation**: Extensive test suite, compare with Erlang compiler output

**Risk**: REPL stability
- **Mitigation**: Robust error handling, session recovery

### Schedule Risks

**Risk**: Feature creep
- **Mitigation**: Strict scope control, defer advanced features

**Risk**: BEAM integration issues
- **Mitigation**: Early prototype integration, consult BEAM documentation

## Success Criteria

The PoC is successful when:

1. ✅ Can parse basic Topos syntax (shapes, flows, pattern matching)
2. ✅ Type inference works for simple programs
3. ✅ Compiles to executable BEAM bytecode
4. ✅ REPL allows interactive development
5. ✅ Can define and use recursive functions
6. ✅ Module system supports basic import/export
7. ✅ Documentation enables others to use and extend

## Future Expansion Path

After the PoC, prioritize:
1. **Row polymorphism** for extensible records
2. **Polymorphic variants** for flexible sum types  
3. **Functors** for parameterized modules
4. **Effect system** for tracking side effects
5. **Advanced pattern matching** (view patterns, guards with bindings)
6. **Supervision trees** and full OTP integration
7. **Distributed computing** primitives

## Appendix A: Core Language Grammar (Simplified)

```bnf
program     ::= definition*

definition  ::= shape_def | flow_def

shape_def   ::= 'shape' type_name type_params? '=' shape_body

shape_body  ::= constructor ('|' constructor)*
             | '{' field_list '}'

constructor ::= con_name type*

flow_def    ::= 'flow' name ':' type
             | 'flow' name pattern* '=' expression

expression  ::= literal
             | variable
             | application
             | 'match' expression clause+
             | 'let' bindings 'in' expression
             | expression '|>' expression

pattern     ::= literal
             | variable
             | constructor pattern*
             | '{' field_patterns '}'
             | pattern 'when' guard

type        ::= type_var
             | type_name
             | type '->' type
             | type_name type*
             | '{' field_types '}'
```

## Appendix B: Example Programs

### Example 1: List Operations
```topos
module Examples.Lists where

shape List a = Nil | Cons a (List a)

flow length : List a -> Int
flow length = match
  | Nil -> 0
  | Cons _ xs -> 1 + length xs

flow filter : (a -> Bool) -> List a -> List a
flow filter pred = match
  | Nil -> Nil
  | Cons x xs -> 
    if pred x 
    then Cons x (filter pred xs)
    else filter pred xs

flow sum : List Int -> Int
flow sum = fold (+) 0

flow fold : (b -> a -> b) -> b -> List a -> b
flow fold f acc = match
  | Nil -> acc
  | Cons x xs -> fold f (f acc x) xs
```

### Example 2: Binary Tree
```topos
module Examples.Tree where

shape Tree a = Leaf | Node a (Tree a) (Tree a)

flow insert : Ord a => a -> Tree a -> Tree a
flow insert x = match
  | Leaf -> Node x Leaf Leaf
  | Node y left right ->
    if x < y
    then Node y (insert x left) right
    else Node y left (insert x right)

flow inorder : Tree a -> List a
flow inorder = match
  | Leaf -> Nil
  | Node x left right ->
    append (inorder left) (Cons x (inorder right))
```

### Example 3: Simple Actor
```topos
actor Stack = {
  shape State = List Int
  shape Message = 
    | Push Int
    | Pop
    | Peek
    
  flow init : () -> State
  flow init () = Nil
  
  flow handle : Message -> State -> (State, Maybe Reply)
  flow handle msg state = match msg
    | Push x -> (Cons x state, None)
    | Pop -> match state
        | Nil -> (Nil, Some Error "Empty stack")
        | Cons _ xs -> (xs, Some Ok)
    | Peek -> match state
        | Nil -> (state, Some Error "Empty stack")
        | Cons x _ -> (state, Some (Ok x))
}
```

## Conclusion

This implementation plan provides a pragmatic path to a working Topos proof-of-concept in 12 weeks. By focusing on core features first—parsing, type inference, compilation, and REPL—we establish a solid foundation for the language while demonstrating its unique value proposition: category theory principles meeting BEAM's practical distributed systems capabilities.

The modular architecture ensures each component can be enhanced independently, while the test-driven approach maintains quality throughout development. Most importantly, this plan delivers a usable system quickly, enabling early feedback and iteration on the language design.
