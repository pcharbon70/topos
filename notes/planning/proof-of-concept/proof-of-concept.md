# Topos Minimal Proof-of-Concept Implementation Plan

**[🧭 Phase Navigation](phase-navigation.md)** | **[Detailed Phase Documents](#phase-documents)**

---

## Phase Documents

This proof-of-concept is organized into 5 detailed phase documents with comprehensive task breakdowns:

- **[Phase 1: Core Language Infrastructure](phase-01.md)** (Weeks 1-3) - Lexer, Parser, Type System, Code Generation
- **[Phase 2: REPL and Basic Runtime](phase-02.md)** (Weeks 4-5) - Interactive Development, Standard Prelude
- **[Phase 3: Pattern Matching Engine](phase-03.md)** (Weeks 6-8) - Advanced Patterns, Decision Trees, Exhaustiveness
- **[Phase 4: Module System](phase-04.md)** (Weeks 9-10) - Modules, Imports, Separate Compilation
- **[Phase 5: Actor Model Integration](phase-05.md)** (Weeks 11-12) - Actors, Supervision, Fault Tolerance

Each phase document includes:
- Detailed section and task breakdowns with subtasks
- Descriptive paragraphs explaining purpose and approach
- Unit tests for each section
- Integration tests for the phase
- Success criteria and key outputs

---

## Executive Overview

This plan outlines the development of Topos, a new category-theory-based functional programming language for the BEAM virtual machine. The minimal PoC will demonstrate Topos's core concepts: categorical abstractions (`shape` for types, `flow` for morphisms), immutability by default, advanced pattern matching, and compilation to BEAM bytecode via Core Erlang.

## Phase 1: Core Language Infrastructure (Weeks 1-3)

### 1.1 Lexer and Parser

**Goal**: Parse Topos syntax into an Abstract Syntax Tree (AST)

**Core Syntax to Support**:
```topos
-- Basic shapes (objects in category theory)
shape Point = { x: Float, y: Float }
shape Maybe a = Some a | None

-- Flows (morphisms between objects)  
flow add : Natural -> Natural -> Natural
flow add x y = x + y

-- Pattern matching as destructuring
flow describe : Maybe Natural -> Text
flow describe = match
  | Some n -> "Value: " <> show n
  | None -> "No value"
end

-- Composition operator
flow process = 
  validate |> transform |> persist
```

**Token Categories**:
- Keywords: `shape`, `flow`, `match`, `where`, `let`, `in`, `do`, `end`
- Operators: `|>`, `->`, `:`, `=`, `<>`, `>>=`
- Delimiters: `{`, `}`, `[`, `]`, `(`, `)`, `|`
- Literals: Numbers, Strings, Atoms (`:atom`)
- Comments: `--` single line, `{- -}` multi-line

**Deliverables**:
- Lexer specification for Topos tokens
- Parser grammar for Topos syntax
- AST data structure definitions
- Parser test suite with edge cases

### 1.2 Core Type System

**Goal**: Implement Hindley-Milner type inference with categorical extensions

**Type System Features**:
```topos
-- Parametric polymorphism
flow identity : forall a. a -> a
flow identity x = x

-- Type constraints (traits as categories)
flow sort : forall a. Ord a => List a -> List a

-- Row polymorphism (for extensible records)
flow getName : forall ρ. {name: Text | ρ} -> Text
flow getName record = record.name

-- Higher-kinded types (functors)
shape Functor f where
  map : forall a b. (a -> b) -> f a -> f b
```

**Type Representation**:
- Type variables: `α, β, γ, ...`
- Type constructors: `List`, `Maybe`, `Process`
- Function types: `τ₁ -> τ₂`
- Record types: `{label₁: τ₁, label₂: τ₂}`
- Variant types: `Constructor₁ τ₁ | Constructor₂ τ₂`

**Deliverables**:
- Type inference engine (Algorithm W)
- Unification with occurs check
- Type scheme generalization
- Constraint solver for traits

### 1.3 Core Erlang Code Generation

**Goal**: Translate typed Topos AST to Core Erlang

**Translation Examples**:

```topos
-- Topos source (factorial.tps)
flow factorial : Natural -> Natural
flow factorial n = match n
  | 0 -> 1
  | n -> n * factorial (n - 1)
end
```

Compiles to Core Erlang:
```erlang
'factorial'/1 =
  fun (N) ->
    case N of
      0 -> 1;
      N -> call 'erlang':'*'(
             N, 
             call 'topos':'factorial'(
               call 'erlang':'-'(N, 1)))
    end
```

**Shape Compilation**:
```topos
-- Algebraic data type
shape Tree a = Leaf | Node a (Tree a) (Tree a)

-- Compiles to tagged tuples
% Leaf -> {leaf}
% Node(x, l, r) -> {node, X, L, R}
```

**Deliverables**:
- Core Erlang AST builder
- Pattern match compilation (decision trees)
- Module structure generator
- Runtime support library

## Phase 2: REPL and Basic Runtime (Weeks 4-5)

### 2.1 Interactive REPL

**REPL Commands**:
```topos
topos> 1 + 1
2 : Natural

topos> flow double x = x * 2
double : Natural -> Natural

topos> :type double
double : Natural -> Natural

topos> :load examples/list.tps
Module Examples.List loaded

topos> :browse List
shape List a = Nil | Cons a (List a)
  map : (a -> b) -> List a -> List b
  filter : (a -> Bool) -> List a -> List a
  fold : (b -> a -> b) -> b -> List a -> b

topos> :quit
```

**Multi-line Input**:
```topos
topos> flow fibonacci n = match n
     |   | 0 -> 0
     |   | 1 -> 1  
     |   | n -> fibonacci (n - 1) + fibonacci (n - 2)
     | end
fibonacci : Natural -> Natural
```

**Deliverables**:
- REPL loop implementation
- Command parser (`:type`, `:load`, `:browse`, etc.)
- Pretty printer for types and values
- History and tab completion

### 2.2 Standard Prelude

**Core Library (prelude.tps)**:
```topos
-- Category-theoretic foundation types
shape Identity a = Identity a

shape Compose f g a = Compose (f (g a))

-- Basic algebraic data types  
shape Bool = True | False

shape List a = Nil | Cons a (List a)

shape Result a b = Ok a | Error b

shape Maybe a = Some a | None

-- Functor trait (endofunctor in category of types)
trait Functor f where
  map : (a -> b) -> f a -> f b
  
  law identity : map id == id
  law composition : map (f |> g) == map f |> map g

-- Monad trait (monoid in category of endofunctors)
trait Monad m where
  return : a -> m a
  bind : m a -> (a -> m b) -> m b
  
  law left_identity : bind (return a) f == f a
  law right_identity : bind ma return == ma
  law associativity : bind (bind ma f) g == bind ma (\x -> bind (f x) g)

-- List operations
flow map : (a -> b) -> List a -> List b
flow map f = match
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
end

flow filter : (a -> Bool) -> List a -> List a
flow filter pred = match
  | Nil -> Nil
  | Cons x xs when pred x -> Cons x (filter pred xs)
  | Cons _ xs -> filter pred xs
end

flow fold : (b -> a -> b) -> b -> List a -> b
flow fold f acc = match
  | Nil -> acc
  | Cons x xs -> fold f (f acc x) xs
end
```

## Phase 3: Pattern Matching Engine (Weeks 6-8)

### 3.1 Advanced Pattern Features

**Pattern Matching Capabilities**:
```topos
-- Guards with pattern bindings
flow safeDivide : Natural -> Natural -> Result Natural Text
flow safeDivide x y = match (x, y)
  | (_, 0) -> Error "Division by zero"
  | (x, y) when x < y -> Ok 0
  | (x, y) -> Ok (x / y)
end

-- Or-patterns for multiple cases
flow classifyNumber : Integer -> Text
flow classifyNumber n = match n
  | 0 -> "zero"
  | 1 | -1 -> "unit"  
  | 2 | 3 | 5 | 7 | 11 -> "small prime"
  | n when n > 0 -> "positive"
  | _ -> "negative"
end

-- Nested patterns
flow headOfHead : List (List a) -> Maybe a
flow headOfHead = match
  | Cons (Cons x _) _ -> Some x
  | _ -> None
end

-- As-patterns for naming
flow duplicateHead : List a -> List a
flow duplicateHead = match
  | Cons x xs as list -> Cons x list
  | Nil -> Nil
end
```

### 3.2 Pattern Compilation Strategy

**Decision Tree Generation**:
```topos
-- Source patterns
flow describe : (Bool, Bool) -> Text
flow describe = match
  | (True, True) -> "both"
  | (True, False) -> "first"
  | (False, True) -> "second"
  | (False, False) -> "neither"
end

-- Compiles to decision tree:
-- 1. Test first element
--    True -> Test second element
--            True -> "both"
--            False -> "first"
--    False -> Test second element
--            True -> "second"
--            False -> "neither"
```

### 3.3 Exhaustiveness and Redundancy Checking

```topos
-- Exhaustiveness warning
flow incomplete : Maybe a -> Natural
flow incomplete = match
  | Some _ -> 1
  -- Warning: Pattern match is not exhaustive
  -- Missing: None
end

-- Redundancy warning
flow redundant : Bool -> Natural
flow redundant = match
  | True -> 1
  | False -> 0
  | True -> 2  -- Warning: Redundant pattern
end
```

## Phase 4: Module System (Weeks 9-10)

### 4.1 Module Structure

**Module Definition (data/list.tps)**:
```topos
module Data.List exports (List, map, filter, fold, append) where

-- Private helper (not exported)
private flow reverse_helper : List a -> List a -> List a
private flow reverse_helper acc = match
  | Nil -> acc
  | Cons x xs -> reverse_helper (Cons x acc) xs
end

-- Public exports
shape List a = Nil | Cons a (List a)

flow map : (a -> b) -> List a -> List b
flow map f = match
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
end

flow filter : (a -> Bool) -> List a -> List a  
flow filter pred = match
  | Nil -> Nil
  | Cons x xs when pred x -> Cons x (filter pred xs)
  | Cons _ xs -> filter pred xs
end

flow fold : (b -> a -> b) -> b -> List a -> b
flow fold f acc = match
  | Nil -> acc
  | Cons x xs -> fold f (f acc x) xs
end

flow append : List a -> List a -> List a
flow append xs ys = match xs
  | Nil -> ys
  | Cons x xs' -> Cons x (append xs' ys)
end

end -- module
```

### 4.2 Import System

**Import Examples (main.tps)**:
```topos
-- Qualified imports
import qualified Data.List as L
import qualified Data.Set as Set

-- Selective imports
import Data.List (List, map, filter)
import Data.Maybe (Maybe(Some, None))

-- Module usage
flow process : List Natural -> Set.Set Natural
flow process xs = 
  xs 
  |> L.filter (> 0)
  |> L.map (* 2)
  |> Set.fromList

flow safe_head : List a -> Maybe a
flow safe_head = match
  | Cons x _ -> Some x
  | Nil -> None
end
```

### 4.3 Module Compilation

Each Topos module compiles to a BEAM module:
- Module name: `Data.List` → `'Elixir.Data.List'`
- Export list: Public functions only
- Private functions: Not in export list
- Module attributes: Store type information

## Phase 5: Actor Model Integration (Weeks 11-12)

### 5.1 Actor Definition Syntax

**Counter Actor (actors/counter.tps)**:
```topos
actor Counter = {
  -- State type (immutable between messages)
  shape State = { 
    count: Natural,
    history: List Natural 
  }
  
  -- Message protocol
  shape Message = 
    | Increment
    | Decrement  
    | Get
    | Reset Natural
  
  -- Initialization
  flow init : () -> State
  flow init () = { count: 0, history: [] }
  
  -- Message handler (returns new state)
  flow handle : Message -> State -> (State, Maybe Reply)
  flow handle msg state = match msg
    | Increment -> 
        let new_count = state.count + 1
        let new_state = state with { 
          count: new_count,
          history: new_count :: state.history 
        }
        in (new_state, None)
        
    | Decrement ->
        let new_count = state.count - 1
        let new_state = state with {
          count: new_count,
          history: new_count :: state.history
        }
        in (new_state, None)
        
    | Get -> 
        (state, Some state.count)
        
    | Reset n ->
        ({ count: n, history: [n] }, None)
  end
}

-- Usage
flow example_usage : Process Unit
flow example_usage = do
  counter <- spawn Counter.init()
  counter ! Increment
  counter ! Increment
  result <- counter ? Get
  IO.println("Count: " <> show result)
end
```

### 5.2 Supervision Trees

**Supervisor Definition (supervisor.tps)**:
```topos
supervisor AppSupervisor = {
  strategy: one_for_one
  max_restarts: 3
  max_seconds: 60
  
  children: [
    worker(Counter, name: :counter1),
    worker(Counter, name: :counter2),
    supervisor(SubSupervisor, [])
  ]
}

-- Child specification
flow child_spec : ChildSpec
flow child_spec = {
  id: :my_worker,
  start: {MyWorker, :start_link, []},
  restart: :permanent,
  shutdown: 5000,
  type: :worker
}
```

## Implementation Architecture

### Technology Stack

**Compiler Implementation Options**:

**Option 1: Erlang/Elixir Implementation**
- Pros: Native BEAM integration, can use OTP libraries
- Cons: Bootstrapping problem for self-hosting

**Option 2: Haskell Implementation**  
- Pros: Strong type system, excellent parsing libraries (Megaparsec)
- Cons: Separate runtime, deployment complexity

**Option 3: Rust Implementation**
- Pros: Performance, memory safety, good error messages
- Cons: Learning curve, longer development time

**Recommendation**: Start with Erlang/Elixir for fast prototyping, plan for self-hosting later.

### Project Structure

```
topos/
├── lib/
│   ├── compiler/
│   │   ├── lexer.erl        # Tokenization
│   │   ├── parser.erl       # AST generation
│   │   ├── types.erl        # Type system
│   │   ├── inference.erl    # Type inference
│   │   ├── patterns.erl     # Pattern compilation
│   │   ├── codegen.erl      # Core Erlang generation
│   │   └── modules.erl      # Module system
│   ├── runtime/
│   │   ├── prelude.erl      # Runtime support
│   │   ├── actors.erl       # Actor primitives
│   │   └── supervisor.erl   # Supervision support
│   └── repl/
│       ├── repl.erl         # REPL loop
│       ├── commands.erl     # REPL commands
│       └── pretty.erl       # Pretty printing
├── stdlib/
│   ├── prelude.tps          # Standard library
│   ├── data/
│   │   ├── list.tps
│   │   ├── set.tps
│   │   └── map.tps
│   └── control/
│       ├── monad.tps
│       └── functor.tps
├── examples/
│   ├── hello.tps
│   ├── factorial.tps
│   ├── fibonacci.tps
│   └── counter.tps
├── test/
│   ├── compiler/
│   ├── stdlib/
│   └── integration/
└── README.md
```

## Testing Strategy

### Compiler Tests

**Lexer Tests**:
```topos
-- Input: test/lexer/operators.tps
flow compose = f |> g >>= h

-- Expected tokens:
FLOW, IDENT(compose), EQUALS, IDENT(f), PIPE_RIGHT, 
IDENT(g), BIND, IDENT(h), EOF
```

**Type Inference Tests**:
```topos
-- Input: test/types/polymorphism.tps
flow identity x = x
flow const x y = x
flow compose f g x = f (g x)

-- Expected types:
-- identity : forall a. a -> a
-- const : forall a b. a -> b -> a  
-- compose : forall a b c. (b -> c) -> (a -> b) -> a -> c
```

### Integration Tests

**Full Compilation Test**:
```topos
-- test/integration/quicksort.tps
flow quicksort : List Natural -> List Natural
flow quicksort = match
  | Nil -> Nil
  | Cons pivot xs ->
      let smaller = filter (< pivot) xs
      let greater = filter (>= pivot) xs
      in append (quicksort smaller) 
                (Cons pivot (quicksort greater))
end

-- Verify: Compiles, type checks, runs correctly
```

## Development Milestones

### Milestone 1 (Week 3): Basic Compilation
```topos
-- hello.tps
flow main : Text
flow main = "Hello, Topos!"
```
✓ Parses successfully  
✓ Type checks  
✓ Generates .beam file  
✓ Runs on BEAM VM

### Milestone 2 (Week 5): Working REPL
```topos
topos> flow fib n = match n | 0 -> 0 | 1 -> 1 | n -> fib(n-1) + fib(n-2) end
fib : Natural -> Natural
topos> fib 10
55 : Natural
```

### Milestone 3 (Week 8): Pattern Matching
```topos
-- Full pattern matching with guards, or-patterns
flow classify : Natural -> Text
flow classify n = match n
  | 0 -> "zero"
  | 1 | 2 | 3 -> "small"
  | n when n < 10 -> "single digit"
  | n when n < 100 -> "double digit"
  | _ -> "large"
end
```

### Milestone 4 (Week 10): Module System
```topos
-- Multi-file compilation
-- math/prime.tps
module Math.Prime exports (isPrime, primes) where
  flow isPrime : Natural -> Bool
  flow primes : Natural -> List Natural
end
```

### Milestone 5 (Week 12): Actors
```topos
-- Working actor with supervision
actor Stack = {
  shape State = List a
  shape Message = Push a | Pop | Size
  
  flow handle msg state = match msg
    | Push x -> (x :: state, None)
    | Pop -> match state
        | Nil -> (Nil, Some (Error "Empty"))
        | Cons _ xs -> (xs, Some Ok)
    | Size -> (state, Some (length state))
  end
}
```

## Performance Targets

### Compilation Speed
- < 100ms for 1000 line module
- Incremental compilation support
- Parallel module compilation

### Runtime Performance  
- Pattern matching: Within 10% of native Erlang
- Function calls: Zero overhead vs Erlang
- Actor messaging: Native BEAM performance

### Memory Usage
- Complete type erasure (no runtime type overhead)
- Immutable data with structure sharing
- Leverage BEAM's garbage collection

## Documentation Plan

### Language Reference
1. **Syntax Guide**: Complete grammar reference
2. **Type System**: Type inference, traits, laws
3. **Pattern Matching**: All pattern forms
4. **Module System**: Import/export, visibility
5. **Actor Model**: Actors, messages, supervision
6. **Standard Library**: All stdlib modules

### Tutorials
1. **Getting Started**: Installation, first program
2. **Functional Basics**: Functions, types, recursion
3. **Pattern Matching**: From simple to advanced
4. **Building Applications**: Modules and organization
5. **Concurrent Programming**: Actors and supervisors
6. **Category Theory**: Mathematical foundations

## Future Roadmap (Post-PoC)

### Advanced Type Features
- **Row polymorphism**: Extensible records/variants
- **Type families**: Type-level functions
- **GADTs**: Generalized algebraic data types
- **Dependent types**: Type-safe proofs

### Category Theory Features
- **Functors as modules**: ML-style functors
- **Natural transformations**: First-class transforms
- **Monad transformers**: Composable effects
- **Comonads**: Context-aware computations

### Distribution Features
- **Distributed actors**: Cross-node messaging
- **Location transparency**: Automatic routing
- **Consensus protocols**: Built-in Raft/Paxos
- **CRDTs**: Conflict-free replicated data

### Tooling
- **Language Server Protocol**: IDE support
- **Debugger**: Step-through debugging
- **Profiler**: Performance analysis
- **Property testing**: QuickCheck-style
- **Formal verification**: Model checking

## Success Metrics

The PoC succeeds when:

1. ✅ **Core language works**: Can write and run Topos programs
2. ✅ **Type system works**: Catches errors, infers types correctly
3. ✅ **BEAM integration works**: Generates valid bytecode, runs on VM
4. ✅ **REPL works**: Interactive development is smooth
5. ✅ **Patterns work**: All pattern forms compile correctly
6. ✅ **Modules work**: Multi-file programs compile and link
7. ✅ **Actors work**: Basic OTP patterns expressible
8. ✅ **Documentation exists**: Others can learn and contribute

## Conclusion

This implementation plan provides a structured path to building Topos as a category-theory-based functional language for the BEAM. By focusing on the essential features that demonstrate the language's unique value—categorical abstractions, advanced pattern matching, and actor model integration—we can deliver a working proof-of-concept in 12 weeks.

The phased approach ensures each component builds on solid foundations, while the focus on BEAM integration leverages decades of distributed systems expertise. Most importantly, this plan creates a language that bridges the gap between mathematical elegance and practical systems programming.
