# Phase 2: REPL and Basic Runtime

## Overview

This phase creates an interactive development environment through a Read-Eval-Print Loop (REPL) and establishes the standard prelude library with essential types and functions. The REPL enables developers to experiment with Topos interactively, evaluate expressions, inspect types and effects, and load modules without full compilation cycles. **The REPL fully supports the algebraic effect system from Phase 1**, allowing developers to execute effectful programs interactively and see effect sets in type signatures. This rapid feedback loop is crucial for language exploration and debugging.

The standard prelude provides foundational category-theoretic abstractions (functors, monads, applicatives) and basic data structures (lists, maybes, results) that all Topos programs build upon. **It also includes builtin effect definitions for IO and Process operations**, establishing the standard effect library. We implement these using the compilation infrastructure from Phase 1, creating the first real Topos library code. The prelude demonstrates how category theory concepts and algebraic effects translate to practical programming patterns, serving as both a utility library and a reference implementation.

This phase runs for **3.5 weeks** and prioritizes developer experience—making the REPL responsive and effect-aware, error messages helpful, and the prelude API intuitive. By the end, developers can write and test Topos code interactively including effectful programs, significantly accelerating development velocity.

---

## 2.1 Interactive REPL
- [ ] **Section 2.1 Complete**

The REPL forms the primary interface for interactive Topos development. Developers type expressions,  see results immediately, and inspect inferred types—enabling rapid prototyping and debugging. The REPL maintains a persistent environment across inputs, allowing function definitions and bindings to accumulate throughout the session. We implement special commands (`:type`, `:load`, `:browse`) for introspection and module management. The REPL must handle multi-line input gracefully, support history navigation, and provide tab completion for a smooth user experience.

### 2.1.1 Core REPL Loop
- [ ] **Task 2.1.1 Complete**

The REPL loop continuously reads input, evaluates it in the current environment, prints results, and loops back. We integrate the Phase 1 compiler (lexer, parser, type checker) to process each input line. For expressions, we evaluate directly and show results. For definitions (`flow` declarations), we add them to the environment. The loop must handle errors gracefully without crashing, allowing users to correct mistakes and continue.

- [ ] 2.1.1.1 Implement input reading with multi-line support detecting incomplete expressions requiring continuation
- [ ] 2.1.1.2 Implement expression evaluation integrating parser, type checker, and code generator from Phase 1
- [ ] 2.1.1.3 Implement result printing showing values with inferred types in human-readable format
- [ ] 2.1.1.4 Implement environment persistence maintaining definitions and bindings across REPL inputs

### 2.1.2 Command System
- [ ] **Task 2.1.2 Complete**

Special commands provide introspection and control. `:type expr` shows the inferred type without evaluating. `:load file.tps` compiles and loads a module into the REPL environment. `:browse Module` displays all exports from a module. `:reload` recompiles the last loaded module. `:quit` exits cleanly. Commands start with `:` to distinguish them from ordinary expressions.

- [ ] 2.1.2.1 Implement `:type` command performing type inference without evaluation and displaying type
- [ ] 2.1.2.2 Implement `:load` command compiling files and importing definitions into REPL environment
- [ ] 2.1.2.3 Implement `:browse` command listing module exports with their types and documentation
- [ ] 2.1.2.4 Implement `:quit`, `:reload`, and `:help` commands for REPL control and assistance

### 2.1.3 Pretty Printing
- [ ] **Task 2.1.3 Complete**

Results must display clearly and readably. We format values according to their types: lists show as `[1, 2, 3]`, records as `{x: 1.0, y: 2.0}`, algebraic data types as `Some 42` or `None`. **Types display with effect annotations** showing function signatures as `String -> Config / {FileIO}` for effectful functions. Types display using mathematical notation where appropriate (`∀a. a -> a` for polymorphic identity). We use colors to highlight different syntactic categories (types in blue, values in green, effects in yellow, errors in red) and limit output length for large values.

- [ ] 2.1.3.1 Implement value pretty-printing rendering values in human-readable format matching source syntax
- [ ] 2.1.3.2 Implement type pretty-printing formatting types with proper precedence and forall quantifiers
- [ ] 2.1.3.3 Implement syntax highlighting using ANSI colors for types, values, and errors
- [ ] 2.1.3.4 Implement output truncation for large values with expansion on request
- [ ] 2.1.3.5 Implement effect set pretty-printing displaying effect annotations in type signatures as `/ {Effect1, Effect2}` with empty set `/ {}` elided for pure functions

### 2.1.4 History and Completion
- [ ] **Task 2.1.4 Complete**

Command history allows users to recall previous inputs using up/down arrows. We persist history across sessions in a `.topos_history` file. Tab completion suggests keywords, defined functions, and module names based on the current input prefix. Completion context-awareness helps users discover available functions without memorizing everything.

- [ ] 2.1.4.1 Implement command history with up/down arrow navigation through previous inputs
- [ ] 2.1.4.2 Implement history persistence saving history to disk and loading on REPL startup
- [ ] 2.1.4.3 Implement tab completion for keywords, built-in functions, and defined identifiers
- [ ] 2.1.4.4 Implement context-aware completion suggesting appropriate completions based on parse context

### 2.1.5 Effect Execution in REPL
- [ ] **Task 2.1.5 Complete**

The REPL executes effectful expressions using the process-based effect runtime from Phase 1. When users evaluate expressions with effects like `perform IO.print("Hello")`, the REPL spawns handler processes, routes effect operations, and displays results. **Effect handlers are automatically provided for builtin effects** (IO, Process) so developers can experiment with effects interactively without manual handler setup. The REPL shows which effects are being performed and handles effect errors gracefully.

- [ ] 2.1.5.1 Integrate effect runtime into REPL evaluation pipeline spawning handler processes for effectful expressions
- [ ] 2.1.5.2 Provide default handlers for builtin effects (IO, Process) automatically wrapping REPL evaluation in try/with blocks
- [ ] 2.1.5.3 Display effect execution feedback showing which effects are being performed during evaluation (optional verbose mode)
- [ ] 2.1.5.4 Handle effect runtime errors gracefully catching handler failures and displaying error messages without crashing REPL

### Unit Tests - Section 2.1
- [ ] **Unit Tests 2.1 Complete**
- [ ] Test REPL evaluation of simple expressions returning correct values and types
- [ ] Test REPL definition accumulation with functions remaining available across inputs
- [ ] Test REPL command execution (`:type`, `:load`, `:browse`) with correct behavior
- [ ] Test REPL error handling recovering from syntax and type errors without crashing
- [ ] Test REPL effect execution evaluating effectful expressions with automatic handler provision
- [ ] Test REPL effect pretty-printing displaying effect sets in type signatures correctly
- [ ] Test REPL `:type` command showing effect annotations for effectful functions

---

## 2.2 Standard Prelude
- [ ] **Section 2.2 Complete**

The prelude defines foundational types and functions that form the basis of all Topos programs. It includes category-theoretic abstractions (Functor, Applicative, Monad traits) and essential data structures (Bool, List, Maybe, Result). The prelude is automatically imported into every module and REPL session, providing a rich standard library without explicit imports. Implementations demonstrate functional programming patterns and serve as reference code for learning Topos.

### 2.2.1 Core Types
- [ ] **Task 2.2.1 Complete**

We define the most basic algebraic data types that programs universally need. `Bool` provides logical values. `Maybe` represents optional values. `Result` encodes computations that may fail. `List` is the fundamental recursive data structure. These types follow category theory principles—`Maybe` and `Result` are monads, `List` is a free monoid—giving them consistent, predictable APIs.

- [ ] 2.2.1.1 Implement `Bool` type with `True` and `False` constructors and basic operations (and, or, not)
- [ ] 2.2.1.2 Implement `Maybe a` type with `Some a` and `None` constructors for optional values
- [ ] 2.2.1.3 Implement `Result a b` type with `Ok a` and `Error b` constructors for error handling
- [ ] 2.2.1.4 Implement `List a` type with `Nil` and `Cons a (List a)` constructors for recursive lists

### 2.2.2 Functor and Monad Traits
- [ ] **Task 2.2.2 Complete**

Functors and monads provide the category-theoretic foundation for composable computation. `Functor` defines `map`, lifting functions into contexts. `Applicative` adds `pure` and `apply` for independent effects. `Monad` provides `return` and `bind` for sequential effects. We implement instances for `Maybe`, `List`, and `Result`, demonstrating how these abstractions unify disparate patterns. Laws (identity, composition, associativity) ensure correctness.

- [ ] 2.2.2.1 Define `Functor` trait with `map : (a -> b) -> f a -> f b` and functor laws
- [ ] 2.2.2.2 Define `Applicative` trait with `pure : a -> f a` and `apply : f (a -> b) -> f a -> f b`
- [ ] 2.2.2.3 Define `Monad` trait with `return : a -> m a` and `bind : m a -> (a -> m b) -> m b`
- [ ] 2.2.2.4 Implement Functor, Applicative, and Monad instances for Maybe, List, and Result types

### 2.2.3 List Operations
- [ ] **Task 2.2.3 Complete**

Lists are the workhorse data structure in functional programming. We implement essential operations: `map` transforms elements, `filter` selects elements matching a predicate, `fold` reduces lists to single values, `append` concatenates lists. These operations showcase recursion, higher-order functions, and pattern matching. They serve as reference implementations for developers learning Topos style.

- [ ] 2.2.3.1 Implement `map : (a -> b) -> List a -> List b` transforming list elements
- [ ] 2.2.3.2 Implement `filter : (a -> Bool) -> List a -> List a` selecting matching elements
- [ ] 2.2.3.3 Implement `fold : (b -> a -> b) -> b -> List a -> b` reducing lists with accumulator
- [ ] 2.2.3.4 Implement `append : List a -> List a -> List a` concatenating two lists

### 2.2.4 Helper Functions
- [ ] **Task 2.2.4 Complete**

Beyond core operations, the prelude includes convenient helpers that simplify common patterns. `compose` chains functions, `const` ignores its second argument, `identity` returns its input unchanged. `head` and `tail` access list parts safely (returning Maybe). These utilities demonstrate functional composition and serve as building blocks for larger programs.

- [ ] 2.2.4.1 Implement `compose : (b -> c) -> (a -> b) -> a -> c` for function composition
- [ ] 2.2.4.2 Implement `identity : a -> a` and `const : a -> b -> a` utility functions
- [ ] 2.2.4.3 Implement `head : List a -> Maybe a` and `tail : List a -> Maybe (List a)` safe list access
- [ ] 2.2.4.4 Implement `length : List a -> Natural`, `reverse : List a -> List a`, and `take : Natural -> List a -> List a`

### 2.2.5 Builtin Effect Definitions
- [ ] **Task 2.2.5 Complete**

The prelude defines standard effects that programs use for I/O and process interaction. **IO effect** provides file operations (read, write) and console output (print, println). **Process effect** enables process spawning and message passing for actor-style concurrency. These effects are implemented using the process-based runtime from Phase 1, with handlers that invoke actual BEAM operations. The prelude makes these effects universally available.

- [ ] 2.2.5.1 Define IO effect with operations `readFile : String -> String`, `writeFile : String -> String -> Unit`, `print : String -> Unit`, and `println : String -> Unit`
- [ ] 2.2.5.2 Define Process effect with operations `spawn : Flow -> ProcessId`, `send : ProcessId -> Message -> Unit`, and `receive : Message` for actor-style concurrency
- [ ] 2.2.5.3 Implement builtin IO effect handler using Erlang file module and io module for actual file and console operations
- [ ] 2.2.5.4 Implement builtin Process effect handler using Erlang spawn, send (!) and receive for actual process operations

### Unit Tests - Section 2.2
- [ ] **Unit Tests 2.2 Complete**
- [ ] Test core type construction and pattern matching for Bool, Maybe, Result, and List
- [ ] Test Functor, Applicative, and Monad instances obeying categorical laws
- [ ] Test list operations (map, filter, fold) producing correct results on various inputs
- [ ] Test helper functions with edge cases (empty lists, identity elements, composition)
- [ ] Test builtin effect definitions (IO, Process) with correct operation signatures and effect tracking
- [ ] Test builtin effect handlers executing actual I/O operations and process operations correctly
- [ ] Test effect handler integration with REPL and compilation pipeline

---

## 2.3 Integration Tests
- [ ] **Section 2.3 Complete**

Integration tests validate that the REPL and prelude work together seamlessly. We test interactive sessions that load modules, define functions, evaluate expressions using prelude functions, and inspect types. These tests ensure the developer experience is smooth and that all components integrate correctly.

### 2.3.1 REPL Workflow Testing
- [ ] **Task 2.3.1 Complete**

We simulate complete REPL sessions that mimic real usage patterns. Tests start the REPL, enter a sequence of commands and expressions, verify outputs, and check that state persists correctly. This catches issues with environment management, command parsing, and multi-line input handling.

- [ ] 2.3.1.1 Test complete REPL session with function definitions, expression evaluation, and type inspection
- [ ] 2.3.1.2 Test REPL module loading with `:load` command importing definitions correctly
- [ ] 2.3.1.3 Test REPL error recovery entering invalid input and continuing successfully afterward
- [ ] 2.3.1.4 Test REPL multi-line input spanning multiple lines for complex expressions

### 2.3.2 Prelude Functionality Testing
- [ ] **Task 2.3.2 Complete**

We write programs using only prelude functions and verify they compile and run correctly. Examples include list processing pipelines (map/filter/fold combinations), monadic computations using Maybe and Result, and function composition chains. This validates that the prelude is self-consistent and sufficient for basic programming.

- [ ] 2.3.2.1 Test list processing pipeline chaining map, filter, and fold operations
- [ ] 2.3.2.2 Test monadic sequencing using Maybe and Result for error-propagating computations
- [ ] 2.3.2.3 Test function composition creating complex functions from simpler ones using compose
- [ ] 2.3.2.4 Test prelude compilation ensuring all prelude modules compile without errors

### 2.3.3 End-to-End REPL Programs
- [ ] **Task 2.3.3 Complete**

We develop small but complete programs interactively in the REPL, testing realistic usage. Examples: implementing quicksort using list operations, writing a recursive tree traversal, building a simple calculator. These tests validate that the REPL handles complex, multi-step development workflows.

- [ ] 2.3.3.1 Implement and test quicksort algorithm interactively in REPL using list operations
- [ ] 2.3.3.2 Implement and test recursive fibonacci with memoization pattern using prelude types
- [ ] 2.3.3.3 Implement and test simple expression evaluator using Result for error handling
- [ ] 2.3.3.4 Implement and test tree traversal functions using Maybe for safe navigation

### 2.3.4 Effect System Integration in REPL
- [ ] **Task 2.3.4 Complete**

We validate that the REPL correctly executes effectful programs using builtin effects from the prelude. Tests verify that IO operations execute properly, effect sets display correctly in type signatures, and the REPL handles effect errors gracefully. This ensures the effect system is fully usable in interactive development.

- [ ] 2.3.4.1 Test interactive effectful programs using IO effect for file reading and console output in REPL
- [ ] 2.3.4.2 Test effect type inspection with `:type` command showing correct effect sets for effectful functions
- [ ] 2.3.4.3 Test effect error handling in REPL catching unhandled effects and handler errors with clear messages
- [ ] 2.3.4.4 Test Process effect in REPL spawning processes and sending messages interactively

---

## Success Criteria

1. **Functional REPL**: Interactive environment supporting expression evaluation, definitions, commands, and effect execution
2. **Type-and-Effect Inspection**: `:type` command correctly showing inferred types and effect sets for all expressions
3. **Module Loading**: `:load` command compiling and importing external modules successfully
4. **Complete Prelude**: Standard library with Bool, List, Maybe, Result, Functor/Monad traits, and builtin effects (IO, Process)
5. **Law Compliance**: All typeclass instances satisfying categorical laws (verified through property tests)
6. **Effect System**: Builtin IO and Process effects with working handlers executing actual operations
7. **Developer Experience**: Smooth REPL interaction with history, completion, effect execution, and helpful error messages

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 3**: Pattern matching testing benefiting from REPL for rapid iteration including effectful patterns
- **Phase 4**: Module system requiring prelude as base library and testing imports/effects in REPL
- **Phase 5**: Actor model leveraging Process effect and needing standard types (Maybe, Result) for message protocols
- **Phase 6**: Advanced effect features building on builtin effects established here
- All future development relying on prelude abstractions, builtin effects, and REPL-based testing

## Key Outputs

- Working REPL with expression evaluation, definitions, introspection commands, and effect execution
- Command system supporting `:type`, `:load`, `:browse`, `:reload`, and `:help`
- Pretty printing for values, types, and effect sets with syntax highlighting
- Effect-aware REPL with automatic handler provision for builtin effects
- Command history and tab completion for improved user experience
- Standard prelude with core types (Bool, List, Maybe, Result)
- Functor, Applicative, and Monad traits with lawful instances
- Essential list operations and helper functions
- Builtin IO and Process effect definitions with working handlers
- Comprehensive test suite covering REPL functionality, prelude correctness, and effect system integration
