# Phase 1: Core Language Infrastructure

## Overview

This phase establishes the foundational compiler infrastructure for Topos, transforming source code into executable BEAM bytecode. We build the lexer for tokenization, the parser for AST generation, and the type inference engine based on Hindley-Milner with categorical extensions. The goal is to create a robust compilation pipeline that can parse basic Topos syntax (`shape` for types, `flow` for morphisms), perform type checking with support for parametric polymorphism, and generate Core Erlang code that executes on the BEAM VM.

By the end of this phase, we will have a working compiler that can take simple Topos programs, infer their types automatically, and produce valid `.beam` files. This establishes the technical foundation for all subsequent phases, enabling iterative development of language features while maintaining a working compilation pathway.

This phase runs for 3 weeks and focuses on correctness over optimization, establishing clean abstractions and comprehensive testing to support future feature additions without architectural rework.

---

## 1.1 Lexer and Parser
- [ ] **Section 1.1 Complete**

The lexer and parser form the front-end of the compiler, transforming raw Topos source code into structured Abstract Syntax Trees (ASTs). The lexer breaks input text into tokens (keywords, operators, identifiers, literals), while the parser organizes these tokens according to Topos grammar rules. We implement this using Erlang's leex (lexical analyzer generator) and yecc (LALR parser generator) tools, which provide mature, battle-tested parsing infrastructure. The parser must handle Topos's unique syntax including categorical terminology (`shape`, `flow`), composition operators (`|>`, `>>=`), and pattern matching constructs. Error recovery and helpful error messages are priorities from the start.

### 1.1.1 Token Recognition
- [ ] **Task 1.1.1 Complete**

Token recognition involves defining lexical rules that classify input characters into meaningful tokens. We must identify keywords (`shape`, `flow`, `match`, `where`, `let`, `in`, `do`, `end`), operators (`|>`, `->`, `:`, `=`, `<>`, `>>=`), delimiters (`{`, `}`, `[`, `]`, `(`, `)`, `|`), literals (numbers, strings, atoms), and comments (single-line `--` and multi-line `{- -}`). The lexer must handle whitespace correctly, track line/column positions for error reporting, and distinguish between similar tokens (e.g., `->` vs `-`).

- [ ] 1.1.1.1 Define token types and lexical rules for all Topos keywords, operators, and delimiters
- [ ] 1.1.1.2 Implement number literal recognition supporting integers, floats, and scientific notation
- [ ] 1.1.1.3 Implement string literal recognition with escape sequences and multi-line support
- [ ] 1.1.1.4 Implement comment recognition for single-line and multi-line comments with proper nesting

### 1.1.2 Grammar Implementation
- [ ] **Task 1.1.2 Complete**

The parser grammar defines the syntactic structure of Topos programs using production rules. We implement a context-free grammar that handles type declarations (`shape`), function definitions (`flow`), pattern matching expressions, let bindings, and composition operators. The grammar must be unambiguous and support operator precedence correctly (e.g., function application binds tighter than `|>`). We use LALR parsing with shift/reduce conflict resolution or PEG parsing with ordered choice.

- [ ] 1.1.2.1 Define grammar production rules for shape declarations with ADT constructors and record syntax
- [ ] 1.1.2.2 Define grammar production rules for flow definitions with type signatures and pattern clauses
- [ ] 1.1.2.3 Define grammar production rules for expressions including composition, application, and let bindings
- [ ] 1.1.2.4 Define operator precedence and associativity tables ensuring correct parsing of complex expressions

### 1.1.3 AST Construction
- [ ] **Task 1.1.3 Complete**

The Abstract Syntax Tree (AST) is the internal representation of parsed Topos programs. We define appropriate data structures using Erlang records for each syntactic category: module definitions, type declarations, function definitions, patterns, expressions, and literals. The AST preserves source location information for error reporting and should be easy to traverse for subsequent compiler passes. We design the AST to be immutable and use pattern matching for processing, leveraging Erlang's native pattern matching capabilities.

- [ ] 1.1.3.1 Define AST node structures for all expression types with source location metadata
- [ ] 1.1.3.2 Define AST node structures for pattern forms including guards, or-patterns, and nested patterns
- [ ] 1.1.3.3 Define AST node structures for declarations (shapes, flows, modules) with visibility annotations
- [ ] 1.1.3.4 Implement AST construction functions that build structured trees from parse results

### 1.1.4 Error Recovery and Reporting
- [ ] **Task 1.1.4 Complete**

High-quality error messages are essential for developer experience. When syntax errors occur, we provide clear messages indicating what went wrong, where in the source file, and what was expected. Error recovery allows the parser to continue after errors to report multiple issues in one pass. We implement error messages with code snippets, color highlighting, and suggestions for fixes.

- [ ] 1.1.4.1 Implement error reporting with source location, line/column numbers, and code context
- [ ] 1.1.4.2 Implement panic-mode error recovery for common syntax errors to continue parsing
- [ ] 1.1.4.3 Create helpful error messages with suggestions for common mistakes (e.g., missing `end`, unmatched delimiters)
- [ ] 1.1.4.4 Add colored terminal output and code snippet formatting for error display

### Unit Tests - Section 1.1
- [ ] **Unit Tests 1.1 Complete**
- [ ] Test lexer tokenization of all keywords, operators, delimiters, and literals with edge cases
- [ ] Test parser handling of valid Topos programs generating correct ASTs
- [ ] Test parser error recovery with intentionally malformed input producing multiple error reports
- [ ] Test source location tracking ensuring accurate line/column information in AST nodes

---

## 1.2 Core Type System
- [ ] **Section 1.2 Complete**

The type system is the heart of Topos, providing static guarantees while inferring types automatically. We implement Hindley-Milner type inference using Algorithm W, extended with support for type classes (traits), row polymorphism for extensible records, and higher-kinded types for functors. The type system ensures that well-typed programs cannot go wrong while minimizing type annotations required from programmers. Type errors must be clear and localized, pointing to the exact source of type mismatches.

### 1.2.1 Type Representation
- [ ] **Task 1.2.1 Complete**

We define the internal representation of types, including type variables (α, β, γ), type constructors (`List`, `Maybe`, `Process`), function types (τ₁ -> τ₂), record types, and variant types. Type schemes represent polymorphic types with forall quantification. We implement type equality checking, substitution operations, and pretty-printing for type expressions used in error messages.

- [ ] 1.2.1.1 Define type term representation with type variables, constructors, functions, records, and variants
- [ ] 1.2.1.2 Implement type substitution operations for unification and instantiation
- [ ] 1.2.1.3 Implement type scheme representation for polymorphic types with quantified variables
- [ ] 1.2.1.4 Implement type pretty-printing for human-readable error messages and REPL output

### 1.2.2 Algorithm W Implementation
- [ ] **Task 1.2.2 Complete**

Algorithm W is the standard approach to Hindley-Milner type inference, combining constraint generation with unification. We traverse the AST, generating type constraints, then solve them using Robinson's unification algorithm with occurs check. The algorithm infers the most general (principal) type for each expression. We handle let-polymorphism correctly, allowing generalization only at let bindings.

- [ ] 1.2.2.1 Implement constraint generation traversing AST and collecting type equations
- [ ] 1.2.2.2 Implement unification algorithm with occurs check preventing infinite types
- [ ] 1.2.2.3 Implement type generalization for let bindings introducing forall quantifiers
- [ ] 1.2.2.4 Implement type instantiation for polymorphic function applications

### 1.2.3 Constraint Solving
- [ ] **Task 1.2.3 Complete**

Beyond simple unification, we need constraint solving for type classes (traits like `Functor`, `Monad`, `Ord`). When a function uses operations from a trait, we generate trait constraints that must be satisfied. Constraint solving searches for trait instances and resolves ambiguous type variables. We implement instance resolution with backtracking and check for coherence (no overlapping instances).

- [ ] 1.2.3.1 Implement trait constraint representation and generation from trait-polymorphic functions
- [ ] 1.2.3.2 Implement instance resolution searching trait instances and unifying with constraints
- [ ] 1.2.3.3 Implement constraint simplification reducing complex constraints to canonical form
- [ ] 1.2.3.4 Implement coherence checking ensuring unique instance resolution without ambiguity

### 1.2.4 Error Messages
- [ ] **Task 1.2.4 Complete**

Type errors are among the most common errors developers encounter. We provide clear, actionable error messages that explain type mismatches, suggest fixes, and show the chain of reasoning that led to the error. For unification failures, we show both types and highlight the incompatible parts. For missing instances, we suggest which trait implementations are needed.

- [ ] 1.2.4.1 Implement type error formatting showing expected vs actual types with highlighting
- [ ] 1.2.4.2 Implement type error localization tracking error sources through AST locations
- [ ] 1.2.4.3 Implement error explanation providing context for common type errors with suggestions
- [ ] 1.2.4.4 Implement error recovery attempting to continue type checking after errors to report multiple issues

### Unit Tests - Section 1.2
- [ ] **Unit Tests 1.2 Complete**
- [ ] Test type inference for simple expressions inferring correct types without annotations
- [ ] Test type inference for polymorphic functions with proper generalization and instantiation
- [ ] Test type checking catching type errors with clear error messages
- [ ] Test trait constraint solving resolving instances correctly and detecting missing instances

---

## 1.3 Core Erlang Code Generation
- [ ] **Section 1.3 Complete**

After parsing and type checking, we generate Core Erlang code that executes on the BEAM VM. Core Erlang is a simplified functional language that serves as an intermediate representation for all BEAM languages. We translate Topos's typed AST into Core Erlang expressions, compiling shapes to tagged tuples, flows to functions, and pattern matching to case expressions. The generated code must preserve Topos semantics while leveraging BEAM optimizations.

### 1.3.1 Expression Translation
- [ ] **Task 1.3.1 Complete**

We translate Topos expressions to equivalent Core Erlang expressions. Function applications become Core Erlang calls, let bindings become Core Erlang let expressions, and composition operators become function call chains. We must handle tail-call optimization correctly, ensuring recursive functions don't overflow the stack. Literals translate directly, and variables map to Core Erlang variables.

- [ ] 1.3.1.1 Implement translation of function applications to Core Erlang call expressions
- [ ] 1.3.1.2 Implement translation of let bindings to Core Erlang let expressions with proper scoping
- [ ] 1.3.1.3 Implement translation of composition operators (|>) to nested function calls
- [ ] 1.3.1.4 Implement translation of literals (numbers, strings, atoms) to Core Erlang constants

### 1.3.2 Pattern Compilation
- [ ] **Task 1.3.2 Complete**

Pattern matching is central to functional programming. We compile Topos patterns to Core Erlang case expressions using decision tree algorithms. This involves converting high-level patterns (guards, or-patterns, as-patterns) into primitive match operations. We optimize pattern matching order to minimize runtime checks and detect unreachable branches.

- [ ] 1.3.2.1 Implement basic pattern compilation for constructors, variables, and wildcards
- [ ] 1.3.2.2 Implement guard compilation translating guard expressions to Core Erlang conditions
- [ ] 1.3.2.3 Implement decision tree generation optimizing pattern match ordering for efficiency
- [ ] 1.3.2.4 Implement exhaustiveness checking warning about non-exhaustive patterns at compile time

### 1.3.3 Type Erasure
- [ ] **Task 1.3.3 Complete**

Topos's rich type system exists only at compile time. At runtime, BEAM operates on untyped terms. Type erasure removes all type information from the AST, leaving only computational content. Polymorphic functions use uniform representation with runtime type information where necessary, and type classes disappear after instance resolution through dictionary-passing transformation. We ensure that erasure preserves program semantics—well-typed programs don't change behavior.

- [ ] 1.3.3.1 Implement type annotation removal stripping all type information from expressions
- [ ] 1.3.3.2 Implement trait dictionary passing for type classes converting to explicit parameters
- [ ] 1.3.3.3 Implement polymorphism handling through monomorphization or uniform representation
- [ ] 1.3.3.4 Verify semantic preservation ensuring erased code has same behavior as typed code

### 1.3.4 Module Generation
- [ ] **Task 1.3.4 Complete**

Each Topos module compiles to a BEAM module with exports, imports, and module attributes. We generate module metadata including function signatures and documentation. Private functions are excluded from exports. Module names follow Erlang naming conventions, potentially using namespaces. We emit `.core` files that erlc compiles to `.beam` bytecode.

- [ ] 1.3.4.1 Implement module structure generation with module name, exports, and attributes
- [ ] 1.3.4.2 Implement function compilation generating Core Erlang function definitions with arities
- [ ] 1.3.4.3 Implement export list generation including only public functions with correct arities
- [ ] 1.3.4.4 Implement Core Erlang file output writing valid .core files that erlc can compile

### Unit Tests - Section 1.3
- [ ] **Unit Tests 1.3 Complete**
- [ ] Test expression translation generating correct Core Erlang for all expression forms
- [ ] Test pattern compilation producing optimal decision trees with exhaustiveness checking
- [ ] Test type erasure preserving semantics while removing all type information
- [ ] Test module generation producing valid .core files that compile to working .beam modules

---

## 1.4 Integration Tests
- [ ] **Section 1.4 Complete**

Integration tests validate the entire compilation pipeline from source to executable bytecode. We test realistic Topos programs that exercise multiple compiler phases together, ensuring that the lexer, parser, type checker, and code generator work cohesively. These tests catch interaction bugs that unit tests miss and validate that generated BEAM modules execute correctly.

### 1.4.1 End-to-End Compilation
- [ ] **Task 1.4.1 Complete**

End-to-end tests compile complete Topos programs and verify they produce correct results. We test examples like factorial, fibonacci, list operations, and simple recursive functions. Each test compiles the source, loads the generated .beam module, calls exported functions, and checks outputs against expected values.

- [ ] 1.4.1.1 Test compilation and execution of simple arithmetic expressions returning correct results
- [ ] 1.4.1.2 Test compilation and execution of recursive functions (factorial, fibonacci) with correct outputs
- [ ] 1.4.1.3 Test compilation and execution of polymorphic functions with different type instantiations
- [ ] 1.4.1.4 Test compilation and execution of pattern matching code with multiple clauses and guards

### 1.4.2 BEAM Bytecode Validation
- [ ] **Task 1.4.2 Complete**

We validate that generated .beam files are well-formed and meet BEAM VM requirements. This includes checking module exports match declarations, function arities are correct, and bytecode passes BEAM validation. We use tools like `beam_lib` to inspect generated modules and verify metadata.

- [ ] 1.4.2.1 Verify generated .beam files load without errors using Erlang's code loader
- [ ] 1.4.2.2 Verify module exports match source declarations with correct function names and arities
- [ ] 1.4.2.3 Verify function calls use correct module-qualified names and argument counts
- [ ] 1.4.2.4 Verify pattern matching compiles to efficient BEAM select_val instructions where applicable

### 1.4.3 Error Handling Pipeline
- [ ] **Task 1.4.3 Complete**

We test that errors at each compiler stage are caught and reported appropriately. Syntax errors produce helpful parser messages, type errors show clear type mismatches, and code generation errors indicate internal compiler issues. Error messages include source locations and suggestions for fixes.

- [ ] 1.4.3.1 Test syntax error reporting with multiple intentional syntax errors showing clear messages
- [ ] 1.4.3.2 Test type error reporting with type mismatches showing expected and actual types
- [ ] 1.4.3.3 Test exhaustiveness warnings for non-exhaustive pattern matches
- [ ] 1.4.3.4 Test error recovery allowing compiler to report multiple errors in single pass

---

## Success Criteria

1. **Lexer and Parser**: Successfully parse valid Topos programs into ASTs with error recovery for malformed input
2. **Type Inference**: Correctly infer types for polymorphic functions and detect type errors with clear messages
3. **Code Generation**: Generate valid Core Erlang that compiles to working .beam modules
4. **Integration**: Compile and run example programs (factorial, list processing) producing correct outputs
5. **Error Messages**: Provide helpful, localized error messages for syntax and type errors
6. **Test Coverage**: 85% test coverage with comprehensive unit and integration tests

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 2**: REPL and standard library requiring working compilation and type inference
- **Phase 3**: Advanced pattern matching building on pattern compilation infrastructure
- **Phase 4**: Module system extending import/export mechanisms
- **Phase 5**: Actor model integration compiling actor syntax to OTP behaviors

## Key Outputs

- Lexer and parser producing structured ASTs from Topos source code
- Type inference engine supporting Hindley-Milner with traits and row polymorphism
- Core Erlang code generator producing valid .beam modules
- Comprehensive test suite covering all compiler phases
- Error reporting infrastructure with source locations and helpful messages
- Working compilation pipeline from source to executable bytecode
