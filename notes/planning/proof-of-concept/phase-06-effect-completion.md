# Phase 6: Effect System Completion

## Overview

This phase completes the algebraic effect system by implementing advanced features deferred from the proof-of-concept. While Phases 1-5 demonstrated minimal viable effects with monomorphic effect tracking, IO/Process effects only, and basic process-based runtime, Phase 6 elevates the effect system to production-ready status. We implement **effect polymorphism** enabling generic effectful code, **expanded effect library** providing standard effects for common patterns, **effect optimizations** reducing runtime overhead, and **advanced effect features** like delimited continuations and scoped effects.

Effect polymorphism allows functions to be generic over effect sets, similar to type polymorphism. A function can work with any effects, specific effect subsets, or no effects at all. This enables building reusable libraries that don't hard-code effect requirements. The expanded effect library provides State, Reader, Writer, Async, and Error effects following algebraic effect principles. Effect optimizations include fusion (combining multiple effect operations), handler inlining (eliminating effect overhead for pure code), and static resolution (compile-time effect dispatch).

This phase runs for **5 weeks** as a post-PoC completion phase, transforming the effect system from demonstration to production capability. By the end, Topos will have a complete, efficient, and ergonomic algebraic effect system rivaling research languages like Koka and Eff while maintaining BEAM compatibility and performance.

---

## 6.1 Effect Polymorphism
- [ ] **Section 6.1 Complete**

Effect polymorphism extends the type system to treat effects as first-class abstraction parameters. Functions can be polymorphic over effect sets using effect variables (ε, δ, φ). Type signatures like `(a -> b / ε) -> List a -> List b / ε` describe functions generic over any effect set. Effect constraints restrict polymorphism: `f : a -> b / {IO | ε}` requires at least IO effect. We implement effect unification, effect inference for polymorphic functions, and effect instantiation at call sites.

### 6.1.1 Effect Variables
- [ ] **Task 6.1.1 Complete**

Effect variables represent unknown or polymorphic effect sets. Syntax: lowercase Greek letters (ε, δ, φ) in type signatures. Effect variables unify like type variables—two effect variables unify if their effect sets are compatible. We implement effect variable representation, substitution, and occurs checking (preventing infinite effect sets).

- [ ] 6.1.1.1 Implement effect variable representation in type-and-effect terms
- [ ] 6.1.1.2 Implement effect variable substitution for unification and instantiation
- [ ] 6.1.1.3 Implement occurs check preventing infinite effect sets during unification
- [ ] 6.1.1.4 Implement effect variable pretty-printing in type signatures

### 6.1.2 Effect Constraints
- [ ] **Task 6.1.2 Complete**

Effect constraints restrict what effects can instantiate effect variables. Row constraints specify required effects: `{FileIO | ε}` means "at least FileIO". Absence constraints specify forbidden effects: `ε \ {Process}` means "any effects except Process". We implement constraint representation, entailment checking, and constraint simplification.

- [ ] 6.1.2.1 Implement row constraint syntax and representation for required effects
- [ ] 6.1.2.2 Implement absence constraint syntax and representation for forbidden effects
- [ ] 6.1.2.3 Implement constraint entailment checking determining if constraints imply others
- [ ] 6.1.2.4 Implement constraint simplification reducing constraints to canonical form

### 6.1.3 Effect Inference for Polymorphism
- [ ] **Task 6.1.3 Complete**

Extending Algorithm W to infer effect variables requires tracking effect constraints alongside type constraints. When inferring a function's effects, we generate effect variables for unknown effect sets, collect constraints from perform operations and handlers, and solve constraints to find most general effect signatures. Let-polymorphism generalizes effect variables at let bindings.

- [ ] 6.1.3.1 Extend Algorithm W to generate effect variables for polymorphic functions
- [ ] 6.1.3.2 Implement effect constraint generation from perform operations and handlers
- [ ] 6.1.3.3 Implement effect unification unifying effect variables with concrete effect sets
- [ ] 6.1.3.4 Implement effect generalization introducing effect quantifiers at let bindings

### 6.1.4 Effect Instantiation
- [ ] **Task 6.1.4 Complete**

When calling polymorphic effectful functions, we instantiate effect variables with concrete effect sets from the calling context. Effect instantiation substitutes effect variables with actual effects, checks that effect constraints are satisfied, and propagates instantiated effects to caller's effect set. This enables generic effectful code while maintaining effect safety.

- [ ] 6.1.4.1 Implement effect variable instantiation substituting variables with concrete effects
- [ ] 6.1.4.2 Implement constraint satisfaction checking ensuring instantiation respects constraints
- [ ] 6.1.4.3 Implement effect propagation merging instantiated effects into caller's effect set
- [ ] 6.1.4.4 Implement effect inference for higher-order functions preserving polymorphism

### Unit Tests - Section 6.1
- [ ] **Unit Tests 6.1 Complete**
- [ ] Test effect variable unification with various effect sets
- [ ] Test effect constraints (row constraints, absence constraints) with entailment checking
- [ ] Test effect polymorphism inference for generic functions
- [ ] Test effect instantiation at call sites with constraint checking
- [ ] Test interaction between type polymorphism and effect polymorphism

---

## 6.2 Expanded Effect Library
- [ ] **Section 6.2 Complete**

Beyond IO and Process effects from the PoC, we implement a comprehensive standard effect library. State effect provides mutable state without actual mutation. Reader effect supplies read-only environment/configuration. Writer effect accumulates output (logs, traces). Async effect handles asynchronous computation with futures/promises. Error effect provides typed exceptions. Each effect has handlers demonstrating different implementation strategies.

### 6.2.1 State Effect
- [ ] **Task 6.2.1 Complete**

State effect provides get/put operations for mutable state without side effects. Operations: `get : Unit -> s / {State s}`, `put : s -> Unit / {State s}`. State handlers thread state through computation as accumulator. Multiple state types can coexist using type parameters. We implement State effect, pure handler (returns final state), and stateful handler (uses actual mutable reference).

- [ ] 6.2.1.1 Define State effect with get and put operations parameterized by state type
- [ ] 6.2.1.2 Implement pure State handler threading state as accumulator through computation
- [ ] 6.2.1.3 Implement stateful State handler using Erlang process dictionary for performance
- [ ] 6.2.1.4 Implement State effect examples (counter, accumulator, mutable algorithms)

### 6.2.2 Reader Effect
- [ ] **Task 6.2.2 Complete**

Reader effect provides read-only access to environment/configuration. Operation: `ask : Unit -> r / {Reader r}`. Reader handlers supply environment value to all ask operations. Useful for dependency injection, configuration, and implicit parameters. We implement Reader effect, handler providing environment, and local modification (temporary environment changes).

- [ ] 6.2.2.1 Define Reader effect with ask operation parameterized by environment type
- [ ] 6.2.2.2 Implement Reader handler providing environment value to computation
- [ ] 6.2.2.3 Implement local operation temporarily modifying environment for subcomputation
- [ ] 6.2.2.4 Implement Reader effect examples (configuration, dependency injection, context)

### 6.2.3 Writer Effect
- [ ] **Task 6.2.3 Complete**

Writer effect accumulates output values (logs, traces, audits). Operation: `tell : w -> Unit / {Writer w}` where w is a monoid. Writer handlers collect all tell outputs into accumulator. Final result is (value, accumulated_output) pair. We implement Writer effect, handler accumulating with monoid append, and filtering (selective output).

- [ ] 6.2.3.1 Define Writer effect with tell operation parameterized by output monoid type
- [ ] 6.2.3.2 Implement Writer handler accumulating output using monoid append operation
- [ ] 6.2.3.3 Implement listen operation capturing output of subcomputation
- [ ] 6.2.3.4 Implement Writer effect examples (logging, tracing, audit trails)

### 6.2.4 Async Effect
- [ ] **Task 6.2.4 Complete**

Async effect handles asynchronous computation with futures/promises. Operations: `async : (Unit -> a / ε) -> Future a / {Async}`, `await : Future a -> a / {Async}`. Async handlers spawn BEAM processes for concurrent computation, leveraging process-based runtime. Integrates with actor model for structured concurrency.

- [ ] 6.2.4.1 Define Async effect with async and await operations using Future type
- [ ] 6.2.4.2 Implement Async handler spawning BEAM processes for concurrent execution
- [ ] 6.2.4.3 Implement await operation blocking until future completes with timeout support
- [ ] 6.2.4.4 Implement Async effect examples (parallel map, concurrent requests, pipelines)

### 6.2.5 Error Effect
- [ ] **Task 6.2.5 Complete**

Error effect provides typed exceptions. Operation: `throw : e -> a / {Error e}`. Error handlers catch exceptions and handle them. Unlike Result types, errors propagate automatically until handled. We implement Error effect, catch handler converting errors to Result, and error transformation (mapping error types).

- [ ] 6.2.5.1 Define Error effect with throw operation parameterized by error type
- [ ] 6.2.5.2 Implement Error handler catching errors and converting to Result type
- [ ] 6.2.5.3 Implement error propagation automatically unwinding until handler found
- [ ] 6.2.5.4 Implement Error effect examples (validation, parsing, error handling)

### Unit Tests - Section 6.2
- [ ] **Unit Tests 6.2 Complete**
- [ ] Test State effect with get/put operations and pure handler
- [ ] Test Reader effect with ask operation and local modification
- [ ] Test Writer effect with tell operation and output accumulation
- [ ] Test Async effect with async/await and concurrent execution
- [ ] Test Error effect with throw/catch and error propagation
- [ ] Test effect composition combining multiple effects in same function

---

## 6.3 Effect Optimizations
- [ ] **Section 6.3 Complete**

Effect system overhead must be minimal for production use. We implement optimizations reducing runtime cost: effect fusion combines multiple effect operations into single operation, handler inlining eliminates overhead for simple handlers, static resolution dispatches effects at compile time when possible, and effect specialization generates optimized code for monomorphic effect use. Goal: effect overhead under 5% compared to hand-written BEAM code.

### 6.3.1 Effect Fusion
- [ ] **Task 6.3.1 Complete**

Effect fusion combines consecutive effect operations into single operation, reducing message passing overhead. Example: `get >>= \s -> put (s+1)` fuses to single modify operation. We implement fusion rules for common patterns, fusion analysis detecting fusible sequences, and fused code generation combining operations.

- [ ] 6.3.1.1 Implement fusion rules for State effect (get-put fusion, consecutive puts)
- [ ] 6.3.1.2 Implement fusion analysis detecting fusible effect operation sequences
- [ ] 6.3.1.3 Implement fused operation generation combining multiple operations
- [ ] 6.3.1.4 Implement fusion correctness preservation ensuring semantics unchanged

### 6.3.2 Handler Inlining
- [ ] **Task 6.3.2 Complete**

Simple handlers can be inlined at effect sites, eliminating process spawning and message passing. Pure handlers (no actual effects) inline completely, removing all effect overhead. We implement inlining analysis determining inlinable handlers, inline transformation replacing effect operations with handler bodies, and specialization generating optimal code for inlined handlers.

- [ ] 6.3.2.1 Implement inlining analysis identifying handlers safe to inline
- [ ] 6.3.2.2 Implement handler body inlining substituting effect operations with handler code
- [ ] 6.3.2.3 Implement specialization generating optimized code for inlined handlers
- [ ] 6.3.2.4 Implement inlining heuristics balancing code size vs performance

### 6.3.3 Static Effect Resolution
- [ ] **Task 6.3.3 Complete**

When effect handlers are statically known (not passed as parameters), we can resolve effect operations at compile time. Static resolution generates direct function calls instead of dynamic dispatch, eliminating effect runtime overhead. We implement static handler analysis, compile-time effect dispatch, and devirtualization transforming dynamic effects to static calls.

- [ ] 6.3.3.1 Implement static handler analysis determining when handlers are compile-time constants
- [ ] 6.3.3.2 Implement static effect dispatch generating direct calls instead of message passing
- [ ] 6.3.3.3 Implement devirtualization transforming effect operations to direct function calls
- [ ] 6.3.3.4 Implement escape analysis detecting when effects don't escape handler scope

### 6.3.4 Effect Specialization
- [ ] **Task 6.3.4 Complete**

Polymorphic effectful functions can be specialized for monomorphic use cases, generating optimized code without effect polymorphism overhead. Specialization duplicates function bodies for each concrete effect set, performs effect-specific optimizations, and generates optimized BEAM code. Balances code size against performance.

- [ ] 6.3.4.1 Implement specialization analysis identifying profitable specialization opportunities
- [ ] 6.3.4.2 Implement function body duplication for each monomorphic effect instantiation
- [ ] 6.3.4.3 Implement effect-specific optimization passes for specialized functions
- [ ] 6.3.4.4 Implement specialization heuristics avoiding code bloat from over-specialization

### Unit Tests - Section 6.3
- [ ] **Unit Tests 6.3 Complete**
- [ ] Test effect fusion combining operations with preserved semantics
- [ ] Test handler inlining eliminating effect overhead for simple cases
- [ ] Test static effect resolution generating direct calls
- [ ] Test effect specialization for monomorphic uses
- [ ] Benchmark optimization impact comparing optimized vs unoptimized effect code

---

## 6.4 Advanced Effect Features
- [ ] **Section 6.4 Complete**

Advanced features enable sophisticated effect patterns: delimited continuations provide resumption control, scoped effects restrict effect visibility, effectful guards allow effects in pattern guards, and effect debugging provides runtime introspection. These features demonstrate the full power of algebraic effects while maintaining implementation feasibility on BEAM.

### 6.4.1 Delimited Continuations
- [ ] **Task 6.4.1 Complete**

Effect handlers have access to continuation—the rest of computation after perform. Handlers can resume continuation zero, one, or multiple times. This enables control flow effects: early return, backtracking, exceptions. We implement continuation capture, continuation invocation, and multi-resume handlers (resuming continuation multiple times).

- [ ] 6.4.1.1 Implement continuation capture at perform sites using BEAM exceptions or CPS
- [ ] 6.4.1.2 Implement continuation resume allowing handlers to continue computation
- [ ] 6.4.1.3 Implement multi-resume handlers supporting multiple continuation invocations
- [ ] 6.4.1.4 Implement continuation examples (generators, backtracking, coroutines)

### 6.4.2 Scoped Effects
- [ ] **Task 6.4.2 Complete**

Effect scoping restricts where effects can be performed. Scoped effects require handler in same syntactic scope, preventing effects from escaping. Useful for resource management: files opened within scope must be closed before scope exits. We implement scope checking, scope-based effect resolution, and scope violation detection.

- [ ] 6.4.2.1 Implement scoped effect declaration syntax marking effects as scoped
- [ ] 6.4.2.2 Implement scope checking preventing scoped effects from escaping handlers
- [ ] 6.4.2.3 Implement resource management patterns using scoped effects (files, locks)
- [ ] 6.4.2.4 Implement scope violation errors with clear messages and suggestions

### 6.4.3 Effectful Guards
- [ ] **Task 6.4.3 Complete**

Extending pattern guards to allow effects enables rich matching conditions. Effectful guards can perform IO, access state, or throw errors during pattern matching. Deferred from Phase 3, this feature requires careful semantics: guard evaluation order, effect handling during matching, and short-circuit evaluation.

- [ ] 6.4.3.1 Extend guard syntax to allow perform operations in guard expressions
- [ ] 6.4.3.2 Implement guard effect checking tracking effects in guard expressions
- [ ] 6.4.3.3 Implement guard evaluation with effect handling during pattern matching
- [ ] 6.4.3.4 Implement guard examples (database lookups, validation, complex conditions)

### 6.4.4 Effect Debugging Tools
- [ ] **Task 6.4.4 Complete**

Effect debugging provides runtime introspection of effect execution. Effect traces show all performed operations and handler invocations. Effect profiling measures effect overhead per effect type. Effect visualization displays effect flow through program. We implement trace collection, profiling instrumentation, and debugging output formatting.

- [ ] 6.4.4.1 Implement effect tracing recording all perform operations and handler calls
- [ ] 6.4.4.2 Implement effect profiling measuring time and message counts per effect
- [ ] 6.4.4.3 Implement effect visualization generating flow diagrams of effect execution
- [ ] 6.4.4.4 Implement debugging commands in REPL for effect introspection

### Unit Tests - Section 6.4
- [ ] **Unit Tests 6.4 Complete**
- [ ] Test delimited continuations with single and multiple resumes
- [ ] Test scoped effects preventing escape and enabling resource safety
- [ ] Test effectful guards in pattern matching with various effects
- [ ] Test effect debugging tools producing correct traces and profiles

---

## 6.5 Integration Tests
- [ ] **Section 6.5 Complete**

Integration tests validate the complete advanced effect system with realistic programs using all features together. We test effect polymorphism in libraries, effect composition combining multiple effects, effect optimizations improving performance measurably, and advanced features in production-like scenarios.

### 6.5.1 Polymorphic Effect Libraries
- [ ] **Task 6.5.1 Complete**

We build libraries using effect polymorphism, demonstrating generic effectful code. Examples: polymorphic map working with any effect, transactional abstractions generic over state effects, logging libraries generic over writer effects. Tests verify libraries work with concrete effect instantiations.

- [ ] 6.5.1.1 Implement polymorphic map function generic over effect sets
- [ ] 6.5.1.2 Implement transaction library using polymorphic State effect
- [ ] 6.5.1.3 Implement logging library using polymorphic Writer effect
- [ ] 6.5.1.4 Test libraries with various concrete effect instantiations

### 6.5.2 Multi-Effect Programs
- [ ] **Task 6.5.2 Complete**

Programs combining multiple effects test effect composition. Example: web server using IO (HTTP), State (session), Reader (config), Error (validation), and Async (concurrency). Tests verify effects compose correctly, handlers nest properly, and effect sets track accurately through complex programs.

- [ ] 6.5.2.1 Implement web server using IO, State, Reader, Error, and Async effects
- [ ] 6.5.2.2 Implement data pipeline using multiple effects for ETL operations
- [ ] 6.5.2.3 Implement game server using effects for game state, networking, and actors
- [ ] 6.5.2.4 Test effect composition correctness and handler nesting

### 6.5.3 Optimization Benchmarks
- [ ] **Task 6.5.3 Complete**

Performance benchmarks validate that optimizations achieve target <5% overhead. We compare optimized effect code against unoptimized effect code and hand-written BEAM code. Measure throughput, latency, and memory usage across effect types. Profile to identify remaining optimization opportunities.

- [ ] 6.5.3.1 Benchmark State effect performance with and without optimizations
- [ ] 6.5.3.2 Benchmark effect polymorphism overhead comparing monomorphic vs polymorphic code
- [ ] 6.5.3.3 Benchmark handler inlining measuring overhead elimination
- [ ] 6.5.3.4 Compare optimized effect code to equivalent hand-written BEAM code

### 6.5.4 Advanced Feature Integration
- [ ] **Task 6.5.4 Complete**

Tests using advanced features demonstrate full effect system capability. Implement generators using delimited continuations, resource management using scoped effects, complex pattern matching using effectful guards. Validate debugging tools work correctly on complex programs.

- [ ] 6.5.4.1 Implement generators using delimited continuations and multi-resume
- [ ] 6.5.4.2 Implement resource-safe file handling using scoped effects
- [ ] 6.5.4.3 Implement complex pattern matching using effectful guards
- [ ] 6.5.4.4 Test debugging tools on multi-effect programs producing useful output

---

## Success Criteria

1. **Effect Polymorphism**: Generic effectful functions with effect variables and constraints
2. **Effect Library**: Complete standard effect library (State, Reader, Writer, Async, Error)
3. **Optimizations**: Effect overhead under 5% compared to hand-written BEAM code
4. **Advanced Features**: Working delimited continuations, scoped effects, and effectful guards
5. **Debugging Tools**: Effect tracing, profiling, and visualization for development
6. **Production Ready**: Effect system suitable for building real applications
7. **Test Coverage**: 85% coverage with comprehensive tests for all advanced features

## Provides Foundation

This phase completes the effect system, enabling:
- **Production Applications**: Full-featured effect system for real-world development
- **Effect Libraries**: Third-party effect libraries leveraging polymorphism
- **Research Extensions**: Foundation for effect system research (effect inference, effect types)
- **BEAM Innovation**: Demonstrating algebraic effects on production VM
- **Future Work**: Effect system evolution and optimization continued development

## Key Outputs

- Effect polymorphism with effect variables and constraints
- Effect unification and inference for polymorphic functions
- Complete standard effect library (State, Reader, Writer, Async, Error)
- Effect optimizations (fusion, inlining, static resolution, specialization)
- Delimited continuations for control flow effects
- Scoped effects for resource safety
- Effectful pattern guards for rich matching
- Effect debugging and profiling tools
- Comprehensive test suite covering all advanced features
- Performance benchmarks demonstrating optimization effectiveness
- Documentation of advanced effect patterns and best practices
- Demonstration applications showcasing full effect system capabilities
