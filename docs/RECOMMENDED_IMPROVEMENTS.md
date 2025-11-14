# Recommended Improvements for Type System Implementation

This document outlines recommended improvements for the Topos type system implementation, organized by priority and category.

## Table of Contents

1. [High Priority Improvements](#high-priority-improvements)
2. [Medium Priority Improvements](#medium-priority-improvements)
3. [Low Priority Improvements](#low-priority-improvements)
4. [Future Enhancements](#future-enhancements)
5. [Technical Debt](#technical-debt)
6. [Performance Optimizations](#performance-optimizations)
7. [Developer Experience](#developer-experience)

---

## High Priority Improvements

### 1. Type Inference Engine (Task 1.3)

**Current State:** Type representation complete, but no inference engine

**Recommendation:** Implement Hindley-Milner type inference (Algorithm W)

**Scope:**
- Implement constraint generation from expressions
- Implement unification algorithm
- Implement constraint solving
- Handle effect inference
- Support row polymorphism constraints

**Benefits:**
- Enable actual type checking of programs
- Automatic type inference for expressions
- Foundation for compiler pipeline

**Estimated Effort:** 3-4 weeks

**Files to Create:**
- `src/compiler/types/topos_type_infer.erl` - Main inference engine
- `src/compiler/types/topos_type_unify.erl` - Unification algorithm
- `src/compiler/types/topos_type_constraint.erl` - Constraint generation
- `test/compiler/types/topos_type_infer_tests.erl` - Inference tests

**Priority Rationale:** Blocking all downstream compiler work

---

### 2. Improved Error Messages

**Current State:** Basic error tuples with format_error/1

**Recommendation:** Rich, context-aware error messages with source locations

**Improvements:**
1. **Source location tracking**
   - Add line/column information to all types
   - Track origin of type variables
   - Show where types were inferred

2. **Error context**
   - Show surrounding code
   - Highlight relevant expressions
   - Point to exact error location

3. **Suggestions**
   - "Did you mean...?" for typos
   - Suggest fixes for common errors
   - Show similar valid alternatives

4. **Multi-error reporting**
   - Collect multiple errors before stopping
   - Show all errors in expression, not just first

**Example Current Error:**
```
{unification_failure, {tcon, integer}, {tcon, string}}
```

**Example Improved Error:**
```
Type mismatch in function call at line 42, column 15:

    result = concat(5, "hello")
                    ^

Expected: String
Found:    Int

Note: Function 'concat' expects two strings:
    concat : String -> String -> String

Suggestion: Did you mean to convert the integer to a string?
    result = concat(int_to_string(5), "hello")
```

**Files to Modify:**
- `src/compiler/types/topos_type_error.erl` - Enhanced error formatting
- `src/compiler/types/topos_types.erl` - Add source location metadata
- `test/compiler/types/topos_type_error_tests.erl` - Test error messages

**Estimated Effort:** 1-2 weeks

**Priority Rationale:** Critical for developer experience and debugging

---

### 3. Exhaustiveness Checking for Pattern Matching

**Current State:** No pattern matching implementation yet

**Recommendation:** Implement exhaustiveness and redundancy checking

**Scope:**
1. **Exhaustiveness checking**
   - Detect missing patterns
   - Show concrete example of uncovered case
   - Use decision tree analysis

2. **Redundancy detection**
   - Detect unreachable patterns
   - Warn about subsumed patterns
   - Help developers clean up code

3. **Pattern subsumption**
   - Determine if pattern A covers pattern B
   - Handle nested patterns correctly
   - Support guards in analysis

**Example:**
```topos
-- Incomplete pattern match
flow handle_maybe : Maybe a -> a
flow handle_maybe = match
  | Some x -> x
  -- ERROR: Missing pattern: None
end

-- Redundant pattern
flow handle_maybe : Maybe a -> a
flow handle_maybe = match
  | Some x -> x
  | None -> error "none"
  | None -> error "unreachable"  -- WARNING: Unreachable pattern
end
```

**Files to Create:**
- `src/compiler/patterns/topos_pattern_check.erl` - Exhaustiveness checking
- `test/compiler/patterns/topos_pattern_check_tests.erl` - Pattern tests

**Estimated Effort:** 2-3 weeks

**Priority Rationale:** Essential for safety guarantees of language

---

### 4. Module System Implementation

**Current State:** No module system yet

**Recommendation:** Implement ML-style module system (Phase 4 of planning)

**Scope:**
- Module definition and exports
- Signature-based abstraction
- Parameterized modules (functors)
- Module type checking
- Compilation to BEAM modules

**Benefits:**
- Code organization
- Namespace management
- Separate compilation
- Abstraction and encapsulation

**Reference:** `notes/research/1.02-ml-module-system/`

**Estimated Effort:** 3-4 weeks

**Priority Rationale:** Required for any non-trivial program

---

## Medium Priority Improvements

### 5. Performance Profiling and Optimization

**Current State:** Basic stress tests but no profiling

**Recommendation:** Add comprehensive performance monitoring

**Tools:**
1. **Built-in profiling**
   - Use Erlang's `eprof` for function-level profiling
   - Use `fprof` for detailed call graphs
   - Use `percept2` for concurrent profiling

2. **Benchmarking suite**
   - Create benchmark suite with representative workloads
   - Track performance over time
   - Catch performance regressions

3. **Optimization targets**
   - Substitution composition (hot path)
   - Effect set operations (frequent)
   - Type variable extraction (recursive)
   - Pretty printing (user-facing)

**Example Benchmark:**
```erlang
bench_substitution_composition() ->
    S1 = create_substitution(100),  % 100 variables
    S2 = create_substitution(100),

    % Time composition
    {Time, _Result} = timer:tc(fun() ->
        topos_type_subst:compose(S2, S1)
    end),

    % Assert performance requirement
    ?assert(Time < 1000),  % Must complete in < 1ms

    io:format("Composition of 100-var substs: ~p μs~n", [Time]).
```

**Files to Create:**
- `benchmark/type_system_bench.erl` - Benchmark suite
- `scripts/profile.sh` - Profiling script

**Estimated Effort:** 1 week

**Priority Rationale:** Ensure acceptable performance before optimizing

---

### 6. Substitution Optimization

**Current State:** Naive recursive substitution

**Recommendation:** Optimize hot paths in substitution

**Optimizations:**
1. **Memoization**
   - Cache substitution results
   - Avoid recomputing same substitution
   - Use ETS table for cache

2. **Structure sharing**
   - Reuse unchanged type subtrees
   - Only rebuild modified parts
   - Reduces allocation

3. **Composition optimization**
   - Special case for empty substitution
   - Special case for single-variable substitution
   - Avoid unnecessary map operations

4. **Early termination**
   - If type has no type variables, skip substitution
   - Check domain/range overlap before composing

**Example:**
```erlang
% Before: Always traverse
apply(Subst, Type) ->
    apply_with_context(Subst, Type, 0, sets:new()).

% After: Skip if no variables
apply(Subst, Type) ->
    case has_type_vars(Type) of
        false -> Type;  % Early return
        true -> apply_with_context(Subst, Type, 0, sets:new())
    end.
```

**Files to Modify:**
- `src/compiler/types/topos_type_subst.erl`

**Estimated Effort:** 1-2 weeks

**Priority Rationale:** Substitution is on critical path of type inference

---

### 7. Effect System Extensions

**Current State:** Basic effect sets with union and normalization

**Recommendation:** Extend effect system capabilities

**Extensions:**
1. **Effect polymorphism**
   - Effect variables: `ε₁`, `ε₂`
   - Polymorphic effects: `∀ε. a -> {ε} b`
   - Effect constraints: `ε₁ ⊆ ε₂`

2. **Effect rows**
   - Row polymorphism for effects: `{io, state | ε}`
   - Open/closed effect sets
   - Effect extension and restriction

3. **Effect handlers**
   - Define effect handlers in type system
   - Track handler scope
   - Effect elimination through handlers

4. **Effect inference**
   - Infer minimum effect set
   - Propagate effects through composition
   - Detect pure functions automatically

**Example:**
```topos
-- Effect polymorphic function
flow with_logging : ∀ε a b. (a -> {ε} b) -> a -> {io, ε} b
flow with_logging f x =
  print("Calling function...")
  result = f x
  print("Function returned")
  result
end

-- Effect inference
flow pure_add : Int -> Int -> {} Int  -- Inferred as pure
flow pure_add x y = x + y

flow impure_add : Int -> Int -> {state} Int  -- Inferred with state effect
flow impure_add x y =
  counter.increment()  -- Has state effect
  x + y
```

**Files to Modify:**
- `src/compiler/types/topos_types.erl` - Effect variable support
- `src/compiler/types/topos_type_infer.erl` - Effect inference
- `test/compiler/types/topos_type_effect_tests.erl` - Effect tests

**Estimated Effort:** 2-3 weeks

**Priority Rationale:** Enables more precise effect tracking

---

### 8. Type Class/Trait System

**Current State:** No type classes yet

**Recommendation:** Implement type class system for ad-hoc polymorphism

**Scope:**
1. **Type class definition**
   - Define traits (Topos term for type classes)
   - Trait methods and signatures
   - Trait laws (documentation)

2. **Instance resolution**
   - Define instances for types
   - Automatic instance lookup
   - Coherence checking (no overlapping instances)

3. **Trait constraints**
   - Constrained polymorphism: `∀a. Eq a ⇒ a -> a -> Bool`
   - Constraint solving during inference
   - Multiple constraints

4. **Standard traits**
   - Mappable (Functor)
   - Chainable (Monad)
   - Eq, Ord
   - Show (pretty-printable)

**Example:**
```topos
-- Define trait
trait Mappable f where
  map : ∀a b. (a -> b) -> f a -> f b
end

-- Define instance
instance Mappable Maybe where
  map f = match
    | Some x -> Some (f x)
    | None -> None
  end
end

-- Use with constraint
flow twice : ∀f a. Mappable f ⇒ (a -> a) -> f a -> f a
flow twice f x = map f (map f x)
```

**Files to Create:**
- `src/compiler/types/topos_type_class.erl` - Type class system
- `src/compiler/types/topos_type_instance.erl` - Instance resolution
- `test/compiler/types/topos_type_class_tests.erl` - Class tests

**Reference:** `notes/research/1.01-original-idea/` (category theory section)

**Estimated Effort:** 3-4 weeks

**Priority Rationale:** Core feature for practical functional programming

---

## Low Priority Improvements

### 9. Type Annotations in Error Messages

**Current State:** Error messages show internal representation

**Recommendation:** Pretty-print types in all error messages

**Improvements:**
- Always use `topos_type_pp:format/1` for types in errors
- Show effect sets in readable form
- Abbreviate long types with `...`
- Use syntax highlighting if terminal supports it

**Example:**
```
Before: {unification_failure, {tcon, integer}, {tapp, {tcon, 'List'}, [{tcon, integer}]}}

After:  Cannot unify Int with List<Int>
```

**Files to Modify:**
- `src/compiler/types/topos_type_error.erl`

**Estimated Effort:** 2-3 days

---

### 10. Documentation Comments

**Current State:** Module-level documentation only

**Recommendation:** Add comprehensive inline documentation

**Standards:**
- EDoc format for all exported functions
- Examples in documentation
- Complexity notes for algorithms
- Link to relevant research documents

**Example:**
```erlang
%% @doc Apply a substitution to a type.
%%
%% Applies the given substitution to all type variables in the type,
%% recursively traversing the type structure. Includes occurs check
%% to prevent creation of infinite types and depth tracking to prevent
%% stack overflow.
%%
%% @param Subst The substitution to apply (maps type var IDs to types)
%% @param Type The type to apply the substitution to
%% @returns The type with substitution applied
%% @throws {circular_substitution, VarId} if occurs check fails
%% @throws {substitution_depth_exceeded, N, Max} if depth limit exceeded
%%
%% Complexity: O(n) where n is the size of the type
%%
%% Examples:
%%   apply(#{1 => {tcon, integer}}, {tvar, 1}) → {tcon, integer}
%%   apply(#{}, {tvar, 1}) → {tvar, 1}
%%
%% See also: topos_type_subst:compose/2, topos_type_subst:occurs_check/2
-spec apply(subst(), topos_types:ty()) -> topos_types:ty().
apply(Subst, Type) ->
    apply_with_context(Subst, Type, 0, sets:new()).
```

**Files to Modify:** All `.erl` files in `src/compiler/types/`

**Estimated Effort:** 3-4 days

---

### 11. QuickCheck Integration

**Current State:** PropEr for property-based testing

**Recommendation:** Also support QuickCheck for commercial use

**Rationale:**
- QuickCheck has commercial version with better shrinking
- Better for mission-critical systems
- Paid support available
- More mature than PropEr

**Scope:**
- Parallel implementations of properties
- Conditional compilation for PropEr vs QuickCheck
- Document both options

**Files to Modify:**
- `test/compiler/types/topos_type_properties.erl`
- `rebar.config`

**Estimated Effort:** 2-3 days

---

### 12. Type System Visualization

**Current State:** Text-only type representation

**Recommendation:** Add graphical type visualization

**Tools:**
1. **GraphViz export**
   - Export types as DOT graphs
   - Show type structure visually
   - Highlight type variables and constraints

2. **HTML reports**
   - Generate HTML documentation
   - Interactive type explorer
   - Click to navigate type structure

3. **REPL integration**
   - Show type trees in REPL
   - Visualize substitution steps
   - Debug type inference graphically

**Example:**
```
Type: ∀a. a -> List<a> -> List<a>

Graph visualization:
    [∀a]
      |
   [tfun]
   /  |  \
[a] [->] [tfun]
         /  |  \
   [List<a>] [->] [List<a>]
```

**Files to Create:**
- `src/compiler/types/topos_type_viz.erl` - Visualization
- `scripts/type_graph.sh` - Generate visualizations

**Estimated Effort:** 1 week

---

## Future Enhancements

### 13. Higher-Kinded Types

**Current State:** First-order type system

**Recommendation:** Add support for higher-kinded types (kinds)

**Motivation:**
- Express Functor, Monad as type classes properly
- More powerful abstractions
- Better code reuse

**Example:**
```topos
-- Higher-kinded type parameter
trait Functor (f : * -> *) where
  map : ∀a b. (a -> b) -> f a -> f b
end

-- f has kind * -> *
instance Functor Maybe where
  map f = match
    | Some x -> Some (f x)
    | None -> None
  end
end
```

**Complexity:** High - requires kind inference and checking

**Estimated Effort:** 4-6 weeks

**Reference:** `notes/research/1.15-advanced-type-system/` (higher-kinded types section)

---

### 14. Dependent Types (Research)

**Current State:** No dependent types

**Recommendation:** Research feasibility of lightweight dependent types

**Motivation:**
- Encode more invariants in types
- Prove program properties at compile time
- Length-indexed vectors, non-empty lists, etc.

**Example:**
```topos
-- Vector indexed by length
shape Vec (n : Nat) a where
  Nil : Vec 0 a
  Cons : ∀n. a -> Vec n a -> Vec (n+1) a
end

-- Type-safe head (can't call on empty vector)
flow head : ∀n a. Vec (n+1) a -> a
flow head = match
  | Cons x _ -> x
  -- No Nil case needed - type system knows n ≥ 1
end
```

**Complexity:** Very high - changes fundamental type system

**Estimated Effort:** 3-6 months (research + implementation)

**Reference:** Idris, Agda, Coq for inspiration

---

### 15. Linear Types for Resource Management

**Current State:** No linear types

**Recommendation:** Research linear types for resource safety

**Motivation:**
- Guarantee resources are used exactly once
- No resource leaks
- File handles, database connections, etc.

**Example:**
```topos
-- Linear type (must be used exactly once)
shape File : Linear where
  Open : String -> {io} File
end

-- Must consume file exactly once
flow read_close : File -> {io} String
flow read_close file =
  content = File.read(file)  -- Consumes file
  File.close(file)           -- ERROR: file already consumed!
  content
end
```

**Complexity:** High - requires linear type checking

**Estimated Effort:** 2-3 months

**Reference:** Rust's ownership system, Linear Haskell

---

### 16. Gradual Typing

**Current State:** Static typing only

**Recommendation:** Research gradual typing for interop with dynamic BEAM code

**Motivation:**
- Smooth interop with Erlang/Elixir
- Gradually migrate dynamically-typed code
- Type holes during development

**Example:**
```topos
-- Dynamic type for Erlang interop
flow call_erlang : String -> Dynamic -> {io} Dynamic
flow call_erlang module_name arg =
  erlang:call(module_name, "function", [arg])
end

-- Cast from dynamic
flow safe_cast : Dynamic -> Maybe Int
flow safe_cast x =
  if is_integer(x) then Some x else None
end
```

**Complexity:** Medium-high

**Estimated Effort:** 6-8 weeks

**Reference:** TypeScript, Typed Racket

---

## Technical Debt

### 17. Refactor Type Representation

**Current State:** Types are bare tuples

**Recommendation:** Consider more structured representation

**Options:**
1. **Records for types**
   - Better field access
   - Clearer code
   - Slightly more verbose

2. **Maps for types**
   - More flexible
   - Easy to extend
   - Pattern matching less clean

3. **Keep tuples but add helpers**
   - Least invasive
   - Add accessor functions
   - Hide representation

**Recommendation:** Option 3 (add helpers) for now, consider records later

**Example:**
```erlang
% Current
{tapp, Con, Args} = Type,

% With helpers
Con = type_app_constructor(Type),
Args = type_app_args(Type),

% Or with records
#tapp{constructor = Con, args = Args} = Type,
```

**Files to Modify:**
- `src/compiler/types/topos_types.erl`

**Estimated Effort:** 2-3 days

---

### 18. Consolidate Test Utilities

**Current State:** Helper functions duplicated across test modules

**Recommendation:** Extract common test utilities

**Utilities to Extract:**
- Type construction helpers
- Assertion helpers
- Test data generators
- Pretty-printing for test output

**Files to Create:**
- `test/compiler/types/topos_test_utils.erl` - Shared utilities

**Files to Modify:** All test modules

**Estimated Effort:** 2-3 days

---

### 19. Configuration System

**Current State:** Hardcoded constants (MAX_SUBST_DEPTH, etc.)

**Recommendation:** Centralized configuration

**Configuration Options:**
- Max substitution depth
- Max type size
- Number of property tests
- Pretty-printing options
- Debug output verbosity

**Files to Create:**
- `src/compiler/topos_config.erl` - Configuration management
- `config/topos.config` - Configuration file

**Estimated Effort:** 2-3 days

---

## Performance Optimizations

### 20. Persistent Data Structures

**Current State:** Standard Erlang data structures

**Recommendation:** Use optimized persistent data structures where beneficial

**Targets:**
1. **Type variable sets** - Consider `gb_sets` instead of `sets`
2. **Substitution maps** - Already using maps (good)
3. **Effect sets** - Consider specialized structure for set operations

**Benchmark First:** Only optimize if profiling shows bottleneck

**Estimated Effort:** 1 week (with benchmarking)

---

### 21. Parallel Type Checking

**Current State:** Sequential type checking

**Recommendation:** Parallelize independent type checking tasks

**Opportunities:**
- Check multiple modules in parallel
- Check multiple function definitions in parallel
- Use BEAM's concurrency naturally

**Example:**
```erlang
% Type check multiple modules in parallel
check_modules(Modules) ->
    Parent = self(),
    Pids = [spawn(fun() ->
        Result = check_module(Mod),
        Parent ! {result, Mod, Result}
    end) || Mod <- Modules],
    collect_results(Pids, #{}).
```

**Estimated Effort:** 1-2 weeks

---

### 22. Incremental Type Checking

**Current State:** Full recheck on every change

**Recommendation:** Only recheck affected definitions

**Approach:**
- Track dependencies between definitions
- Recheck only changed definitions and dependents
- Cache type checking results

**Benefit:** Much faster iteration during development

**Complexity:** Medium-high (dependency tracking)

**Estimated Effort:** 3-4 weeks

---

## Developer Experience

### 23. Type Holes

**Current State:** No type hole support

**Recommendation:** Add type holes for incremental development

**Usage:**
```topos
-- Type hole: compiler tells you what type is needed
flow complex_function : Int -> String
flow complex_function x =
  intermediate = some_computation(x)
  _ = ?hole  -- What type should go here?
  final_result
end

-- Compiler output:
-- Type hole at line 4: Expected type String, context available:
--   x : Int
--   intermediate : List<Int>
```

**Benefit:** Helps developers understand types during development

**Files to Modify:**
- Type inference engine (when implemented)
- Error reporting

**Estimated Effort:** 1-2 weeks

---

### 24. Language Server Protocol (LSP)

**Current State:** No editor integration

**Recommendation:** Implement LSP server for IDE support

**Features:**
- Type on hover
- Go to definition
- Find references
- Auto-completion
- Inline error messages
- Refactoring support

**Benefit:** Professional IDE experience

**Estimated Effort:** 4-6 weeks

**Priority:** After basic compiler is working

---

### 25. REPL with Type Information

**Current State:** No REPL yet

**Recommendation:** REPL shows inferred types

**Features:**
```
topos> x = 42
x : Int

topos> f = \y -> y + 1
f : Int -> {} Int

topos> :type map
map : ∀a b f. Mappable f ⇒ (a -> b) -> f a -> f b

topos> :type-of some_complex_expr
List<Int -> String>
```

**Benefit:** Interactive exploration and learning

**Estimated Effort:** 2-3 weeks (requires REPL implementation)

---

### 26. Compiler Debug Mode

**Current State:** Limited debugging output

**Recommendation:** Comprehensive debug mode

**Debug Information:**
- Show type inference steps
- Show constraint generation
- Show constraint solving
- Show substitutions applied
- Export debug logs

**Usage:**
```bash
topos compile --debug=infer file.tps   # Show inference steps
topos compile --debug=all file.tps      # Show everything
topos compile --debug-log=out.log file.tps  # Save to file
```

**Benefit:** Essential for debugging compiler and understanding type errors

**Files to Create:**
- `src/compiler/topos_debug.erl` - Debug infrastructure

**Estimated Effort:** 1 week

---

## Implementation Roadmap

### Phase 1: Critical Path (Weeks 1-6)
1. Type Inference Engine (High Priority #1)
2. Improved Error Messages (High Priority #2)
3. Module System Implementation (High Priority #4)

### Phase 2: Core Features (Weeks 7-12)
4. Exhaustiveness Checking (High Priority #3)
5. Effect System Extensions (Medium Priority #7)
6. Type Class System (Medium Priority #8)

### Phase 3: Quality & Performance (Weeks 13-16)
7. Performance Profiling (Medium Priority #5)
8. Substitution Optimization (Medium Priority #6)
9. Documentation Comments (Low Priority #10)
10. Consolidate Test Utilities (Technical Debt #18)

### Phase 4: Developer Experience (Weeks 17-20)
11. Type Holes (DX #23)
12. Compiler Debug Mode (DX #26)
13. REPL with Type Information (DX #25)

### Phase 5: Advanced Features (Future)
14. Higher-Kinded Types (Future #13)
15. Language Server Protocol (DX #24)
16. Gradual Typing (Future #16)

### Phase 6: Research (Long-term)
17. Dependent Types (Future #14)
18. Linear Types (Future #15)

---

## Prioritization Criteria

When deciding what to implement next, consider:

1. **Blocking:** Does it block other work?
2. **Impact:** How many users/use cases does it affect?
3. **Effort:** How long will it take?
4. **Risk:** How likely are we to encounter problems?
5. **Learning:** What will we learn from it?

**Formula:** Priority = (Blocking × 3 + Impact × 2) / (Effort × Risk)

---

## Contributing

When implementing improvements:

1. **Read relevant research documents first**
2. **Create design document for complex features**
3. **Add tests before implementation (TDD)**
4. **Update this document when completed**
5. **Add property tests for mathematical properties**
6. **Document in code with examples**

---

## Feedback and Updates

This document should be:
- Reviewed quarterly
- Updated when priorities change
- Referenced in planning documents
- Linked from task documentation

**Last Updated:** 2024-11-14 (Initial version for Task 1.2.1)
**Next Review:** After Task 1.3 (Type Inference) completion

---

## References

- Research documents: `notes/research/`
- Planning documents: `notes/planning/proof-of-concept/`
- Property testing: `docs/PROPERTY_TESTING.md`
- Testing scenarios: `docs/TESTING_SCENARIOS.md`
- Code review: `notes/planning/proof-of-concept/task-1.2.1-review.md`
