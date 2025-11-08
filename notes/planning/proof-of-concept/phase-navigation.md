# Topos Proof-of-Concept - Phase Navigation

## All Phases

### [Phase 1: Core Language Infrastructure](phase-01.md) (Weeks 1-3)
**Foundation of the compiler**

Establishes the complete compilation pipeline from source code to BEAM bytecode. Implements lexer/parser for Topos syntax, Hindley-Milner type inference with categorical extensions, and Core Erlang code generation. This phase provides the essential infrastructure that all subsequent phases build upon.

**Key Deliverables:**
- Lexer and parser producing ASTs
- Type inference engine with Algorithm W
- Core Erlang code generator
- Basic pattern compilation
- Error reporting infrastructure

---

### [Phase 2: REPL and Basic Runtime](phase-02.md) (Weeks 4-5)
**Interactive development and standard library**

Creates an interactive REPL for rapid prototyping and establishes the standard prelude with foundational types and functions. Developers can evaluate expressions, inspect types, and load modules interactively. The prelude provides essential category-theoretic abstractions (Functor, Monad) and core data structures (List, Maybe, Result).

**Key Deliverables:**
- Working REPL with expression evaluation
- Command system (:type, :load, :browse)
- Pretty printing and syntax highlighting
- Standard prelude with core types
- Functor and Monad traits with instances
- Essential list operations

---

### [Phase 3: Pattern Matching Engine](phase-03.md) (Weeks 6-8)
**Advanced pattern matching**

Implements sophisticated pattern matching with guards, or-patterns, nested patterns, and as-patterns. The pattern compiler generates efficient decision trees, performs exhaustiveness checking, and detects redundant patterns. This phase brings Topos's pattern matching to ML-family language standards.

**Key Deliverables:**
- Guards, or-patterns, nested patterns, as-patterns
- Decision tree compilation
- Exhaustiveness checking with examples
- Redundancy detection
- Optimized code generation
- SMT integration for guard analysis

---

### [Phase 4: Module System](phase-04.md) (Weeks 9-10)
**Code organization and namespaces**

Establishes hierarchical module organization with visibility control, qualified and selective imports, and separate compilation. Modules compile to BEAM modules with explicit exports. The system supports incremental builds, detects circular dependencies, and enables interoperability with Erlang code.

**Key Deliverables:**
- Module declaration and export system
- Qualified and selective imports
- Name resolution with shadowing
- Circular dependency detection
- Interface files for separate compilation
- Incremental and parallel compilation
- Standard library organization

---

### [Phase 5: Actor Model Integration](phase-05.md) (Weeks 11-12)
**Concurrency and fault tolerance**

Integrates Topos with BEAM's actor model, implementing actors with immutable state, message protocols, and functional handlers. Supervision trees provide fault tolerance through declarative restart strategies. Process primitives (spawn, send, call) enable concurrent programming with type safety. This completes the proof-of-concept by demonstrating category theory on BEAM.

**Key Deliverables:**
- Actor definition syntax
- State types and message protocols
- Functional message handlers
- Process primitives with type safety
- Supervision trees and restart strategies
- Communication patterns (sync/async)
- Fault tolerance infrastructure

---

## Quick Links

**By Development Stage:**
- [Foundation](phase-01.md) → [Development Tools](phase-02.md) → [Language Features](phase-03.md) → [Organization](phase-04.md) → [Concurrency](phase-05.md)

**By Topic:**
- **Compilation**: [Phase 1](phase-01.md) - Lexer, Parser, Type System, Code Generation
- **Developer Experience**: [Phase 2](phase-02.md) - REPL, Prelude
- **Language Features**: [Phase 3](phase-03.md) - Pattern Matching
- **Architecture**: [Phase 4](phase-04.md) - Modules, Imports, Compilation
- **Concurrency**: [Phase 5](phase-05.md) - Actors, Supervision

**Back to:**
- [Proof-of-Concept Overview](proof-of-concept.md)
- [Planning Directory](../README.md)

---

## Phase Dependencies

```
Phase 1 (Foundation)
   ↓
Phase 2 (REPL + Prelude) ─┐
   ↓                      │
Phase 3 (Patterns)        │← Uses REPL for testing
   ↓                      │
Phase 4 (Modules)         │← Needs prelude as base library
   ↓                      │
Phase 5 (Actors)          │← Builds on all previous phases
   └──────────────────────┘
```

Each phase builds incrementally on previous work, with later phases benefiting from testing infrastructure established in Phase 2.

---

## Progress Tracking

Use checkboxes in each phase document to track progress:
- `- [ ]` = Not started
- `- [x]` = Completed

Each phase includes:
- Section-level completion tracking
- Task-level completion tracking
- Subtask-level completion tracking
- Unit tests per section
- Integration tests per phase

---

## Implementation Timeline

| Phase | Duration | Week Range | Status |
|-------|----------|------------|--------|
| Phase 1 | 3 weeks | Weeks 1-3 | ❌ Not Started |
| Phase 2 | 2 weeks | Weeks 4-5 | ❌ Not Started |
| Phase 3 | 3 weeks | Weeks 6-8 | ❌ Not Started |
| Phase 4 | 2 weeks | Weeks 9-10 | ❌ Not Started |
| Phase 5 | 2 weeks | Weeks 11-12 | ❌ Not Started |
| **Total** | **12 weeks** | **3 months** | **0% Complete** |

---

## Getting Started

1. Begin with [Phase 1: Core Language Infrastructure](phase-01.md)
2. Follow the phases in sequence for dependency management
3. Mark checkboxes as you complete tasks
4. Run tests after each section
5. Integration tests validate phase completion

## Support Documents

- [Proof-of-Concept Overview](proof-of-concept.md) - High-level goals and architecture
- [Planning README](../README.md) - Planning directory overview
- [Research Documents](../../research/README.md) - Theoretical foundations
