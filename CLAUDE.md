# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Topos is a category theory-inspired functional programming language for the BEAM VM. The project is currently in **research and design phase** with no implementation yet—only extensive documentation exploring language design, theoretical foundations, and implementation planning.

**Core Philosophy:**
- **Shapes** (types) and **Flows** (functions) using intuitive terminology instead of heavy mathematical jargon
- **Composition as first-class** with `|>` pipe and `>>=` Kleisli composition operators
- **Category theory made practical** - Functors as "Mappable", Monads as "Chainable"
- **Immutability everywhere** with persistent data structures
- **BEAM-first design** leveraging 30+ years of Erlang/OTP battle-testing

## Repository Structure

### Research Documents (`notes/research/`)

16 research documents (1.01-1.16) exploring theoretical foundations and design decisions. Each document lives in its own subdirectory:

```
notes/research/1.XX-topic-name/1.XX-topic-name.md
```

**Key Research Areas:**
- 1.01: Original Idea (core philosophy)
- 1.02: ML-style Module System with functors
- 1.03: Immutability foundations
- 1.07: Error Handling (type system + "let it crash")
- 1.08: Advanced Concurrency (session types, choreographic programming)
- 1.13: Foreign Function Interface (Rustler integration)
- 1.15: Advanced Type System (row polymorphism, polymorphic variants)
- 1.16: Performance Optimization Directives

**Reading Flow:**
- Research defines **WHAT** Topos should be
- See `notes/research/README.md` for recommended reading order
- Each document includes: theoretical foundations, BEAM integration, comparative analysis, implementation considerations

### Planning Documents (`notes/planning/`)

Implementation roadmap divided into 5 phases (12 weeks):

```
notes/planning/proof-of-concept/
├── phase-01.md  # Core Language Infrastructure (Weeks 1-3)
├── phase-02.md  # REPL and Basic Runtime (Weeks 4-5)
├── phase-03.md  # Pattern Matching Engine (Weeks 6-8)
├── phase-04.md  # Module System (Weeks 9-10)
├── phase-05.md  # Actor Model Integration (Weeks 11-12)
├── phase-navigation.md  # Central navigation hub
└── proof-of-concept.md  # High-level overview
```

**Planning Structure:**
- Each phase has detailed sections, tasks, and subtasks with checkboxes
- Every section includes descriptive paragraphs explaining purpose and approach
- Unit tests per section, integration tests per phase
- Success criteria and key outputs documented
- Planning defines **HOW** to build Topos

## Architecture Concepts Spanning Multiple Documents

### Compilation Pipeline (Research: 1.02, 1.12, 1.16 | Planning: Phase 1)

Topos → Lexer → Parser → Type Checker → Core Erlang → BEAM Bytecode

- **Type Inference:** Hindley-Milner (Algorithm W) with row polymorphism, type constraints
- **Type Erasure:** Complete removal at runtime for zero overhead
- **Pattern Compilation:** Decision trees with exhaustiveness checking
- **Target:** Core Erlang intermediate representation (not Erlang source)

### Type System (Research: 1.15 | Planning: Phase 1, 3)

- **Parametric polymorphism** with forall quantifiers
- **Row polymorphism** for extensible records: `{x: Float, y: Float | ρ}`
- **Polymorphic variants** with structural typing: `[> 'Red | 'Green | 'Blue]`
- **Higher-kinded types** for Functors/Monads
- **Type classes as traits** with instance resolution

### Module System (Research: 1.02 | Planning: Phase 4)

Based on ML (OCaml/SML) with BEAM adaptations:

- **Three visibility levels:** private (default), export, public export
- **Parameterized modules (functors):** ML-style with applicative/generative semantics
- **Signature-based abstraction:** Separate interfaces from implementations
- **Categories as modules:** Objects are types, morphisms are functions
- **Natural transformations** as module morphisms
- Compiles to BEAM modules with module-qualified calls

### Actor Model (Research: 1.01, 1.08 | Planning: Phase 5)

Functional approach to BEAM processes:

- **Immutable state** between messages
- **Message protocols** as sum types (algebraic data types)
- **Handlers as pure functions:** `(Message, State) -> (State, MaybeReply)`
- **Supervision trees** with declarative restart strategies
- Compiles to OTP gen_server/supervisor behaviors
- Process modules for stateful computations separate from pure modules

### Pattern Matching (Research: 1.10 | Planning: Phase 3)

- **Guards:** Boolean predicates after patterns (`when` clauses)
- **Or-patterns:** Multiple alternatives in one clause
- **Nested patterns:** Arbitrary depth destructuring
- **As-patterns:** Naming subpatterns while keeping whole value
- **Exhaustiveness checking** with concrete missing examples
- **Redundancy detection** with subsumption analysis
- Decision tree compilation for optimal performance

### Category Theory Integration (Research: 1.01, 1.07, 1.08)

Not just inspiration—explicit language features:

- **Functors:** `trait Mappable f where map : (a -> b) -> f a -> f b`
- **Monads:** `trait Chainable m where return : a -> m a; bind : m a -> (a -> m b) -> m b`
- **Natural transformations:** First-class constructs for module adaptation
- **Effect systems:** Row-typed effects tracking computational side effects
- **Categorical laws** enforced through property-based testing and documentation

## Document Conventions

### Research Documents

Each follows consistent structure:
1. **Theoretical foundations** - Academic research and mathematical principles
2. **Practical integration** - How concepts map to BEAM architecture
3. **Comparative analysis** - Lessons from Haskell, OCaml, Erlang, Elixir, Rust, etc.
4. **Implementation considerations** - Concrete approaches for building features
5. **Examples** - Code in proposed Topos syntax

### Planning Documents

Follow agentjido/cot style:
- Hierarchical numbering: Phase → Section (X.X) → Task (X.X.X) → Subtask (X.X.X.X)
- Completion checkboxes at every level: `- [ ] **Section X.X Complete**`
- Descriptive paragraphs (1-2) for every section and task
- 4+ concrete subtasks per task
- Unit tests at end of each section
- Integration tests as dedicated final section
- Success Criteria, Provides Foundation, Key Outputs

### Code Examples

Use `.topos` or `.tps` file extensions with syntax:

```topos
-- Shape declarations (algebraic data types)
shape Maybe a = Some a | None

-- Flow declarations (pure functions)
flow map : (a -> b) -> List a -> List b
flow map f = match
  | Nil -> Nil
  | Cons x xs -> Cons (f x) (map f xs)
end

-- Actors (concurrent processes)
actor Counter = {
  shape State = { count: Natural }
  shape Message = Increment | Decrement | Get

  flow handle : Message -> State -> (State, Maybe Reply)
  flow handle = match
    | Increment, s -> ({ count: s.count + 1 }, None)
    | Get, s -> (s, Some s.count)
  end
}
```

## Working with This Repository

### Adding Research Documents

1. Create new directory: `notes/research/1.XX-topic-name/`
2. Add document: `notes/research/1.XX-topic-name/1.XX-topic-name.md`
3. Update `notes/research/README.md` with new entry and reading order
4. Update main `README.md` if adding major feature area
5. Follow research document structure (theory → practice → comparison → implementation)

### Adding Planning Documents

1. Create phase document in `notes/planning/proof-of-concept/phase-XX.md`
2. Follow agentjido/cot structure with sections, tasks, subtasks
3. Include descriptive paragraphs for all sections and tasks
4. Add unit tests per section, integration tests at phase level
5. Update `phase-navigation.md` with phase summary and links
6. Ensure tasks have 4+ concrete subtasks each

### Updating Documentation

When making significant changes:
- Update both research (theory/design) and planning (implementation) documents
- Maintain consistency between README files at different levels
- Keep phase-navigation.md synchronized with phase documents
- Update reading order recommendations if adding dependencies

## Important Notes

- **No Implementation Yet:** This is design documentation only. No compiler, runtime, or tooling exists.
- **BEAM Target:** All design decisions account for BEAM VM constraints and capabilities
- **Interoperability:** Must work with existing Erlang/Elixir code (via Core Erlang and OTP)
- **Category Theory:** Make abstract math practical, not just theoretical exercise
- **MIT Licensed:** Open source under MIT (Topos Contributors)

## Terminology Consistency

Use Topos-specific terminology in documentation:
- **Shape** not "type" or "data type"
- **Flow** not "function" or "procedure"
- **Mappable** not "Functor"
- **Chainable** not "Monad"
- **Actor** not "process" or "gen_server"
- **Morphism** in category theory contexts (flows are morphisms)
- **Object** in category theory contexts (shapes are objects)

## Key Resources

- Main README: Project overview, goals, features, inspiration
- Research README: Detailed research methodology and reading order
- Planning README: Relationship between research and planning phases
- Phase Navigation: Quick access to all implementation phases
- License: MIT License in root directory
