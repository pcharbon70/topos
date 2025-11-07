# Topos

> A category theory-inspired functional programming language for the BEAM VM

## Overview

Topos is a research project exploring the design of a programming language that unifies category theory's mathematical rigor with the BEAM VM's legendary fault tolerance and concurrency capabilities. The language aims to make abstract mathematical concepts practical and accessible while leveraging the proven strengths of Erlang's runtime.

## Core Philosophy

Topos reimagines functional programming by making category theory concepts first-class language features rather than academic curiosities:

- **Shapes** instead of types - intuitive terminology for algebraic data types
- **Flows** instead of functions - emphasizing data transformation and composition
- **Composition as foundation** - the pipe operator `|>` and Kleisli composition `>>=` as primary abstractions
- **Immutability everywhere** - all data structures are immutable by default with persistent data structures
- **Actor model integration** - seamless blend of pure functional programming with BEAM's process model

## Key Features

### Category Theory Made Practical

- **Functors, Applicatives, and Monads** - simplified as "Mappable" and "Chainable" traits
- **Natural transformations** - first-class support for transforming between functors
- **Explicit effect system** - computational effects tracked through monads
- **Categorical laws** - property-based testing for algebraic properties

### BEAM-First Design

- **Process modules** - stateful computations as OTP processes
- **Supervision trees** - declarative fault tolerance
- **Hot code reloading** - leveraging BEAM's capabilities with pure function guarantees
- **Hex.pm integration** - seamless interoperability with Erlang/Elixir ecosystem

### Advanced Module System

- **ML-style parameterized modules (functors)** - inspired by OCaml and Standard ML
- **Three-level visibility control** - private, export, and public export
- **Signature-based abstraction** - interfaces separate from implementations
- **Category-as-module** - modules literally form categories with types as objects

### Developer Experience

- **Mandatory documentation** - first-class language construct with runtime introspection
- **Built-in testing** - property-based testing, doctests, and categorical law verification
- **Time-travel debugging** - leveraging immutability for reversible execution
- **FFI with safety** - category theory-based foreign function interface

## Project Status

⚠️ **This is a research project in early design phase**

Currently, the repository contains extensive research and design documentation exploring various language features. No compiler or runtime implementation exists yet.

## Documentation

Comprehensive research documents are available in [`notes/research/`](notes/research/):

- **[1.01 - Original Idea](notes/research/1.01-original_idea/1.01-original_idea.md)** - Core design philosophy
- **[1.02 - Modules](notes/research/1.02-modules/1.02-modules.md)** - Advanced module system design
- **[1.03 - Immutability](notes/research/1.03-immutability/1.03-immutability.md)** - Immutability as foundation
- **[1.04 - Documentation](notes/research/1.04-documentation/1.04-documentation.md)** - Documentation as language construct
- **[1.05 - Packages](notes/research/1.05-packages/1.05-packages.md)** - Ecosystem integration
- **[1.06 - Testing](notes/research/1.06-testing/1.06-testing.md)** - Built-in testing framework
- **[1.07 - Error Handling](notes/research/1.07-error-handling/1.07-error-handling.md)** - Type systems meet "let it crash"
- **[1.08 - Advanced Concurrency](notes/research/1.08-advanced-concurrency/1.08-advanced-concurrency.md)** - Session types and choreographic programming
- **[1.09 - Reflection and Introspection](notes/research/1.09-reflection-and-introspection/1.09-reflection-and-introspection.md)** - Runtime reflection
- **[1.10 - Pattern Matching](notes/research/1.10-pattern-matching/1.10-pattern-matching.md)** - Advanced pattern features
- **[1.11 - Domain-Specific Languages](notes/research/1.11-domain-specific-langage/1.11-domain-specific-langage.md)** - First-class DSL support
- **[1.12 - Build System](notes/research/1.12-build-system/1.12-build-system.md)** - Compilation pipeline
- **[1.13 - Foreign Function Interface](notes/research/1.13-foreign-function-interface/1.13-foreign-function-interface.md)** - Safe native code interop
- **[1.14 - Development Tools](notes/research/1.14-development-tools/1.14-development-tools.md)** - Debugging and tooling
- **[1.15 - Advanced Type System](notes/research/1.15-advanced-type-system/1.15-advanced-type-system.md)** - Category theory-based type system
- **[1.16 - Performance Optimization Directives](notes/research/1.16-performance-optimization-directives/1.16-performance-optimization-directives.md)** - BEAM-specific optimizations

See the [research directory README](notes/research/README.md) for a detailed overview and recommended reading order.

## Example Code

```topos
-- Define a shape (algebraic data type)
shape User = {
  name: Text,
  age: Natural,
  email: Email
} derives [Eq, Show, Mappable]

-- Define a flow (pure function)
flow greet : User -> Text
flow greet user = "Hello, " <> user.name

-- Composition with the pipe operator
flow process_user : User -> Result Success Error
flow process_user =
  validate
  |> transform
  |> persist

-- Actor as immutable state machine
actor Counter = {
  shape State = { count: Natural }
  shape Message = Increment | Decrement | Get

  flow handle : Message -> State -> (State, Maybe Reply)
  flow handle = match
    | Increment, s -> ({ count: s.count + 1 }, None)
    | Decrement, s -> ({ count: s.count - 1 }, None)
    | Get, s       -> (s, Some s.count)
}
```

## Goals

1. **Demonstrate that category theory can be practical** - Make abstract mathematics accessible to working programmers
2. **Unify functional and distributed programming** - Show that pure functions and actor models complement rather than conflict
3. **Leverage existing ecosystems** - Build on the BEAM's 30+ years of production success rather than starting from scratch
4. **Make correctness convenient** - Design where the easiest path is also the correct path

## Non-Goals

- Replace Erlang or Elixir for existing projects
- Support every category theory concept (focus on practical subset)
- Create yet another general-purpose language without a clear niche
- Ignore performance or developer experience for theoretical purity

## Inspiration

Topos draws inspiration from:

- **Category theory** - Mathematical foundations for composition and abstraction
- **ML family** (OCaml, Standard ML, Haskell) - Module systems and type systems
- **BEAM languages** (Erlang, Elixir) - Concurrency and fault tolerance
- **Idris/Agda** - Dependent types and visibility control
- **Rust** - Safety guarantees and modern ergonomics

## Contributing

This project is currently in the research phase. Contributions in the form of feedback on design documents, additional research, or prototype implementations are welcome.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
