# Phase 4: Module System

## Overview

This phase implements Topos's hierarchical module system, enabling code organization across files and namespaces. Modules provide encapsulation through visibility control (private vs. exported definitions), namespace management to avoid naming conflicts, and separate compilation to support large codebases. We design the module system following ML traditions (OCaml, Standard ML) while integrating smoothly with BEAM's module structure and Erlang/Elixir interoperability.

Each Topos module compiles to a BEAM module with explicit exports. The import system supports qualified imports (prefixed with module name), selective imports (choosing specific definitions), and module aliases. We implement name resolution that handles shadowing correctly, prevents circular dependencies, and provides clear error messages for unresolved names. The module system must support both

 hierarchical organization (Data.List, Data.Set) and flat structure (Prelude) as needed.

This phase runs for 2 weeks and prioritizes clean semantics and developer ergonomics. By the end, developers can organize Topos code into reusable modules, manage dependencies explicitly, and interoperate with existing BEAM libraries.

---

## 4.1 Module Structure
- [ ] **Section 4.1 Complete**

Module definitions establish boundaries between code units. Each module declares its name, exports (public API), and imports (dependencies). The module body contains type definitions (shapes), function definitions (flows), and trait implementations. Private definitions (not exported) provide internal implementation details. Module structure determines what code is visible where, enabling information hiding and abstraction.

### 4.1.1 Module Declaration Syntax
- [ ] **Task 4.1.1 Complete**

Module declarations specify the module name and what it exports. Syntax: `module Data.List exports (List, map, filter) where ... end`. The export list can include types, functions, or entire submodules. Module names use hierarchical dot notation (Java-style). We parse module declarations, build module objects holding metadata, and validate that all exported names are defined.

- [ ] 4.1.1.1 Implement module declaration parsing with module name and export list
- [ ] 4.1.1.2 Implement hierarchical name validation ensuring module names follow conventions
- [ ] 4.1.1.3 Implement export list parsing supporting types, functions, and constructor exports
- [ ] 4.1.1.4 Implement module metadata construction storing exports, imports, and definitions

### 4.1.2 Export System
- [ ] **Task 4.1.2 Complete**

The export system determines a module's public API. We support exporting types (with or without constructors), functions, and trait instances. Selective constructor export allows hiding internal representation—exporting type name but not constructors creates abstract types. Re-exports enable module facades that aggregate multiple modules. Export validation ensures all exported names exist and have correct kinds (type vs value).

- [ ] 4.1.2.1 Implement type export with constructor visibility control (all, specific, or none)
- [ ] 4.1.2.2 Implement function export with type signature inclusion for documentation
- [ ] 4.1.2.3 Implement re-export syntax allowing modules to re-export imports creating facades
- [ ] 4.1.2.4 Implement export validation checking all exported names exist and are correctly qualified

### 4.1.3 Visibility Control
- [ ] **Task 4.1.3 Complete**

Visibility annotations (private, export) control access to definitions. Default is private—visible only within the module. Marking definitions with `export` or including them in export lists makes them public. We implement visibility checking that prevents access to private definitions from outside. This enables information hiding and allows implementations to change without breaking clients.

- [ ] 4.1.3.1 Implement visibility annotation parsing with `private` and `export` keywords
- [ ] 4.1.3.2 Implement visibility checking preventing access to private definitions across modules
- [ ] 4.1.3.3 Implement automatic export inference adding definitions to exports based on annotations
- [ ] 4.1.3.4 Implement visibility error messages explaining access violations with suggestions

### 4.1.4 Module Compilation
- [ ] **Task 4.1.4 Complete**

Each Topos module compiles to a BEAM module with the same name (hierarchical names become atoms with dots). We generate module attributes storing type information for cross-module type checking. The export list becomes BEAM exports with correct arities. Private functions don't appear in exports. Module initialization code runs when the module loads, though most Topos modules have no side effects.

- [ ] 4.1.4.1 Implement BEAM module name generation from Topos hierarchical module names
- [ ] 4.1.4.2 Implement module attribute generation storing type signatures and metadata
- [ ] 4.1.4.3 Implement export list generation with correct function arities for BEAM
- [ ] 4.1.4.4 Implement module compilation pipeline from Topos AST to Core Erlang module

### Unit Tests - Section 4.1
- [ ] **Unit Tests 4.1 Complete**
- [ ] Test module declaration parsing with various export lists and module names
- [ ] Test export system correctly exposing types, functions, and constructors
- [ ] Test visibility control preventing access to private definitions
- [ ] Test module compilation generating valid BEAM modules with correct exports

---

## 4.2 Import System
- [ ] **Section 4.2 Complete**

The import system brings definitions from other modules into scope. We support multiple import styles: qualified (using module prefix), selective (importing specific names), and aliased (renaming modules). Import resolution happens during name resolution phase, mapping imported names to their defining modules. Circular imports are forbidden—we detect cycles and report errors. Unused imports generate warnings to keep code clean.

### 4.2.1 Qualified Imports
- [ ] **Task 4.2.1 Complete**

Qualified imports bring a module's exports into scope with a prefix. Syntax: `import qualified Data.List as L`. Usage: `L.map`, `L.filter`. This avoids naming conflicts and documents where functions come from. The import introduces the alias name in the namespace, and all access goes through the alias. We validate that qualified names resolve correctly and that the module exists.

- [ ] 4.2.1.1 Implement qualified import parsing with `qualified` keyword and alias
- [ ] 4.2.1.2 Implement qualified name resolution prefixing imports with module alias
- [ ] 4.2.1.3 Implement module loading finding and compiling imported modules as needed
- [ ] 4.2.1.4 Implement module dependency tracking building dependency graph for compilation order

### 4.2.2 Selective Imports
- [ ] **Task 4.2.2 Complete**

Selective imports bring specific names into scope unqualified. Syntax: `import Data.List (List, map, filter)`. Usage: `map`, `filter` without prefix. The import list explicitly names what to import. This reduces verbosity for frequently-used functions while keeping imports explicit. We check that all requested names exist in the imported module and warn about conflicts with local definitions.

- [ ] 4.2.2.1 Implement selective import parsing with explicit name lists
- [ ] 4.2.2.2 Implement selective name resolution adding imported names to local namespace
- [ ] 4.2.2.3 Implement import list validation checking all names exist in target module
- [ ] 4.2.2.4 Implement conflict detection warning about name shadowing from imports

### 4.2.3 Name Resolution
- [ ] **Task 4.2.3 Complete**

Name resolution maps identifiers to their definitions, handling shadowing and scoping rules. Local definitions shadow imports, which shadow prelude definitions. Qualified names bypass local scope and directly reference module members. Unresolved names produce clear errors indicating similar names that might be meant (typo detection). The resolver builds a symbol table mapping each name to its definition.

- [ ] 4.2.3.1 Implement symbol table construction building hierarchical namespace from definitions and imports
- [ ] 4.2.3.2 Implement name lookup with shadowing rules prioritizing local over imported definitions
- [ ] 4.2.3.3 Implement qualified name resolution handling module-prefixed names correctly
- [ ] 4.2.3.4 Implement typo detection suggesting similar names for unresolved identifiers

### 4.2.4 Circular Dependency Detection
- [ ] **Task 4.2.4 Complete**

Circular module dependencies cause compilation problems—if A imports B and B imports A, neither can compile first. We detect cycles by building the dependency graph and running cycle detection (DFS). Cycles produce errors showing the import chain. We also check for indirect cycles (A -> B -> C -> A). The dependency order determines compilation sequence for multi-module programs.

- [ ] 4.2.4.1 Implement dependency graph construction from module import declarations
- [ ] 4.2.4.2 Implement cycle detection algorithm finding strongly connected components in graph
- [ ] 4.2.4.3 Implement cycle error reporting showing complete import chain forming cycle
- [ ] 4.2.4.4 Implement topological sorting determining valid compilation order for acyclic graphs

### Unit Tests - Section 4.2
- [ ] **Unit Tests 4.2 Complete**
- [ ] Test qualified imports resolving module-prefixed names correctly
- [ ] Test selective imports bringing specific names into scope unqualified
- [ ] Test name resolution handling shadowing and scope correctly
- [ ] Test circular dependency detection catching direct and indirect cycles

---

## 4.3 Module Compilation and Linking
- [ ] **Section 4.3 Complete**

Multi-module compilation requires coordinating compilation order, sharing type information, and linking BEAM modules. We implement separate compilation—each module compiles independently using interface files from dependencies. Interface files contain exported type signatures and metadata. The linker resolves cross-module function calls and verifies all dependencies exist. This enables incremental compilation—recompiling only changed modules.

### 4.3.1 Interface Files
- [ ] **Task 4.3.1 Complete**

Interface files (`.tps.interface`) store a module's public API without implementation details. They contain type signatures for exported functions, definitions for exported types, and trait instances. Importing modules read interfaces to type-check cross-module references without recompiling dependencies. Interfaces enable parallel compilation and protect against internal changes breaking dependents.

- [ ] 4.3.1.1 Implement interface file format encoding type signatures, types, and exports compactly
- [ ] 4.3.1.2 Implement interface generation extracting public API from compiled modules
- [ ] 4.3.1.3 Implement interface loading reading interface files for imported modules
- [ ] 4.3.1.4 Implement interface validation detecting version mismatches and incompatibilities

### 4.3.2 Separate Compilation
- [ ] **Task 4.3.2 Complete**

Separate compilation compiles each module independently. The compiler reads interfaces for imports, performs type checking using imported signatures, and generates code. This enables incremental builds—unchanged modules don't recompile. We track file timestamps to determine what needs recompilation. Parallel compilation becomes possible since modules compile independently (respecting dependency order).

- [ ] 4.3.2.1 Implement incremental compilation tracking timestamps and recompiling only changed files
- [ ] 4.3.2.2 Implement parallel compilation spawning independent module compilations concurrently
- [ ] 4.3.2.3 Implement dependency-ordered compilation ensuring imports compile before dependents
- [ ] 4.3.2.4 Implement compilation cache storing intermediate results to speed up rebuilds

### 4.3.3 Cross-Module References
- [ ] **Task 4.3.3 Complete**

Functions can call functions in other modules. At compile time, we verify the target function exists and has compatible type. At runtime, calls use qualified names (Module:function/arity). The code generator emits module-qualified calls for imported functions. We handle both direct module calls and calls through variables holding function references.

- [ ] 4.3.3.1 Implement cross-module call generation using module-qualified function names
- [ ] 4.3.3.2 Implement type checking for cross-module calls verifying signatures match
- [ ] 4.3.3.3 Implement function reference handling for higher-order cross-module functions
- [ ] 4.3.3.4 Implement hot code loading support allowing modules to upgrade independently

### 4.3.4 Standard Library Organization
- [ ] **Task 4.3.4 Complete**

The standard library organizes into hierarchical modules: Data.List, Data.Set, Control.Monad, etc. We establish conventions for module organization, documentation, and API design. The Prelude module auto-imports into every module, providing essential definitions. Library modules follow consistent patterns for naming, error handling, and performance characteristics.

- [ ] 4.3.4.1 Implement hierarchical standard library structure with logical module grouping
- [ ] 4.3.4.2 Implement Prelude auto-import mechanism making essential definitions always available
- [ ] 4.3.4.3 Implement module documentation generation creating browsable API references
- [ ] 4.3.4.4 Implement standard library conventions for naming, error handling, and design patterns

### Unit Tests - Section 4.3
- [ ] **Unit Tests 4.3 Complete**
- [ ] Test interface file generation and loading preserving type information correctly
- [ ] Test incremental compilation recompiling only changed modules
- [ ] Test cross-module function calls working correctly with type checking
- [ ] Test standard library organization with Prelude auto-import

---

## 4.4 Integration Tests
- [ ] **Section 4.4 Complete**

Integration tests validate the complete module system with realistic multi-module programs. We create projects with multiple interdependent modules, test compilation order, verify imports work correctly, and check that modules interoperate properly. These tests ensure the module system scales to real codebases and catches issues with dependency management, name resolution, and separate compilation.

### 4.4.1 Multi-Module Programs
- [ ] **Task 4.4.1 Complete**

We develop complete programs spanning multiple modules. Examples: a JSON library with separate modules for parsing, generation, and types; a web server with modules for routing, handlers, and middleware; a compiler with modules for lexing, parsing, type checking, and code generation. These programs exercise all module system features in realistic contexts.

- [ ] 4.4.1.1 Test multi-module library with public API and internal implementation modules
- [ ] 4.4.1.2 Test module dependency chains with deep hierarchies (A imports B imports C imports D)
- [ ] 4.4.1.3 Test module re-exports creating facade modules that aggregate functionality
- [ ] 4.4.1.4 Test mixed import styles (qualified and selective) in same program

### 4.4.2 Compilation Order Testing
- [ ] **Task 4.4.2 Complete**

We verify that modules compile in correct dependency order. Tests intentionally create various dependency patterns and check that the compiler determines the right compilation sequence. We also test incremental compilation—changing one module and verifying only affected modules recompile. Parallel compilation tests run with multiple concurrent module compilations.

- [ ] 4.4.2.1 Test topological sort producing valid compilation order for complex dependency graphs
- [ ] 4.4.2.2 Test incremental build recompiling minimal set of modules after changes
- [ ] 4.4.2.3 Test parallel compilation correctly handling concurrent module builds
- [ ] 4.4.2.4 Test compilation performance scaling to projects with 100+ modules

### 4.4.3 Interoperability Testing
- [ ] **Task 4.4.3 Complete**

Topos modules must interoperate with Erlang and Elixir code. We test calling Erlang functions from Topos, exposing Topos functions to Erlang, and mixing Topos and Erlang modules in a project. Type boundaries require careful handling—Topos types must map sensibly to BEAM terms. We document interoperability patterns and best practices.

- [ ] 4.4.3.1 Test calling Erlang standard library functions from Topos with correct types
- [ ] 4.4.3.2 Test exposing Topos functions for calling from Erlang/Elixir code
- [ ] 4.4.3.3 Test type conversions between Topos types and Erlang terms
- [ ] 4.4.3.4 Test mixed projects combining Topos, Erlang, and Elixir modules

---

## Success Criteria

1. **Module Organization**: Hierarchical module system with namespace management and visibility control
2. **Import System**: Qualified and selective imports working correctly with name resolution
3. **Separate Compilation**: Incremental and parallel compilation using interface files
4. **Dependency Management**: Cycle detection and topological sorting for compilation order
5. **Interoperability**: Seamless integration with Erlang/Elixir codebases
6. **Standard Library**: Organized standard library with Prelude auto-import

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 5**: Actor model requiring module-level actor definitions and supervision
- Future development of module functors (ML-style parameterized modules)
- Package management and distribution of Topos libraries
- Large-scale application development with clean module boundaries

## Key Outputs

- Module declaration and export system with visibility control
- Import system supporting qualified and selective imports
- Name resolution with shadowing and qualified names
- Circular dependency detection and compilation ordering
- Interface files for separate compilation
- Incremental and parallel compilation infrastructure
- Cross-module type checking and call generation
- Standard library organization with hierarchical modules
- Comprehensive module system test suite
- Interoperability guidelines for Erlang/Elixir integration
