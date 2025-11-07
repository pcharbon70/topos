# Phase 5: Actor Model Integration

## Overview

This phase integrates Topos with BEAM's actor model, bringing together functional programming and concurrent processes. Actors provide isolated state, message-based communication, and fault tolerance—the foundation of Erlang's legendary reliability. We design actor syntax that makes category theory concepts explicit while compiling to standard OTP behaviors. Actors maintain immutable state between messages, update state functionally, and handle failures gracefully through supervision trees.

The actor system treats processes as objects in a category where morphisms are message handlers. State transitions become pure functions from (Message, State) to (NewState, Reply). This functional approach to actors preserves referential transparency while leveraging BEAM's process isolation and fault recovery. We implement both low-level process primitives and high-level supervision abstractions, enabling everything from simple stateful servers to complex distributed systems.

This phase runs for 2 weeks and completes the proof-of-concept by demonstrating Topos's unique value proposition: marrying category theory's mathematical rigor with BEAM's practical distributed computing. By the end, developers can write concurrent, fault-tolerant systems using familiar functional patterns backed by battle-tested BEAM infrastructure.

---

## 5.1 Actor Definition Syntax
- [ ] **Section 5.1 Complete**

Actor definitions describe stateful processes using functional syntax. An actor specifies its state type, message protocol, initialization function, and message handler. The handler is a pure function from (Message, State) to (NewState, MaybeReply)—making state transitions explicit and testable. We compile actors to OTP gen_server behaviors, mapping Topos's functional handler to gen_server callbacks. This design makes actors predictable while maintaining BEAM compatibility.

### 5.1.1 State Type System
- [ ] **Task 5.1.1 Complete**

Actor state must be explicitly typed, defining what data the actor maintains across messages. State types are typically records or ADTs containing all persistent information. State is immutable—handlers return new state rather than mutating. We enforce this through the type system, preventing shared mutable state that causes concurrency bugs. State can include other actor references (pids), enabling actor hierarchies and supervision.

- [ ] 5.1.1.1 Implement actor state type declaration with immutability constraints
- [ ] 5.1.1.2 Implement state initialization function specifying starting state
- [ ] 5.1.1.3 Implement state validation ensuring state types are serializable for process migration
- [ ] 5.1.1.4 Implement state introspection allowing debugging and monitoring of actor state

### 5.1.2 Message Protocol
- [ ] **Task 5.1.2 Complete**

Message protocols define what messages an actor accepts, using sum types (variants). Each message constructor represents a different request type. Example: `shape Message = Increment | Decrement | Get | Reset Natural`. This makes the actor's API explicit in types. Pattern matching on messages routes to appropriate handling logic. We generate message type definitions that serialize correctly over BEAM channels.

- [ ] 5.1.2.1 Implement message type declaration as algebraic data types
- [ ] 5.1.2.2 Implement message pattern matching in handlers routing messages to logic
- [ ] 5.1.2.3 Implement message serialization for sending between processes
- [ ] 5.1.2.4 Implement message type validation catching protocol violations at compile time

### 5.1.3 Handler Implementation
- [ ] **Task 5.1.3 Complete**

Message handlers are pure functions: `handle : Message -> State -> (State, Maybe Reply)`. They pattern match on messages, compute new state, and optionally produce replies. Handlers cannot perform side effects directly—they return state changes and replies that the runtime applies. This functional approach makes handlers testable without spawning processes. We compile handlers to gen_server handle_call/handle_cast callbacks.

- [ ] 5.1.3.1 Implement handler function type checking ensuring correct signature
- [ ] 5.1.3.2 Implement handler pattern matching compiling to efficient message dispatch
- [ ] 5.1.3.3 Implement reply generation handling synchronous and asynchronous messages
- [ ] 5.1.3.4 Implement handler purity checking preventing side effects in handler functions

### 5.1.4 Process Primitives
- [ ] **Task 5.1.4 Complete**

Low-level primitives enable process creation and communication. `spawn` creates new processes from actor definitions. `send` (written `!`) sends asynchronous messages. `call` (written `?`) sends synchronous messages and waits for replies. `link` and `monitor` establish failure detection relationships. These primitives map directly to BEAM operations but integrate with Topos's type system for safety.

- [ ] 5.1.4.1 Implement `spawn` primitive creating new actor processes from definitions
- [ ] 5.1.4.2 Implement `send` (!) operator for asynchronous message sending with type checking
- [ ] 5.1.4.3 Implement `call` (?) operator for synchronous request-reply with timeout handling
- [ ] 5.1.4.4 Implement `link` and `monitor` primitives for failure detection and supervision

### Unit Tests - Section 5.1
- [ ] **Unit Tests 5.1 Complete**
- [ ] Test actor state type checking enforcing immutability and serializability
- [ ] Test message protocol definition and pattern matching in handlers
- [ ] Test handler function purity and correct state transitions
- [ ] Test process primitives (spawn, send, call) with type safety

---

## 5.2 Supervision Trees
- [ ] **Section 5.2 Complete**

Supervision trees provide fault tolerance through hierarchical process monitoring. Supervisors watch child processes and restart them on failure according to configurable strategies. We implement OTP supervision patterns using declarative syntax. Supervisors themselves are actors, creating recursive supervision hierarchies. The "let it crash" philosophy separates normal logic from error handling—processes fail fast, supervisors handle recovery.

### 5.2.1 Supervisor Syntax
- [ ] **Task 5.2.1 Complete**

Supervisor definitions specify restart strategy and child specifications. Syntax: `supervisor Name = { strategy: one_for_one, children: [...] }`. Strategies include one_for_one (restart only failed child), one_for_all (restart all children), and rest_for_one (restart failed child and those after it). Max_restarts and max_seconds limit restart frequency preventing crash loops.

- [ ] 5.2.1.1 Implement supervisor declaration syntax with strategy and child list
- [ ] 5.2.1.2 Implement restart strategy types (one_for_one, one_for_all, rest_for_one)
- [ ] 5.2.1.3 Implement restart intensity limits (max_restarts, max_seconds) preventing loops
- [ ] 5.2.1.4 Implement supervisor compilation to OTP supervisor behavior

### 5.2.2 Child Specifications
- [ ] **Task 5.2.2 Complete**

Child specifications describe supervised processes. Each child has an ID, start function, restart policy (permanent, transient, temporary), shutdown timeout, and type (worker or supervisor). We generate child specs from actor definitions automatically, with sensible defaults. Developers can customize child specs for specific requirements like graceful shutdown times.

- [ ] 5.2.2.1 Implement child specification syntax with ID, start function, and restart policy
- [ ] 5.2.2.2 Implement automatic child spec generation from actor definitions
- [ ] 5.2.2.3 Implement restart policy types (permanent, transient, temporary) with correct semantics
- [ ] 5.2.2.4 Implement shutdown timeout configuration for graceful termination

### 5.2.3 Restart Strategies
- [ ] **Task 5.2.3 Complete**

Different restart strategies suit different use cases. One-for-one restarts failed children independently—best for independent workers. One-for-all restarts all children when any fails—useful when children depend on each other. Rest-for-one restarts failed child and subsequent children—for when later children depend on earlier ones. We implement all strategies following OTP semantics exactly.

- [ ] 5.2.3.1 Implement one-for-one strategy restarting only failed children
- [ ] 5.2.3.2 Implement one-for-all strategy restarting all children on any failure
- [ ] 5.2.3.3 Implement rest-for-one strategy restarting failed child and all after it
- [ ] 5.2.3.4 Implement simple-one-for-one strategy for dynamic child pools

### 5.2.4 Supervision Tree Compilation
- [ ] **Task 5.2.4 Complete**

Supervision trees compile to OTP supervisor behaviors with child specs. We generate init callbacks returning supervision strategy and child list. Child start functions spawn actors with correct arguments. The compilation ensures that supervision trees integrate seamlessly with existing OTP applications, allowing Topos supervisors to manage Erlang processes and vice versa.

- [ ] 5.2.4.1 Implement supervisor behavior generation with init callback
- [ ] 5.2.4.2 Implement child start function generation spawning actors correctly
- [ ] 5.2.4.3 Implement supervision tree initialization handling application startup
- [ ] 5.2.4.4 Implement hot code upgrade support for supervised processes

### Unit Tests - Section 5.2
- [ ] **Unit Tests 5.2 Complete**
- [ ] Test supervisor declaration and compilation to OTP supervisors
- [ ] Test restart strategies (one-for-one, one-for-all, rest-for-one) with simulated failures
- [ ] Test child specifications with different restart policies and shutdown timeouts
- [ ] Test supervision tree compilation and integration with OTP application structure

---

## 5.3 Process Communication Patterns
- [ ] **Section 5.3 Complete**

Beyond basic message passing, we implement higher-level communication patterns common in actor systems. Request-reply with timeouts prevents indefinite blocking. Broadcast sends messages to multiple actors. Process registries enable name-based lookup. Selective receive allows mailbox filtering. These patterns build on primitives to provide convenient, type-safe communication abstractions.

### 5.3.1 Synchronous Request-Reply
- [ ] **Task 5.3.1 Complete**

Synchronous calls block until receiving a reply or timeout. Syntax: `result <- actor ? Message`. The `?` operator sends a message and waits for response. Timeouts prevent indefinite blocking—unresponsive actors cause timeout errors rather than deadlocks. We implement call operation using gen_server:call, handling timeouts and failures gracefully.

- [ ] 5.3.1.1 Implement synchronous call operator (?) with type-checked request and reply
- [ ] 5.3.1.2 Implement timeout handling returning timeout errors after specified duration
- [ ] 5.3.1.3 Implement call failure handling converting process exits to Result types
- [ ] 5.3.1.4 Implement call performance optimization reducing overhead for simple calls

### 5.3.2 Asynchronous Fire-and-Forget
- [ ] **Task 5.3.2 Complete**

Asynchronous sends don't wait for replies. Syntax: `actor ! Message`. The `!` operator sends and returns immediately. This enables high throughput—senders don't block waiting for processing. Message order is preserved per sender-receiver pair. We implement send using gen_server:cast or direct message passing depending on whether replies are possible.

- [ ] 5.3.2.1 Implement asynchronous send operator (!) with message type checking
- [ ] 5.3.2.2 Implement send performance optimization avoiding unnecessary overhead
- [ ] 5.3.2.3 Implement message ordering guarantees preserving order per sender-receiver pair
- [ ] 5.3.2.4 Implement send failure handling detecting dead processes and full mailboxes

### 5.3.3 Process Registration
- [ ] **Task 5.3.3 Complete**

Process registration maps names to process IDs, enabling location-independent messaging. Global registration makes processes findable system-wide. Local registration scopes names per node. We integrate with BEAM's process registry (erlang:register) while adding type safety. Named processes receive messages at registered names rather than PIDs.

- [ ] 5.3.3.1 Implement process registration with unique names checking for conflicts
- [ ] 5.3.3.2 Implement name lookup resolving registered names to process IDs
- [ ] 5.3.3.3 Implement global registration for distributed process lookup across nodes
- [ ] 5.3.3.4 Implement unregistration and automatic cleanup on process termination

### 5.3.4 Selective Receive
- [ ] **Task 5.3.4 Complete**

Selective receive filters mailbox contents, receiving only messages matching patterns. This allows prioritizing urgent messages or waiting for specific replies while deferring others. We compile selective receive to BEAM's optimized receive expressions with patterns. Timeout support prevents indefinite waiting. This pattern enables complex protocols without manual mailbox management.

- [ ] 5.3.4.1 Implement selective receive syntax with pattern matching and timeout
- [ ] 5.3.4.2 Implement selective receive compilation to BEAM receive expressions
- [ ] 5.3.4.3 Implement timeout handling returning timeout results or default values
- [ ] 5.3.4.4 Implement selective receive optimization leveraging BEAM's receive optimization

### Unit Tests - Section 5.3
- [ ] **Unit Tests 5.3 Complete**
- [ ] Test synchronous request-reply with various timeout scenarios
- [ ] Test asynchronous send performance and message ordering
- [ ] Test process registration and name-based messaging
- [ ] Test selective receive filtering messages correctly

---

## 5.4 Integration Tests
- [ ] **Section 5.4 Complete**

Integration tests validate the complete actor system with realistic concurrent programs. We build applications using actors and supervisors, test fault tolerance through simulated failures, measure performance under load, and verify correct behavior in distributed scenarios. These tests ensure that Topos actors integrate seamlessly with BEAM's process model and provide the expected reliability.

### 5.4.1 Actor Communication Testing
- [ ] **Task 5.4.1 Complete**

We test various communication patterns between actors. Test synchronous calls, asynchronous sends, broadcast to multiple actors, and request-reply chains. Verify message ordering, timeout handling, and failure detection. Test both happy paths and error scenarios (dead processes, slow actors, network partitions in distributed tests).

- [ ] 5.4.1.1 Test point-to-point messaging between actor pairs with various message types
- [ ] 5.4.1.2 Test broadcast messaging sending to multiple actors simultaneously
- [ ] 5.4.1.3 Test message chains where actors call other actors forming pipelines
- [ ] 5.4.1.4 Test error handling when messages are sent to dead or non-existent processes

### 5.4.2 Fault Tolerance Testing
- [ ] **Task 5.4.2 Complete**

We verify that supervision trees provide fault tolerance. Crash child processes deliberately and verify supervisors restart them correctly according to strategy. Test restart intensity limits preventing crash loops. Verify that state is reset on restart (or recovered if persistence added). Test cascading failures and verify that supervision strategies contain failures appropriately.

- [ ] 5.4.2.1 Test supervisor restart of crashed children with different strategies
- [ ] 5.4.2.2 Test restart intensity limits stopping supervisor after too many crashes
- [ ] 5.4.2.3 Test cascading failure handling ensuring failures don't propagate unexpectedly
- [ ] 5.4.2.4 Test state recovery after restart with various initialization strategies

### 5.4.3 Performance Benchmarking
- [ ] **Task 5.4.3 Complete**

We measure actor system performance under load. Benchmark message throughput (messages/second), latency (message round-trip time), and scalability (performance with many actors). Compare Topos actors to native Erlang gen_servers—aim for overhead under 10%. Test with various workloads: many small messages, few large messages, request-reply vs fire-and-forget.

- [ ] 5.4.3.1 Benchmark message throughput with varying numbers of actors and message rates
- [ ] 5.4.3.2 Benchmark message latency measuring round-trip time for synchronous calls
- [ ] 5.4.3.3 Benchmark actor spawn time and memory overhead per actor
- [ ] 5.4.3.4 Compare Topos actor performance to equivalent Erlang gen_server implementation

### 5.4.4 Distributed Actor Testing
- [ ] **Task 5.4.4 Complete**

We test actors across multiple BEAM nodes (distributed Erlang). Spawn actors on remote nodes, send messages between nodes, verify that process linking and monitoring work across nodes. Test network partitions and node failures. This validates that Topos actors leverage BEAM's distribution transparency, enabling truly distributed systems.

- [ ] 5.4.4.1 Test remote actor spawning creating processes on different nodes
- [ ] 5.4.4.2 Test distributed messaging sending messages between nodes with location transparency
- [ ] 5.4.4.3 Test distributed failure detection using links and monitors across nodes
- [ ] 5.4.4.4 Test network partition handling and recovery when nodes disconnect

---

## Success Criteria

1. **Actor Syntax**: Clean functional syntax for actors compiling to OTP behaviors
2. **State Immutability**: Type-enforced immutable state with functional updates
3. **Supervision Trees**: Declarative supervision with all OTP restart strategies
4. **Communication Patterns**: Synchronous and asynchronous messaging with type safety
5. **Fault Tolerance**: Working supervision with automatic restart and recovery
6. **Performance**: Actor overhead under 10% compared to native Erlang gen_servers
7. **BEAM Integration**: Seamless interoperability with existing OTP applications

## Provides Foundation

This phase completes the proof-of-concept by demonstrating:
- **Category Theory + BEAM**: Functional abstractions running on battle-tested infrastructure
- **Type Safety + Concurrency**: Static types preventing common concurrency bugs
- **Fault Tolerance**: "Let it crash" philosophy with automatic recovery
- **Distributed Computing**: Foundation for building distributed Topos applications
- **Production Readiness**: Real systems can be built using Topos actors

## Key Outputs

- Actor definition syntax with state types and message protocols
- Pure functional message handlers with (Message, State) -> (NewState, Reply) signature
- Process primitives (spawn, send, call, link, monitor) with type safety
- Supervision tree syntax compiling to OTP supervisors
- All OTP restart strategies (one-for-one, one-for-all, rest-for-one)
- Communication patterns (request-reply, fire-and-forget, selective receive)
- Process registration and name-based messaging
- Complete actor system test suite
- Performance benchmarks validating efficiency
- Distributed actor examples demonstrating scalability
- Integration with OTP application structure
- Documentation of actor model usage patterns
