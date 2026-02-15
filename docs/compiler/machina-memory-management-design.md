# Memory Management Design: Confined Value Semantics

## Status

Design proposal, February 2026.

## Overview

This document defines Machina's memory management model: **Confined Value
Semantics (CVS)**. The core principle is that values are confined to the scope
that holds them — function-scoped references cannot escape, region-allocated
data stays in its region, and state machine data stays in its heap until
explicitly transferred.

CVS builds on Machina's existing value semantics and ownership model (parameter
modes, `T^` heap values, move-only ownership, second-class slices) and extends
it with three new mechanisms:

1. **Copy-on-write (COW)** for heap-backed collection types.
2. **Regions** as a first-class scoping construct with interior references.
3. **Shared immutable values** for cross-machine read-only data.

Together with the existing rules, these form a layered system where each layer
is opt-in and progressively disclosed:

```
Layer 1: Value semantics + COW collections     (everyday code, no ceremony)
Layer 2: Second-class references               (function-scoped borrowing, no annotations)
Layer 3: Regions with first-class references    (opt-in for indirection/iteration)
Layer 4: Per-machine heaps + transfer           (actor isolation, move on send)
Layer 5: Shared immutable values                (cross-machine read-only data)
```

Most code lives in Layers 1-2. Layers 3-5 are reached only when the problem
demands it.

## Goals

- Preserve the existing value-semantics mental model: changing one value never
  secretly changes another.
- Make performance predictable for heap-backed collections (no silent deep
  copies).
- Provide safe indirection and interior references without lifetime annotations
  or a borrow checker.
- Align memory isolation with the typestate machine / managed-mode architecture:
  each machine owns its heap, communication transfers ownership.
- Support a principled path to cross-process orchestration (shared memory,
  channels) without changing source-level semantics.

## Non-Goals (V1)

- General-purpose garbage collection.
- Rust-style lifetime parameters or borrow checking.
- Tracing GC as a fallback.
- Automatic parallelism or work-stealing across machines (runtime concern, not
  memory model).
- Cross-process transport implementation (deferred; only the semantic contract
  is defined here).

## Relationship to Existing Features

CVS does not replace the current memory safety model. It extends it. Everything
in the existing model remains:

| Existing feature | Status | CVS interaction |
|------------------|--------|-----------------|
| Value semantics (assignment = independent copy) | Unchanged | COW optimizes the copy for collections |
| Parameter modes (`inout`, `out`, `sink`) | Unchanged | Regions interact with `inout` (see below) |
| Heap values (`T^`, `^expr`, `move`) | Unchanged | Heap values can live inside regions |
| Slices (second-class, no escape) | Unchanged | Regions provide a more powerful alternative |
| No overlapping mutable arguments | Unchanged | Applies within regions as well |
| Initialization tracking | Unchanged | Extends to region-allocated values |

---

## Layer 1: Copy-on-Write for Heap-Backed Collections

### Problem

Machina's value semantics mean that `let b = a` for a dynamic array produces an
independent copy. The compiler can optimize this to a move when `a` is not used
afterward, but in general the programmer has limited visibility into when a deep
copy actually occurs. For large collections passed through multiple layers, this
creates unpredictable performance cliffs.

### Design

Heap-backed collection types (`T[*]`, `string`, and future map/set types) use
**copy-on-write** internally. The user-visible semantics are unchanged — values
are independent after assignment — but the backing storage is shared until
mutation.

Rules:

1. Assignment (`let b = a`) shares the backing storage and increments a
   reference count. Both `a` and `b` see the same data. This is O(1).
2. Mutation through any handle (e.g., `a.append(x)`) checks the reference
   count. If the count is 1 (unique), the mutation happens in place. If the
   count is greater than 1 (shared), the backing storage is copied first, then
   mutated. This is the only point where a deep copy occurs.
3. The reference count is non-atomic. State machines are single-threaded
   (handlers are synchronous, run-to-completion), so no atomic operations are
   needed within a machine.
4. Move optimization still applies: when the compiler can prove that `a` is not
   used after `let b = a`, it elides the reference count entirely and transfers
   ownership.
5. COW is an implementation strategy for built-in collection types only.
   User-defined types retain pure value semantics with compiler-optimized copy
   elision.

### The `nocopy` qualifier

For performance-sensitive code where even a COW-deferred copy is unacceptable,
a type or binding can be marked `nocopy`. This makes any implicit copy a
compile error — the programmer must explicitly `.copy()` or restructure to use
moves.

```
let a: nocopy u64[*] = [1, 2, 3];
let b = a;          // error: implicit copy of nocopy value
let b = a.copy();   // ok: explicit copy
let b = move a;     // ok: move, no copy
```

`nocopy` is also valid on struct fields and function parameters:

```
type Buffer = {
    data: nocopy u8[*],
    pos: u64,
}

fn consume(sink buf: nocopy Buffer) { ... }
```

### Interaction with parameter modes

COW does not change parameter mode semantics:

- Default (read-only): callee receives a shared COW handle. No copy unless the
  caller mutates the original while the callee still holds a reference. In
  practice, the callee's reference is short-lived (function duration), so this
  is almost always zero-copy.
- `inout`: callee mutates the caller's value directly. No COW interaction (there
  is no second handle).
- `sink`: ownership transfers. No COW interaction (the value moves).

### What COW does not solve

COW addresses the "invisible deep copy" problem for collections. It does not
help with:

- Indirection and interior references (Layer 3 addresses this).
- Sharing across machines (Layer 5 addresses this).
- User-defined types with expensive copy semantics (use `nocopy` or restructure
  with `sink`/move).

---

## Layer 2: Second-Class References (Existing)

This layer is already implemented in Machina. Summarized here for completeness
and to establish terminology used in later sections.

**Second-class references** are references that cannot escape their immediate
scope. In Machina, this manifests as:

- Default parameter mode: read-only borrow, cannot be returned or stored.
- `inout` parameter mode: mutable borrow, scoped to the call.
- Slices (`T[]`): non-owning views that cannot be returned, stored in
  aggregates, or outlive their source.

The rule is simple: **references are scoped to the function call or block that
creates them.** No lifetime annotations. No escape. This handles the common case
of "I need to read or mutate something without copying it."

The limitation is equally clear: you cannot build data structures that contain
references, return views from functions, or write iterators that yield
references. Layer 3 addresses this.

---

## Layer 3: Regions

### Problem

Second-class references are safe but restrictive. Many common patterns require
references that outlive a single function call but are still bounded:

- Iterators that yield references into a collection.
- Views or slices returned from helper functions.
- Indices, lookup tables, or caches that reference primary data.
- Graph or tree structures with internal pointers.
- Closures that capture references to surrounding data.

Arenas with integer handles can solve these problems, but they impose
significant ergonomic overhead (explicit handle types, manual lookup, no direct
field access) and introduce their own safety gaps (dangling handles after
removal, no individual deallocation).

### Design

A **region** is a scoped memory area. All values allocated within a region share
the region's lifetime. Within a region, references are **first-class**: they can
be stored in structs, returned from functions, passed to closures, and used in
iterators — as long as nothing escapes the region boundary.

```
region r {
    let data = r.alloc([1, 2, 3, 4, 5]);
    let slice = data[1..3];          // reference into data — valid within r
    for item in &data { ... }        // iterator yielding references — valid within r
    let idx = build_index(&data);    // struct holding references — valid within r
}
// everything in r is freed here
```

The single rule: **nothing allocated in or referencing into a region may outlive
the region's scope.** The compiler enforces this structurally — the region scope
is a lexical boundary, not an inferred lifetime.

### Why this is not lifetime checking

In Rust, every reference has an individually inferred lifetime, and the borrow
checker reasons about the relationships between these lifetimes. In Machina
regions:

- All references within a region share **one** lifetime: the region's lexical
  scope.
- The compiler checks a single property: does this value escape the region? If
  yes, compile error. If no, it is safe.
- No lifetime annotations. No lifetime parameters on functions. No
  higher-ranked trait bounds.

This is closer to Tofte-Talpin region inference (MLKit, Cyclone) than to Rust's
borrow checker, but made explicit rather than inferred.

### Region allocation

Values are allocated into a region using `r.alloc(expr)`:

```
region r {
    let node = r.alloc(Node { value: 42, children: [] });
}
```

`r.alloc(expr)` returns a reference to the allocated value. The reference is
valid for the lifetime of `r`. The allocated value is owned by the region and
freed when the region ends.

Allocation within a region is bump-allocation by default: fast (pointer
increment), no individual deallocation, bulk free at region end. This makes
regions ideal for phase-oriented work (parse, transform, emit) where all
intermediate data can be discarded together.

### References within regions

Within a region, references behave like first-class values:

```
type IndexEntry = {
    key: &string,       // reference to a string
    source: &Record,    // reference to a record
}

region r {
    let records = r.alloc(load_records());
    let index = r.alloc(IndexEntry[*]::new());

    for rec in &records {
        if rec.active {
            index.append(IndexEntry {
                key: &rec.name,
                source: rec,
            });
        }
    }

    // index contains references into records — safe because both live in r
    let found = lookup(index, "alice");
    process(found);
}
```

### Returning values from regions

To get data out of a region, you must copy or move it into the enclosing scope
as an owned value:

```
region r {
    let nodes = r.alloc(parse(input));
    let result = nodes.to_owned();    // deep copy out of r into caller's scope
}
// result is an owned value, independent of r
use(result);
```

Attempting to return a region reference is a compile error:

```
region r {
    let data = r.alloc([1, 2, 3]);
    data    // error: region reference cannot escape region r
}
```

### Nested regions

Regions can be nested. Inner region references cannot escape to the outer
region:

```
region outer {
    let shared = outer.alloc(Config { ... });

    region inner {
        let temp = inner.alloc(parse(input));
        process(temp, shared);    // ok: outer refs are visible in inner
    }
    // inner is freed, temp is gone

    use(shared);    // ok: shared lives in outer
}
```

References from an outer region are accessible within an inner region (the outer
region outlives the inner one). References from an inner region cannot escape to
the outer region.

### Interaction with parameter modes

Functions that operate on region-allocated data use references naturally:

```
fn build_index(records: &Record[*]) -> IndexEntry[*] {
    // returns a collection of IndexEntry values containing references
    // caller and callee must be in the same region scope
    ...
}
```

The compiler ensures that functions receiving region references do not store
them in locations that outlive the region. Within a region scope, this is
checked structurally (same as the "does it escape the region?" rule).

`inout` works on region-allocated values:

```
region r {
    let data = r.alloc([1, 2, 3]);
    sort(inout data);    // ok: mutate in place within region
}
```

### Interaction with closures

Closures within a region can capture region references:

```
region r {
    let threshold = r.alloc(Config { min: 10 });
    let filter = |item: &Record| -> bool {
        item.score >= threshold.min    // captures reference to threshold
    };
    let results = records.filter(filter);
}
```

Outside a region, closures capture by value (with move optimization), as they
do today.

### Region design constraints

Regions have deliberate limitations:

1. **No individual deallocation.** A region frees all its memory at once when
   the scope ends. If you allocate 10,000 objects and logically discard 9,999,
   the memory is held until the region ends. For long-lived regions, this means
   careful scoping.

2. **No cross-region references.** A reference allocated in region `a` cannot
   point to data in region `b` (unless `b` is a parent region of `a`). This
   prevents dangling references when one region is freed before the other.

3. **Lexical scoping only.** Region lifetimes are determined by lexical scope,
   not by data flow analysis. This is less flexible than Rust's lifetimes but
   eliminates the need for annotations and inference.

4. **No region-polymorphic functions in V1.** A function cannot be generic over
   "any region." The caller and callee must share a common region scope. This
   may be relaxed in future versions.

---

## Layer 4: Per-Machine Heaps and Transfer Semantics

### Problem

Machina's managed-mode typestate machines are independent, mailbox-driven
entities. Each machine processes one event at a time (synchronous,
run-to-completion handlers). When machines communicate via `send`, `request`,
and `reply`, data must cross machine boundaries safely — without shared mutable
state, without dangling references, and without a global garbage collector.

### Design

Each managed machine has its own **heap** — a private memory region that holds
all of the machine's state, local variables, and dynamically allocated data.
Machine heaps are isolated: no machine can directly reference another machine's
heap.

#### The machine heap as an implicit root region

A machine's heap is conceptually a region that lives for the duration of the
machine's lifecycle. State fields, handler-local allocations, and any sub-regions
opened during handler execution all live within (or nested under) this root
region.

```
typestate Connection {
    // These fields live in the machine's heap for its entire lifecycle.
    var buffer: u8[*] = [];
    var peer: string = "";

    state Connected {
        on Data(payload) {
            buffer.append(payload.bytes);    // mutates machine-owned data
        }
    }
}
```

#### Transfer on send

When a machine sends a value through a mailbox (`send`, `request`, `reply`),
the value is **moved** from the sender's heap to the receiver's heap. The
sender loses access. This is enforced statically using the same move semantics
as `sink` parameters.

```
state Connected {
    on FlushRequested -> Idle {
        let report = Report {
            data: move buffer,      // buffer is moved into report
            timestamp: now(),
        };
        send(monitor, move report); // report is moved to monitor's mailbox
        // report is inaccessible here
        // buffer is inaccessible here (was moved into report)
        buffer = u8[*]::new();      // re-initialize for new state
        Idle
    }
}
```

Attempting to use a sent value after the send is a compile error:

```
send(target, move msg);
use(msg);    // error: use after move
```

#### Transfer implementation strategies

The `send` operation has well-defined semantics (move) but the runtime may
choose different strategies depending on context:

| Context | Strategy | Cost |
|---------|----------|------|
| Same process, small value | memcpy into receiver's heap | O(n) where n = value size |
| Same process, large value | Pointer handoff between heaps (shared backing allocator with ownership tags) | O(1) |
| Cross-process, same host | Serialize into shared memory segment, receiver deserializes | O(n) |
| Cross-host | Serialize over network channel | O(n) + network |

The programmer writes the same `send` regardless of context. The runtime
selects the strategy. This is the key to transparent cross-process promotion:
the source-level semantics are identical whether machines are co-located or
distributed.

#### Interaction with regions

A machine handler may open sub-regions for temporary work. Data that must
survive the handler (state fields, outgoing messages) must be owned values, not
region references:

```
state Processing {
    on Analyze(input) -> Done {
        region scratch {
            let parsed = scratch.alloc(parse(input));
            let refs = build_cross_refs(parsed);    // references within scratch
            let summary = summarize(refs).to_owned(); // copy result out
        }
        // scratch is freed; summary is an owned value
        send(reporter, move summary);
        Done
    }
}
```

This pattern — open a region for complex intermediate work, copy out the final
result as an owned value, close the region — is idiomatic for handler
implementations that need temporary indirection.

#### Transactional interaction

Transfer semantics align with the transactional handler model defined in the
typestate machines design: outgoing messages (`send`, `request`, `reply`) are
committed only when the handler succeeds. If the handler fails, no messages
are sent and no values are transferred. This means the sender's heap is
unchanged on failure — the moved values are restored (or never actually
transferred, depending on implementation).

---

## Layer 5: Shared Immutable Values

### Problem

Pure transfer semantics mean that if 50 machines need to read the same large
configuration table, the table must be copied into each machine's heap. For
large read-only datasets (configurations, lookup tables, trained models, static
assets), this is wasteful.

### Design

A value can be **frozen** into a `shared` value. A `shared` value is deeply
immutable and can be referenced by multiple machines without transfer.

```
let config = shared Config.load("settings.toml");
```

`shared` values are the single exception to the confinement rule: they are not
confined to one machine's heap because immutability makes sharing safe.

#### Rules

1. **Deep immutability.** A `shared` value and everything reachable from it is
   immutable. The compiler enforces this — you cannot obtain a mutable reference
   to any part of a `shared` value.

2. **One-way freezing.** Creating a `shared` value from a regular value is a
   one-way operation. The original value is consumed (moved into the shared
   form). There is no way to "thaw" a `shared` value back into a mutable one
   (other than copying it).

3. **Reference-counted lifecycle.** `shared` values are reference-counted. When
   the last reference is dropped, the value is freed. The reference count uses
   atomic operations (since multiple machines may hold references). This is the
   only place in the memory model where atomic operations are required.

4. **Send without transfer.** Sending a `shared` value to another machine does
   not move it — it shares a reference. Both sender and receiver can continue to
   read the value.

```
let config = shared Config.load("settings.toml");
send(machine_a, config);    // shared reference, not moved
send(machine_b, config);    // same
use(config);                // still accessible
```

5. **No `shared` references into regions.** A `shared` value cannot contain
   references into a region (since the region may be freed while the `shared`
   value is still alive). `shared` values must be self-contained.

#### Creating shared values

From a regular value (consumes the original):

```
var table = build_lookup_table();
let shared_table = shared move table;
// table is no longer accessible
```

From a literal or constructor:

```
let shared_config = shared Config { debug: false, max_retries: 3 };
```

#### Type representation

The type of a shared value is `shared T`. This is a distinct type from `T`:

```
fn read_config(cfg: shared Config) { ... }  // accepts shared reference
fn edit_config(sink cfg: Config) { ... }    // accepts owned mutable value
```

`shared T` supports all read operations that `T` supports. It does not support
mutation.

#### Interaction with COW

When a `shared T` is copied into a regular `T` (e.g., for mutation), a deep
copy is performed:

```
let cfg: shared Config = shared Config { debug: false, max_retries: 3 };
var local_cfg: Config = cfg.copy();    // deep copy into mutable value
local_cfg.debug = true;                // ok: local_cfg is independent
```

This is explicit — the programmer asks for the copy. There is no implicit
conversion from `shared T` to `T`.

---

## Concept Budget

The full model has six concepts. The first two cover the vast majority of code:

| Concept | When to use | Rust equivalent |
|---------|-------------|-----------------|
| Values (default) | Everyday code | Owned values + Clone |
| `nocopy` | Performance-critical paths | No direct equivalent |
| Second-class refs | Function-scoped borrowing | `&`/`&mut` with elided lifetimes |
| Regions | Complex intermediate structures | `'a` lifetimes + Arena + scoped threads |
| Transfer on send | Machine-to-machine communication | `Send` + ownership transfer |
| `shared` | Cross-machine read-only data | `Arc<T>` where `T: Sync` |

Progressive disclosure means most users learn values and parameter modes first,
encounter regions only when they need complex data manipulation, and reach
`shared` only when building multi-machine systems.

---

## Design Rationale

### Why not Rust-style lifetimes?

Lifetimes with borrow checking are the most powerful reference-safety system in
production use. They enable zero-cost borrowing in all positions. But they
impose significant cognitive overhead: lifetime parameters, lifetime elision
rules, higher-ranked trait bounds, variance, `Pin`, `PhantomData`, and complex
compiler error messages. This overhead is disproportionate for Machina's primary
use case (state machines communicating through messages), where most data is
either owned or temporary.

CVS trades universal zero-cost borrowing for simplicity: references are either
second-class (simple, no annotations) or region-scoped (powerful, one rule).
The cost is paid in copies at region boundaries and in the inability to express
certain patterns (region-polymorphic functions, references in return position
outside regions). For Machina's domain, this is the right tradeoff.

### Why not garbage collection?

GC eliminates the entire reference management problem but introduces
unpredictable pauses, higher memory overhead, and a runtime dependency that
complicates cross-process deployment and FFI. Machina targets systems where
deterministic resource management matters: state machines with real-time
event handling, embedded systems, and performance-sensitive services.

Per-machine GC (as in Erlang/BEAM) is a more viable option but still imposes
per-machine overhead and complicates the transfer model (GC'd values don't
have a clear ownership boundary for transfer). CVS achieves similar isolation
without a GC runtime.

### Why not pure MVS without extensions?

Pure mutable value semantics (as in Hylo/Val) work well for simple cases but
struggle with:

- **Performance predictability**: copy elision is best-effort; the programmer
  cannot reliably predict when copies occur.
- **Indirection**: any data structure with internal pointers (graphs, trees,
  indices) requires manual arena/handle management.
- **Sharing**: no mechanism for read-only data shared across actors.

CVS starts from MVS and adds targeted extensions (COW, regions, shared) to
address these gaps while preserving the value-semantics mental model as the
default.

### Why regions instead of arenas with handles?

Arenas with integer handles solve the indirection problem but at an ergonomic
cost: explicit handle types, manual lookup syntax, no direct field access, and
a new class of bugs (dangling handles, use after removal). Regions provide the
same bulk-allocation and bulk-deallocation benefits with real references instead
of handles, direct field access, and compiler-enforced safety.

The tradeoff is that regions are less flexible than arenas for certain patterns
(e.g., free-list reuse of individual slots). For those cases, an arena library
can be built on top of regions.

### Why shared immutable values instead of channels?

Channels (typed, linear, session-typed) are a powerful abstraction for
structured communication. Machina may add them in the future. But channels solve
a different problem: structured interaction sequences. `shared` values solve a
simpler problem: multiple readers of the same static data. Adding `shared` first
gives immediate relief for the common case (shared config, lookup tables) without
the complexity of channel semantics.

---

## Known Limitations

These are acknowledged limitations of the CVS model. Some may be addressed in
future versions; others are inherent tradeoffs.

1. **No individual deallocation in regions.** Long-lived regions with high
   churn waste memory. Mitigation: scope regions tightly; use sub-regions for
   phases.

2. **No region-polymorphic functions (V1).** A function cannot abstract over
   "any region." This limits library design. May be addressed with a lightweight
   region parameter syntax in a future version.

3. **Copy at region boundaries.** Getting owned data out of a region requires
   a copy. For large results, this is unavoidable. Mitigation: keep regions
   tightly scoped so only small results need to escape.

4. **Atomic reference counting for `shared`.** This is the only atomic
   operation in the memory model. For high-frequency creation/destruction of
   `shared` values, this may be a bottleneck. Mitigation: `shared` values are
   intended for long-lived, rarely-changing data.

5. **COW mutation cliffs.** A COW collection with many shared references will
   deep-copy on first mutation. The copy point is predictable (at mutation, not
   at assignment) but may surprise users who expect in-place performance.
   Mitigation: `nocopy` makes this visible; move semantics avoid it entirely.

6. **FFI boundary.** External code (C libraries, system calls) does not respect
   confinement. An `unsafe` boundary is required for FFI, and safety guarantees
   end there. This is unavoidable and shared with every safe-by-default language.

7. **No lazy iteration over non-owned data outside regions.** Without regions,
   iterators must yield owned values (copies). Within regions, iterators can
   yield references. This means performance-sensitive iteration requires region
   scoping.

---

## Interaction with Typestate Machines

CVS is designed to align with Machina's typestate machine architecture.

### Direct mode

Direct-mode typestate machines are local values. They follow standard value
semantics and ownership rules. No special memory management is needed — the
machine is an owned value, its state transitions are method calls, and it is
dropped when it goes out of scope.

### Managed mode

Managed-mode machines are runtime-owned. Each machine has an isolated heap (its
root region). The runtime manages machine lifecycles (Created, Running, Faulted,
Stopped) and heap deallocation follows machine lifecycle:

- **Created/Running**: heap is live, handlers allocate and deallocate within it.
- **Faulted**: heap is frozen (no new allocations). Pending outgoing messages
  are discarded (transactional semantics). `shared` references held by the
  machine are released (reference counts decremented).
- **Stopped**: heap is freed in bulk. All owned data is deallocated. `shared`
  references are released.

### Mailbox payloads

Mailbox payloads (the values in `send`, `request`, `reply`) are owned values
that transfer from sender heap to receiver heap. The envelope model carries the
payload as bytes or as a transferable value, depending on the runtime strategy.

`shared` values in payloads are not transferred — a reference is shared. The
envelope carries a reference (pointer + refcount increment) rather than a copy.

### Typed handles

`Machine<T>` handles are lightweight identifiers (not references into another
machine's heap). They are value types that can be freely copied, stored, and
sent. Sending a `Machine<T>` handle to another machine does not transfer any
heap data — it transfers a routing identifier.

---

## Summary

Confined Value Semantics is a memory management model built on one principle:
**values belong to the scope that holds them.** Escape is prevented by default
(second-class references), enabled within bounds (regions), and controlled
across machines (transfer on send). The single exception — `shared` immutable
values — is safe by construction because immutability eliminates the hazards
that confinement guards against.

| Layer | Mechanism | Confinement rule |
|-------|-----------|------------------|
| 1 | Values + COW | Values are independent; COW defers copy to mutation |
| 2 | Second-class refs | References cannot escape their scope |
| 3 | Regions | References cannot escape their region |
| 4 | Machine heaps | Data cannot escape its machine without explicit transfer |
| 5 | `shared` | Exception: deeply immutable data may be shared across machines |

The model avoids garbage collection, lifetime annotations, and borrow checking.
The cost is paid in copies at confinement boundaries and in the restriction that
references are always bounded by a visible scope. For Machina's domain — state
machines communicating through messages — this tradeoff favors simplicity and
predictability over maximum flexibility.

---

## Prior Art

| Language / System | Relevant idea | CVS relationship |
|-------------------|---------------|------------------|
| Hylo/Val | Mutable value semantics, second-class references | Direct foundation for Layers 1-2 |
| Swift | COW for value-type collections | Inspiration for Layer 1 COW strategy |
| MLKit / Cyclone | Region-based memory management | Foundation for Layer 3 region design |
| Pony | Reference capabilities for actors (`iso`, `val`) | Similar goals; CVS is simpler (fewer capabilities) |
| Erlang/BEAM | Per-process heaps, copy-on-send | Proven model for Layer 4 machine isolation |
| Austral | Linear types, radically simplified | Shows viability of simple ownership without borrow checking |
| Koka | Perceus (precise reference counting with reuse) | Alternative approach to deterministic memory management |
| Lobster | Compile-time lifetime analysis to eliminate RC | Practical middle ground for reference counting optimization |
