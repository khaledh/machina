# Machina

A systems programming language where **state machines are a first-class language
feature** -- not a pattern you assemble from enums and match statements, but
something the compiler understands, checks, and enforces.

Machina asks: what if the language itself could prevent invalid state
transitions, enforce role-based permissions, and manage long-lived stateful
workflows -- all at compile time?

## Linear Types

A `@linear type` declares a state machine: states, transitions, and the rules
that govern them. The compiler tracks state at every point in the program.

```mc
@linear
type Door = {
    states { Closed, Open }

    actions {
        open:  Closed -> Open,
        close: Open   -> Closed,
    }
}

Door :: {
    fn open(self) -> Open { self }
    fn close(self) -> Closed { self }
}

let door = Door::Closed {};
let door = door.open();    // door: Door::Open
let door = door.close();   // door: Door::Closed
door.open();               // COMPILE ERROR: `open` requires Closed, got Open
```

Invalid transitions are not runtime errors or failed tests. They do not compile.

Linear values must also be consumed -- the compiler rejects code that silently
drops a value without completing its lifecycle.

## Machines

Real workflows need persistence, identity, and concurrent access. A **machine**
hosts linear type instances and adds infrastructure without changing the workflow
definition:

```mc
@linear
type PullRequest = {
    id: u64,

    states { Draft, PendingCI, Review, Approved, @final Merged }

    actions {
        submit:  Draft    -> PendingCI,
        approve: Review   -> Approved,
        merge:   Approved -> Merged,
    }

    triggers { CIPassed: PendingCI -> Review }

    roles {
        Author   { submit, merge }
        Reviewer { approve }
    }
}

machine PRService hosts PullRequest(key: id) {
    action submit(draft) -> PendingCI {
        send(self.ci_service, RunCI { pr_id: draft.id });
        draft.submit()   // calls base implementation
    }

    trigger CIPassed(pending) { Review {} }
}
```

The type defines the workflow. The machine overrides only the actions that need
infrastructure -- everything else runs the base implementation directly.

## Sessions

A **session** is a typed handle for interacting with a hosted instance through a
specific role. The compiler enforces both state and permissions:

```mc
let service = PRService::spawn(ci_service)?;
let author = service.create(PullRequest as Author)?;
// author: PullRequest::Draft

let pending = author.submit()?;
// pending: PullRequest::PendingCI

author.approve();
// COMPILE ERROR: `approve` is not available on PullRequest::Draft
//   (session role: Author); `approve` requires role Reviewer
```

When resuming an existing instance whose state is unknown at compile time,
`resume` returns a state union. You match to re-enter the typed world:

```mc
let session = service.resume(PullRequest as Author, pr_id)?;
match session {
    draft: Draft => { draft.submit()? }
    _ => { HttpResponse { status: 409 } }
}
```

Every `resume` requires a match. In exchange, the compiler guarantees you will
never call `submit` on an already-merged PR.

## Concurrency Without Async

All operations on hosted instances go through the machine's mailbox and are
processed sequentially. Session methods block and return the next typed state.
There is no `async`/`await`, no function coloring, no futures to manage -- just
sequential code with compile-time state guarantees.

Currently single-threaded. Multithreading is planned for a future release.

## Where Machina Fits

Systems languages today give you different slices of the problem:

| | State safety | Workflow hosting | Concurrency model | Complexity |
|---|---|---|---|---|
| **Rust** | Typestate possible but manual (phantom types, trait bounds) | Not built-in; assembled from libraries | async/await + Send/Sync | High |
| **Go** | None at compile time | Goroutines + channels, but no state guarantees | Goroutines + channels | Low |
| **Zig** | None at compile time | Not built-in | Manual threading | Low |
| **Swift** | Limited (enums + exhaustive switch) | Actors, but no transition rules | async/await + actors | Medium |
| **Machina** | First-class (`@linear type`) | Built-in (machines + sessions) | Mailbox serialization, no async | Medium |

**Rust** can encode typestate, but it requires phantom types, zero-sized structs, and trait bound gymnastics. It works, but it is not the language helping you -- it is you fighting the language into shape. Machina makes state machines declarative.

**Go** is simple and productive, but its type system cannot prevent invalid state transitions. Those bugs surface at runtime. Machina catches them at compile time.

**Zig** prioritizes simplicity and control. It has no interest in high-level workflow modeling. If you want bare-metal control without abstraction overhead, Zig is the right choice. Machina targets a higher level of the stack.

**Swift** has actors and enums, but no way to declare that an actor's state must follow a specific transition graph. Machina's linear types fill that gap.

**When to reach for Machina:** you are building a system where entities move through defined states -- payment processing, order fulfillment, approval workflows, CI pipelines, device lifecycle management -- and you want the compiler to guarantee that your code respects the rules. If your problem is "things with states and transitions, accessed by different roles, possibly long-lived," Machina is designed for exactly that.

**When not to:** if you need a mature ecosystem, broad platform support, or are doing low-level kernel/driver work, Machina is not there yet. It is early-stage and targets ARM64 only.

## A General-Purpose Foundation

Linear types and machines are the headline feature, but Machina is a full
language with a solid core:

- **Generics** with monomorphization -- zero-cost, fully type-inferred
- **Traits** with methods and properties for abstraction
- **Error unions + `?`** for explicit, low-ceremony error handling
- **Pipe operator `|>`** for composing iterator pipelines
- **Ownership + value semantics** with explicit `inout`/`out`/`sink` parameter modes
- **Built-in collections** -- growable arrays, sets, maps
- **`using` blocks** for scoped resource management
- **LSP + editor tooling** for IDE support

## Current Status

Machina is in early development and not yet stable. The compiler targets
**ARM64** (macOS on Apple Silicon or Linux on ARM64).

## Quickstart

```sh
git clone https://github.com/khaledh/machina.git
cd machina
cargo mcc run examples/quickstart/hello.mc
```

## Learn More

- [Getting Started](docs/getting-started.md) -- build the compiler, write your first program
- [Why Machina](docs/why-machina.md) -- a full payment workflow showing what the model can do
- [Language Tour](docs/tour.md) -- quick pass over core syntax and features
- [Guide](docs/guide/) -- topic-focused guides covering types, functions, generics, traits, iterators, and more
- [Reference](docs/reference/) -- grammar, operators, keywords
- [Examples](examples/) -- runnable code samples

## Project Structure

```
src/core/       compiler pipeline and IR
src/driver/     batch compiler driver (mcc)
src/services/   analysis/query services for IDE features
tooling/lsp/    machina-lsp language server
tooling/vscode/ VS Code / Cursor extension
runtime/        runtime support library
std/            standard library modules
```

## License

MIT -- see [LICENSE](LICENSE).
