# Linear Types and Sessions — Overview

This document defines Machina's direction for **linear types**, **machines**,
and **typed sessions** — a unified model for stateful workflow management with
compile-time safety.

## The Model in Three Layers

### Layer 1: Linear Type as Typestate (Direct Mode)

A `@linear type` without roles or triggers is a plain typestate — a value whose
type tracks its state at compile time:

```mc
@linear
type Door = {
    states {
        Closed,
        Open,
    }

    actions {
        open: Closed -> Open,
        close: Open -> Closed,
    }
}

Door :: {
    fn open(self) -> Open { self }
    fn close(self) -> Closed { self }
}

let door = Door::Closed {};
let door = door.open();   // door: Door::Open
let door = door.close();  // door: Door::Closed
```

The type declares the workflow graph; the method block provides action
implementations. Each action consumes the current value and returns a new one in
the target state. Direct-mode calls are always blocking — regular function
calls, no machine involved. The compiler enforces linearity:

```mc
let door = Door::Closed {};
let door = door.open();
door.open();   // COMPILE ERROR: `open` requires Door::Closed, got Door::Open
```

```mc
let door = Door::Closed {};
// function returns without using `door`
// COMPILE ERROR: linear value `door` must be used — cannot silently drop
```

This is the foundation. Everything else builds on it.

### Layer 2: Hosted Linear Type + Machine

When a linear type needs persistence, identity, concurrency, and roles, it is
hosted by a machine. The type declares the workflow graph and provides base
action implementations; the machine overrides only the actions that need
infrastructure access:

```mc
@linear
type PullRequest = {
    id: u64,
    author: UserId,
    reviewers: List<UserId>,

    states {
        Draft,
        PendingCI,
        Review,
        Approved,
        @final Merged,
    }

    actions {
        submit: Draft -> PendingCI,
        approve: Review -> Approved,
        merge: Approved -> Merged,
    }

    triggers {
        ci_passed: PendingCI -> Review,
    }

    roles {
        Author { submit, merge }
        Reviewer { approve }
    }
}

PullRequest :: {
    fn submit(self) -> PendingCI { self }
    fn approve(self) -> Approved { self }
    fn merge(self) -> Merged { self }
}
```

```mc
machine PRService hosts PullRequest(key: id) {
    // Override only the action that needs infrastructure
    action submit(draft) -> PendingCI {
        request(self.ci_service, RunCI { pr_id: draft.id });
        draft.submit()   // calls base implementation
    }

    // approve, merge — no override needed, base runs directly

    trigger ci_passed(pending) { Review {} }

    // ...
}
```

### Layer 3: Session — Typed Interaction with a Hosted Linear Type

A session is a typed handle for interacting with one hosted instance through one
role. It gives clients the same typestate progression as direct mode, but over
a hosted machine with blocking, persistence, and concurrency:

```mc
let service = PRService::spawn(ci_service)?;
let author = service.create(PullRequest as Author)?;
// author: PullRequest::Draft

let author = author.add_reviewer(reviewer_id)?;
let pending = author.submit()?;
// pending: PullRequest::PendingCI — no Author actions available here
```

The compiler checks every step. Invalid operations are caught at compile time:

```mc
author.approve();
// COMPILE ERROR: `approve` is not available on PullRequest::Draft
//   (session role: Author); `approve` requires role Reviewer

pending.submit();
// COMPILE ERROR: `submit` requires PullRequest::Draft, got PullRequest::PendingCI
```

```mc
let author = service.create(PullRequest as Author)?;
let pending = author.submit()?;
author.revise("oops");
// COMPILE ERROR: `author` was consumed by `submit`
```

For an existing instance whose state is unknown at compile time, `resume`
returns a union of all possible states. The caller matches to get a typed
handle — the same way you match on any enum:

```mc
let session = service.resume(PullRequest as Author, pr_id)?;

match session {
    draft: Draft => {
        let _pending = draft.submit()?;
        HttpResponse { status: 200 }
    }
    _ => {
        HttpResponse { status: 409, message: "PR is not in Draft state" }
    }
}
```

The `resume` + `match` pattern is the bridge from the untyped world (an HTTP
request with a PR id) into the typed world (a `Draft` session handle). Every
`resume` requires a match — in exchange, the compiler guarantees that you will
never call `submit` on an Approved PR. The runtime cannot reach an invalid
state.

---

## Concurrency Model

This is central to Machina's design and differs fundamentally from async-based
and lock-based approaches.

**Mailbox serialization.** All operations targeting instances within a machine —
session actions, delivered triggers, and any other inputs — go through the
machine's mailbox and are processed one at a time. Instance state is never
concurrently accessed. There are no data races on instance state — not by
programmer discipline, but by construction.

**Blocking without async coloring.** Session methods block by default and
return the next typed state. There is no `async`/`await`, no function coloring,
no futures to manage. Normal code is normal:

```mc
let author = author.revise("cleanup")?;   // blocks, round-trips to machine
let pending = author.submit()?;            // blocks, round-trips to machine
```

The runtime handles interleaving. The programmer writes sequential code.

**`wait()` is suspension, not concurrency.** When a session is in a state with
only trigger-driven outgoing transitions, the client can `wait()`. This
registers interest in the next state change and suspends — it does not create a
new thread. When a trigger transitions the instance, the machine notifies the
waiting client. There is no race between `wait()` and trigger delivery — the
mailbox serializes everything.

**Staleness is handled, not prevented.** When a session action arrives, the
runtime checks the instance's actual state against the action's expected state.
Mismatch returns `SessionError::InvalidState`. The client re-resumes and
matches on the actual state. This is clean and explicit — no silent corruption.

**For comparison:**
- **Rust**: ownership + Send/Sync enforces data race freedom but requires
  programmer discipline with async, channels, and locks.
- **Go**: goroutines + channels provide concurrency but offer no compile-time
  guarantees about valid state transitions.
- **Erlang/Elixir**: actor mailboxes provide serialization (closest to Machina),
  but the state machine is ad-hoc, encoded in `handle_call` clauses with no
  static typing.
- **TypeScript/XState**: state machines are defined declaratively, but enforced
  at runtime through assertions, not the type system.

Machina combines mailbox serialization (like Erlang) with compile-time state
tracking (like Rust typestate) and blocking semantics (like Go) — without the
drawbacks of any.

---

## Core Concepts

### V1 Constructs

The V1 language surface has three core constructs:

| Construct | Purpose |
|-----------|---------|
| `@linear type` | Stateful workflow: struct fields + enum states + transitions + roles |
| `machine` | Runtime host for instances; implements handlers |
| `session` | Typed, linear capability over a hosted instance |

A `@linear type` is a composition of familiar concepts:

| Part | Analogy | What it gives you |
|------|---------|-------------------|
| Top-level fields | Struct | Shared data across all states |
| `states { }` | Enum | Variants with optional payloads, pattern matching |
| `actions { }` / `triggers { }` | Transition rules | Which variant-to-variant moves are legal |
| `roles { }` | Access control | Who can invoke which actions |
| `@linear` | Ownership/move semantics | Compiler enforces consumption |

### Linear Type

A `@linear type` declares a workflow graph: fields that persist across all
states, states (as enum variants with optional payloads), actions (client-driven
transitions), triggers (system-driven transitions), and roles (permission sets
over actions).

A linear type without roles or triggers is a direct-mode typestate. Add roles,
and it becomes hostable. This is a spectrum, not two different constructs.

The identity key is a regular field — which field serves as the identity key is
declared at the machine hosting site, not in the type itself.

### Machine

A machine hosts instances of a linear type. The hosting declaration specifies
which field is the identity key: `machine PRService hosts PullRequest(key: id)`.

The type's method block provides base action implementations. The machine
overrides only the actions that need infrastructure access (other machines,
databases, external services). Unoverridden actions run the base implementation
directly.

Inside handlers, `self` refers to the machine (infrastructure); the first
parameter is the instance (domain object). The first parameter's type is
inferred from the type's action/trigger declaration — it is always the source
state.

Three handler types coexist:
- **`action` overrides** — instance-level, client-driven (via sessions).
  Optional; base runs if not overridden.
- **`trigger` handlers** — instance-level, system-driven (via `self.deliver()`).
- **`on` handlers** — machine-level, message-driven (via mailbox).

The relationship: external message → `on` handler → `self.deliver(key,
trigger)` → trigger handler → state transition. `on` handlers operate on the
machine as a whole; `action` and `trigger` handlers operate on specific
instances.

### Session

A session is a typed, linear value targeting one instance through one role. It
progresses through compiler-checked states.

- **`create`** — new instance, statically typed at the initial state.
- **`resume`** — existing instance, returns a state union (match required).
- **`wait()`** — suspend until a trigger-driven state change.

The role is specified once at creation (`PullRequest as Author`) and is implicit
thereafter. The compiler tracks it internally to determine which methods are
available. Diagnostics and LSP hover always show the role:

```
error: `approve` is not available on PullRequest::Draft (session role: Author)
  note: `approve` requires role Reviewer

hover: PullRequest::Draft (as Author)
  available: submit, revise, comment, add_reviewer
```

### Actions vs Triggers

- **Actions** are client-driven — invoked through a session, subject to role
  permissions.
- **Triggers** are system-driven — delivered by the machine via
  `self.deliver(key, trigger)`, not subject to role permissions.

Both cause state transitions. Both undergo runtime state validation. The
distinction is in how they arrive, not in what they do.

---

## Construct Consolidation

`@linear type` is the universal workflow primitive in Machina. It subsumes the
roles previously played by separate `typestate` and `protocol` constructs:

- **`@linear type` without roles/triggers** = plain typestate (unmanaged linear
  lifecycle, used directly without hosting).
- **`@linear type` with roles + machine hosting** = managed workflow (replaces
  protocol-based machine interactions).

`typestate` and `protocol` are retired as separate keywords. Typestates become
a usage pattern (linear type in direct mode). Protocol message contracts become
linear type workflows with role-constrained actions.

### Why This Works

1. **Typestate was already a state machine.** A linear type adds optional
   roles on top. Strip those away and it is the same thing.

2. **Protocols were workflow in disguise.** Send/recv sequencing encoded a state
   machine with channel-oriented syntax. A linear type makes the state machine
   explicit. (Some protocol use cases are more transport/message-contract
   shaped — those are handled by machine-level request/reply.)

3. **Roles generalize both sides.** Protocols had exactly two roles by
   construction. Linear type roles are N-ary and more flexible.

4. **The compiler checking is the same.** Linear ownership, exhaustive state
   matching, action availability per state — these checks apply uniformly
   whether the type is used directly or hosted.

Sessions recover the typestate programming experience for managed, hosted
instances. Where direct linear types give the caller a typed value that
progresses through states directly, sessions give the caller a typed handle
that progresses through the same states over a hosted machine — with blocking,
persistence, and concurrent access handled by the machine.

---

## Problem

Managed machines are runtime-owned actors. Once a machine is spawned, outside
code holds a `Machine<T>` handle, not a value whose type reflects the machine's
current state.

The compiler can check payload shapes, role conformance, request/reply
contracts, and machine handle types. But it cannot generally check whether a
specific message is valid for the machine's **current runtime state**.

This is fundamentally different from direct typestate, where the caller owns a
concrete state value and the type system knows exactly which state it is
operating on.

Sessions close this gap.

---

## Terms

- **linear type** (`@linear type`): a type with struct-like fields, enum-like
  states, transition rules, and optional roles. Values must be consumed (linear
  ownership). Without roles/triggers, used directly as typestate. With roles,
  hostable by a machine.
- **machine**: a long-lived runtime host that owns and mutates instances of a
  linear type, implements handlers, and manages persistence.
- **role**: a named permission set over actions. Specified at session creation;
  implicit in the session type thereafter.
- **session**: a typed, linear capability for interacting with one instance
  through one role.
- **action**: a client-driven state transition, invoked through a session.
- **trigger**: a system-driven state transition, delivered by the machine to an
  instance.
- **transport**: the runtime mechanism used to carry session actions and results.

V2 terms (see [04-channel.md](04-channel.md)):
- **channel**: a lightweight typed message pipe for unbounded streaming and
  fan-out, without state progression.

---

## Design Principles

1. **The type defines the workflow; the machine adds infrastructure.**
   The linear type declares states, actions, triggers, and roles, and provides
   base action implementations via method blocks. The machine overrides actions
   that need infrastructure, provides trigger handlers, and manages persistence.

2. **Linear types are struct + enum + transitions + access control.**
   Fields are struct-like. States are enum variants. Actions and triggers
   define valid transitions. Roles constrain access. `@linear` enforces
   consumption. No new concepts — familiar parts composed together.

3. **Sessions are typed client-side capabilities.**
   A session is a linear value in user code. It targets one hosted instance
   and one role. It progresses through compiler-checked states.

4. **Roles are implicit in session types.**
   The role is specified once at session creation. After that, the session type
   only reflects the type and its current state. The compiler tracks the role
   internally to determine which methods are available.

5. **Machines host instances.**
   A machine is a runtime authority, not the workflow model itself. The machine
   designates which field is the identity key at the hosting site.

6. **Actions are client-driven; triggers are system-driven.**
   Both cause state transitions in the workflow graph.

7. **Blocking by default, no async function coloring.**
   Session methods block and return the next typed state. Non-blocking is
   explicit and secondary. Blocking does not force an async language model.

8. **Persistence is the machine's concern.**
   The caller never touches storage directly. The machine handles load on
   session resume, save on action commit.

---

## V1 Scope

The V1 core is **`@linear type` + machine + session**.

### Type model

- Flat fields, states (enum variants including `@final`), actions (optionally
  fallible), triggers, roles.
- Method blocks for action implementations.
- Direct-mode linear types (no roles/triggers) as typestate replacement.
- States as enum variants with optional payloads.

### Transition model

- `action` (client-driven) and `trigger` (system-driven) as transition inputs.
- Base action implementations in the type's method block; machine overrides for
  infrastructure.
- Handlers return the next state; may `emit` ordinary typed values as side
  effects.
- Automatic transition notifications on state changes.
- `self` = machine; first handler param = instance (author-named).

### Session model

- `create` (statically typed) and `resume` (returns state union, match
  required) with `as` syntax for roles.
- Session types reflect type + state; role is implicit (visible in diagnostics).
- Typed session progression with compile-time role/state checking.
- `wait()` for trigger-driven state transitions.
- Blocking by default.

### Machine model

- `machine ... hosts Type(key: field)` with action overrides, `trigger`, and
  `on` handlers.
- `self.deliver(key, trigger)` for routing triggers to instances.
- Cross-machine composition via blocking session calls in action handlers.
- Machine-owned persistence (API shape open, architectural role fixed).
- Identity key designated at machine hosting site.

### Runtime

- In-memory instance storage.
- Runtime serialization for concurrent sessions on same instance.
- `SessionError::InvalidState` for stale session handles.

### Secondary features

- `emit` — handler side-effect primitive for typed value emission.
- `lookup` — advisory field inspection (not authoritative).

---

## V2 and Beyond

### Channels and event flow

- `channel` declarations — typed message pipes for streaming and fan-out.
- `events { }` block in linear types — declares emittable event types.
- `emit` routing to co-hosted channels.
- Iterator/pipe integration for channel readers.
- `self.listen()` pull-to-push bridge for machine-to-machine subscriptions.
- `subscribes` declarative subscription sugar.

### Extended features

- Multi-type hosting (`machine ... hosts A, B`).
- `@exclusive` role enforcement.
- `nowait` non-blocking session form.
- Composite identity keys (use struct key type as workaround until then).
- `select` syntax for waiting on multiple sessions.

### Infrastructure

- Distributed/network transport.
- Event-sourced persistence.

---

This document set supersedes `machine-session-design.md` and
`machine-session-server-design.md`.
