# Linear Type Declaration

## Transition Model

Every action and trigger handler follows the same execution model:

```
(new_state | error, emitted_values) = handle(current_state, input)
```

### Transition inputs

Two kinds of input can cause a state transition:

- **Action** — client-driven. Invoked through a session. Subject to role
  permissions.
- **Trigger** — system-driven. Delivered by the machine via
  `self.deliver(key, trigger)`. Not subject to role permissions.

Both are declared in the type with the same shape:

```mc
actions {
    submit: Draft -> PendingCI,
}

triggers {
    ci_passed: PendingCI -> Review,
}
```

### Transition outputs

A handler produces two things:

1. **The new state** — the handler's return expression. The compiler checks
   that it matches the declared target state.

2. **Emitted values** — zero or more typed values emitted via `emit` during
   handler execution. These are ordinary types, not a special declaration form.

Additionally, the runtime generates a **transition notification** whenever the
instance changes state (e.g., when a PullRequest moves from Draft to
PendingCI). These are derived from the type's state declarations and require
no handler code.

### Unified picture

```
         ┌──────────┐
         │ Instance │
         └────┬─────┘
              │
    ┌─────────┴─────────┐
    │   current state   │
    └─────────┬─────────┘
              │
     action or trigger
              │
    ┌─────────┴─────────┐
    │     handler()     │
    │  emit value;      │  ──→  emitted values (V2: → channels)
    │  emit value;      │
    │  return new_state │
    └─────────┬─────────┘
              │
    ┌─────────┴─────────┐
    │    new state      │  ──→  transition notification (V2: → channels)
    └───────────────────┘
```

This is the single execution model behind all state changes.

---

## Declaration Shape

A `@linear type` combines struct-like fields, enum-like states, transition
rules, and access control in a single declaration:

```mc
@linear
type PullRequest = {
    // Fields — struct-like, shared across all states
    id: u64,
    author: UserId,
    reviewers: List<UserId>,

    // States — enum-like variants, each can carry a payload
    states {
        Draft,
        PendingCI,
        Review { comments: List<Comment> },
        Approved,
        @final Merged,
    }

    // Actions — client-driven transitions, invoked through sessions
    actions {
        submit: Draft -> PendingCI,
        revise(note: string): Draft -> Draft,
        comment(text: string): Draft -> Draft,
        comment(text: string): Review -> Review,
        add_reviewer(reviewer: UserId): Draft -> Draft,
        approve: Review -> Approved,
        reject(reason: string): Review -> Draft,
        merge: Approved -> Merged,
    }

    // Triggers — system-driven transitions, delivered by the machine
    triggers {
        ci_passed: PendingCI -> Review,
        ci_failed(reason: string): PendingCI -> Draft,
    }

    // Roles — permission sets over actions
    roles {
        Author { submit, revise, comment, add_reviewer, merge }
        Reviewer { comment, approve, reject }
    }
}
```

The structure mirrors what programmers already know:

| Part | Analogy | Purpose |
|------|---------|---------|
| Top-level fields | Struct fields | Data shared across all states |
| `states { }` | Enum variants | Possible states, with optional payloads |
| `actions { }` | Transition rules | Client-driven state changes |
| `triggers { }` | Transition rules | System-driven state changes |
| `roles { }` | Access control | Who can invoke which actions |

## Direct Mode (Typestate)

A linear type without roles or triggers is a plain typestate and can be used
directly without machine hosting:

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

// Direct mode — caller owns the value, no machine involved
let door = Door::Closed {};
let door = door.open();   // door: Door::Open
let door = door.close();  // door: Door::Closed
```

The compiler checks linear ownership and state progression. Matching on states
works exactly like matching on enum variants:

```mc
match door {
    closed: Closed => { closed.open(); }
    open: Open => { open.close(); }
}
```

## Fields

Top-level fields are declared like struct fields — flat, comma-separated, before
any sub-blocks. They persist across all state transitions and can be read and
modified by action and trigger handlers.

Fields model relationships and metadata: an author, a list of reviewers,
creation timestamps, etc. Relationships to other types are plain typed values
(e.g., `UserId`, `List<UserId>`) — not a special relationship construct.

## Identity Key

The identity key is a regular field in the type. Which field serves as the
identity key is declared at the machine hosting site:

```mc
machine PRService hosts PullRequest(key: id) { ... }
```

This keeps the type declaration clean — the `id` field is just data. The
machine designates it as the identity key, meaning:
- it uniquely identifies an instance within the hosting machine,
- it is stable across the instance's entire lifecycle (immutable once assigned),
- it is the basis for session targeting (`resume` uses it to find the instance),
- it is the persistence key (the machine uses it to load/store instance state),
- it must be a type that supports equality comparison and hashing (e.g., `u64`,
  `string`, or a struct key type).

Direct-mode linear types (used as typestates) do not need a key.

## States

States are declared as enum variants inside a `states { }` block:

```mc
states {
    Draft,
    Active { count: u64 },
    @final Completed,
}
```

- States may carry state-specific fields (payloads), just like enum variants.
- `@final` marks a terminal state. No actions or triggers may have a `@final`
  state as their source.
- Top-level fields are available in all states. State-specific fields are only
  available in that state.
- Matching on states uses the same syntax as matching on enums.

## Actions

Actions are declared inside an `actions { }` block:

```mc
actions {
    submit: Draft -> PendingCI,
    revise(note: string): Draft -> Draft,
    comment(text: string): Draft -> Draft,
    comment(text: string): Review -> Review,
    write(data: Bytes): Open -> Open | IoError,
}
```

`name(params...): SourceState -> TargetState` declares a client-driven state
transition. Actions are invoked through sessions.

Actions may declare error types after a `|`:
`name(params...): SourceState -> TargetState | ErrorType`. A fallible action
can fail with the declared error type instead of transitioning. Multiple error
types are separated by `|`:
`connect(addr: Address): Disconnected -> Connected | DnsError | TimeoutError`.

- Same-named actions with different source states are allowed (state-dispatch).
- Same-named actions with the same source state are disallowed.

## Triggers

Triggers are declared inside a `triggers { }` block:

```mc
triggers {
    ci_passed: PendingCI -> Review,
    ci_failed(reason: string): PendingCI -> Draft,
}
```

`name(params...): SourceState -> TargetState` declares a system-driven state
transition. Triggers are delivered by the machine to instances, caused by
external systems, correlated replies from other machines, timers, or other
asynchronous sources.

Triggers follow the same syntax rules as actions. The distinction is in how they
are invoked: actions are invoked by session holders; triggers are delivered by
the machine via `self.deliver()`. Both undergo the same runtime state check: if
the instance is not in the trigger's declared source state, delivery fails with
a typed result (see [02-machine.md](02-machine.md#selfdeliverkey-trigger)).

## Roles

Roles are declared inside a `roles { }` block:

```mc
roles {
    Author { submit, revise, comment, add_reviewer, merge }
    Reviewer { comment, approve, reject }
}
```

Each role lists its permitted actions. Roles only constrain actions, not
triggers — triggers are system-driven and not subject to role permissions.

## Method Blocks

Action implementations are provided in a method block, separate from the type
declaration — using Machina's standard method block syntax:

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
```

Methods whose names match declared actions are validated as action
implementations. The compiler checks that:
- The receiver type matches the action's source state.
- The return type matches the action's target state (or the declared error
  types for fallible actions).
- The parameter list matches the action's declared parameters.

When an action name is unique (only one source state), bare `self` is allowed —
the compiler infers the source state. When multiple actions share the same name,
the receiver annotation identifies which source-state transition this method
implements:

```mc
// comment is declared for both Draft -> Draft and Review -> Review
fn comment(self: Draft, text: string) -> Draft { self }
fn comment(self: Review, text: string) -> Review { self }
```

A method block can also contain regular (non-action) methods:

```mc
PullRequest :: {
    fn submit(self) -> PendingCI { self }
    fn add_reviewer(self, reviewer: UserId) -> Draft {
        self.reviewers.push(reviewer);
        self
    }
    fn comment(self: Draft, text: string) -> Draft { self }
    fn comment(self: Review, text: string) -> Review { self }
    fn approve(self) -> Approved { self }
    fn reject(self, reason: string) -> Draft { self }
    fn merge(self) -> Merged { self }

    // Regular method — not an action, no transition
    fn reviewer_count(self) -> u64 {
        self.reviewers.len()
    }
}
```

Every declared action must have a matching method in the method block. This
provides the base implementation that runs in direct mode and serves as the
default when hosted by a machine.

Fallible action methods return a result type:

```mc
File :: {
    fn write(self, data: Bytes) -> Open | IoError {
        // may fail with IoError
        self.buffer.append(data)?;
        self
    }
}
```

## Events (V2)

In V2, an `events { }` block declares which types the linear type can emit.
Event types are plain types defined elsewhere and referenced by name:

```mc
type CommentAdded = { author: UserId, text: string }
type ReviewerAssigned = { reviewer: UserId }

@linear
type PullRequest = {
    // ... fields, states, actions, triggers, roles ...

    events {
        CommentAdded,
        ReviewerAssigned,
    }
}
```

The `events` block constrains what handlers can `emit`. The compiler validates
that `emit` calls inside machine handlers only emit declared event types.

## Blocking Semantics (Direct Mode)

In direct mode, action calls are regular function calls — always blocking,
always synchronous. There is no machine, no mailbox, no round-trip:

```mc
let door = Door::Closed {};
let door = door.open();   // immediate function call, returns Door::Open
let door = door.close();  // immediate function call, returns Door::Closed
```

Non-blocking (`nowait`) is only available in hosted mode, where a machine
mediates the interaction (see [03-session.md](03-session.md#blocking-semantics)).

## What a Linear Type Does Not Define

A linear type declares the workflow graph and provides base action
implementations via method blocks. It does not define:
- storage strategy,
- transport semantics,
- infrastructure concerns (the machine provides those via overrides).
