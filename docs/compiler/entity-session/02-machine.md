# Machine Declaration

A machine hosts instances of a linear type. The type's method block provides
base action implementations. The machine **overrides** only the actions that
need infrastructure access, and provides trigger and `on` handlers.

```mc
type CIPassed = { commit_sha: string }
type CIFailed = { reason: string }

machine PRService hosts PullRequest(key: id) {
    fields {
        ci_service: Machine<CIService>,
    }

    fn new(ci_service: Machine<CIService>) -> Self {
        Self { ci_service: ci_service }
    }

    // --- Action override (only for actions needing infrastructure) ---

    action submit(draft) -> PendingCI {
        send(self.ci_service, RunCI { pr_id: draft.id });   // point-to-point
        draft.submit()   // calls base implementation from method block
    }

    // No override needed for add_reviewer, approve, reject, merge, revise,
    // comment — the base implementations from PullRequest's method block
    // run directly.

    // --- Trigger handlers (system-driven) ---
    // Trigger names are event types declared externally.

    trigger CIPassed(pending) {
        Review {}
    }

    trigger CIFailed(pending) {
        Draft {}
    }

    // --- Machine-level handlers ---
    // on handlers receive external events from the machine's mailbox.

    on CIResult(result) {
        if result.success {
            self.deliver(result.pr_id, CIPassed { commit_sha: result.sha });
        } else {
            self.deliver(result.pr_id, CIFailed { reason: result.reason });
        }
    }
}
```

## `machine ... hosts Type(key: field)`

Declares that this machine type hosts instances of the named linear type. The
`(key: field)` clause designates which field of the linear type serves as the
identity key for instance lookup, session targeting, and persistence.

The compiler validates that the machine provides a handler for every trigger
declared in the type. Action overrides are optional — if the machine does not
override an action, the base implementation from the type's method block runs
directly.

## `fields { ... }`

Machine-level fields. These are the machine's own state, not part of any
instance. Typical uses include handles to other machines, configuration,
counters, database connections, etc.

Current implementation note: V1 machine fields are best understood as
**immutable machine configuration** carried in the generated `Machine<T>`
handle. They are initialized by `spawn(...)` via `fn new(...)` and then read
from handlers through `self`. A fuller mutable machine-owned runtime state model
is future work.

## `fn new() -> Self`

Machine constructor. Returns the initial machine configuration. Called by
`PRService::spawn(...)`, which mirrors the constructor parameter list and
forwards its arguments.

## Action Overrides and Trigger Handlers

### Action Overrides

`action name(instance_param, params...) -> TargetState { body }`

An action override replaces the base implementation from the type's method
block. Only actions that need infrastructure access (other machines, databases,
external services) need to be overridden. The override typically performs
infrastructure work and then calls the base implementation:

```mc
action submit(draft) -> PendingCI {
    send(self.ci_service, RunCI { pr_id: draft.id });   // point-to-point
    draft.submit()   // delegates to base implementation
}
```

Actions without overrides run the base implementation directly. This keeps the
machine focused on infrastructure concerns — the domain logic lives in the
type's method block.

### Fallible Action Overrides

An override can declare additional error types beyond those in the base action.
The override's error set must be a superset of the base action's errors:

```mc
// Base action: write(data: Bytes): Open -> Open | IoError
action write(file, data: Bytes) -> Open | IoError | QuotaExceeded {
    if self.quota_remaining(file.id) < data.len() {
        return QuotaExceeded {};
    }
    file.write(data)?    // propagates IoError from base
}
```

### Trigger Handlers

`trigger EventType(instance_param) { body }`

Trigger handlers are always provided by the machine — triggers have no base
implementation in the type's method block because they are system-driven, not
client-driven. The trigger name is the event type name (PascalCase), matching
the externally declared type that appears in the linear type's `triggers` block.

### Handler Parameters

The first parameter is the current instance — its type is inferred from the
type's action/trigger declaration (the source state). For example, if the type
declares `submit: Draft -> PendingCI` in its `actions` block, then the
machine's `action submit(draft)` handler receives `draft` typed as `Draft`. The
author chooses the parameter name, typically something descriptive of the source
state (e.g., `draft`, `review`, `pending`). The body must return the next
state. The compiler validates that the returned type matches the declared target
state.

Inside the handler:
- The first parameter provides access to the type's fields (e.g.,
  `draft.reviewers`, `draft.id`) and state-specific fields (payloads).
- `self` refers to the machine, providing access to machine fields (e.g.,
  `self.ci_service`) and machine operations (e.g., `self.deliver()`,
  `send()`).

In the current implementation, access to machine fields through `self` is
read-only configuration access.

This separation is clear and unambiguous: the instance parameter is the domain
object; `self` is the infrastructure.

### Summary

| Handler type | Provided by | Purpose |
|--------------|-------------|---------|
| Base action methods | Type's method block | Domain logic, runs in direct and hosted mode |
| Action overrides | Machine | Infrastructure wrapping, calls base via `instance.action()` |
| Trigger handlers | Machine | System-driven state transitions |
| `on` handlers | Machine | Machine-level message processing |

## `emit` Statement

Handlers may emit typed values using `emit`:

```mc
action comment(pr, text: string) {
    emit CommentAdded { author: session.user, text: text };
    pr  // state doesn't change
}
```

`emit` accepts any typed value — there is no special event declaration form.
The runtime collects emitted values during handler execution.

In V1, `emit` is a fire-and-forget side effect: emitted values are collected
by the runtime during handler execution. They do not affect control flow or
the session reply — the only thing that determines the next state is the
handler's return expression.

How emitted values are routed is a runtime concern. The runtime may forward
them to other machines' mailboxes, log them, or discard them — the language
does not specify routing policy in V1. In V2, channels formalize this: emitted
values are delivered to co-hosted channels with explicit routing (see
[04-channel.md](04-channel.md)).

Emitted values are ordinary types — the same types that can appear as triggers
in another linear type's `triggers` block. This is the event/trigger
unification: one machine emits an event; another machine's `on` handler may
receive it and deliver it as a trigger via `self.deliver()`.

A handler can emit zero or many values. The final expression is the new state.

## `send` Statement

`send(target, value)` delivers a typed value to a specific machine's mailbox.
Unlike `emit`, which is undirected, `send` has an explicit destination:

```mc
action submit(draft) -> PendingCI {
    send(self.ci_service, RunCI { pr_id: draft.id });
    draft.submit()
}
```

The target is a `Machine<T>` handle — typically a machine field. The value is
any typed value. The target machine receives it via an `on` handler.

`send` is fire-and-forget: it delivers the value to the target's mailbox and
returns immediately. There is no reply, no acknowledgment, and no correlation
with future inbound events. If a reply is needed, it arrives as a separate
event in the sender's mailbox, handled by the sender's own `on` handler with
manual correlation.

### `emit` vs `send`

| | `emit value` | `send(target, value)` |
|---|---|---|
| Destination | None (undirected) | Explicit machine handle |
| Routing | Runtime concern | Point-to-point |
| Use case | Auditing, logging, channel routing (V2) | Cross-machine coordination |
| Reply semantics | None | None (replies are separate inbound events) |

Use `emit` when producing a value without a specific recipient — the runtime
decides what to do with it. Use `send` when delivering to a known machine.

## Handler Execution Model

On every handler completion, the runtime:

1. Receives the new state from the handler return expression.
2. Collects all values emitted via `emit` during handler execution.
3. If new state differs from old state, generates a transition notification
   (e.g., `PullRequest::Review`).
4. Persists the new state.
5. Stages the session reply (if action) with success + new state tag.

In V2, step 5 also delivers emitted values and transition notifications to
co-hosted channels.

## `on` Handlers

`on` handlers are machine-level handlers. They process messages from the
machine's general mailbox — external events from other machines, timers,
administrative commands, etc.

`on` handlers operate on the machine as a whole, not on a specific instance.
They can route external events to instances as triggers using
`self.deliver(key, event)`.

Three handler types coexist in a machine:
- `action` handlers — instance-level, client-driven (via sessions).
- `trigger` handlers — instance-level, system-driven (via `self.deliver()`).
- `on` handlers — machine-level, message-driven (via mailbox).

The typical flow from external input to state change:

```
external message  →  on handler  →  self.deliver(key, event)  →  trigger handler  →  state transition
```

`on` handlers are the entry point for external messages. They decide which
instance to target and which trigger to deliver. `trigger` handlers apply the
state transition. This separation keeps the type's workflow graph clean —
triggers are declared in the type (as event types); how they arrive is the
machine's concern.

## `self.deliver(key, event)`

Routes an event to a specific instance by identity key. The machine's `on`
handler bridges between the mailbox world (external events, timers, etc.)
and the type's state graph. The event must be a type declared in the
hosted linear type's `triggers` block.

`self.deliver()` returns a typed result indicating what happened:

```mc
type DeliverResult =
    | Delivered
    | InstanceNotFound
    | InvalidState
```

The operation is authoritative — it performs instance lookup, state check, and
trigger dispatch atomically within the machine's serialized processing. There
is no TOCTOU gap inside `deliver` itself.

This mirrors the session side: session actions get `SessionError::InvalidState`
when state is stale; `self.deliver()` gets `InvalidState` when a trigger
doesn't apply to the instance's current state. Both callers get a typed
outcome and can react accordingly.

```mc
on CIResult(result) {
    if result.success {
        match self.deliver(result.pr_id, CIPassed { commit_sha: result.sha }) {
            _: Delivered => {}
            _: InstanceNotFound => {
                // PR was removed before CI completed
            }
            _: InvalidState => {
                // PR is no longer in PendingCI
            }
        }
    } else {
        self.deliver(result.pr_id, CIFailed { reason: result.reason });
    }
}
```

When the `on` handler doesn't need to react to delivery failure, the result
can be ignored:

```mc
on CIResult(result) {
    if result.success {
        self.deliver(result.pr_id, CIPassed { commit_sha: result.sha });
    } else {
        self.deliver(result.pr_id, CIFailed { reason: result.reason });
    }
}
```
