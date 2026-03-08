# Machine Declaration

A machine hosts instances of a linear type. The type's method block provides
base action implementations. The machine **overrides** only the actions that
need infrastructure access, and provides trigger and `on` handlers.

```mc
machine PRService hosts PullRequest(key: id) {
    fields {
        ci_service: Machine<CIService>,
    }

    fn new(ci_service: Machine<CIService>) -> Self {
        Self { ci_service: ci_service }
    }

    // --- Action override (only for actions needing infrastructure) ---

    action submit(draft) -> PendingCI {
        request(self.ci_service, RunCI { pr_id: draft.id });
        draft.submit()   // calls base implementation from method block
    }

    // No override needed for add_reviewer, approve, reject, merge, revise,
    // comment — the base implementations from PullRequest's method block
    // run directly.

    // --- Trigger handlers (system-driven) ---

    trigger ci_passed(pending) {
        Review {}
    }

    trigger ci_failed(pending, reason: string) {
        Draft {}
    }

    // --- Machine-level handlers ---

    on CIResult(result) for RunCI(req) {
        if result.success {
            self.deliver(req.pr_id, ci_passed {});
        } else {
            self.deliver(req.pr_id, ci_failed { reason: result.reason });
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

Machine-level fields, persisted across all dispatches. These are the machine's
own state, not part of any instance. Machine fields typically hold handles to
other machines, database connections, configuration, etc.

## `fn new() -> Self`

Machine constructor. Returns initial machine state. Called by
`PRService::spawn()`.

## Action Overrides and Trigger Handlers

### Action Overrides

`action name(instance_param, params...) -> TargetState { body }`

An action override replaces the base implementation from the type's method
block. Only actions that need infrastructure access (other machines, databases,
external services) need to be overridden. The override typically performs
infrastructure work and then calls the base implementation:

```mc
action submit(draft) -> PendingCI {
    request(self.ci_service, RunCI { pr_id: draft.id });
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

`trigger name(instance_param, params...) { body }`

Trigger handlers are always provided by the machine — triggers have no base
implementation in the type's method block because they are system-driven, not
client-driven.

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
  `self.ci_service`) and machine operations (e.g., `self.deliver()`).

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
by the runtime and made available to the machine for logging, auditing, or
internal use (e.g., writing to a log, updating a counter). They do not affect
control flow or the session reply — the only thing that determines the next
state is the handler's return expression. In V2, emitted values are
additionally delivered to co-hosted channels (see
[04-channel.md](04-channel.md)). Also in V2, the type's `events { }` block
constrains what can be emitted (see [01-entity.md](01-entity.md#events-v2)).

A handler can emit zero or many values. The final expression is the new state.

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
machine's general mailbox — lifecycle messages, correlated replies from other
machines, timers, administrative commands, etc.

`on` handlers operate on the machine as a whole, not on a specific instance.
They can route external triggers to instances using
`self.deliver(key, trigger)`.

Three handler types coexist in a machine:
- `action` handlers — instance-level, client-driven (via sessions).
- `trigger` handlers — instance-level, system-driven (via `self.deliver()`).
- `on` handlers — machine-level, message-driven (via mailbox).

The typical flow from external input to state change:

```
external message  →  on handler  →  self.deliver(key, trigger)  →  trigger handler  →  state transition
```

`on` handlers are the entry point for external messages. They decide which
instance to target and which trigger to deliver. `trigger` handlers apply the
state transition. This separation keeps the type's workflow graph clean —
triggers are declared in the type; how they arrive is the machine's concern.

## `self.deliver(key, trigger)`

Routes a trigger to a specific instance by identity key. The machine's `on`
handler bridges between the mailbox world (correlated replies, external
messages) and the type's state graph.

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
on CIResult(result) for RunCI(req) {
    if result.success {
        match self.deliver(req.pr_id, ci_passed {}) {
            _: Delivered => {}
            _: InstanceNotFound => {
                // PR was removed before CI completed
            }
            _: InvalidState => {
                // PR is no longer in PendingCI
            }
        }
    } else {
        self.deliver(req.pr_id, ci_failed { reason: result.reason });
    }
}
```

When the `on` handler doesn't need to react to delivery failure, the result
can be ignored:

```mc
on CIResult(result) for RunCI(req) {
    if result.success {
        self.deliver(req.pr_id, ci_passed {});
    } else {
        self.deliver(req.pr_id, ci_failed { reason: result.reason });
    }
}
```
