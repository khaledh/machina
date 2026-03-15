# Cross-Machine Composition

## V1: Event-Based Coordination

In V1, inter-machine communication uses two primitives:

- **`emit`** — a handler stages a typed value for the runtime to collect.
  The runtime decides how to route it (see [02-machine.md](02-machine.md)
  for `emit` semantics). Routing policy is a runtime concern in V1;
  channels formalize it in V2.
- **`on` handlers** — a machine receives external events from its mailbox.
  Messages arrive from other machines, external systems, timers, etc.
  The `on` handler decides how to process them, including routing to
  instances as triggers via `self.deliver()`.

There are no blocking cross-machine calls and no first-class request/reply
semantics.

### Example: CI Integration

The PR service emits a value when a PR is submitted. The runtime routes it
(how is a deployment concern in V1). Later, a CI result arrives at the
machine's mailbox as an external event:

```mc
// PRService action override — emit signals CI should run
action submit(draft) -> PendingCI {
    emit RunCI { pr_id: draft.id };   // runtime collects for routing
    draft.submit()
}

// PRService on handler — receives CI result from mailbox
on CIResult(result) {
    if result.success {
        self.deliver(result.pr_id, CIPassed { commit_sha: result.sha });
    } else {
        self.deliver(result.pr_id, CIFailed { reason: result.reason });
    }
}
```

The outbound side (`emit`) and inbound side (`on`) are decoupled — the
machine does not hold a handle to the CI service or specify a destination.
The runtime infrastructure handles delivery in both directions.

This model covers:
- Notifications and auditing
- Asynchronous background work
- External system integration (CI, auth, payments)
- "Tell, don't ask" coordination

## What V1 Does Not Cover

V1 does not support:
- Blocking cross-machine session calls inside handlers
- First-class request/reply contracts between machines
- Typed correlation between outgoing events and incoming replies
- Protocol-level sequencing across machine boundaries
- Guaranteed routing policy (V2 channels address this)

These are intentionally out of scope. The mechanical problems they introduce
(deadlock under mailbox serialization, cross-machine atomicity, session
ownership inside machines) require a carefully designed abstraction. See
[10-inter-machine-communication.md](10-inter-machine-communication.md) for
the full analysis and V2 direction.

## Relationship to Other Constructs

### Usage Tiers

- **Direct linear type** (no roles): Caller owns the value. Method block
  provides action implementations; calls are direct function calls.
- **Hosted linear type** (roles + machine): managed workflow with sessions.
  Method block provides base implementations; machine overrides as needed.
- **Full linear type** (roles + triggers + machine): complete model with
  system-driven transitions.

The compiler shares state-struct generation, linear checking, and
state-dispatch logic between direct and hosted modes.

### `on` Handlers

`on` handlers are machine-level message handlers. They process events from
the machine's general mailbox — external events from other machines, timers,
administrative commands, etc.

`action` and `trigger` handlers are instance-level handlers that operate on
specific instances.
