# Cross-Machine Composition

## V1: Event-Based Coordination

In V1, inter-machine communication uses three primitives:

- **`send(target, value)`** — delivers a typed value to a specific machine's
  mailbox. The target is a `Machine<T>` handle, typically held as a machine
  field. Point-to-point, fire-and-forget.
- **`emit value`** — stages a typed value for runtime collection. Undirected
  — no specific destination. Used for auditing, logging, and channel routing
  (V2).
- **`on` handlers** — a machine receives events from its mailbox. Messages
  arrive via `send` from other machines, external systems, timers, etc.
  The `on` handler decides how to process them, including routing to
  instances as triggers via `self.deliver()`.

There are no blocking cross-machine calls and no first-class request/reply
semantics.

### Example: CI Integration

The PR service sends a value to the CI service when a PR is submitted. Later,
a CI result arrives at the PR service's mailbox:

```mc
machine PRService hosts PullRequest(key: id) {
    fields {
        ci_service: Machine<CIService>,
    }

    // Action override — send to CI service
    action submit(draft) -> PendingCI {
        send(self.ci_service, RunCI { pr_id: draft.id });
        draft.submit()
    }

    // on handler — receives CI result from mailbox
    on CIResult(result) {
        if result.success {
            self.deliver(result.pr_id, CIPassed { commit_sha: result.sha });
        } else {
            self.deliver(result.pr_id, CIFailed { reason: result.reason });
        }
    }
}
```

The outbound side (`send`) specifies the destination explicitly. The inbound
side (`on`) processes events from the mailbox. Correlation between the
outbound `RunCI` and the inbound `CIResult` is manual — the `pr_id` field
serves as the correlation key.

`send` is also checked statically:
- the target must be a `Machine<T>` handle
- the destination machine must define an `on` handler for the payload type

In the current implementation, machine fields like `ci_service` are wired in
through `spawn(...)`/`new(...)` and behave as immutable machine config carried
in the `Machine<T>` handle.

This model covers:
- Notifications and auditing
- Asynchronous background work
- External system integration (CI, auth, payments)
- "Tell, don't ask" coordination

## What V1 Does Not Cover

V1 does not support:
- Blocking cross-machine session calls inside handlers
- First-class request/reply contracts between machines
- Typed correlation between outgoing sends and incoming replies
- Protocol-level sequencing across machine boundaries

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
the machine's general mailbox — events from `send`, external systems, timers,
administrative commands, etc.

`action` and `trigger` handlers are instance-level handlers that operate on
specific instances.
