# Linear Types

`@linear type` is Machina's primary stateful-modeling feature.

It gives us one surface for two related jobs:

- direct-mode lifecycle-safe values
- hosted long-lived workflows driven through machines

Without roles or triggers, a linear type behaves like direct typestate. Add
roles and a hosting machine, and the same model becomes a persisted, resumable
workflow with async events.

## Direct Mode

Direct-mode linear types are local values whose legal transitions are encoded in
the type system.

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
    fn open(self) -> Open {
        Open {}
    }

    fn close(self) -> Closed {
        Closed {}
    }
}

fn main() {
    let door = Door::Closed {};
    let door = door.open();
    let _door = door.close();
}
```

Why this is useful:

- invalid transitions fail at compile time
- consumed values cannot be reused
- lifecycle rules live in the type itself instead of in ad hoc runtime checks

Common direct-mode uses:

- builders with strict ordering
- startup/shutdown lifecycles
- handles/resources that must be opened before use
- request objects that should not be misused after completion

Runnable examples:

- `examples/linear/door.mc`
- `examples/linear/connection.mc`
- `examples/linear/request_builder.mc`

## Basic Shape

```mc
@linear
type Connection = {
    fd: u64,
    retries: u64,

    states {
        Disconnected,
        Connected,
    }

    actions {
        connect: Disconnected -> Connected,
        disconnect: Connected -> Disconnected,
    }
}
```

Pieces:

- top-level fields persist across states
- `states { ... }` defines the allowed lifecycle phases
- `actions { ... }` defines client-driven transitions
- the method block provides the transition bodies

## Hosted Workflows

Add roles and a hosting machine, and a linear type becomes a long-lived
workflow entity.

```mc
@linear
type Approval = {
    id: u64,

    states {
        Review,
        Approved,
    }

    actions {
        approve: Review -> Approved,
    }

    roles {
        Reviewer { approve }
    }
}

Approval :: {
    fn approve(self) -> Approved {
        Approved {}
    }
}

machine ApprovalService hosts Approval(key: id) {
    fn new() -> Self { Self {} }
}

fn main() -> () | MachineError | SessionError {
    let service = ApprovalService::spawn()?;
    let review = service.create(Approval as Reviewer)?;
    let _approved = review.approve()?;
}
```

Hosted mode adds:

- `create(...)` to start a hosted instance
- `resume(...)` to re-enter a workflow later by key
- `send(...)` to deliver machine messages
- `lookup(...)` for advisory reads without opening a session
- `wait()` for trigger-driven transitions
- stale-session rejection when an instance changed underneath a caller

Runnable examples:

- `examples/linear/approval_hosted.mc`
- `examples/linear/payment_lifecycle.mc`

## Roles and Permissions

Roles constrain which actions are available on a hosted session.

```mc
roles {
    Merchant { authorize, capture }
    Compliance { refund }
}
```

That means the session type is doing two jobs at once:

- tracking the current state
- tracking the caller's permissions

If an action is invalid for either reason, it fails before the program runs.

## Triggers and Machines

Machines host instances of linear types and route external events into them.

```mc
type FraudAlert = {
    payment_id: u64,
}

machine PaymentService hosts Payment(key: id) {
    fn new() -> Self { Self {} }

    trigger FraudAlert(payment) {
        payment;
        Declined {}
    }

    on FraudAlert(event) {
        let _result = self.deliver(event.payment_id, event);
    }
}
```

That gives us the full hosted story:

- external message arrives
- machine `on` handler runs
- `self.deliver(...)` routes it to the right instance
- trigger handler executes
- instance state changes

## Machine Handles

Hosted machines are passed around as `Machine<T>` handles.

That means real workflows can be structured as ordinary functions:

```mc
fn checkout(service: Machine<PaymentService>) -> u64 | MachineError | SessionError {
    let created = service.create(Payment as Merchant)?;
    created.id
}
```

This matters because the workflow usually spans multiple callers and time
boundaries. The key plus `Machine<T>` handle is how we re-enter the typed world
from a queue message, webhook, or database row.

## What To Read Next

- `../why-machina.md` for the full payment walkthrough
- `../compiler/entity-session/00-overview.md` for the design model
- `../compiler/entity-session/03-session.md` for hosted session behavior

## Legacy Note

The older `typestate` guide is still available at `typestate.md` as a migration
note, but `@linear type` is the primary path going forward.
