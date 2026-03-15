# Auth Check: V1 Event-Only Flow

> This document walks through a cross-machine auth-check scenario using only
> V1 primitives (`emit`, `on`, `self.deliver()`, manual correlation). The goal
> is to surface the exact pain points that a V2 interaction model should
> address.

## Scenario

An `OrderService` manages orders. Before an order can be confirmed, it needs
authorization from an `AuthService`. The auth check is asynchronous — the
order transitions to a pending state while waiting for a response.

## Type Declarations

```mc
// --- Event types (plain structs) ---

type AuthCheck = {
    order_id: u64,
    user_id: u64,
}

type AuthApproved = {
    order_id: u64,
}

type AuthDenied = {
    order_id: u64,
    reason: string,
}

// --- Order entity ---

@linear
type Order = {
    id: u64,
    user_id: u64,
    total: u64,

    states {
        Draft,
        PendingAuth,
        Confirmed,
        Rejected,
        @final Cancelled,
    }

    actions {
        submit: Draft -> PendingAuth,
        cancel: Draft -> Cancelled,
    }

    triggers {
        AuthApproved: PendingAuth -> Confirmed,
        AuthDenied: PendingAuth -> Rejected,
    }

    roles {
        Buyer { submit, cancel }
    }
}

Order :: {
    fn submit(self) -> PendingAuth { self }
    fn cancel(self) -> Cancelled { self }
}
```

## Machine Definitions

### OrderService

```mc
machine OrderService hosts Order(key: id) {
    fn new() -> Self {
        Self {}
    }

    // Override submit to emit an auth check request
    action submit(draft) -> PendingAuth {
        emit AuthCheck {
            order_id: draft.id,
            user_id: draft.user_id,
        };
        draft.submit()
    }

    // Trigger handlers — delivered via self.deliver()
    trigger AuthApproved(pending) {
        Confirmed {}
    }

    trigger AuthDenied(pending) {
        Rejected {}
    }

    // on handlers — receive events from mailbox
    on AuthApproved(approval) {
        self.deliver(approval.order_id, approval);
    }

    on AuthDenied(denial) {
        self.deliver(denial.order_id, denial);
    }
}
```

### AuthService

```mc
machine AuthService {
    fn new() -> Self {
        Self {}
    }

    on AuthCheck(check) {
        // Business logic: check user permissions, fraud rules, etc.
        if self.is_authorized(check.user_id, check.order_id) {
            emit AuthApproved { order_id: check.order_id };
        } else {
            emit AuthDenied {
                order_id: check.order_id,
                reason: "insufficient funds",
            };
        }
    }
}
```

### Client

```mc
fn main() -> () | MachineError | SessionError {
    let service = OrderService::spawn()?;

    let buyer = service.create(Order as Buyer)?;
    let pending = buyer.submit()?;
    // pending: Order::PendingAuth — no Buyer actions available

    // Block until a trigger moves the order
    let next = pending.wait()?;
    match next {
        confirmed: Confirmed => {
            println("order confirmed");
        }
        rejected: Rejected => {
            println("order rejected");
        }
    }
}
```

## The Flow

```
Client                  OrderService              Runtime              AuthService
  |                          |                        |                      |
  |-- submit() ------------>|                        |                      |
  |                          |-- emit AuthCheck ---->|                      |
  |                          |<-- PendingAuth -------|                      |
  |<-- PendingAuth ---------|                        |                      |
  |                          |                        |-- AuthCheck ------->|
  |   (waiting)              |                        |                      |
  |                          |                        |                      |-- decide
  |                          |                        |<-- emit AuthApproved|
  |                          |<-- AuthApproved ------|                      |
  |                          |-- self.deliver() ---->|                      |
  |                          |   (trigger handler)   |                      |
  |                          |<-- Confirmed ---------|                      |
  |<-- Confirmed ------------|                        |                      |
```

The runtime sits between machines and handles event routing. In V1, routing
policy is a runtime/deployment concern — the language does not specify how
`emit AuthCheck` reaches `AuthService` or how `emit AuthApproved` reaches
`OrderService`.

## Pain Points

### 1. Manual Correlation

The `order_id` field appears in every event type — `AuthCheck`, `AuthApproved`,
`AuthDenied`. The developer must:
- Include the correlation key in every event struct
- Copy it correctly at every emit site
- Match on it in every `on` handler

The compiler cannot verify that `AuthApproved.order_id` refers to a valid
pending order, that it matches the original `AuthCheck.order_id`, or that the
developer didn't forget to include it. Correlation is entirely manual and
error-prone.

**V2 question**: Should the language provide a typed correlation token that
connects an outgoing event to its expected replies?

### 2. No Timeout Mechanism

If `AuthService` never responds, the order stays in `PendingAuth` forever.
The client's `wait()` blocks indefinitely. There is no V1 mechanism for:
- Declaring a timeout on a pending state
- Automatically transitioning on timeout (e.g., `PendingAuth -> Cancelled`)
- Notifying the waiting client that the interaction timed out

The developer could work around this with an external timer that emits a
`AuthTimeout` event, but this requires:
- Declaring `AuthTimeout` as a trigger type
- Adding a `PendingAuth -> Cancelled` trigger transition
- Setting up external timer infrastructure
- More manual correlation (which order timed out?)

**V2 question**: Should pending interactions have declarative timeout
semantics?

### 3. No Duplicate Protection

Nothing prevents `AuthService` from emitting `AuthApproved` twice for the
same `order_id`. The second delivery would fail with `InvalidState` (the
order is already `Confirmed`), but:
- The `on` handler must handle or ignore the `InvalidState` result
- There is no language-level idempotency guarantee
- The developer must decide what "duplicate reply" means for their domain

This is manageable but noisy. The `on` handler becomes defensive:

```mc
on AuthApproved(approval) {
    match self.deliver(approval.order_id, approval) {
        _: Delivered => {}
        _: InvalidState => {}      // already moved past PendingAuth
        _: InstanceNotFound => {}  // order was cancelled
    }
}
```

**V2 question**: Should interaction tokens enforce at-most-once reply
semantics?

### 4. No Reply Completeness Check

The compiler cannot verify that `AuthService` handles all possible reply
types. If the developer forgets to emit `AuthDenied` in the failure branch,
the order silently hangs in `PendingAuth`. There is no compile-time contract
saying "an `AuthCheck` must eventually produce either `AuthApproved` or
`AuthDenied`."

**V2 question**: Should interaction types declare their valid reply set,
enabling the compiler to check completeness?

### 5. Routing is Opaque

The `emit AuthCheck { ... }` call gives no indication of where the event
goes. The developer must know (from documentation or convention) that
`AuthService` handles `AuthCheck` events. If a second machine also declares
`on AuthCheck(...)`, the behavior is undefined — the runtime picks one, both,
or neither, depending on deployment configuration.

This is intentional in V1 (routing is a runtime concern), but it means the
developer cannot reason about event flow from the source code alone.

**V2 question**: Should channels or subscriptions make routing explicit and
compiler-checkable?

### 6. Verbosity vs Business Logic

The actual business logic is small:
- Submit an order → check auth → confirm or reject

But the V1 implementation requires:
- 3 event type declarations (AuthCheck, AuthApproved, AuthDenied)
- Manual correlation fields in each
- 2 trigger declarations
- 2 trigger handlers
- 2 on handlers (one per reply type)
- Defensive match on deliver results

The ceremony-to-logic ratio is high. Much of this is structural bookkeeping
that a V2 interaction model could eliminate.

## What the Compiler Can and Cannot Help With

| Aspect | Compiler support in V1 |
|--------|----------------------|
| Order lifecycle (states, transitions) | Full — compile-time checked |
| Role permissions (Buyer can submit) | Full — compile-time checked |
| Linear consumption (no double-submit) | Full — compile-time checked |
| Session state tracking (wait after submit) | Full — compile-time checked |
| Correlation (order_id matches) | None — manual |
| Reply completeness (all branches emit) | None — manual |
| Timeout (PendingAuth doesn't hang) | None — external infrastructure |
| Duplicate protection (at-most-once reply) | Partial — InvalidState at runtime |
| Routing (AuthCheck reaches AuthService) | None — runtime concern |

## V2 Requirements Derived from This Example

If V2 introduces interaction semantics, this example suggests the following
requirements:

1. **Typed correlation**: connecting an outgoing event to its expected replies
   without manual field threading.
2. **Reply contract**: declaring the valid reply set for an interaction
   (e.g., `AuthCheck -> AuthApproved | AuthDenied`) with compiler-checked
   completeness.
3. **Timeout semantics**: declarative timeout on pending interactions with
   automatic state transition.
4. **At-most-once delivery**: interaction tokens that prevent duplicate
   replies at the language level.
5. **Explicit routing**: making event destinations visible in source code,
   either via channels or typed subscriptions.

These map naturally to the "interactions as linear types" direction outlined
in [10-inter-machine-communication.md](10-inter-machine-communication.md):
an interaction type would declare states (Pending, Approved, Denied, TimedOut),
enforce linear consumption (must be resolved), and carry correlation
implicitly.
