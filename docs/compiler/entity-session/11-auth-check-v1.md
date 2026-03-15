# Auth Check: V1 Flow

> This document walks through a cross-machine auth-check scenario using only
> V1 primitives (`send`, `on`, `self.deliver()`, manual correlation). The goal
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
    fields {
        auth_service: Machine<AuthService>,
    }

    fn new(auth_service: Machine<AuthService>) -> Self {
        Self { auth_service: auth_service }
    }

    // Override submit to send an auth check
    action submit(draft) -> PendingAuth {
        send(self.auth_service, AuthCheck {
            order_id: draft.id,
            user_id: draft.user_id,
        });
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
    fields {
        order_service: Machine<OrderService>,
    }

    fn new(order_service: Machine<OrderService>) -> Self {
        Self { order_service: order_service }
    }

    on AuthCheck(check) {
        // Business logic: check user permissions, fraud rules, etc.
        if self.is_authorized(check.user_id, check.order_id) {
            send(self.order_service, AuthApproved { order_id: check.order_id });
        } else {
            send(self.order_service, AuthDenied {
                order_id: check.order_id,
                reason: "insufficient funds",
            });
        }
    }
}
```

### Client

```mc
fn main() -> () | MachineError | SessionError {
    // NOTE: circular dependency — AuthService needs OrderService's handle and
    // vice versa. This is shown schematically; a real V1 program would need
    // deferred handle injection or a coordinator. See Pain Point #5 below.
    let auth = AuthService::spawn(order_service)?;
    let service = OrderService::spawn(auth)?;

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
Client                  OrderService                          AuthService
  |                          |                                      |
  |-- submit() ------------>|                                      |
  |                          |-- send(auth, AuthCheck) ----------->|
  |                          |                                      |
  |<-- PendingAuth ---------|                                      |
  |                          |                                      |-- decide
  |   (waiting)              |                                      |
  |                          |<-- send(order, AuthApproved) -------|
  |                          |                                      |
  |                          |-- on handler                        |
  |                          |-- self.deliver(order_id, ...)       |
  |                          |-- trigger handler -> Confirmed      |
  |                          |                                      |
  |<-- Confirmed ------------|                                      |
```

Routing is explicit — `send` specifies the destination machine via a
`Machine<T>` handle. Each machine holds a handle to the machine it
needs to communicate with.

## Pain Points

### 1. Manual Correlation

The `order_id` field appears in every event type — `AuthCheck`, `AuthApproved`,
`AuthDenied`. The developer must:
- Include the correlation key in every event struct
- Copy it correctly at every send site
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
types. If the developer forgets to send `AuthDenied` in the failure branch,
the order silently hangs in `PendingAuth`. There is no compile-time contract
saying "an `AuthCheck` must eventually produce either `AuthApproved` or
`AuthDenied`."

**V2 question**: Should interaction types declare their valid reply set,
enabling the compiler to check completeness?

### 5. Circular Handle Dependencies

`OrderService` needs a `Machine<AuthService>` to send auth checks.
`AuthService` needs a `Machine<OrderService>` to send replies. This creates
a circular dependency at spawn time — which machine is spawned first?

Workarounds exist (deferred handle injection, a shared coordinator), but
the language doesn't help here. The circular dependency is an artifact of
point-to-point `send` requiring both sides to hold handles to each other.

**V2 question**: Should interaction tokens or channels eliminate the need for
bidirectional handle wiring?

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
| Reply completeness (all branches send) | None — manual |
| Timeout (PendingAuth doesn't hang) | None — external infrastructure |
| Duplicate protection (at-most-once reply) | Partial — InvalidState at runtime |
| Routing (AuthCheck reaches AuthService) | Explicit — `send(target, value)` |
| Handle wiring (circular dependencies) | None — manual at spawn time |

## V2 Requirements Derived from This Example

If V2 introduces interaction semantics, this example suggests the following
requirements:

1. **Typed correlation**: connecting an outgoing send to its expected replies
   without manual field threading.
2. **Reply contract**: declaring the valid reply set for an interaction
   (e.g., `AuthCheck -> AuthApproved | AuthDenied`) with compiler-checked
   completeness.
3. **Timeout semantics**: declarative timeout on pending interactions with
   automatic state transition.
4. **At-most-once delivery**: interaction tokens that prevent duplicate
   replies at the language level.
5. **Handle wiring**: eliminating circular machine handle dependencies for
   request/reply patterns (interaction tokens or channels could carry the
   return address implicitly).

These map naturally to the "interactions as linear types" direction outlined
in [10-inter-machine-communication.md](10-inter-machine-communication.md):
an interaction type would declare states (Pending, Approved, Denied, TimedOut),
enforce linear consumption (must be resolved), and carry correlation
implicitly.

## Conceptual Rewrite with an Interaction Handle

The V1 walkthrough above is still useful because it makes the pain visible.
The next question is whether the interaction-handle concept from
[12-interaction-handle.md](12-interaction-handle.md) actually reduces that
pain in a meaningful way.

This section stays deliberately conceptual. It does **not** choose final
syntax. The point is to test the model, not the spelling.

### Conceptual Shift

In V1, the auth check is represented indirectly:
- `OrderService` sends `AuthCheck`
- `AuthService` later sends `AuthApproved` or `AuthDenied`
- `OrderService` manually correlates the reply back to the waiting order

With an interaction handle, the request is still delivered by directed send,
but the pending auth check is also represented as a typed value:
- the outgoing request creates a pending interaction handle
- the interaction handle carries correlation identity
- later replies resolve that interaction rather than being matched purely by
  user-managed fields

So the conceptual change is:
- **V1**: send now, manually reconstruct the conversation later
- **V2 direction**: send now, keep the conversation alive as a typed value

### Conceptual OrderService Rewrite

In V1, the submit override is:

```mc
action submit(draft) -> PendingAuth {
    send(self.auth_service, AuthCheck {
        order_id: draft.id,
        user_id: draft.user_id,
    });
    draft.submit()
}
```

Conceptually, with an interaction handle:

```mc
action submit(draft) -> PendingAuth {
    // Directed send still performs the routing.
    // The important difference is that the auth check also creates a
    // pending interaction value associated with this order.
    send(self.auth_service, AuthCheck {
        order_id: draft.id,
        user_id: draft.user_id,
    });

    // Conceptually: store or attach the pending interaction to the waiting
    // order/session rather than rebuilding the connection from raw fields.
    draft.submit()
}
```

The routing step is unchanged. The semantic difference is that the machine now
has something explicit to resolve later:
- not just "some reply might arrive"
- but "this specific auth interaction is pending"

### Conceptual Reply Handling Rewrite

In V1, reply handling is shaped around raw event correlation:

```mc
on AuthApproved(approval) {
    self.deliver(approval.order_id, approval);
}
```

Conceptually, with an interaction handle, reply handling is shaped around
resolving the pending interaction first and only then driving the entity:

```mc
on AuthApproved(approval) {
    // Conceptually:
    // 1. resolve the pending auth interaction for this reply
    // 2. then deliver the resolved result to the waiting order
}
```

This is where the design earns its keep. If the interaction-handle model is
real, the compiler/runtime should help with:
- finding the correct in-flight interaction
- rejecting replies that do not match an active interaction
- preserving the connection between the request and the allowed reply set

### What Gets Simpler

The interaction-handle concept appears to improve several things immediately:

1. **Correlation has a home**
   - Instead of threading `order_id` through every reply path as ad hoc
     bookkeeping, the interaction itself owns the pending link between request
     and reply.

2. **Reply handling becomes about resolution**
   - The machine is no longer just receiving unrelated `AuthApproved` values.
     It is resolving a known pending auth interaction.

3. **The model names the missing state explicitly**
   - In V1, "waiting for auth" exists only implicitly in the combination of:
     `PendingAuth` state + copied ids + later `on` handlers.
   - With an interaction handle, the waiting conversation becomes an explicit
     thing the system can reason about.

### What Does *Not* Get Simpler Yet

This conceptual rewrite is useful partly because it shows what remains open:

1. **Timeouts are still unspecified**
   - A pending interaction needs a way to become `TimedOut`, but this note does
     not define it yet.

2. **Duplicate replies are still unspecified**
   - Resolving the same interaction twice should probably be rejected, but that
     rule has not been designed yet.

3. **Reply completeness is still unspecified**
   - The model suggests an expected reply set, but does not yet say how or when
     the compiler checks it.

4. **Circular handle wiring is only partially addressed**
   - The interaction handle may reduce the need to model both directions as raw
     machine handles, but the exact routing/return-address story is still open.

### Pressure-Test Result

This rewrite suggests that the interaction-handle model is worth continuing:
- it does **not** change the routing story
- it gives correlation a proper semantic home
- and it turns "waiting for a reply" into something the language could
  represent directly

But it also shows the next design question clearly:
- once a pending interaction exists, **what are its resolution rules?**

That should be the next focused design step after this comparison.
