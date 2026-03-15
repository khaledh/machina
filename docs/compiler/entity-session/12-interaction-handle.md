# Interaction Handle Concept

> This note is the first narrow design step for `#182`. It does **not** try to
> solve the entire interaction model. It defines only the core concept: what an
> interaction handle is, how it relates to `send(...)`, and how it gives
> correlation a first-class home.

## Goal

V1 already gives us explicit point-to-point routing:

```mc
send(self.auth_service, AuthCheck {
    order_id: draft.id,
    user_id: draft.user_id,
});
```

That solves routing, but not the rest of the conversation:
- What reply is expected?
- Which in-flight request does an inbound reply belong to?
- How does the compiler help connect the outbound request to the later reply?

The interaction handle concept addresses that gap without immediately designing
timeouts, retries, or full protocol sequencing.

## Core Idea

An interaction starts with a directed send, but instead of treating that send as
an isolated fire-and-forget event, the language models the **pending
conversation** as a value.

That value is an **interaction handle**.

Conceptually, that handle is created *from* a directed send. The exact surface
syntax is intentionally left open at this stage.

The interaction handle is not the reply itself. It is the typed representation
of:
- the fact that a request is in flight
- the destination machine
- the interaction's correlation identity
- the set of replies this interaction expects

The exact syntax is still open. The important part is the model:
- `send(...)` remains the routing primitive
- the interaction handle is the semantic object layered on top

## Relationship to `send(...)`

This design keeps the current routing story intact:

- `send(target, value)`
  - explicit point-to-point delivery
  - no reply semantics
  - valid in V1

- interaction handle
  - a V2 abstraction built on directed delivery
  - introduces typed correlation and pending-conversation state

So the interaction handle is **not** a replacement for `send(...)`.
It is the first step beyond it.

## What the Handle Carries

At minimum, an interaction handle should carry:

1. **Destination identity**
   - which machine was contacted

2. **Request identity**
   - the outbound request type and payload shape

3. **Correlation identity**
   - the token or key that ties future inbound replies back to this interaction

4. **Expected reply set**
   - the reply types that are valid for this interaction

5. **Pending state**
   - the fact that this interaction is still unresolved

The key move is that correlation stops being an ad hoc field repeated through
every event and instead becomes part of the interaction's own typed state.

## Correlation

This is the main reason to introduce the handle at all.

In V1 auth-check flow, correlation is entirely manual:
- `order_id` is copied into `AuthCheck`
- `order_id` is copied into `AuthApproved`
- `order_id` is copied into `AuthDenied`
- `OrderService` manually matches the inbound reply back to the waiting order

That works, but the compiler cannot help much.

With an interaction handle, the design target is:
- the outbound request creates a pending interaction value
- later inbound replies are matched against that interaction value
- the correlation rule is part of the model, not user bookkeeping

The exact mechanism is still open. It could be:
- a runtime-managed opaque interaction id
- a typed correlation token surfaced to user code
- a compiler-synthesized link between the outbound request and allowed replies

This note does **not** choose between those yet.

## Auth-Check Sketch

Using the auth-check example as the concrete pressure test, the conceptual shift
would be:

### V1

```mc
send(self.auth_service, AuthCheck {
    order_id: draft.id,
    user_id: draft.user_id,
});
```

Later:

```mc
on AuthApproved(approval) {
    self.deliver(approval.order_id, approval);
}
```

### V2 Direction

Conceptually:

```mc
// Directed send + a pending interaction handle.
send(self.auth_service, AuthCheck {
    order_id: draft.id,
    user_id: draft.user_id,
});
```

Later, the machine would not just receive `AuthApproved`; it would resolve a
pending interaction created from that directed send.

This is the conceptual improvement we want:
- routing remains explicit
- reply handling stays asynchronous
- correlation becomes part of the interaction model

## Deliberately Out of Scope for This Note

This note does **not** yet decide:
- timeout semantics
- duplicate reply handling
- retry behavior
- whether reply completeness is enforced statically
- exact surface syntax for creating or resolving interaction handles
- whether channels participate directly in interaction resolution

Those should follow from the core interaction-handle model, not be solved
before it exists.

## Next Step

The next useful design move is to rewrite the auth-check flow in
[11-auth-check-v1.md](11-auth-check-v1.md) using the interaction-handle concept
and see whether the result is actually simpler and safer.
