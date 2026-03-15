# Inter-Machine Communication

## Current Scope (V1)

The `@linear type` + hosted machine + session model provides strong semantics
for a single entity's lifecycle:

- **States** define valid configurations
- **Actions** define valid transitions with typed parameters and results
- **Triggers** define event-driven transitions
- **Roles** define who may perform which actions
- **Sessions** provide typed client-to-machine interaction (create/resume)
- **Persistence** is the machine's responsibility, keyed by identity field

For inter-machine communication, V1 provides **fire-and-forget primitives**:

- `send(target, value)` — point-to-point delivery to a specific machine's
  mailbox via a `Machine<T>` handle
- `emit value` — undirected event production (runtime-collected, no
  specific destination)
- `on` handlers — receive events from the machine's mailbox
- `self.deliver(key, event)` — route mailbox events to hosted instance
  triggers

`send` makes routing explicit in source code. `emit` is for undirected
production (auditing, logging, channel routing in V2). Replies to a `send`
arrive as separate inbound events — correlation is manual.

In the current implementation, destination handles are usually wired through
machine constructors and carried as immutable machine config.

These primitives are sufficient for:
- Notifications and auditing
- Asynchronous background work
- External system integration
- "Tell, don't ask" coordination

## The Gap

States, roles, and actions constrain **operations on an entity**. They do not
constrain **interactions between machines**.

What the current model cannot cleanly express:

- Machine A sends `AuthCheck` to Machine B and must eventually receive either
  `AuthApproved` or `AuthDenied`
- Only those replies are legal
- Replies must correlate to the original request
- After sending `AuthCheck`, only certain next messages are valid
- Participant roles in the interaction itself (distinct from entity roles)

This is the space the retired `protocol` feature was trying to cover.

### Two Layers of Semantics

There are two distinct kinds of constraints:

**Entity semantics** (V1 — solved):
- Lifecycle states and transitions
- Role-based permissions
- Persisted state and identity
- Local invariants

**Interaction semantics** (not yet addressed):
- Request/reply contracts between machines
- Correlation between request and response
- Valid message sequences across machine boundaries
- Multi-machine conversation rules
- Deadlock-safe coordination

These are related but not the same abstraction. Attempting to force interaction
semantics into entity roles and actions would likely produce a muddy model.

## V1 Boundary

V1 explicitly does not promise:
- First-class inter-machine request/reply
- Protocol sequencing between machines
- Blocking conversational semantics between machines

This is an intentional boundary, not an oversight. The mechanical problems
that request/reply introduces are substantial:

- **Deadlock**: if Machine A's handler blocks on Machine B, and B's handler
  blocks on A, the mailbox-serialized model deadlocks
- **Cross-machine atomicity**: if A calls B successfully but A's own transition
  then fails, B has already transitioned with no rollback
- **Session ownership**: unclear lifetime semantics for machine handles held
  inside other machines

These are not small details — they require a carefully designed abstraction.

## V2 Direction: Interactions as Linear Types

The most promising direction for V2 is to model **the interaction itself** as
a linear type.

A conversation between two machines has lifecycle-shaped properties:
- It has states: `Requested`, `Replied`, `TimedOut`, `Completed`
- It has transitions: send request, receive reply, time out
- It has linearity: you cannot forget an in-flight request, replies must
  correspond to outstanding interactions, completion consumes the interaction

This means V2 might not need a wholly new construct. Instead, interaction
lifecycles could be a **composition pattern** over existing linear types, with
channels (04-channel.md) as the transport. This would be a much smaller
language surface than reviving the old protocol feature.

### Replies as Future Events

To avoid deadlock, replies should be modeled as **asynchronous events**, not
synchronous blocking returns. Machine A emits `AuthCheck`; sometime later,
Machine A's `on` handler receives `AuthApproved` or `AuthDenied`.

This keeps the mailbox-serialized execution model clean but introduces the
central design problem: **correlation**.

### Correlation: The Central Design Problem

When a reply arrives as a future event, the receiving machine must know which
in-flight interaction it belongs to.

**Option A — Manual correlation (minimal language support):**
- Developer stores request IDs in entity fields
- Reply handler matches and updates manually
- Simple runtime model, but noisy and error-prone
- Weakens the "compiler helps enforce the design" story

**Option B — Language-supported correlation (recommended for V2):**
- Typed correlation token or interaction handle
- A linear "pending interaction" value that must be consumed
- Reply handling is tied back to the interaction type
- Better fit for Machina's goals: illegal states are harder to represent

V1 can tolerate manual correlation in experiments. But if V2 makes
request/reply real, the language should help with correlation — otherwise
developers will rebuild protocol problems in ad-hoc user code.

## Driving Use Case: Auth Check

Before designing V2 interaction semantics, write the auth-check flow
end-to-end using only V1 primitives. This makes the pain points concrete.
See [11-auth-check-v1.md](11-auth-check-v1.md) for the full worked example.

### The scenario

1. Machine A (e.g., `OrderService`) needs authorization before proceeding
2. Machine A sends `AuthCheck { order_id, user_id }` to Machine B via `send(self.auth_service, ...)`
3. Machine B (`AuthService`) handles it, decides, sends `AuthApproved { order_id }` or `AuthDenied { order_id, reason }` back via `send(self.order_service, ...)`
4. Machine A receives the reply in an `on` handler
5. Machine A must correlate `order_id` back to the waiting entity
6. Machine A transitions the entity based on the result

### What to look for

- Where is correlation managed? (entity fields, separate lookup, etc.)
- What happens if the reply never arrives? (no timeout mechanism in V1)
- What happens if a duplicate reply arrives? (no idempotency guarantee)
- How verbose is the bookkeeping relative to the actual business logic?
- What errors are possible that the compiler cannot catch?

The answers will determine what V2 correlation support should look like.

## Summary

| Aspect | V1 | V2 (proposed) |
|---|---|---|
| Entity lifecycle | Fully supported | Same |
| Client-to-machine | Sessions (create/resume) | Same |
| Machine-to-machine | `send` (point-to-point) + `emit` (undirected) | Interaction lifecycles |
| Routing | Explicit via `send(target, value)` | + channels for fan-out |
| Request/reply | Not supported | Async with typed correlation |
| Correlation | Manual | Language-supported |
| Blocking semantics | Direct mode only | No cross-machine blocking |
