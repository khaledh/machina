# Typestate Machines: Async Events + Protocol Flows (Draft)

## Status

Draft proposal, revised after external design review.

This extends the existing typestate prototype (`--experimental typestate`) while
avoiding function-coloring (`async fn`/`await`).

## Terminology

- `typestate`: source-level definition of states and transitions.
- `machine`: runtime instance of a typestate.
- `protocol`: abstract interaction contract.
- `role`: one side of a protocol.
- `flow`: directional protocol rule.

## Review-Driven Decisions

The following are adopted as baseline constraints for this design:

1. Correlation-by-convention is rejected.
2. Managed mode is mailbox-only.
3. Handler semantics are transactional (defined below).
4. V1 conformance scope is explicit: **Shape** only.
5. Typed handles are preferred over raw machine ids at source level.
6. Typestate-level default handlers are supported to control error explosion.

## Problem

Current typestate is great for direct local transitions (`c.connect()`), but we
also need long-lived machines that react to async external and inter-machine
stimuli.

We need that without splitting programs into sync/async worlds.

## Goals

1. Keep handlers synchronous and run-to-completion.
2. Support mailbox-driven managed machines.
3. Support inter-machine communication with protocol contracts.
4. Preserve direct typestate mode for local deterministic code.

## Non-Goals (V1)

1. No general async/await language feature.
2. No full session-type refinement/projection checks.
3. No distributed transport guarantees.

## Design Principle

**Asynchrony lives at the machine boundary, not inside handlers.**

Machine handler model:

`(state, incoming envelope) -> transition result`

where transition result includes next state, emitted effects, or error.

## Core Language Model

### Payloads remain regular types

No new `message` keyword in v1.

```machina
type AuthorizeReq = { user: string }
type AuthApproved = {}
type AuthDenied = {}
```

### Protocols use roles + flows

Flows are declared once at protocol scope and reference named roles.

```machina
protocol Auth {
    role Client;
    role Server;

    flow Client -> Server: AuthorizeReq -> AuthApproved | AuthDenied;
    flow Server -> Client: SessionRevoked;
}
```

### Typestate implements protocol role(s)

```machina
typestate Gateway : Auth::Client { ... }
typestate AuthService : Auth::Server { ... }
```

## Conformance Scope (V1)

V1 checks **Shape conformance** only:
- required incoming flow payloads have handlers,
- outgoing `Send`/`Request`/`Reply` obey protocol flow payload types.

Not in v1:
- coverage/exhaustiveness across all reachable states for all async conditions,
- formal refinement against projected role FSM.

## Event Handling Model

### State-scoped handlers

```machina
state Connected {
    on IncomingRequest(req: IncomingRequest) -> Connected | Closing | ProtocolError {
        ...
    }
}
```

### Typestate-level default handlers

Catch-all handlers can be declared at typestate scope to avoid per-state
boilerplate for cross-cutting failures.

```machina
typestate Connection {
    on WriteFailed(err: WriteFailed) -> Closing {
        Closing
    }
}
```

(Exact precedence rules are specified below.)

### Default Handler Precedence

For the same incoming payload kind:
1. state-local `on` handler is selected first,
2. typestate-level default `on` handler is used only if no state-local handler exists.

## Send / Request / Reply

Proposed primitives:

- `emit Send(to: Handle<P, R>, Payload { ... })`
- `emit Request(to: Handle<P, R>, Req { ... }) -> Pending<RespSet>`
- `reply(cap, Resp { ... })`

### Typed request/reply safety

To avoid raw `corr: u64` bugs, request/reply uses compiler/runtime-managed
capabilities:

- responder receives a linear `ReplyCap<RespSet>` with the request handler.
- `reply(cap, x)` requires `type(x)` in `RespSet`.
- cap must be consumed exactly once unless handler is explicitly marked no-reply.
- source-level code does not manually construct/compare correlation ids.

This is the v1 pragmatic alternative to full typed channels.

## Direct vs Managed Modes

Two modes coexist:

1. `Direct mode`: existing local value transitions (`c.connect()`).
2. `Managed mode`: runtime-owned machine, mailbox/event driven.

### Managed registration rule

Registering a machine in managed mode is consuming:
- source handle becomes managed mailbox endpoint,
- direct transition calls on that managed instance are disallowed.

This prevents bypassing ordering and protocol semantics.

## Runtime Architecture (Managed Mode)

Per process:
- machine table (`MachineId -> state payload`)
- subscription registry (`event kind + routing key -> machine id`)
- per-machine bounded FIFO mailbox
- ready queue (machine ids)
- effect executor
- dead-letter/fault hooks

Source-level APIs should use typed handles:

```text
Handle<Protocol, Role>
```

Raw `MachineId` remains runtime/internal plumbing.

## Envelope Model

```text
Envelope {
  dst: MachineId,
  src: Option<MachineId>,
  payload_kind: KindId,
  payload: bytes/value,
  reply_cap: Option<ReplyCapId>,
  routing: Option<RoutingKey>
}
```

## Transactional Handler Semantics

A single handler dispatch executes as one transition transaction:

1. Load current state.
2. Run handler.
3. If handler succeeds:
   - commit state transition,
   - commit outbox effects,
   - update subscriptions.
4. If handler faults/errors before successful completion:
   - do not commit outbox effects from that dispatch,
   - state remains pre-dispatch (or transitions to configured fault state),
   - runtime applies fault policy.

This prevents half-applied transitions.

## Ordering and Delivery Guarantees (V1)

1. One event at a time per machine.
2. FIFO per receiver mailbox.
3. No global order guarantee across different senders.
4. Bounded mailbox by default.
5. `send/request` returns typed delivery/backpressure errors.

## Inter-Machine Example (Role-Based)

```machina
type IncomingRequest = { user: string }
type AuthorizeReq = { user: string }
type AuthApproved = {}
type AuthDenied = {}

protocol Auth {
    role Client;
    role Server;

    flow Client -> Server: AuthorizeReq -> AuthApproved | AuthDenied;
}

typestate Connection : Auth::Client {
    fields { auth: Handle<Auth, Server> }

    fn new(auth: Handle<Auth, Server>) -> Connected {
        Connected { auth: auth }
    }

    state Connected {
        on IncomingRequest(req: IncomingRequest) -> AwaitAuth {
            let pending = emit Request(to: self.auth, AuthorizeReq { user: req.user });
            AwaitAuth { pending: pending }
        }
    }

    state AwaitAuth {
        fields { pending: Pending<AuthApproved | AuthDenied> }

        on Response(pending: Pending<AuthApproved | AuthDenied>, resp: AuthApproved | AuthDenied) -> Connected | Closing {
            match resp {
                AuthApproved => Connected,
                AuthDenied => Closing,
            }
        }
    }

    state Closing {}
}

typestate AuthService : Auth::Server {
    fn new() -> Ready { Ready }

    state Ready {
        on AuthorizeReq(req: AuthorizeReq, cap: ReplyCap<AuthApproved | AuthDenied>) -> Ready {
            if req.user == "alice" {
                reply(cap, AuthApproved);
            } else {
                reply(cap, AuthDenied);
            }
            Ready
        }
    }
}
```

Notes:
- Syntax above is proposal-level.
- `Pending<...>` and `ReplyCap<...>` are linear capability-like values.

## Runtime Sequence (Inter-Machine)

```mermaid
sequenceDiagram
    participant IO as "I/O Adapter"
    participant RT as "Runtime Scheduler"
    participant REG as "Subscription Registry"
    participant MQC as "Connection Mailbox"
    participant C as "Connection Machine"
    participant MQA as "Auth Mailbox"
    participant A as "Auth Machine"

    IO->>RT: IncomingRequest(user="alice")
    RT->>REG: route IncomingRequest
    REG-->>RT: Connection machine
    RT->>MQC: enqueue

    RT->>MQC: dequeue
    RT->>C: dispatch IncomingRequest
    C-->>RT: next=AwaitAuth, outbox=[Request(AuthorizeReq)], pending=P42

    RT->>MQA: enqueue AuthorizeReq + reply_cap(R42)
    RT->>MQA: dequeue
    RT->>A: dispatch AuthorizeReq
    A-->>RT: next=Ready, outbox=[Reply(R42, AuthApproved)]

    RT->>MQC: enqueue Response(P42, AuthApproved)
    RT->>MQC: dequeue
    RT->>C: dispatch Response(P42, AuthApproved)
    C-->>RT: next=Connected, outbox=[]
```

## Failure and Fault Policy (V1 Draft)

Default policy:
- log fault diagnostics,
- mark machine `Faulted`,
- stop further dispatch for that machine unless explicitly restarted.

Required runtime hooks:
- dead-letter callback
- mailbox overflow/backpressure callback
- machine fault callback

## Compiler Pipeline Impact

The concrete phased implementation plan is defined in
**V1 Implementation Plan (Actionable)** below. This section stays as an
architecture summary:

1. Extend frontend surface (`protocol`/`role`/`flow`, `on`, `Request`, `reply`).
2. Enforce shape conformance and capability linearity in typecheck/semck.
3. Lower managed machine operations to explicit runtime-facing operations.
4. Integrate managed runtime dispatch, envelopes, and effect execution.
5. Keep backend mostly unchanged by lowering these concerns before codegen.

## Diagnostics (Planned)

- duplicate `on` for same `(state, payload)`
- missing required flow handler for role conformance
- invalid `Send/Request` target role or payload type
- invalid `reply(...)` payload for flow response set
- `reply` capability not consumed on all paths
- `reply` capability consumed multiple times
- managed machine used via direct call path
- mailbox full / delivery failure surfaced at call site

## Open Questions

1. Should no-reply paths require explicit annotation (for example `@[no_reply]`)?
2. Do we expose `Pending<T>` directly in user states in v1, or keep it lowered and implicit?
3. Are typed handles mandatory at source level in v1, or do we allow a temporary internal-only raw-id escape hatch?
4. Should minimal sequential flow syntax be added in v1.1 to prepare for stronger (non-shape) protocol checks?

## Future Direction: Typed Channels

Typed channels remain a strong long-term direction. This proposal intentionally
starts with capability-based request/reply as a lower-risk step that preserves
the same core benefit: correlation is structural, not convention-based.

## V1 Implementation Plan (Actionable)

This plan assumes we implement a local-process, single-scheduler runtime first.

### Milestone 0: Semantic Freeze (Must Complete First)

1. Lock source surface included in v1:
   - `protocol`, `role`, `flow`
   - typestate `on` handlers
   - `emit Send(...)`, `emit Request(...)`, `reply(...)`
2. Lock capability types and linearity rules:
   - `Handle<P, R>`
   - `Pending<RespSet>`
   - `ReplyCap<RespSet>`
3. Lock handler transaction semantics (commit/rollback/fault transitions).
4. Lock managed-mode exclusivity rule (no direct calls on managed machine).
5. Lock default handler precedence (state-local before typestate-level default).

### Milestone 1: Frontend + Static Rules (No Runtime Yet)

1. Parser/AST:
   - add protocol/role/flow nodes
   - add `on` handler nodes
   - add `Send/Request/reply` expression/statement nodes
2. Resolver:
   - resolve role paths (`Auth::Client`)
   - bind flow payload types and response sets
   - bind typestate-role implementations
3. Typecheck/Semck:
   - shape conformance checks for required incoming flows
   - `Send/Request` payload legality checks by role flow
   - `reply` payload type-in-response-set checks
   - linear capability checks (`ReplyCap` single-consume, all paths)
4. Diagnostics:
   - add dedicated error codes for flow mismatch, reply misuse, linearity errors.
5. Tests:
   - parser golden tests
   - resolve/typecheck conformance tests
   - negative tests for each diagnostic class.

### Milestone 2: Managed Runtime Core

1. Runtime data model:
   - machine table + lifecycle (`Running | Faulted | Stopped`)
   - bounded per-machine mailbox
   - ready queue
   - dead-letter/fault hooks
2. Dispatch engine:
   - dequeue + run-to-completion dispatch
   - transactional commit of state + outbox + subscription updates
3. Driver wiring:
   - runtime APIs for spawn/send/poll
   - compile-time lowering target for managed-machine operations
4. Tests:
   - deterministic single-machine dispatch ordering
   - bounded mailbox backpressure behavior
   - transactional rollback on handler fault.

### Milestone 3: Inter-Machine Request/Reply

1. Envelope plumbing for machine-to-machine delivery.
2. `Request` lowering:
   - create `Pending` token at caller
   - create `ReplyCap` token at callee dispatch context
3. `reply` lowering:
   - consume `ReplyCap`
   - emit routed response envelope back to requester
4. Pending-response dispatch:
   - map response envelopes to waiting machine handlers
   - enforce token matching semantics
5. Tests:
   - happy-path request/reply
   - wrong-type reply rejection at compile time
   - missing reply and double-reply path errors
   - destination missing/faulted delivery behavior.

### Milestone 4: Integration Hardening

1. LSP/analysis diagnostics parity for new syntax and checks.
2. Dump/trace support for machine transitions, queue depth, effect outcomes.
3. Documentation:
   - user-facing typestate async/protocol guide
   - examples for direct mode vs managed mode and inter-machine flow.
4. Regression suite:
   - compile-only protocol fixtures
   - runtime integration fixtures for mailbox and reply capability semantics.

### Implementation Order in Repo

1. `src/core/parse/*` and tree model updates.
2. `src/core/resolve/*` symbol binding and role references.
3. `src/core/typecheck/*` + `src/core/semck/*` conformance + linear capability checks.
4. `src/core/elaborate/*` lowering to explicit runtime-facing operations.
5. runtime crate support (`runtime/*`) and driver integration (`src/driver/*`).
6. analysis/LSP surface updates (`src/services/*`, `tooling/lsp/*`).

### Definition of Done for V1

1. Compiler accepts valid role/flow/handler/reply programs and rejects invalid ones with specific diagnostics.
2. Managed machines run with deterministic per-mailbox FIFO dispatch and bounded queues.
3. Request/reply correlation is enforced structurally via capabilities, not user-managed ids.
4. Direct typestate mode remains unchanged for non-managed code paths.
5. Example suite demonstrates:
   - single-machine async event handling,
   - two-machine protocol request/reply,
   - fault handling behavior.
