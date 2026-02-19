# Protocol Design

## Overview

Protocols define abstract interaction contracts between machine roles. A
protocol declares message types, role-local states with transitions, and
directional request/reply contracts. Typestates implement protocol roles and are
checked for conformance at compile time.

For the typestate language model, see `typestate-design.md`. For runtime
execution, see `machine-runtime-design.md`.

## Surface Syntax

### Protocol Declaration

```mc
protocol Auth {
    msg Start
    msg AuthorizeReq = { user: string }
    msg AuthApproved
    msg AuthDenied
    req Client -> Server: AuthorizeReq => AuthApproved | AuthDenied

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthorizeReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthApproved@Server -> Ready;
            on AuthDenied@Server -> Idle;
        }
        @final state Ready;
    }

    role Server {
        state Ready {
            on AuthorizeReq@Client -> Ready {
                effects: [ AuthApproved ~> Client ]
            }
        }
    }
}
```

### Syntax Decisions

- `msg <Name> [= <TypeExpr>]` — lightweight message declaration.
- `role <Role> { state <State> { ... } }` — role-local states with transitions.
- `req <FromRole> -> <ToRole>: <RequestMsg> => <ReplyMsg> | ...` — directional
  request/reply contracts.
- Triggers are message arrivals: `on Msg@Role` means this role receives `Msg`
  from `Role`.
- `on Start` is sugar for `on Start@System` (startup trigger).
- `effects: [ Msg ~> Role, ... ]` — ordered, must-emit list of outgoing effects.
- `@final` marks terminal states (consistent with language-wide attribute style).
- No `recv`/`send` keywords, no `?`/`!` symbolic forms, no `Role::Msg` trigger
  form.

### Typestate Role Implementation

```mc
typestate Gateway : Auth::Client { ... }
typestate AuthService : Auth::Server { ... }
```

Multi-role implementation:

```mc
typestate Gateway : { Auth::Client, Logging::Source } { ... }
```

### Explicit Role Binding

Typestate fields declare which concrete handles fulfill peer roles:

```mc
typestate Gateway : Auth::Client {
    fields { auth: Machine<AuthService> as Server }
    ...
}
```

Validation rules:
- Each referenced peer role in protocol transitions must have exactly one bound
  field.
- Bound field type must be `Machine<T>` where `T` implements that peer role.
- Duplicate bindings for the same role are rejected.

## Transition Semantics

- **Trigger model**: `on Msg@Role` means this role must be able to receive
  `Msg` from `Role`.
- **Determinism**: within one protocol role state, triggers must be unambiguous.
  Duplicate trigger keys (`Msg@Role`) in the same state are rejected.
- **Effects semantics**: `effects: [A ~> X, B ~> Y]` is ordered and mandatory.
  All listed effects are part of the transition obligation.
- **Failure model**: a transition that cannot satisfy mandatory effects is
  considered failed. Runtime commit/rollback policy governs visibility, but
  protocol conformance assumes transition obligations are not silently dropped.
- **Request contracts**: `req FromRole -> ToRole: Req => RepA | RepB` declares
  directional request semantics and allowable response families. Multiple
  in-flight requests are allowed; correlation is resolved through request
  provenance labels.

## Conformance Model (Tiered)

To keep complexity under control, conformance evolves in tiers with explicit
guarantees and non-guarantees at each level.

### Tier A: Shape (Implemented)

Checks:
- Required incoming flow payloads must have handlers.
- Outgoing emitted payloads must be allowed by protocol flows for the role.
- Request/response shape and reply-cap linearity checks.

Does not guarantee:
- State-local legality.
- Peer handle role compatibility.
- Global protocol progress/deadlock properties.

### Tier B: State-Aware Projection-Lite (Implemented)

Checks:
- Per-state allowed incoming/outgoing message sets.
- State-local legality checks for handlers and emits.
- Destination role compatibility checks for `send/request`.
- Directional request contract conformance.

Does not guarantee:
- Full multiparty global compatibility/progress proofs.
- Deadlock freedom across arbitrary protocol topologies.

### Tier C: Sequencing / Progression (In Progress)

Current checks:
- Local progression coherence per handler trigger:
  - trigger must be declared by protocol transition for current role/state,
  - observed emits and next-state edges must be compatible with that trigger.

Does not guarantee:
- Global protocol progress/deadlock freedom.
- Full session-type compatibility theorems.

## What Exists Today (Implementation Status)

### Source Surface

Implemented constructs:
- `protocol <Name> { role <Role>; flow <From> -> <To>: <Payload> [-> <Resp...>]; }`
- `typestate <T> : { <Protocol>::<Role>, ... } { ... }`
- Managed handlers with request/reply forms used by flow checks.

Evidence:
- Parser: `src/core/parse/decl.rs` (`parse_protocol_def`, typestate role impl
  parsing)
- Tree model: `src/core/tree/model.rs` (`ProtocolDef`, `ProtocolFlow`,
  `ProtocolRole`)

### Resolve Stage

- Protocol defs and protocol roles are entered into symbols/defs.
- Flow role names are validated against protocol-local role declarations.
- Typestate role-impl paths bind to protocol-role defs.
- Diagnostics for malformed/undefined/mistyped role impl paths.

Evidence:
- `src/core/resolve/resolver.rs`
- `src/core/resolve/errors.rs`

### Typecheck Validation

- Shape conformance (typestate-level).
- Request/response shape checks.
- Provenance ambiguity checks with request-site labels.
- `ReplyCap` linearity checks (must consume exactly once).

Evidence:
- `src/core/typecheck/validate/protocol.rs`
- `src/core/typecheck/validate/reply_cap.rs`
- `src/core/typecheck/errors.rs`

### Runtime Integration

- Runtime executes machine dispatch, requests, replies, pending correlation, and
  transactional commit/rollback.
- Protocol semantics are enforced at compile time; runtime is payload/event-driven,
  not role/protocol-driven.

## Remaining Gaps

### Per-State Protocol Conformance Depth

Current checks are typestate-wide set checks. They do not fully verify which
flows are legal in a specific typestate state, or whether a state advertises
handlers that are illegal for that state's projected protocol behavior.

### No Global Protocol-Level Progression Proof

Semck performs local progression coherence checks, but global multiparty
progress/deadlock proofs and theorem-level session conformance across composed
machines remain out of scope.

### Multi-Role Implementation Constraints Are Shallow

A typestate can implement multiple roles, but cross-role conflict checks are
minimal (especially around overlapping payload families and ambiguous routing).

### Runtime Does Not Enforce Protocol Metadata

Runtime dispatch is descriptor/event based; protocol role identity is not part
of runtime safety checks. Acceptable for V1 if compile-time guarantees are
strong enough, but a gap for defense-in-depth.

### Diagnostics Can Become More Protocol-Native

Needed improvements:
- Clearer "expected by transition X in role/state Y" messages.
- Better quick-fix quality for missing handlers/transition mismatches.
- Protocol-focused analysis queries for IDE surfaces.

## Internal Protocol Index

A canonical internal index (resolve output side-table) precomputes:
- protocol → roles
- role → states
- state → transitions (`trigger`, `next_state`, `effects`)
- request/reply contracts
- source span/identity per entry

This avoids re-scanning AST repeatedly in typecheck validate, and gives one
canonical source for protocol diagnostics and IDE queries.

Conceptual shape:

```text
ProtocolIndex
  Auth
    roles:
      Client:
        states:
          Idle:
            transitions:
              - trigger: Start
                next: Awaiting
                effects: [AuthReq~>Server]
          Awaiting:
            transitions:
              - trigger: AuthOk@Server
                next: Ready
              - trigger: AuthErr@Server
                next: Idle
      Server: ...
    req_contracts:
      AuthReq:
        from: Client
        to: Server
        replies: [AuthOk, AuthErr]
```

## Examples

### Inter-Machine Protocol (Full)

```mc
type IncomingRequest = { user: string }
type AuthorizeReq = { user: string }
type AuthApproved = {}
type AuthDenied = {}

protocol Auth {
    msg Start
    msg IncomingRequest = { user: string }
    msg AuthorizeReq = { user: string }
    msg AuthApproved
    msg AuthDenied
    req Client -> Server: AuthorizeReq => AuthApproved | AuthDenied

    role Client {
        state Running {
            on IncomingRequest@System -> Running {
                effects: [ AuthorizeReq ~> Server ]
            }
            on AuthApproved@Server -> Running;
            on AuthDenied@Server -> Closing;
        }
        state Closing;
    }

    role Server {
        state Ready {
            on AuthorizeReq@Client -> Ready {
                effects: [ AuthApproved ~> Client ]
            }
        }
    }
}

typestate Connection : Auth::Client {
    fields { auth: Machine<AuthService> as Server }

    fn new(auth: Machine<AuthService>) -> Running {
        Running { auth: auth }
    }

    state Running {
        on IncomingRequest(req) {
            request(self.auth, AuthorizeReq { user: req.user });
        }

        on AuthApproved(ok) for AuthorizeReq(origin) {
            ok;
            origin;
            Running
        }

        on AuthDenied(err) for AuthorizeReq(origin) -> Closing {
            err;
            origin;
            Closing
        }
    }

    state Closing {}
}

typestate AuthService : Auth::Server {
    fn new() -> Ready { Ready }

    state Ready {
        on AuthorizeReq(req) {
            if req.user == "alice" {
                reply(AuthApproved);
            } else {
                reply(AuthDenied);
            }
        }
    }
}
```

### Destination Role Compatibility

```mc
@machines
fn main() -> () | MachineError {
    let c = Client::spawn()?;
    let s = Server::spawn()?;
    c.request(s, AuthReq { token: "t" })?;  // OK: destination = Auth::Server

    let c2 = Client::spawn()?;
    c.request(c2, AuthReq { token: "t" })?; // ERROR: expected Auth::Server peer
}
```

### TCP Handshake (Role-State Protocol)

```mc
protocol TcpHandshake {
    msg Start
    msg Syn
    msg SynAck
    msg Ack
    msg Timeout

    role Client {
        state Closed {
            on Start -> SynSent {
                effects: [ Syn ~> Server ]
            }
        }
        state SynSent {
            on SynAck@Server -> Established {
                effects: [ Ack ~> Server ]
            }
            on Timeout@System -> Closed;
        }
        @final state Established;
    }

    role Server {
        state Listen {
            on Syn@Client -> AwaitAck {
                effects: [ SynAck ~> Client ]
            }
        }
        state AwaitAck {
            on Ack@Client -> Listen;
        }
    }
}
```

## Implementation Plan

### Phase 1: Protocol Index Foundation

1. Add `ProtocolIndex` side-table in resolve/typecheck context boundary.
2. Build it once from resolved module protocol defs and role impl bindings.
3. Unit tests for index correctness.
4. Update `grammar.bnf` to include protocol V1 syntax.

### Phase 2: Explicit Role Binding

1. Add typestate field binding syntax (`field: Machine<T> as Role`).
2. Validate that every referenced peer role has exactly one binding.
3. Expose role→field binding metadata in typecheck contexts.
4. Focused diagnostics for missing/duplicate/incompatible role bindings.

### Phase 3: State-Level Conformance

1. Collect per-state handler/emit/request facts.
2. Add validations: handler payload legality and outgoing payload legality per
   state+role.
3. Keep typestate-wide checks as fallback until parity, then remove redundant
   checks.

### Phase 4: Destination Role Compatibility

1. Add compile-time mapping from `Machine<T>` destinations to role sets.
2. Validate `send/request` destination compatibility.
3. Diagnostics naming source role, expected peer role, and concrete bound field.

### Phase 4.1: Request Contract Tightening

1. Enforce directional request contracts from `req FromRole -> ToRole: Request => Reply...`.
2. Validate request/reply conformance against both request message and direction.

### Phase 5: Diagnostics + IDE Surfacing

1. Protocol-centric diagnostic phrasing and compact type rendering.
2. Code-action hooks for missing handler stubs and invalid response variants.
3. Protocol conformance facts in analysis services for richer hover/help.

### Phase 6: Sequencing Design Spike

1. Prototype progression checks on a narrow subset.
2. Validate complexity/perf/diagnostic quality.
3. Decide whether to adopt Tier C in core typecheck or keep as optional lint.

## Diagnostics

Protocol-specific diagnostics:
- Duplicate `on` for same `(state, payload)`.
- Overlapping pattern-form `on` handlers for same selector.
- Ambiguous response provenance requiring explicit `for RequestType(binding)`.
- Missing required protocol transition handler for role conformance.
- Invalid `Send/Request` target role or payload type.
- Invalid `reply(...)` payload for request response set.
- `reply` capability not consumed on all paths.
- `reply` capability consumed multiple times.
- Managed machine used via direct call path.

## Out of Scope

- Distributed transport semantics.
- Runtime role-level authorization or protocol enforcement.
- Full session-type progression theorem/proof system.
- Async/await language model changes.
