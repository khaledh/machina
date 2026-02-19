# Protocols in Machina: Current State, Gaps, and Implementation Plan

## 1. Purpose

This document defines the protocol subsystem as it exists today, identifies the
remaining gaps, and proposes a concrete implementation plan to close those gaps
without over-complicating the typechecker/runtime boundary.

Scope: typestate-managed machines and protocol/role/state-transition conformance.

---

## 2. What Exists Today (Implemented)

## 2.1 Source surface

Implemented source constructs:
- `protocol <Name> { role <Role>; flow <From> -> <To>: <Payload> [-> <Resp...>]; }`
- `typestate <T> : { <Protocol>::<Role>, ... } { ... }`
- managed handlers with request/reply forms used by flow checks.

### 2.1.1 Target source surface (design update)

To reduce verbosity and remove trigger ambiguity, we standardize on a
role/state-centric protocol surface that mirrors typestate shape:
- `msg <Name> [= <TypeExpr>]`
- `role <Role> { state <State> { on <Msg>@<Role> -> <Next> { effects: [ <Msg> ~> <Role>, ... ] } } }`
- `req <FromRole> -> <ToRole>: <RequestMsg> => <ReplyMsg> | <ReplyMsg> ...` for request/reply contracts

Key syntax decisions:
- triggers are message arrivals only (including startup)
- startup is modeled uniformly as `Start@System`
- `on Start` is sugar for `on Start@System`
- no `recv`/`send` keywords in protocol transitions
- no `?`/`!` symbolic receive/send forms
- no `Role::Msg` trigger form (avoids namespacing confusion)
- `effects: [ ... ]` is an ordered, must-emit list
- final states use `@final` for consistency with language-wide attribute style

### 2.1.2 Core semantics for protocol transitions

To avoid ambiguity, protocol transition semantics are explicit:

- Trigger model:
  - `on Msg@Role` means this role must be able to receive `Msg` from `Role`.
  - `on Start` is equivalent to `on Start@System`.
- Determinism:
  - within one protocol role state, triggers must be unambiguous
  - duplicate trigger keys (`Msg@Role`) in the same state are rejected.
- Effects semantics:
  - `effects: [A ~> X, B ~> Y]` is ordered and mandatory
  - all listed effects are part of the transition obligation.
- Failure model:
  - a transition that cannot satisfy mandatory effects is considered failed
  - runtime commit/rollback policy governs visibility, but protocol conformance
    assumes transition obligations are not silently dropped.
- Conformance stance:
  - protocol checks treat effect declarations as the contract surface
  - implementation sends outside the allowed protocol transition surface are
    rejected by protocol conformance checks.
- Request contracts:
  - `req FromRole -> ToRole: Req => RepA | RepB` declares directional request
    semantics and allowable response families
  - multiple in-flight requests are allowed; correlation is resolved through
    existing request provenance labels in typestate handlers.

Example (target):

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

Evidence:
- parser: `src/core/parse/decl.rs` (`parse_protocol_def`, typestate role impl parsing)
- tree model: `src/core/tree/model.rs` (`ProtocolDef`, `ProtocolFlow`, `ProtocolRole`)

## 2.2 Resolve stage support

Implemented:
- protocol defs and protocol roles are entered into symbols/defs
- flow role names are validated against protocol-local role declarations
- typestate role-impl paths bind to protocol-role defs
- diagnostics for malformed/undefined/mistyped role impl paths.

Evidence:
- resolver symbol population/binding: `src/core/resolve/resolver.rs`
- resolver errors: `src/core/resolve/errors.rs`
- tests:
  - `src/tests/resolve/t_resolve.rs`
  - `src/tests/analysis/t_analysis_db.rs` (hover/def/completion for role refs)

## 2.3 Typecheck validation support

Implemented protocol checks:
- protocol **shape conformance** (typestate-level):
  - required incoming flow payloads must have handlers
  - outgoing emitted payloads must be allowed by protocol flows for the role
- request/response shape checks:
  - request sites must have matching response handlers
  - response handlers may not claim unsupported response variants
- provenance ambiguity checks with request-site labels
- `ReplyCap` linearity checks (must consume exactly once; no double-consume).

Evidence:
- `src/core/typecheck/validate.rs`
- protocol-related diagnostics in `src/core/typecheck/errors.rs`
- tests:
  - `src/tests/core/t_api.rs`
  - `tests/compiler/typestate.rs`

## 2.4 Runtime integration state

Implemented:
- runtime executes machine dispatch, requests, replies, pending correlation, and
  transactional commit/rollback.
- protocol semantics are mostly enforced at compile time via typecheck rules;
  runtime is currently payload/event-driven, not role/protocol-driven.

Evidence:
- `runtime/machine/runtime.c`
- `runtime/tests/*` machine runtime fixtures.

---

## 3. Current Limits / Gaps

These are the main missing pieces relative to a robust protocol story.

## 3.1 Per-state protocol conformance is missing

Current checks are typestate-wide set checks. They do not verify:
- which flows are legal in a specific typestate state,
- whether a state advertises handlers that are illegal for that state’s
  projected protocol behavior.

Impact: we can accept implementations that are globally shape-valid but
state-locally incorrect.

## 3.1.1 Protocol surface is still flow-centric

Current surface is compact for simple message families, but weak for:
- readable state-local contracts per role,
- explicit trigger source readability,
- direct alignment with typestate state implementations.

Impact: conformance logic must infer more than source syntax directly conveys.

## 3.2 Destination role compatibility is now enforced

`send/request` destinations are validated against explicit peer-role bindings
(`Machine<Peer> as Role`) and protocol direction/contracts.

Remaining gap: sequencing/progression is still local (per handler/transition)
rather than a full global protocol proof.

## 3.3 No global protocol-level progression proof (yet)

Semck now performs **starter local progression coherence checks**:
- handler trigger must map to at least one protocol transition for that state,
- emitted `(payload, destination-role)` edges must be allowed by that trigger,
- returned next state must be allowed by that trigger.

What is still out of scope:
- global multiparty progress/deadlock proofs,
- theorem-level session conformance across composed machines.

## 3.4 Multi-role implementation constraints are shallow

A typestate can implement multiple roles, but cross-role conflict checks are
minimal (especially around overlapping payload families and ambiguous routing
intent at the conformance layer).

## 3.5 Runtime does not enforce protocol metadata

Runtime dispatch is descriptor/event based; protocol role identity is not part
of runtime safety checks.

This is acceptable for v1 if compile-time guarantees are strong enough, but it
is still a gap for defense-in-depth and diagnostics.

## 3.6 Tooling/diagnostics can become more protocol-native

We already have strong diagnostics, but we still need:
- clearer “expected by transition X in role/state Y” messages,
- better quick-fix quality for missing handlers/transition mismatches,
- protocol-focused analysis queries for IDE surfaces.

---

## 4. Target Conformance Model (Incremental)

To keep complexity under control, we should evolve conformance in tiers.

### Tier A (already implemented): shape
- incoming/outgoing payload family checks by role
- request/reply response-shape + provenance + capability linearity

### Tier B (implemented): state-aware projection-lite
- per-state allowed incoming/outgoing message sets
- state-local legality checks for handlers and emits
- destination role compatibility checks for `send/request`

### Tier C (in progress): sequencing/progression
- starter local progression coherence checks in semck (implemented)
- future expansion to stronger cross-machine/global progression claims

Current practical focus is maturing Tier C starter checks without inflating
typecheck/runtime complexity.

### 4.1 Explicit guarantees by tier

To avoid over-claiming, each tier has explicit guarantees and non-guarantees.

- Tier A guarantees:
  - message family shape checks by role
  - request/reply shape and reply-cap linearity checks.
- Tier A does not guarantee:
  - state-local legality
  - peer handle role compatibility
  - global protocol progress/deadlock properties.

- Tier B guarantees:
  - state-local transition legality (handled/emitted families per state)
  - destination peer-role compatibility using explicit role bindings
  - directional request contract conformance.
- Tier B does not guarantee:
  - full multiparty global compatibility/progress proofs
  - deadlock freedom across arbitrary protocol topologies.

- Tier C current guarantees:
  - local progression coherence per handler trigger:
    - trigger must be declared by protocol transition for current role/state
    - observed emits and next-state edges must be compatible with that trigger.
- Tier C still does not guarantee:
  - global protocol progress/deadlock freedom,
  - full session-type compatibility theorems.

---

## 5. Design Additions Needed for Tier B

## 5.1 Introduce a canonical protocol index

Add a compact internal index (resolve output side-table) that precomputes:
- protocol -> roles
- role -> states
- state -> transitions (`trigger`, `next_state`, `effects`)
- request/reply contracts (`req FromRole -> ToRole: Request => Reply...`)
- source span/identity per entry.

Why:
- avoid re-scanning AST repeatedly in typecheck validate,
- give one canonical source for protocol diagnostics and future IDE queries.

## 5.2 Model typestate state-level machine behavior facts

From existing elaboration/typecheck info, derive per-state facts:
- handled incoming payload selectors
- emitted/requested payload selectors
- response handlers (with provenance label information).

Why:
- enforce conformance at state granularity, not only typestate granularity.

## 5.3 Add explicit role binding syntax in typestate

Add explicit mapping from protocol peer roles to concrete machine handles in
typestate fields.

Proposed syntax (v1):
- `fields { auth: Machine<AuthService> as Server }`

Meaning:
- the `auth` handle fulfills protocol peer role `Server` for this typestate's
  implemented role(s).
- role compatibility diagnostics can point to concrete fields directly.

Validation rules:
- each referenced peer role in protocol transitions must have exactly one bound
  field
- bound field type must be `Machine<T>` where `T` implements that peer role
- duplicate bindings for the same role are rejected in v1.

## 5.4 Add peer-role compatibility metadata for handles

Introduce a lightweight mapping from managed handle types to implemented role
sets (at least for typestates that declare role impls), and validate callsites:
- `request(dst, payload)` and `send(dst, payload)` must target a machine that can
  legally receive that payload per the protocol transition contract from caller role.

This can start as compile-time metadata only (no runtime role tag needed).

## 5.5 Keep runtime protocol-agnostic for now (explicitly)

Do not add runtime role checks in this phase. Keep runtime generic and move
correctness burden to compile-time conformance (with clear diagnostics).

## 5.6 Examples of intended Tier B semantics

The additions above are compile-time checks over the target protocol surface.
The examples below use the standardized concise syntax.

### Example A: state-level conformance (projection-lite)

```mc
protocol Auth {
    msg Start
    msg AuthReq
    msg AuthOk
    msg AuthErr
    req Client -> Server: AuthReq => AuthOk | AuthErr

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }

        state Awaiting {
            on AuthOk@Server -> Ready;
            on AuthErr@Server -> Idle;
        }

        state Ready;
    }

    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate Gateway : { Auth::Client } {
    fields { auth: Machine<AuthService> as Server }

    fn new(auth: Machine<AuthService>) -> Idle {
        Idle { auth: auth }
    }

    state Idle { /* emits AuthReq to Server */ }
    state Awaiting { /* handles AuthOk/AuthErr from Server */ }
    state Ready {}
}
```

Intended validation behavior:
- `Idle` is allowed to emit `AuthReq` to `Server`.
- `Awaiting` is allowed to handle `AuthOk`/`AuthErr` as responses to `AuthReq`.
- if `Ready` also declared `on AuthOk(...)` without matching request provenance,
  that should fail state-level conformance.

### Example B: destination peer-role compatibility

```mc
protocol Auth {
    msg Start
    msg AuthReq = { token: string }
    msg AuthOk = { user_id: u64 }
    req Client -> Server: AuthReq => AuthOk

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }

    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate Client : { Auth::Client } { /* ... */ }
typestate Server : { Auth::Server } { /* ... */ }

@machines
fn main() -> () | MachineError {
    let c = Client::spawn()?;
    let s = Server::spawn()?;
    c.request(s, AuthReq { token: "t" })?; // OK (destination role = Server)

    let c2 = Client::spawn()?;
    c.request(c2, AuthReq { token: "t" })?; // ERROR (expected Auth::Server peer)
}
```

Intended validation behavior:
- callsite checks must verify destination handle role compatibility
  (`Auth::Server` required for `Client -> Server: AuthReq`).
- diagnostic should include source role, expected peer role, and destination
  typestate role set.

### Example C: internal `ProtocolIndex` shape (conceptual)

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

Typecheck validators use this canonical index rather than re-walking protocol
AST nodes ad-hoc.

---

## 6. Implementation Plan

## Phase 1: Protocol Index Foundation

1. Add `ProtocolIndex` side-table in resolve/typecheck context boundary.
2. Build it once from resolved module protocol defs and role impl bindings.
3. Add unit tests for index correctness (roles, states, transitions, request contracts).
4. Update `grammar.bnf` to include the protocol v1 syntax decisions (`Msg@Role`,
   `effects: [ ... ]`, directional `req`, `on Start` sugar).

Deliverable: typecheck validate and future analysis can consume stable protocol
facts without ad-hoc AST traversal.

## Phase 2: Explicit Role Binding

1. Add typestate field binding syntax (`field: Machine<T> as Role`).
2. Validate that every referenced peer role has exactly one binding.
3. Build and expose role->field binding metadata in typecheck contexts.
4. Add focused diagnostics for missing/duplicate/incompatible role bindings.

Deliverable: conformance checks and diagnostics can refer to concrete fields.

## Phase 3: State-Level Conformance

1. Collect per-state handler/emit/request facts (reuse existing collectors where
   possible, split into reusable helpers).
2. Add validations:
   - handler payload legality per state+role projection-lite rule,
   - outgoing payload legality per state+role projection-lite rule.
3. Keep typestate-wide checks as fallback until parity is reached, then remove
   redundant checks.

Deliverable: state-local protocol correctness diagnostics.

## Phase 4: Destination Role Compatibility

1. Add compile-time mapping from `Machine<T>` destinations to role sets.
2. Validate `send/request` destination compatibility against protocol transition
   direction and payload.
3. Add diagnostics that name:
   - source typestate/role,
   - expected peer role,
   - concrete bound field and its role/type.

Deliverable: peer-role-safe inter-machine messaging.

## Phase 4.1: Request Contract Tightening

1. Enforce directional request contracts from
   `req FromRole -> ToRole: Request => Reply...`.
2. Validate request/reply conformance against both request message and direction.
3. Verify that request contracts are realizable by declared transitions.

Deliverable: unambiguous request/reply conformance.

## Phase 5: Diagnostics + IDE surfacing

1. Add protocol-centric diagnostic phrasing and compact type rendering.
2. Add code-action hooks for:
   - missing required handler stub,
   - invalid response variant correction candidates.
3. Expose protocol conformance facts in analysis services for richer hover/help.

Deliverable: protocol errors are easier to fix in-editor.

## Phase 6: Sequencing Design Spike (No immediate ship)

1. Prototype progression checks on a narrow subset (single protocol, single role
   implementation, explicit state mapping).
2. Validate complexity/perf/diagnostic quality.
3. Decide whether to adopt Tier C in core typecheck or keep as optional lint.

Deliverable: informed decision on session-like sequencing without premature
complexity.

---

## 7. Testing Plan

For each implementation phase:
- add strict compile tests under `src/tests/core/*` and `tests/compiler/*`
- add negative fixtures with one targeted failure per case
- keep runtime tests unchanged unless protocol metadata is intentionally moved
  into runtime (not planned in this document).

Key regression groups:
- valid multi-role typestate conformance
- explicit role binding validity (missing/duplicate/wrong-type)
- per-state illegal handler/emit
- destination role mismatch
- provenance-labeled concurrent request/reply transitions.

---

## 8. Out of Scope for This Plan

- distributed transport semantics
- runtime role-level authorization or protocol enforcement
- full session-type progression theorem/proof system
- async/await language model changes.

---

## 9. Success Criteria

This plan is complete when:

1. Protocol conformance is state-aware (not only typestate-wide).
2. Role-to-field binding is explicit and validated.
3. Destination peer-role compatibility is enforced for `send/request`.
4. Tier guarantees/non-guarantees are documented and reflected in diagnostics.
5. Diagnostics are explicit enough to repair conformance issues quickly.
6. Existing runtime model remains simple and stable while compile-time protocol
   guarantees become materially stronger.
7. Request/reply contracts are directional and correlation-safe by construction.
