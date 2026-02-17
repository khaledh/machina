# Protocols in Machina: Current State, Gaps, and Implementation Plan

## 1. Purpose

This document defines the protocol subsystem as it exists today, identifies the
remaining gaps, and proposes a concrete implementation plan to close those gaps
without over-complicating the typechecker/runtime boundary.

Scope: typestate-managed machines and protocol/role/flow conformance.

---

## 2. What Exists Today (Implemented)

## 2.1 Source surface

Implemented source constructs:
- `protocol <Name> { role <Role>; flow <From> -> <To>: <Payload> [-> <Resp...>]; }`
- `typestate <T> : { <Protocol>::<Role>, ... } { ... }`
- managed handlers with request/reply forms used by flow checks.

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

## 3.2 Destination role compatibility is not fully enforced

We validate payload legality against flows, but we do not fully enforce that
the destination machine (typed handle) is compatible with the required peer role
for each `send/request`.

Impact: payload shape can pass while peer role intent is under-constrained.

## 3.3 No protocol-level sequencing/progression checks

We currently do not model temporal ordering constraints within protocols beyond
payload families (no session/projection progression).

Impact: legal-by-shape sequences can violate intended protocol order.

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
- clearer “expected by flow X in role Y” messages,
- better quick-fix quality for missing handlers/flow mismatches,
- protocol-focused analysis queries for IDE surfaces.

---

## 4. Target Conformance Model (Incremental)

To keep complexity under control, we should evolve conformance in tiers.

### Tier A (already implemented): shape
- incoming/outgoing payload family checks by role
- request/reply response-shape + provenance + capability linearity

### Tier B (next): state-aware projection-lite
- per-state allowed incoming/outgoing flow sets
- state-local legality checks for handlers and emits
- destination role compatibility checks for `send/request`

### Tier C (later): sequencing/progression
- protocol progression constraints across transitions/events
- richer session-like checks where useful

Tier B is the practical next milestone.

---

## 5. Design Additions Needed for Tier B

## 5.1 Introduce a canonical protocol index

Add a compact internal index (resolve output side-table) that precomputes:
- protocol -> roles
- role -> incoming/outgoing flow entries
- flow payload + response set + source span/identity.

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

## 5.3 Add peer-role compatibility metadata for handles

Introduce a lightweight mapping from managed handle types to implemented role
sets (at least for typestates that declare role impls), and validate callsites:
- `request(dst, payload)` and `send(dst, payload)` must target a machine that can
  legally receive that payload per the protocol flow from caller role.

This can start as compile-time metadata only (no runtime role tag needed).

## 5.4 Keep runtime protocol-agnostic for now (explicitly)

Do not add runtime role checks in this phase. Keep runtime generic and move
correctness burden to compile-time conformance (with clear diagnostics).

---

## 6. Implementation Plan

## Phase 1: Protocol Index Foundation

1. Add `ProtocolIndex` side-table in resolve/typecheck context boundary.
2. Build it once from resolved module protocol defs and role impl bindings.
3. Add unit tests for index correctness (roles, flows, payload/response sets).

Deliverable: typecheck validate and future analysis can consume stable protocol
facts without ad-hoc AST traversal.

## Phase 2: State-Level Conformance

1. Collect per-state handler/emit/request facts (reuse existing collectors where
   possible, split into reusable helpers).
2. Add validations:
   - handler payload legality per state+role projection-lite rule,
   - outgoing payload legality per state+role projection-lite rule.
3. Keep typestate-wide checks as fallback until parity is reached, then remove
   redundant checks.

Deliverable: state-local protocol correctness diagnostics.

## Phase 3: Destination Role Compatibility

1. Add compile-time mapping from `Machine<T>` destinations to role sets.
2. Validate `send/request` destination compatibility against protocol flow
   direction and payload.
3. Add diagnostics that name:
   - source typestate/role,
   - expected peer role,
   - actual destination typestate roles (or missing role impl).

Deliverable: peer-role-safe inter-machine messaging.

## Phase 4: Diagnostics + IDE surfacing

1. Add protocol-centric diagnostic phrasing and compact type rendering.
2. Add code-action hooks for:
   - missing required handler stub,
   - invalid response variant correction candidates.
3. Expose protocol conformance facts in analysis services for richer hover/help.

Deliverable: protocol errors are easier to fix in-editor.

## Phase 5: Sequencing Design Spike (No immediate ship)

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
- per-state illegal handler/emit
- destination role mismatch
- provenance-labeled concurrent request flows.

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
2. Destination peer-role compatibility is enforced for `send/request`.
3. Diagnostics are explicit enough to repair conformance issues quickly.
4. Existing runtime model remains simple and stable while compile-time protocol
   guarantees become materially stronger.
