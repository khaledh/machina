# Typestate Machines: Runtime Execution Integration (Draft)

## Status

Draft implementation design (revised after multi-model design review).

This document is intentionally separate from
`typestate-machines-async-events-design.md`:
- `typestate-machines-async-events-design.md` defines language/runtime semantics.
- this document defines the compiler↔runtime execution bridge needed to run
  managed machines end-to-end.

## Terminology

- `typestate`: source-level definition of states and transitions.
- `machine`: runtime instance of a typestate.
- `managed mode`: runtime-owned machine lifecycle + mailbox dispatch.
- `direct mode`: existing value-level transitions (`c.connect()`).

## Problem

Current repository status is split:
- frontend supports protocol/flow/on/send/request/reply semantics (experimental),
- runtime has managed scheduler primitives (`runtime/machine/runtime.*`),
- compiler/runtime bridge exists end-to-end, but still needs execution-path
  hardening and DX cleanup.

As a result, managed execution is viable today, but we still need to polish:
better defaults, clearer API contracts, and stronger execution diagnostics.

## Goal

Enable this to run end-to-end:

1. compile typestate+protocol code,
2. spawn/start machines with generated descriptors,
3. enqueue/send/request/reply through runtime,
4. dispatch handlers transactionally,
5. observe state transitions and outputs at runtime.

And expose this behind an ergonomic source surface where typical app code does
not manually construct runtime drivers.

## Non-Goals (V1 execution slice)

1. No distributed/network transport.
2. No preemptive/parallel machine dispatch.
3. No async/await function-coloring model.
4. No full protocol projection refinement checks.

## ABI Freeze (V1)

This section is normative for implementation order and correctness.

### 1. Envelope Payload ABI (Boxed)

`mc_machine_envelope_t` currently has two 64-bit payload slots. V1 execution
uses these slots as boxed payload metadata:

- `payload0`: pointer to heap box holding payload value bytes.
- `payload1`: payload layout/type id (used for decode + drop glue lookup).

Ownership contract:

1. sender-side lowering allocates payload box and transfers ownership to runtime
   enqueue/request staging,
2. runtime owns payload while envelope is in mailbox/ready transit,
3. on successful handler dispatch, handler decode consumes payload box,
4. on dead-letter/drop/rollback paths, runtime drops payload via layout id.

No inline payload optimization in v1 (can be added later).

### 2. Machine State ABI (Opaque Token + Commit Swap)

V1 treats `state_word` as opaque machine-state token owned by generated code.

Recommended token representation for v1:
- pointer to heap state object containing `{state_tag, state_payload}`.

Commit/rollback contract:

1. handler decodes current token but does not mutate committed state object,
2. handler builds next-state token separately,
3. runtime swaps state token only on successful transactional commit,
4. on rollback/fault/stop, old token remains current and staged token is dropped.

This avoids partial state writes and preserves transactional semantics.

### 3. Event Kind Tag Assignment (Capsule-Deterministic)

Event kind tags are assigned sequentially per capsule during descriptor build.

Rules:

1. one payload type maps to one stable tag in the capsule,
2. all modules in the capsule share the same mapping,
3. descriptor generation is the single source of truth.

No per-module independent tag assignment.

### 4. Dispatch Thunk Contract

For each typestate descriptor, compiler emits one dispatch thunk that:

1. decodes machine state token and incoming envelope,
2. resolves handler by `(state_tag, event_kind)`,
3. applies precedence: state-local handler first, typestate-level fallback second,
4. stages outputs into `mc_machine_dispatch_txn_t`,
5. returns `MC_DISPATCH_OK`/`FAULT`/`STOP`.

Missing handler policy in v1: return `MC_DISPATCH_FAULT` with deterministic fault
code.

### 5. Compiler Safety Invariants

Compiler must guarantee at generated ABI boundaries:

1. state tag ↔ state payload coherence,
2. event kind ↔ payload layout coherence,
3. dispatch table completeness for reachable `(state, kind)` pairs,
4. reply envelope kind/tag correctness for `reply(value)` and `reply(cap, value)`,
5. no side effects outside staged txn outputs in managed handlers.

## Runtime Transaction Model (Execution Requirements)

Runtime is commit authority. Transaction unit is:
- next state token,
- outbox effects,
- subscription updates,
- request/reply correlation updates.

### Request/Reply Staging Requirement

`request`/`reply` effects must be staged and committed atomically with state.

Rationale:
- immediate request/reply side effects can violate rollback guarantees.
- transactional semantics require all-or-nothing delivery with state transition.

V1 requirement:
- runtime txn preflight must include request/reply capacity/validity checks.
- commit applies state+effects atomically.

## Execution Architecture

### 1. Generated Machine Descriptor Per Typestate

For each typestate used in managed mode, compiler emits descriptor data:
- state tag mapping,
- event kind mapping,
- handler table by `(state_tag, event_kind)`,
- payload/state layout metadata,
- protocol role/flow metadata needed for conformance/routing checks.

Descriptor is backend data, not user-visible source syntax.

### 2. Handler ABI

Compiler lowers each `on ...` handler to runtime-callable thunk ABI:

```c
mc_dispatch_result_t handler_fn(
  void* ctx,
  mc_machine_id_t machine_id,
  uint64_t current_state,
  const mc_machine_envelope_t* env,
  mc_machine_dispatch_txn_t* txn,
  uint64_t* fault_code
)
```

### 3. Send/Request/Reply Lowering

Inside handler lowering:
- `send(...)` => staged outbox effect,
- `request(...)` => staged request effect + staged correlation binding,
- `reply(payload)` => staged reply effect using implicit reply capability,
- `reply(cap, payload)` => staged reply effect consuming explicit capability.

Important: no immediate runtime delivery from handler body.

### 4. Transaction Commit Boundary

Runtime commit behavior:
- `MC_DISPATCH_OK` => commit staged unit atomically,
- `MC_DISPATCH_FAULT` / `MC_DISPATCH_STOP` => rollback staged unit.

### 5. Managed Entry API (Language/Std Bridge)

Need two source-callable API layers (std wrappers over intrinsics):

1. ergonomic default API:
   - implicit process runtime bootstrap/shutdown,
   - `Typestate::spawn(...)` and typed handle operations (`send/request`),
   - no user-managed step loop in common app code.

2. explicit control API (advanced/tests):
   - create runtime,
   - spawn/start/step/send against a runtime handle,
   - deterministic control for runtime tests and embedding.

Direct mode APIs remain unchanged.

## Managed API Semantics (V1)

### `Machine<T>`

`spawn` returns typed handle, not direct typestate value. This enforces managed
exclusivity via existing move semantics.

### Fallible Spawn/Send/Request

Managed spawn/send/request APIs are fallible:
- `Typestate::spawn(...) -> Machine<T> | MachineError`
- `send(...) -> () | MachineError`
- `request(...) -> u64 | MachineError` in default implicit-correlation form
- explicit advanced forms may expose `Pending<T>` where source code opts in

No silent drop at source API boundary.

### Step Status

`step()` returns status enum, not bool:
- `Idle`
- `DidWork`
- `Faulted(machine_id, code)`

This keeps loop control and observability explicit for advanced/runtime-test
paths while default app code uses implicit runtime dispatch.

## Compiler Pipeline Changes

### A. Elaboration

Add managed-machine elaboration products:
- `MachineDescriptorPlan` per typestate,
- handler thunk symbols + ABI shims,
- envelope/state encode/decode plans,
- layout/drop metadata references.

Keep these in elaboration side tables/plans, not type tables.

### B. Backend Lowering

Implement lowering for semantic send/request/reply operations and handler thunks:
- generate staging calls/records,
- materialize descriptors and thunk tables,
- lower `Pending` and `ReplyCap` runtime representation consistently.

### C. Driver / Link Integration

For binaries using managed machines:
- include machine runtime object (`machine_runtime.c`) in runtime link set,
- initialize implicit runtime context,
- register descriptors,
- run dispatcher without requiring user-managed step loops,
- shut down runtime context on process exit.

## Source-Level API Sketch (Ergonomic Target)

```mc
@machines
fn main() -> () | MachineError {
    let auth = AuthService::spawn()?;
    let gate = GatewayClient::spawn(auth)?;

    gate.send(AuthorizeReq { user: "alice" })?;
}
```

This keeps asynchrony at machine boundaries without infecting function types or
forcing explicit runtime wiring in app code.

### Explicit Runtime API Sketch (Advanced/Test Path)

Illustrative pseudocode (exact std wrapper names may differ as runtime APIs evolve):

```mc
requires {
    std::machine::Runtime
}

fn main() {
    let mut rt = Runtime::new();
    let auth = rt.spawn(AuthService::new());
    rt.start(auth);
    rt.step();
}
```

## Diagnostics

Add execution-path diagnostics (compiler + runtime):
- missing handler for descriptor event kind,
- descriptor/state/payload layout mismatch (compiler bug class),
- transactional commit preflight rejection surfaced with fault reason,
- dead-letter/fault hook telemetry for runtime-level delivery failures.

## Testing Strategy

### 1. Compiler Integration Tests

- compile+run single-machine event handling,
- compile+run two-machine request/reply success,
- compile+run denial/error reply path,
- compile+run rollback path (fault after staging).

### 2. Runtime Integration Tests

- transaction rollback keeps old state token,
- boxed payload drop correctness across commit/rollback/dead-letter,
- staged request/reply atomicity,
- reply-cap consumption and retry behavior.

### 3. Examples

Keep runnable fixtures current as lowering/runtime evolves:
- `examples/typestate/machine_events_check.mc`
- `examples/typestate/inter_machine_req_reply_check.mc`

These fixtures should have two coverage forms:
- ergonomic API tests (implicit runtime),
- explicit runtime API tests (deterministic stepping).

## Implementation Plan (Execution Milestone)

1. **Freeze ABI**: payload/state representation, tag assignment, invariants.
2. **Extend runtime txn model**: stage request/reply and payload/state drop paths.
3. **Generate descriptors+thunks in elaborate**: include tag/layout metadata.
4. **Lower send/request/reply/handlers in backend**: wire transactional staging.
5. **Add std/runtime bridge + driver/link wiring**: expose managed API.
   - include implicit runtime bootstrap/shutdown for binaries.
   - keep explicit runtime API for tests/embedding.
6. **Add end-to-end tests + runnable examples**: keep ergonomic and explicit
   runtime flows covered.

## Open Questions

1. Should implicit runtime bootstrap be always-on for binaries, or lazy-on-first
   `spawn`?
2. Should dead-letter/fault hooks be enabled by default in debug builds?
3. Do we add explicit `stop(handle)` in v1 or defer to post-v1?
