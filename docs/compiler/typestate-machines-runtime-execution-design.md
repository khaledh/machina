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
- frontend supports protocol/flow/on/emit/reply semantics (experimental),
- runtime has managed scheduler primitives (`runtime/machine_runtime.*`),
- backend/elaboration do not yet bridge typestate handlers/effects into runtime
  execution.

As a result, managed/event typestate examples are currently check-only.

## Goal

Enable this to run end-to-end:

1. compile typestate+protocol code,
2. create/start machines,
3. enqueue/send/request/reply through runtime,
4. dispatch handlers transactionally,
5. observe state transitions and outputs at runtime.

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
4. reply envelope kind/tag correctness for `reply(cap, value)`,
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

### 3. Emit/Reply Lowering

Inside handler lowering:
- `emit Send(...)` => staged outbox effect,
- `emit Request(...)` => staged request effect + staged pending token binding,
- `reply(cap, payload)` => staged reply effect consuming reply capability.

Important: no immediate runtime delivery from handler body.

### 4. Transaction Commit Boundary

Runtime commit behavior:
- `MC_DISPATCH_OK` => commit staged unit atomically,
- `MC_DISPATCH_FAULT` / `MC_DISPATCH_STOP` => rollback staged unit.

### 5. Managed Entry API (Language/Std Bridge)

Need source-callable API (std wrappers over intrinsics) for:
- create runtime,
- spawn machine with descriptor + initial state,
- start machine,
- enqueue external events,
- run one/loop dispatch steps.

Direct mode APIs remain unchanged.

## Managed API Semantics (V1)

### `Handle<P, R>`

`spawn` returns typed handle, not direct typestate value. This enforces managed
exclusivity via existing move semantics.

### Fallible Send/Request

Managed send/request APIs are fallible:
- `send(...) -> SendResult`
- `request(...) -> RequestResult<Pending<T>>`

No silent drop at source API boundary.

### Step Status

`step()` returns status enum, not bool:
- `Idle`
- `DidWork`
- `Faulted(machine_id, code)`

This keeps loop control and observability explicit.

## Compiler Pipeline Changes

### A. Elaboration

Add managed-machine elaboration products:
- `MachineDescriptorPlan` per typestate,
- handler thunk symbols + ABI shims,
- envelope/state encode/decode plans,
- layout/drop metadata references.

Keep these in elaboration side tables/plans, not type tables.

### B. Backend Lowering

Implement lowering for semantic `Emit` / `Reply` nodes and handler thunks:
- generate staging calls/records,
- materialize descriptors and thunk tables,
- lower `Pending` and `ReplyCap` runtime representation consistently.

### C. Driver / Link Integration

For binaries using managed machines:
- include machine runtime object (`machine_runtime.c`) in runtime link set,
- initialize runtime,
- register descriptors,
- execute user-driven stepping loop.

## Source-Level API Sketch (V1)

```mc
requires {
    std::machine::Runtime
}

fn main() -> u64 {
    let mut rt = Runtime::new();

    let auth = rt.spawn(AuthService::new());
    let gate = rt.spawn(GatewayClient::new(auth));
    rt.start(auth);
    rt.start(gate);

    // external stimulus
    let _ = rt.send(gate, AuthorizeReq { user: "alice" });

    loop {
        match rt.step() {
            StepStatus::DidWork => {}
            StepStatus::Idle => break,
            StepStatus::Faulted(id, code) => {
                // V1: application-defined fault policy handling
                break;
            }
        }
    }

    0
}
```

This keeps asynchrony at machine boundaries without infecting function types.

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

Promote check-only fixtures to runnable fixtures once lowering lands:
- `examples/typestate/machine_events_check.mc`
- `examples/typestate/inter_machine_req_reply_check.mc`

## Implementation Plan (Execution Milestone)

1. **Freeze ABI**: payload/state representation, tag assignment, invariants.
2. **Extend runtime txn model**: stage request/reply and payload/state drop paths.
3. **Generate descriptors+thunks in elaborate**: include tag/layout metadata.
4. **Lower emit/reply/handlers in backend**: wire transactional staging.
5. **Add std/runtime bridge + driver/link wiring**: expose managed API.
6. **Add end-to-end tests + runnable examples**: replace check-only fixtures.

## Open Questions

1. Should v1 also provide `run_until_idle()` wrapper over repeated `step()`?
2. Should dead-letter/fault hooks be enabled by default in debug builds?
3. Do we add explicit `stop(handle)` in v1 or defer to post-v1?
