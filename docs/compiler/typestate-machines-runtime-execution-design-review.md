# Typestate Machines: Runtime Execution Integration — Design Review

Review of the proposal in
[typestate-machines-runtime-execution-design.md](typestate-machines-runtime-execution-design.md).

## Status

Review completed February 2026.

Three independent expert panels evaluated the design from different
perspectives, each running on a different model architecture.

| Panel | Model | Perspective | Rating |
|-------|-------|-------------|--------|
| A | Claude Opus 4.6 | Type Systems, Soundness, Semantic Correctness | 6.5/10 |
| B | GPT-5.3-Codex | Runtime Engineering, Compiler Backend, Systems | 6/10 |
| C | Gemini 3 Pro | Devil's Advocate, DX, Practical Pitfalls | 4/10 |

Cross-model average: **5.5/10** — sound architecture, critical specification
gaps at the compiler↔runtime boundary.

## Implementation Context

Before reviewing the design, the panels were given full context on what already
exists:

| Component | Status |
|-----------|--------|
| Runtime C library (lifecycle, mailbox, ready queue, txn dispatch, request/reply correlation, dead-letter/fault hooks) | Complete (~930 lines, fully tested) |
| Parser (protocol, role, flow, on handlers, emit Send/Request, reply, Pending, ReplyCap) | Complete |
| Type checking (handler validation, protocol conformance, ReplyCap linearity via CFG dataflow) | Complete |
| Elaboration for emit/reply | Missing (marked unreachable) |
| Backend lowering for emit/reply | Missing |
| Machine descriptor generation | Missing |
| Managed entry std API | Missing |
| End-to-end execution | Missing |

The design doc under review is specifically about bridging the implemented
frontend and runtime.

---

## Consensus Strengths

### 1. The Runtime's Transactional Model Is Genuinely Well-Built (3/3)

All panels praised the transactional dispatch design:
- Preflight validates all destinations exist and have mailbox capacity BEFORE
  any mutation.
- On `MC_DISPATCH_OK`: commits state + outbox + subscriptions atomically.
- On `MC_DISPATCH_FAULT`/`STOP`: rollback — no staged effects are committed.
- Preflight failure after OK becomes FAULT (prevents partial commits).
- Reply capability consumption is atomic with envelope delivery.

This is better than most actor runtimes at this maturity stage.

### 2. The Typed Correlation System Is Strong (2/3)

Panels A and B noted that the earlier design review's "correlation by
convention" concern has been addressed: `Pending<T>` and `ReplyCap<T>` are
already genuine type-level entities with CFG-based linearity analysis
(double-consume detection, must-consume-on-all-paths enforcement). This is
production-quality infrastructure for an experimental feature.

### 3. Handler ABI Shape Is Correct (3/3)

The callback form with staged transaction is the right design — it cleanly
separates handler logic from commit authority. The runtime is "commit
authority," not the compiler-generated code.

### 4. Phased Approach Is Sound (2/3)

The 6-step implementation plan (lower emit/reply → descriptors → std bridge →
driver → tests → examples) is sequentially sensible and delivers incrementally.

---

## Consensus Concerns

### 1. CRITICAL: Payload Capacity and Ownership (3/3)

Unanimously identified as the **blocking issue** that must be resolved before
any implementation begins.

The runtime envelope has two `uint64_t` payload slots (16 bytes total):

```c
typedef struct mc_machine_envelope {
    uint64_t kind;
    uint64_t src;
    uint64_t reply_cap_id;
    uint64_t pending_id;
    uint64_t payload0;    // 8 bytes
    uint64_t payload1;    // 8 bytes
} mc_machine_envelope_t;
```

Real payloads like `AuthorizeReq { user: string }` don't fit — a string alone
is `{ ptr, len, cap }` = 24 bytes.

**Questions the design must answer:**
- Is the payload heap-boxed? If so, who allocates and who frees?
- What happens to boxed payloads on transactional rollback?
- What happens to payloads in dead-lettered envelopes?
- Can the C runtime call a Machina destructor for dropped payloads?
- If the sender's data is stack-allocated, copying into the envelope may cause
  use-after-free.

**Recommended approach (Panel A):** Heap-box payloads for V1. `payload0` =
pointer to heap-allocated struct, `payload1` = type tag or size. Generated
handler casts and frees. Define ownership transfer as: sender allocates, runtime
owns during transit, receiver's handler consumes (frees after use), dead-letter
hook frees on drop. Consider inline-if-small optimization later.

### 2. CRITICAL: State Data Storage and Rollback (3/3)

The runtime stores machine state as a single `uint64_t state_word`. But states
like `AwaitAuth { pending_corr: u64, fd: u64 }` have more data than fits in
64 bits.

**The transactional gap:** If a handler constructs a new state variant and
writes it into the machine context, then faults — the `state_word` tag rolls
back, but the context memory now contains garbage from the partially-constructed
new state.

**Questions the design must answer:**
- Where do state variant fields live? (Heap pointer? Inline union? Arena slot?)
- How is the max state size computed? (`Max(SizeOf(State_A), SizeOf(State_B),
  ...)` for the union of all states.)
- On rollback, is the old state preserved? (Requires either copy-on-write or
  building the new state on the stack and only writing to the slot on commit.)

**Recommended approach (Panel A):** Generated handler must not write the new
state into the machine slot until it returns `MC_DISPATCH_OK`. Build the new
state on the stack; copy to the slot only as part of the commit path. This is
a codegen contract that must be specified.

### 3. HIGH: Missing Compiler Invariants Specification (3/3)

The design doc proposes an untyped ABI without defining what invariants the
compiler must establish to make the untyped boundary safe.

**Required compiler contracts:**

1. **State tag ↔ state data coherence.** Whenever `current_state == N`, the
   machine context contains a valid instance of the state variant mapped to
   tag N. The descriptor must make this mapping bijective.

2. **Event kind ↔ payload coherence.** Whenever `env->kind == K`, the payload
   slots contain valid data for the payload type mapped to kind K.

3. **Dispatch table completeness.** For every reachable `(state_id,
   event_kind)` pair, the descriptor maps to a valid handler or the runtime
   has defined fallback behavior (fault, dead-letter).

4. **Side-effect boundary.** Handler code must not perform effects outside
   `emit`/`reply`/state construction. (See Transactional Semantics below.)

5. **Kind tag consistency.** Event kind tags must be consistent across
   compilation units within a capsule.

### 4. HIGH: Event Kind Tag Assignment Strategy (2/3)

If `AuthorizeReq` is assigned kind tag 5 in one compilation unit and kind tag 7
in another, inter-machine communication breaks silently. The design doc mentions
"event kind mapping" but doesn't specify:
- Are tags global or per-typestate?
- How are they assigned? (Hash, sequential, declared?)
- What happens in a multi-module capsule?

**Recommendation:** Sequential per-capsule assignment for V1. Document as a
constraint.

### 5. HIGH: Implementation Plan Has a Dependency Inversion (2/3)

Step 1 (lower emit/reply) depends on Step 2 details (descriptor/effect ABI).
The correct order is:

1. Phase 0: Freeze execution ABI (state encoding, payload encoding,
   drop/rollback contract)
2. Phase 1: Extend runtime txn model if needed (stage request/reply as effects)
3. Phase 2: Generate descriptors + handler ABI thunks in elaborate
4. Phase 3: Implement backend lowering + link runtime symbols
5. Phase 4: Add std bridge API + driver wiring
6. Phase 5: End-to-end tests + convert examples

Additionally, `machine_runtime.c` is not currently in the link path — the
runtime source list in `src/main.rs` must be updated.

---

## Panel-Specific Findings

### Panel A: Type Systems (Claude Opus 4.6)

Unique contributions:

1. **Pending/ReplyCap type erasure analysis.** The type parameter `T` (response
   set) is fully erased at the ABI boundary. This is safe if and only if: (a)
   the compiler statically verified the handler produces only response types in
   T, (b) linearity is enforced, (c) the runtime routes by capability ID, and
   (d) the compiler generates correct kind tags in reply envelopes. Points
   a-c are done. **Point d is unspecified** — how does `reply(cap,
   AuthApproved{})` set the `kind` field so the requester's `on Response(pending,
   AuthApproved)` matches it?

2. **Managed exclusivity enforcement.** The correct approach uses Machina's
   existing move semantics: `rt.spawn(T::new())` consumes the value; returns
   a `Handle<P, R>` with no direct transition methods. No new enforcement
   mechanism is needed — but the design doc's API sketch doesn't make this
   explicit. The return type must be `Handle`, not the original typestate.

3. **Side-effect boundary.** Handler code that writes to heap memory, calls
   runtime functions directly, or modifies `ctx` fields before commit can
   violate transactional semantics. For V1, recommend accepting this limitation
   and documenting it (same as Erlang — messages are transactional, process-
   internal mutations are not). Longer term, enforce purity statically.

4. **Dispatch thunk structure.** Must be specified — it's the sole runtime-
   visible entry point per typestate. Should include: state-local handler
   first, typestate-level default as fallback, missing-handler fault path.

### Panel B: Runtime and Compiler (GPT-5.3-Codex)

Unique contributions:

1. **Runtime txn model gap for request/reply.** Current txn stages only outbox
   and subscriptions. `__mc_machine_runtime_request()` and
   `__mc_machine_runtime_reply()` are immediate side effects, not staged in the
   transaction. If a handler calls `reply()` and then faults, the reply is
   already delivered but the state change is rolled back. This violates
   transactional semantics.

   **This is a real runtime bug/gap.** The fix: request/reply must be staged
   as outbox effects and committed atomically, not executed immediately during
   the handler.

2. **Backend type lowering.** `Pending` and `ReplyCap` types currently hit an
   unsupported-type panic in `src/core/backend/lower/types.rs`. These need
   explicit lowering decisions (likely `uint64_t` cap IDs).

3. **Descriptor placement.** Belongs in elaborate-side tables (after
   monomorphization), with backend only materializing globals and thunks.
   Monomorphization already runs before elaborate in the compile flow, so
   descriptors see concrete types.

4. **Realistic MVP slice.** Single-machine + `emit Send` only + fixed-size
   payload subset + explicit `while rt.step() {}`. Defer request/reply to a
   second increment. This is the minimum viable end-to-end slice.

### Panel C: Devil's Advocate (Gemini 3 Pro)

Unique contributions:

1. **The "Phantom Reply" deadlock.** A client sends a request to a server with
   a full mailbox. The request is rejected (or dead-lettered). The client enters
   `Waiting` state with a `Pending` token, expecting a reply that will never
   come. The client waits forever.

   The `Pending` type in the client state implies a contract that a result
   *will* come, but the runtime transport is lossy (bounded mailboxes). The type
   system promises safety that the runtime physically contradicts.

   **This is a real protocol-level livelock scenario** that the current design
   doesn't address. Solutions include: timeouts, request-failed completion
   events, or making request sends block until mailbox has space (violates
   run-to-completion).

2. **`rt.step()` API is opaque.** What does `true`/`false` mean? "Did work"?
   "Queue not empty"? If a machine crashes inside step, does it return false?
   Panic? The developer has no visibility into what happened.

   **Recommendation:** `rt.step()` should return a status enum
   (`Idle | DidWork | MachineFault(id)`) or similar. Additionally, provide
   `rt.run_until_idle(callback)` with observability hooks for tracing.

3. **`rt.send()` must be fallible.** If the target mailbox is full, the event
   silently vanishes. The source API must return `Result<(), SendError>` so
   developers can handle backpressure.

4. **State size union.** The compiler must compute the maximum state size across
   all variants to allocate the backing store. This is the same problem as enum
   layout in Rust — compute max variant size, use that for the slot. In-place
   transition is a `memcpy` of the new variant into the fixed-size slot.

---

## Concrete Failure Scenario: The Phantom Reply Deadlock

A realistic scenario where the design silently fails:

```
// Server has bounded mailbox (cap=1)
let auth = rt.spawn(AuthService::new());
let gate = rt.spawn(GatewayClient::new(auth.id()));
rt.start(auth);
rt.start(gate);

// Fill server mailbox
rt.send(auth, SomeOtherMsg {});

// Client tries to authorize — emits Request to server
rt.send(gate, StartAuth { user: "alice" });

// Client handler runs: emit Request(to: auth, AuthorizeReq{...})
// Server mailbox is full → request is dead-lettered
// Client transitions to AwaitAuth { pending: Pending<...> }
// Client holds a Pending token, waiting for a Response event
// Server never received the request → never replies
// Client waits forever in AwaitAuth
```

**No error is produced.** The dead-letter hook fires (if configured) but the
client has no mechanism to learn that its request was dropped. The `Pending`
type implies a guarantee the runtime cannot uphold.

**Desired behavior:** Either (a) `emit Request` returns a result type so the
handler can transition to an error state, or (b) the runtime injects a
`RequestFailed` completion event to the sender when the request cannot be
delivered.

---

## Synthesized Recommendations

Ordered by priority. Confidence reflects cross-model agreement.

| # | Recommendation | Confidence | Source |
|---|---------------|------------|--------|
| 1 | **Define payload boxing/ownership strategy** — who allocates, who frees, what happens on rollback and dead-letter | Very High | All 3 |
| 2 | **Define state data storage and rollback contract** — new state built on stack, written to slot only on commit | Very High | All 3 |
| 3 | **Add "Compiler Invariants" section** enumerating every safety property the compiler must guarantee for the untyped ABI | Very High | All 3 |
| 4 | **Stage request/reply in txn** — current runtime executes request/reply immediately, violating transactional semantics | Very High | B |
| 5 | **Reorder implementation plan** — freeze ABI first, extend runtime txn, THEN lower compiler constructs | High | B, A |
| 6 | **Define event kind tag assignment** — sequential per-capsule for V1 | High | A, B |
| 7 | **Make `rt.send()` fallible** — return `Result<(), SendError>`, not void | High | C |
| 8 | **Introduce `Handle<P, R>`** as typed wrapper around machine ID | High | A |
| 9 | **Address request delivery failure** — Pending token implies a guarantee the runtime can't uphold with bounded mailboxes | High | C |
| 10 | **Specify dispatch thunk structure** including default handler fallback and missing-handler behavior | Medium | A |
| 11 | **Make `rt.step()` return status enum** or provide observability hooks | Medium | C |
| 12 | **Add `machine_runtime.c` to link path** in `src/main.rs` | Medium | B |

---

## Comparison to Previous Design Review

The earlier review of `typestate-machines-async-events-design.md` raised
concerns that are now partially addressed:

| Previous Concern | Status |
|-----------------|--------|
| Correlation by convention | **Resolved** — `Pending<T>` and `ReplyCap<T>` with linearity checking |
| Transactional handler semantics | **Partially resolved** — runtime has txn model, but request/reply bypass it |
| Managed exclusivity | **Designed** — move semantics described, not yet enforced |
| Typed machine handles | **Not addressed** — examples still use raw `u64` |
| Failure semantics | **Partially resolved** — fault policy exists, but request delivery failure creates Phantom Reply deadlock |
| Hierarchical event handlers | **Not addressed** in this doc (runtime execution scope) |

## Open Questions From the Design Doc

The doc listed three open questions. Panel assessment:

1. **Dispatch loop: explicit `while rt.step()` or default `rt.run()`?**
   Start with explicit stepping for V1 (debuggable, deterministic). Add
   `rt.run_until_idle(callback)` as a convenience once observability hooks
   exist. (Panels A, C)

2. **How much runtime tracing by default?**
   Dead-letter and fault hooks should be on by default in debug builds. Add
   queue-depth and handler-duration metrics as opt-in. (Panel B)

3. **Explicit machine shutdown or rely on process teardown?**
   Process teardown is acceptable for V1. Add explicit `rt.stop(id)` for
   orderly shutdown later. Document that pending `Pending` tokens become
   orphaned on teardown. (Panels A, C)

---

## Summary Verdict

The two halves — compiler frontend and C runtime — are each individually
strong and well-tested. The bridge specification between them, which is the
subject of this document, needs one more revision focused on three areas:

1. **Payload representation** (blocking — cannot generate code without this)
2. **State storage and rollback** (blocking — affects codegen correctness)
3. **Compiler invariants documentation** (non-blocking but high risk without it)

Once these are addressed, the implementation plan is tractable and the
architecture is sound. This is a **"needs spec revision before implementation"**
situation, not a **"rethink the approach"** situation.
