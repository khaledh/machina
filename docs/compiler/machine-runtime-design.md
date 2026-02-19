# Machine Runtime Design

## Overview

The machine runtime is a message-passing scheduler for managed state machines.
It owns machine lifecycle, mailbox dispatch, transactional handler execution, and
inter-machine request/reply correlation — a tiny actor system embedded in
compiled code.

This document covers runtime architecture, ABI contracts, and the
compiler→runtime integration bridge. For the source-level typestate language
model, see `typestate-design.md`. For the user-facing API, see
`machine-surface-design.md`.

## The Big Picture

```
┌──────────────────────────────────────────────────────┐
│                  Machine Runtime                     │
│                                                      │
│  ┌───────────┐  ┌───────────┐  ┌───────────┐         │
│  │ Machine 1 │  │ Machine 2 │  │ Machine 3 │         │
│  │ RUNNING   │  │ CREATED   │  │ RUNNING   │         │
│  │           │  │           │  │           │         │
│  │ ┌───────┐ │  │ ┌───────┐ │  │ ┌───────┐ │         │
│  │ │Mailbox│ │  │ │Mailbox│ │  │ │Mailbox│ │         │
│  │ │[✉][✉] │ │  │ │[✉]    │ │  │ │       │ │         │
│  │ └───────┘ │  │ └───────┘ │  │ └───────┘ │         │
│  └───────────┘  └───────────┘  └───────────┘         │
│                                                      │
│  Ready Queue: [ 1 ]                                  │
│  ~~~~~~~~~~~~~~~▲~~~~~~~~~~~~~~~~~~                  │
│                 │                                    │
│          "Machine 1 has                              │
│           mail to process"                           │
└──────────────────────────────────────────────────────┘
```

## Core Concepts

### Machines = Slots in a Table

Each machine is a numbered slot (1-based IDs). A slot holds:

- **Lifecycle state** — `CREATED -> RUNNING -> FAULTED/STOPPED`
- **State word** — an opaque value owned by compiler-generated code
- **Mailbox** — a bounded ring buffer of envelopes
- **Dispatch callback** — the function to call when processing a message

```
Machine Table (dense array):
┌─────┬───────────┬────────────┬─────────────┐
│ ID  │ Lifecycle │ State Word │ Mailbox     │
├─────┼───────────┼────────────┼─────────────┤
│  1  │ RUNNING   │ 0xABC...   │ [✉][✉][ ]   │
│  2  │ CREATED   │ 0x000...   │ [✉][ ][ ]   │
│  3  │ STOPPED   │ 0x000...   │ [ ][ ][ ]   │
└─────┴───────────┴────────────┴─────────────┘
```

### Envelopes = Messages Between Machines

An envelope carries a typed payload from one machine to another:

```
┌─ Envelope ──────────────────────────┐
│  kind:       42  (event type tag)   │
│  src:        1   (sender machine)   │
│  payload0:   -> heap pointer        │
│  payload1:   layout id for glue     │
│  reply_cap:  0   (for requests)     │
│  pending_id: 0   (for responses)    │
└─────────────────────────────────────┘
```

### Ready Queue = "Who Has Work?"

A ring buffer of machine IDs that have pending mail. Only `RUNNING` machines
with non-empty mailboxes get queued. Each machine appears at most once (guarded
by `in_ready_queue` flag).

```
Ready Queue:  head              tail
               ↓                 ↓
             [ 1 | 3 | 5 | _ | _ ]
               └──── 3 items ────┘
```

### Dispatch = One Message At A Time

The core loop (`dispatch_one_txn`) does exactly one step:

```
  ┌───────────────────────────────────────────────┐
  │              dispatch_one_txn                 │
  │                                               │
  │  1. Pop machine ID from ready queue           │
  │  2. Pop one envelope from its mailbox         │
  │  3. Call the dispatch callback                │
  │  4. Callback fills in a transaction (txn)     │
  │  5. Preflight: can all effects be committed?  │
  │  6. Commit: apply state + deliver messages    │
  │     OR                                        │
  │  6. Fault/Stop: discard everything            │
  └───────────────────────────────────────────────┘
```

## Lifecycle Model

```
spawn(cap=4)     start()        enqueue(✉)      dispatch()
    │               │               │               │
    ▼               ▼               ▼               ▼
 CREATED ──────→ RUNNING ──────→ RUNNING ──────→ RUNNING
 (buffered)     (scheduled)    (has mail,       (processed 1,
                                in ready queue)  maybe faulted)
                                    │
                          fault ────┤──── stop
                            │       │      │
                            ▼       │      ▼
                         FAULTED    │   STOPPED
                                    │
                            (pending entries cleaned up
                             on fault/stop transitions;
                             STOPPED also drains/releases
                             per-machine resources)
```

- `spawn` creates in `Created`.
- `start` transitions `Created -> Running`.
- If messages were buffered while `Created`, `start` schedules machine on ready
  queue.

When a machine enters `STOPPED`, runtime performs eager per-machine cleanup:
- pending correlations for that requester are reclaimed,
- machine-owned subscriptions are removed,
- mailbox envelopes are drained and mailbox storage is released,
- dispatch/descriptor/state pointers are cleared.

Machine IDs remain stable and reserved; stale handles continue to observe
`NotRunning` instead of becoming `Unknown`.

## Transaction Model

Every dispatch is **transactional** — all-or-nothing. A callback produces a
`txn` struct with staged effects:

```
┌─ Transaction ────────────────────────────┐
│                                          │
│  State update:  state_word = 0xDEF       │
│                                          │
│  Outbox:        [send ✉ to Machine 3]    │
│                 [send ✉ to Machine 5]    │
│                                          │
│  Requests:      [request to Machine 2    │
│                  with pending_id=7]      │
│                                          │
│  Replies:       [reply via cap_id=3]     │
│                                          │
│  Subscriptions: [add kind=42 route=1]    │
│                                          │
└──────────────────────────────────────────┘
```

**Preflight** checks everything before mutating anything:
- Do all destination machines exist and accept messages?
- Is there enough mailbox space everywhere?
- Are pending IDs valid and unique?

If preflight passes → **commit all at once**. If anything fails → **fault**,
nothing changes.

```
Callback returns OK ──→ Preflight ──→ ✓ Commit all effects
                                  └──→ ✗ Fault (rollback)

Callback returns FAULT ──→ Apply fault policy, discard effects
Callback returns STOP  ──→ Mark STOPPED, discard effects, eager cleanup
```

### Request/Reply Staging

`request`/`reply` effects are staged and committed atomically with state.
Immediate request/reply side effects would violate rollback guarantees.

Runtime txn preflight includes request/reply capacity/validity checks. Commit
applies state + effects atomically.

## Request/Reply Correlation

Machines can do request/reply, not just fire-and-forget sends:

```
  Machine 1                              Machine 2
  ─────────                              ─────────
      │                                      │
      │── request(dst=2, kind=Ping) ────────→│
      │   reply_cap_id=7 minted              │
      │   pending table: {7 -> requester=1}  │
      │                                      │
      │                    reply(cap=7) ─────│
      │←─── response envelope ───────────────│
      │     pending_id=7                     │
      │     pending table: entry consumed    │
```

The **pending table** tracks inflight requests. Each entry maps a `pending_id`
back to the requester. When a reply arrives, the runtime routes it back and
consumes the capability.

## Ordering and Delivery Guarantees (V1)

1. One event at a time per machine.
2. FIFO per receiver mailbox.
3. No global order guarantee across different senders.
4. Bounded mailbox by default.
5. `send/request` returns typed delivery/backpressure errors.

## Fault Policy (V1)

Default policy:
- log fault diagnostics,
- mark machine `Faulted`,
- stop further dispatch for that machine.

Required runtime hooks:
- dead-letter callback
- mailbox overflow/backpressure callback
- machine fault callback

Source-level managed API does not expose restart; low-level runtime lifecycle
overrides are test/runtime-internal control paths.

---

## ABI Freeze (V1)

This section is normative for implementation order and correctness.

### 1. Envelope Payload ABI (Boxed)

`mc_machine_envelope_t` has two 64-bit payload slots used as boxed payload
metadata:

- `payload0`: pointer to heap box holding payload value bytes.
- `payload1`: payload layout/type id (used for decode + drop glue lookup).

Ownership contract:
1. Sender-side lowering allocates payload box and transfers ownership to runtime
   enqueue/request staging.
2. Runtime owns payload while envelope is in mailbox/ready transit.
3. On successful handler dispatch, handler decode consumes payload box.
4. On dead-letter/drop/rollback paths, runtime drops payload via layout id.

No inline payload optimization in V1.

### 2. Machine State ABI (Opaque Token + Commit Swap)

V1 treats `state_word` as opaque machine-state token owned by generated code.

Recommended token representation: pointer to heap state object containing
`{state_tag, state_payload}`.

Commit/rollback contract:
1. Handler decodes current token but does not mutate committed state object.
2. Handler builds next-state token separately.
3. Runtime swaps state token only on successful transactional commit.
4. On rollback/fault/stop, old token remains current and staged token is dropped.

### 3. Event Kind Tag Assignment (Capsule-Deterministic)

Event kind tags are assigned sequentially per capsule during descriptor build.

Rules:
1. One payload type maps to one stable tag in the capsule.
2. All modules in the capsule share the same mapping.
3. Descriptor generation is the single source of truth.

### 4. Dispatch Thunk Contract

For each typestate descriptor, compiler emits one dispatch thunk that:
1. Decodes machine state token and incoming envelope.
2. Resolves handler by `(state_tag, event_kind)`.
3. Applies precedence: state-local handler first, typestate-level fallback
   second.
4. Stages outputs into `mc_machine_dispatch_txn_t`.
5. Returns `MC_DISPATCH_OK`/`FAULT`/`STOP`.

Missing handler policy: return `MC_DISPATCH_FAULT` with deterministic fault code.

### 5. Compiler Safety Invariants

Compiler must guarantee at generated ABI boundaries:
1. State tag ↔ state payload coherence.
2. Event kind ↔ payload layout coherence.
3. Dispatch table completeness for reachable `(state, kind)` pairs.
4. Reply envelope kind/tag correctness for `reply(value)` and
   `reply(cap, value)`.
5. No side effects outside staged txn outputs in managed handlers.

---

## Descriptor-Driven Dispatch

Instead of manually binding callbacks, the compiler generates a **descriptor
blob** — a binary table that maps `(state_tag, event_kind)` → `thunk_id`:

```
Descriptor for "TrafficLight" machine:
┌────────────┬────────────┬───────────────┐
│ State Tag  │ Event Kind │ Thunk ID      │
├────────────┼────────────┼───────────────┤
│ 1 (Red)    │ 10 (Timer) │ thunk_42      │
│ 2 (Green)  │ 10 (Timer) │ thunk_43      │
│ 2 (Green)  │ 20 (Stop)  │ thunk_44      │
└────────────┴────────────┴───────────────┘

Thunk Registry (global):
  thunk_42 -> fn red_on_timer(...)
  thunk_43 -> fn green_on_timer(...)
  thunk_44 -> fn green_on_stop(...)
```

When an envelope arrives, the runtime looks up the row, resolves the thunk, and
calls it. The thunk carries a `next_state_tag` so the state machine transitions
automatically.

### Descriptor Binary Format

```
Binary format (little-endian):

Offset  Content
──────  ───────
0x00    "MCHD"              (magic bytes)
0x04    1                   (schema version, u32)
0x08    len + "Ping"        (typestate name, length-prefixed string)
        state_count         (u32)
        event_count         (u32)
        row_count           (u32)
        role_count          (u32)
        ... state entries   (tag, def_id, layout_ty, name)
        ... event entries   (kind, layout_ty, key_kind, payload type name)
        ... dispatch rows   (state_tag, event_kind, site_key,
                             state_local_thunk_id, fallback_thunk_id)
        ... role entries    (role path, def_id)
```

This blob is embedded as a global byte array in the generated code.

### Handler ABI

Each handler needs an ABI adapter. The runtime calls thunks with a fixed C
signature, but handlers have typed parameters. The thunk bridges the gap:

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

Thunk operation:
1. Cast `current_state` (u64) → ptr to state struct.
2. Load payload from `env.payload0` → ptr to payload type.
3. Call the actual handler function.
4. Heap-box the returned state (min 1 byte for runtime).
5. Write txn: `has_next_state=1`, `next_state=boxed_ptr`.
6. Return `MC_DISPATCH_OK` (0).

For response handlers, the thunk also extracts `env.origin_payload0` and
`env.origin_request_site_key` to pass provenance information to the handler.

## Emit ABI (Staging Shims)

Compiler-generated handler code calls global ABI shims instead of getting a
runtime pointer:

```c
// Inside a handler — no runtime pointer needed:
__mc_machine_emit_send(dst=3, kind=42, payload0=..., payload1=...);
__mc_machine_emit_request(dst=2, kind=10, ...);
__mc_machine_emit_reply(cap=7, kind=20, ...);
```

These work because the runtime sets up a **staging context** before calling the
handler:

```
dispatch_one_txn:
  ┌─ staging_begin(ctx, rt, machine_id) ────────┐
  │                                             │
  │  handler runs, calls emit_send/request/reply│
  │  -> effects accumulate in staging ctx       │
  │                                             │
  ├─ merge staged effects into txn ─────────────┤
  │                                             │
  ├─ preflight + commit (or rollback) ──────────┤
  │                                             │
  └─ staging_end(ctx) ──────────────────────────┘
```

## The Bridge Layer

The public API uses typed C structs (`mc_machine_runtime_t *rt`), but Machina's
compiler emits code passing everything as `u64` integers. The bridge wraps every
API:

```
// Typed API (used by C tests):
__mc_machine_runtime_spawn(rt, mailbox_cap, &out_id);

// Bridge API (used by compiled Machina code):
__mc_machine_runtime_spawn_u64(runtime_handle, mailbox_cap);
//                              ^^^^^^^^^^^^^^
//                              rt pointer cast to u64
```

There is also a **managed runtime** — a process-global singleton so `@machines`
entrypoints don't need to thread a runtime handle manually:

```
__mc_machine_runtime_managed_bootstrap_u64()  -> creates or returns global
__mc_machine_runtime_managed_current_u64()    -> returns global (or 0)
__mc_machine_runtime_managed_shutdown_u64()   -> drops global
```

---

## Compiler Integration

### End-to-End Pipeline

```
  Machina Source             Compiler Pipeline              Executable
  ──────────────             ─────────────────              ──────────

  typestate Ping {     ┌─── Desugar ───────────┐
    state Ready { }    │  Generate hidden      │
    on Msg -> Ready    │  types + handlers     │
    ...                └──────────┬────────────┘
  }                               │
                       ┌──────────▼────────────┐
  @machines            │  Elaborate            │
  fn main() {          │  Build machine plans: │
    ...                │  state tags, event    │
  }                    │  kinds, dispatch rows │
                       └──────────┬────────────┘
                                  │
                       ┌──────────▼────────────┐
                       │  Backend Lower        │
                       │  - Serialize desc blob│
                       │  - Generate thunks    │
                       │  - Generate bootstrap │
                       │  - Wrap main()        │
                       └──────────┬────────────┘
                                  │
                       ┌──────────▼────────────┐
                       │  Code Generation      │        ┌──────────────┐
                       │  IR -> ARM64 asm      │──────→ │  Linked with │
                       └───────────────────────┘        │  C runtime   │
                                                        └──────────────┘
```

### Step 1: Typestate Desugaring

See `typestate-design.md` for full details. The key insight: the user writes a
typestate declaration, but the rest of the compiler sees ordinary structs and
functions.

### Step 2: Elaboration — Building Machine Plans

The elaboration pass (`machine_plan.rs`) inspects desugared types and handlers
to build a **machine plan** — a complete description of the typestate's runtime
shape.

**State tags** — deterministic 1-based indices:

```
State          Tag
─────          ───
Ready           1
Waiting         2
Done            3
```

**Event kinds** — deterministic 1-based indices for each distinct payload type:

```
Payload Type   Kind
────────────   ────
Msg             1
Ping            2
Pong            3
```

**Dispatch table** — maps `(state_tag, event_kind)` to handler thunks:

```
┌───────────┬────────────┬──────────────────┬────────────────────┐
│ State Tag │ Event Kind │ State-Local Thunk│ Fallback Thunk     │
├───────────┼────────────┼──────────────────┼────────────────────┤
│ 1 (Ready) │ 1 (Msg)    │ __ts_on_1        │ (none)             │
│ 1 (Ready) │ 2 (Ping)   │ (none)           │ __ts_on_2          │
│ 2 (Wait)  │ 2 (Ping)   │ (none)           │ __ts_on_2          │
└───────────┴────────────┴──────────────────┴────────────────────┘
```

**Thunk plans** — each handler gets a thunk record:

```
Thunk                          Handler         Next State Tag
─────                          ───────         ──────────────
__mc_machine_dispatch_thunk_42  __ts_on_1       1 (Ready)
__mc_machine_dispatch_thunk_43  __ts_on_2       2 (Wait)
```

### Step 3: Backend Lowering — Generating Artifacts

The backend generates three kinds of artifacts:

**3a. Descriptor blob** — the dispatch table serialized into binary format
(parsed by `__mc_machine_runtime_register_descriptor` in `descriptor.c`).
Embedded as a global byte array in generated code.

**3b. Dispatch thunks** — ABI adapters bridging the fixed C thunk signature to
typed handler parameters.

**3c. Bootstrap function** — `__mc_machine_bootstrap` (a weak symbol called once
on first `__mc_machine_runtime_new`):

```
fn __mc_machine_bootstrap():
    // Register every thunk with its metadata:
    __mc_machine_runtime_register_thunk_meta_u64(42, &thunk_42, next_tag=1)
    __mc_machine_runtime_register_thunk_meta_u64(43, &thunk_43, next_tag=2)

    // Register every descriptor blob:
    __mc_machine_runtime_register_descriptor_u64(&ping_desc_bytes, len)
```

### Step 4: Spawn — Creating a Machine

When user code calls `Typestate::spawn(...)`, the compiler lowers it to a
generated spawn wrapper:

```
fn __ts_spawn_Ping(arg: u64) -> __mc_machine_handle_Ping | MachineError {
    let rt = __mc_machine_runtime_managed_current_u64();
    if rt == 0 { return MachineError::SpawnFailed; }

    let id = __mc_machine_runtime_spawn_u64(rt, mailbox_cap);
    if id == 0 { return MachineError::SpawnFailed; }

    if __mc_machine_runtime_bind_descriptor_u64(rt, id, desc_id, initial_tag) == 0 {
        return MachineError::BindFailed;
    }

    let state = __ts_ctor_Ping(arg);
    let boxed = heap_box(state);

    if __mc_machine_runtime_set_state_u64(rt, id, boxed.ptr_word) == 0 {
        return MachineError::BindFailed;
    }

    if __mc_machine_runtime_start_u64(rt, id) == 0 {
        return MachineError::StartFailed;
    }

    return __mc_machine_handle_Ping { _id: id };
}
```

### Step 5: Send/Request/Reply — Emitting Effects

Handler code that sends messages gets lowered to emit ABI shims. The compiler
determines the `event_kind` at compile time from the payload type.

**Send:**
```
// send(target, MyEvent { x: 42 })  lowers to:
__mc_machine_emit_send(target_id, 3, payload_ptr, layout_id);
```

**Request:**
```
// request:my_site(target, MyRequest { data: 1 })  lowers to:
__mc_machine_emit_request(target_id, 5, payload_ptr, layout_id, 0xA3B2...);
//                                                                ^^^^^^^^
//                                    request_site_key = FNV-1a hash of "my_site"
```

**Reply:**
```
// cap.reply(MyResponse { result: 42 })  lowers to:
__mc_machine_emit_reply(cap_id, 7, payload_ptr, layout_id);
```

### Step 6: The `@machines` Entrypoint

The `@machines` attribute wraps the user's `main()` with runtime lifecycle. See
`machine-surface-design.md` for the source-level view.

---

## Runtime Metrics

The runtime tracks cleanup counters for observability/tests:
- Pending lifecycle counts:
  `__mc_machine_runtime_pending_created_count(...)`,
  `__mc_machine_runtime_pending_cleanup_count(...)`
- Stop cleanup count:
  `__mc_machine_runtime_stopped_cleanup_count(...)`

## File Layout

```
runtime/machine/
├── runtime.h       <- public types + function declarations
├── internal.h      <- shared helpers across .c files
├── runtime.c       <- core: slots, mailbox, ready queue, dispatch loop,
│                      preflight/commit, public API
├── bridge.c        <- u64 wrappers, managed runtime, bootstrap
├── descriptor.c    <- binary descriptor parser, thunk registry
├── descriptor.h    <- descriptor/thunk internal types
├── pending.c       <- pending reply-cap table CRUD + timeouts
├── pending.h       <- pending table internal API
├── emit.c          <- staging context + ABI shims
└── emit.h          <- staging types
```
