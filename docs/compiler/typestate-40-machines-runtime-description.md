# Machine Runtime Architecture

The machine runtime is a **message-passing scheduler** for state machines. Think of it
like a tiny actor system embedded in compiled code.

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

### 1. Machines = Slots in a Table

Each machine is a numbered slot (1-based IDs). A slot holds:

- **Lifecycle state** -- `CREATED -> RUNNING -> FAULTED/STOPPED`
- **State word** -- an opaque value owned by the compiler-generated code
- **Mailbox** -- a bounded ring buffer of envelopes
- **Dispatch callback** -- the function to call when processing a message

When a machine enters `STOPPED`, runtime performs eager per-machine cleanup:
- pending correlations for that requester are reclaimed,
- machine-owned subscriptions are removed,
- mailbox envelopes are drained and mailbox storage is released,
- dispatch/descriptor/state pointers are cleared.

Machine IDs remain stable and reserved; stale handles continue to observe
`NotRunning` instead of becoming `Unknown`.

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

### 2. Envelopes = Messages Between Machines

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

### 3. Ready Queue = "Who Has Work?"

A ring buffer of machine IDs that have pending mail. Only `RUNNING` machines with
non-empty mailboxes get queued. Each machine appears **at most once** (guarded by
`in_ready_queue` flag).

```
Ready Queue:  head              tail
               ↓                 ↓
             [ 1 | 3 | 5 | _ | _ ]
               └──── 3 items ────┘
```

### 4. Dispatch = One Message At A Time

The core loop (`dispatch_one_txn`) does exactly **one** step:

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

## The Transaction Model

This is the key insight. Every dispatch is **transactional** -- all-or-nothing.

A callback produces a `txn` struct with staged effects:

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

If preflight passes -> **commit all at once**. If anything fails -> **fault**, nothing
changes.

```
Callback returns OK ──→ Preflight ──→ ✓ Commit all effects
                                  └──→ ✗ Fault (rollback)

Callback returns FAULT ──→ Apply fault policy, discard effects
Callback returns STOP  ──→ Mark STOPPED, discard effects, eager cleanup
```

## Request/Reply Correlation

Machines can do **request/reply**, not just fire-and-forget sends:

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

The **pending table** tracks inflight requests. Each entry maps a `pending_id` back to the
requester. When a reply arrives, the runtime routes it back and consumes the capability.

## Descriptor-Driven Dispatch

Instead of manually binding callbacks, the compiler generates a **descriptor blob** -- a
binary table that maps `(state_tag, event_kind)` -> `thunk_id`:

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

When an envelope arrives, the runtime looks up the row, resolves the thunk, and calls it.
The thunk can also carry a `next_state_tag` so the state machine transitions
automatically.

## The Emit ABI (Staging Shims)

Compiler-generated handler code doesn't get a pointer to the runtime. Instead, it calls
global ABI shims:

```c
// Inside a handler -- no runtime pointer needed:
__mc_machine_emit_send(dst=3, kind=42, payload0=..., payload1=...);
__mc_machine_emit_request(dst=2, kind=10, ...);
__mc_machine_emit_reply(cap=7, kind=20, ...);
```

These work because the runtime sets up a **staging context** before calling the handler:

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

The public API uses typed C structs (`mc_machine_runtime_t *rt`), but Machina's compiler
emits code that passes everything as `u64` integers. The **bridge** wraps every API:

```
// Typed API (used by C tests):
__mc_machine_runtime_spawn(rt, mailbox_cap, &out_id);

// Bridge API (used by compiled Machina code):
__mc_machine_runtime_spawn_u64(runtime_handle, mailbox_cap);
//                              ^^^^^^^^^^^^^^
//                              rt pointer cast to u64
```

There's also a **managed runtime** -- a process-global singleton so `@machines`
entrypoints don't need to thread a runtime handle manually:

```
__mc_machine_runtime_managed_bootstrap_u64()  -> creates or returns global
__mc_machine_runtime_managed_current_u64()    -> returns global (or 0)
__mc_machine_runtime_managed_shutdown_u64()   -> drops global
```

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

## Lifecycle Summary

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

## Runtime Metrics

The runtime tracks cleanup counters for observability/tests:
- pending lifecycle counts:
  `__mc_machine_runtime_pending_created_count(...)`,
  `__mc_machine_runtime_pending_cleanup_count(...)`
- stop cleanup count:
  `__mc_machine_runtime_stopped_cleanup_count(...)`

---

## Compiler Integration

Everything above describes the runtime in isolation. This section explains how the
**compiler turns Machina source code into executable code** that calls the runtime.

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

Source-level typestate declarations are **syntactic sugar**. The compiler desugars them
into hidden types and methods before anything else sees them.

Given this source:

```
typestate Ping {
    fn new() -> Ready {
        Ready { count: 0 }
    }

    state Ready {
        fields {
            count: u64,
        }

        on Msg(n: u64) -> Ready {
            Ready { count: self.count + n }
        }
    }
}
```

The desugarer generates:

```
// Hidden state layout type (one per state)
struct __ts_Ping_Ready {
    count: u64,
}

// Hidden constructor
fn __ts_ctor_Ping() -> __ts_Ping_Ready {
    __ts_Ping_Ready { count: 0 }
}

// Hidden handler method
fn __ts_on_1(self: __ts_Ping_Ready, msg: Msg) -> __ts_Ping_Ready {
    __ts_Ping_Ready { count: self.count + msg.n }
}
```

The key insight: **the user writes a typestate declaration, but the rest of the compiler
sees ordinary structs and functions.** The typestate semantics are enforced by the plans
built in the next step.

### Step 2: Elaboration -- Building Machine Plans

The elaboration pass (`machine_plan.rs`) inspects the desugared types and handlers to
build a **machine plan** -- a complete description of the typestate's runtime shape.

For each typestate it produces:

**State tags** -- deterministic 1-based indices:

```
State          Tag
─────          ───
Ready           1
Waiting         2
Done            3
```

**Event kinds** -- deterministic 1-based indices for each distinct payload type:

```
Payload Type   Kind
────────────   ────
Msg             1
Ping            2
Pong            3
```

**Dispatch table** -- maps `(state_tag, event_kind)` to handler thunks:

```
┌───────────┬────────────┬──────────────────┬────────────────────┐
│ State Tag │ Event Kind │ State-Local Thunk│ Fallback Thunk     │
├───────────┼────────────┼──────────────────┼────────────────────┤
│ 1 (Ready) │ 1 (Msg)    │ __ts_on_1        │ (none)             │
│ 1 (Ready) │ 2 (Ping)   │ (none)           │ __ts_on_2          │
│ 2 (Wait)  │ 2 (Ping)   │ (none)           │ __ts_on_2          │
└───────────┴────────────┴──────────────────┴────────────────────┘
```

Handlers that are identical across all states become **fallback thunks**. State-specific
handlers take priority over fallbacks at dispatch time.

**Thunk plans** -- each handler gets a thunk record:

```
Thunk                          Handler         Next State Tag
─────                          ───────         ──────────────
__mc_machine_dispatch_thunk_42  __ts_on_1       1 (Ready)
__mc_machine_dispatch_thunk_43  __ts_on_2       2 (Wait)
```

The `next_state_tag` lets the runtime update the machine's state tag automatically after a
successful transition.

### Step 3: Backend Lowering -- Generating Artifacts

The backend takes the machine plans and generates three kinds of artifacts.

#### 3a. Descriptor Blob

The dispatch table is serialized into a **binary blob** -- the same format parsed by
`__mc_machine_runtime_register_descriptor` in `descriptor.c`.

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

This blob is embedded as a **global byte array** in the generated code.

#### 3b. Dispatch Thunks

Each handler needs an ABI adapter -- the runtime calls thunks with a fixed C signature,
but handlers have typed parameters. The thunk bridges the gap:

```
Runtime calls:
  thunk(ctx, machine_id, current_state, env_ptr, txn_ptr, fault_code_ptr) -> u8
         ^^^                  ^^^          ^^^
       opaque            state word     envelope

Thunk unpacks envelope and calls the actual handler:

  ┌─ __mc_machine_dispatch_thunk_42 ─────────────────────────────┐
  │                                                              │
  │  1. Cast current_state (u64) -> ptr to __ts_Ping_Ready       │
  │  2. Load payload from env.payload0 -> ptr to Msg             │
  │  3. Call __ts_on_1(state_ptr, payload_ptr)                   │
  │     -> returns new __ts_Ping_Ready value                     │
  │  4. Heap-box the returned state (min 1 byte for runtime)     │
  │  5. Write txn: has_next_state=1, next_state=boxed_ptr        │
  │  6. Return MC_DISPATCH_OK (0)                                │
  │                                                              │
  └──────────────────────────────────────────────────────────────┘
```

For response handlers (handling replies to requests), the thunk also extracts
`env.origin_payload0` and `env.origin_request_site_key` to pass provenance information to
the handler.

#### 3c. Bootstrap Function

The compiler generates a function called `__mc_machine_bootstrap` (a weak symbol the
runtime calls once on first `__mc_machine_runtime_new`):

```
fn __mc_machine_bootstrap():
    // Register every thunk with its metadata:
    __mc_machine_runtime_register_thunk_meta_u64(42, &thunk_42, next_tag=1)
    __mc_machine_runtime_register_thunk_meta_u64(43, &thunk_43, next_tag=2)

    // Register every descriptor blob:
    __mc_machine_runtime_register_descriptor_u64(&ping_desc_bytes, len)
```

This runs **once** before any machines are spawned. It populates the process-global thunk
registry and descriptor registry that the runtime uses during dispatch.

### Step 4: Spawn -- Creating a Machine

When user code calls `Typestate::spawn(...)`, the compiler lowers it to a
generated **spawn wrapper**:

```
// User writes:
let ping = Ping::spawn(some_arg)?;

// Compiler lowers to:
//   __ts_spawn_Ping(some_arg)

fn __ts_spawn_Ping(arg: u64) -> __mc_machine_handle_Ping | MachineError {
    // 0. Use managed runtime in @machines entrypoints
    let rt = __mc_machine_runtime_managed_current_u64();
    // Current lowering treats missing managed runtime as spawn failure.
    // (A follow-up can map this to RuntimeUnavailable directly.)
    if rt == 0 { return MachineError::SpawnFailed; }

    // 1. Allocate machine slot
    let id = __mc_machine_runtime_spawn_u64(rt, mailbox_cap);
    if id == 0 { return MachineError::SpawnFailed; }

    // 2. Bind descriptor (connects dispatch table)
    if __mc_machine_runtime_bind_descriptor_u64(rt, id, desc_id, initial_tag) == 0 {
        return MachineError::BindFailed;
    }

    // 3. Run constructor to get initial state
    let state = __ts_ctor_Ping(arg);
    let boxed = heap_box(state);

    // 4. Seed state word
    // (state word = pointer to heap-allocated state struct)
    if __mc_machine_runtime_set_state_u64(rt, id, boxed.ptr_word) == 0 {
        return MachineError::BindFailed;
    }

    // 5. Start machine (CREATED -> RUNNING)
    if __mc_machine_runtime_start_u64(rt, id) == 0 {
        return MachineError::StartFailed;
    }

    return __mc_machine_handle_Ping { _id: id };
}
```

The `desc_id` is the descriptor identifier returned by `register_descriptor` during
bootstrap. The compiler generates a helper
(`__mc_machine_descriptor_id_<Typestate>`) that returns the right ID for each typestate.

### Step 5: Send / Request / Reply -- Emitting Effects

Handler code that sends messages gets lowered to the emit ABI shims. The compiler
determines the `event_kind` at compile time from the payload type.

**Send:**

```
// User writes (inside a handler):
send(target, MyEvent { x: 42 });

// Compiler lowers to:
__mc_machine_emit_send(
    target_id,    // destination machine
    3,            // event_kind for MyEvent (known at compile time)
    payload_ptr,  // heap pointer to boxed MyEvent
    layout_id     // payload layout identifier
);
```

**Request:**

```
// User writes:
let pending = request:my_site(target, MyRequest { data: 1 });

// Compiler lowers to:
let pending = __mc_machine_emit_request(
    target_id,
    5,              // event_kind for MyRequest
    payload_ptr,
    layout_id,
    0xA3B2...       // request_site_key = FNV-1a hash of "my_site"
);
```

The `request_site_key` is a deterministic 64-bit hash (FNV-1a with high bit set) of the
label string. This lets the runtime correlate responses back to specific request sites,
enabling **labeled provenance** -- the response handler knows which `request` call it's
answering.

**Reply:**

```
// User writes (inside a handler that received a request):
cap.reply(MyResponse { result: 42 });
// (explicit form is also valid: reply(cap, MyResponse { ... }))

// Compiler lowers to:
__mc_machine_emit_reply(
    cap_id,     // reply capability from the inbound envelope
    7,          // event_kind for MyResponse
    payload_ptr,
    layout_id
);
```

### Step 6: The `@machines` Entrypoint

The `@machines` attribute on `main()` wraps the user's code with runtime lifecycle:

```
// User writes:
@machines
fn main() -> () | MachineError {
    let p = Ping::spawn()?;
    p.send(Start {})?;
}

// Compiler rewrites to:
fn main() -> () | MachineError {
    // 1. Bootstrap runtime (triggers __mc_machine_bootstrap)
    let rt = __mc_machine_runtime_managed_bootstrap_u64();

    // 2. Run user code (spawns machines, sends initial messages)
    let result = {
        let p = __ts_spawn_Ping()?;  // typed machine handle
        p.send(Start {})?;            // typed send wrapper
    };

    // 3. Auto-drive dispatch loop (only when runtime is available)
    if rt != 0 {
        loop {
            let status = __mc_machine_runtime_step_u64(rt);
            if status != MC_STEP_DID_WORK { break; }
            //  MC_STEP_IDLE     = 0  (no runnable machines)
            //  MC_STEP_DID_WORK = 1  (dispatched one envelope)
            //  MC_STEP_FAULTED  = 2  (dispatched + faulted)
        }
    }

    // 4. Tear down
    __mc_machine_runtime_managed_shutdown_u64();

    result
}
```

The loop runs until there is no more runnable work (`IDLE`) or a faulted step is observed
(`FAULTED`). User-visible failures still flow through typed wrappers (`spawn/send/request`)
as `MachineError` variants.

### Putting It All Together

Here's the full sequence for a simple program:

```
                 Compile Time                           Run Time
                 ────────────                           ────────

  typestate Ping {           ┌─────────────────────┐
    state Ready { }          │ Desugar + elaborate │
    on Msg -> Ready { ... }  │ -> state tags       │
  }                          │ -> event kinds      │
                             │ -> dispatch rows    │
  @machines                  │ -> thunks           │
  fn main() {                └──────────┬──────────┘
    let p = Ping::spawn()?;             │
    p.send(Msg {})?;                    │
  }                                     ▼

              Generates:                Executes:
              ──────────                ─────────

  desc blob (MCHD...)                  1. main() calls bootstrap
  thunk_42 (adapter fn)                   -> __mc_machine_bootstrap()
  __mc_machine_bootstrap()                -> registers thunk_42
  __ts_spawn_Ping()                       -> registers descriptor blob
  __ts_ctor_Ping()
  wrapped main()                        2. main() body runs
                                           -> __ts_spawn_Ping()
                                              -> spawn slot, bind desc
                                              -> run constructor
                                              -> set state word
                                              -> start machine
                                           -> p.send(Msg {})
                                              -> typed wrapper packs payload
                                              -> __mc_machine_runtime_send_u64

                                        3. dispatch loop
                                           -> step_u64(rt)
                                           -> pop machine 1 from ready queue
                                           -> pop Msg envelope from mailbox
                                           -> mc_descriptor_dispatch:
                                              state_tag=1, event_kind=1
                                              -> find row -> thunk_42
                                              -> thunk_42 calls __ts_on_1
                                              -> handler returns new state
                                              -> txn: has_next_state=1
                                           -> preflight + commit
                                           -> step_u64 returns DID_WORK

                                        4. step_u64(rt) -> IDLE (no more work)

                                        5. shutdown
