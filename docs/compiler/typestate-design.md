# Typestate Language Design

## Overview

Typestates are a source-level feature that encodes object lifecycle as a sequence
of named states with typed transitions. Each state carries its own fields, and
methods on a state consume the current state and produce the next one. This
prevents illegal state transitions at compile time using existing ownership
semantics (`sink`).

## Terminology

- `typestate`: source-level definition of states and transitions.
- `machine`: runtime instance of a typestate (see `machine-runtime-design.md`).
- `direct mode`: existing value-level transitions (`c.connect()`).
- `managed mode`: runtime-owned machine lifecycle + mailbox dispatch.

## Surface Syntax

```machina
typestate Connection {
    fields {
        addr: string,
        retries: u64,
    }

    fn new(addr: string) -> Disconnected {
        Disconnected { addr, retries: 0 }
    }

    state Disconnected {
        fn connect(fd: u64) -> Connected | ConnectError {
            if fd == 0 {
                ConnectError {}
            } else {
                Connected { fd }
            }
        }
    }

    state Connected {
        fields {
            fd: u64,
        }

        fn close() -> Disconnected {
            Disconnected
        }
    }
}
```

Usage (direct mode):

```machina
fn main() {
    let c0 = Connection::new("localhost");
    let c1 = c0.connect(42);
    match c1 {
        connected: Connected => {
            let _c2 = connected.close();
        }
        ConnectError => {}
    }
}
```

### Syntax Rules

- `typestate.fields { ... }` declares carried fields available to all states.
- `fn new(...) -> State` is required at typestate scope and defines the lifecycle
  entry state.
- `state.fields { ... }` declares state-local fields (optional).
- State methods are declared directly inside `state { ... }`.
- State methods have implicit `sink self: <CurrentState>`.
- State names are scoped to the `typestate` block in source; desugaring rewrites
  to generated nominal types.
- Typestate-level fields are available on every generated state type.
- State literal construction is restricted to typestate methods; external
  construction must go through `Typestate::new(...)`.

### Event Handlers (Managed Mode)

When a typestate is used in managed mode, states declare event handlers instead
of (or in addition to) direct transition methods:

```machina
state Connected {
    on IncomingRequest(req) {
        ...
    }

    on Ping(p) -> stay {
        println(f"ping {p.id}");
    }
}
```

Handler syntax:
- `on Payload(binding)` — same-type shorthand (selector = payload type).
- `on Payload { ... }` — payload unused.
- Omitting `-> ...` means implicit same-state transition.
- `-> stay` is the explicit same-state marker (preferred for states with fields).
- `for RequestType(binding)` — response provenance binding (see
  `machine-surface-design.md`).

Typestate-level default handlers provide cross-cutting fallbacks:

```machina
typestate Connection {
    on WriteFailed(err: WriteFailed) -> Closing {
        Closing
    }
}
```

Precedence: state-local handler first, typestate-level default only as fallback.

## Lowering Strategy (Desugar)

Typestates lower into existing top-level constructs before resolve/typecheck.
This keeps typestate-specific complexity out of resolver/typechecker internals.

### Pipeline Placement

```
Parse -> TypestateDesugar -> Resolve -> Typecheck -> Semck -> Elaborate -> Backend
```

### Lowering Steps

1. **Generate one struct type per state:**
   - `Connection$Disconnected`
   - `Connection$Connected`

2. **Flatten typestate-level fields into every generated state struct:**
   - `Connection$Disconnected { addr: string, retries: u64 }`
   - `Connection$Connected { addr: string, retries: u64, fd: u64 }`

3. **Rewrite state body methods into inherent methods on the source state type:**
   - `Connection$Disconnected :: { fn connect(sink self, fd: u64) -> ... }`
   - `Connection$Connected :: { fn close(sink self) -> ... }`

4. **Rewrite typestate-scope constructor:**
   - `fn new(...) -> State` rewrites to
     `Connection :: { fn new(...) -> Connection$State { ... } }`

5. **Rewrite state references in signatures/bodies:**
   - `Disconnected` → `Connection$Disconnected`
   - `Connected` → `Connection$Connected`

6. **Apply implicit carried-field move on state literals:**
   - In state methods, `TargetState { ... }` auto-fills missing typestate-level
     fields from `self.<field>`.
   - Carry is move/ownership transfer (not implicit copy).

7. **Generate event handler thunks (managed mode):**
   - Each `on ...` handler lowers to a runtime-callable thunk with the dispatch
     ABI (see `machine-runtime-design.md`).

### Why Desugar-First

- Minimal compiler risk: parser + desugar work, little change in
  resolver/typechecker core.
- Reuses existing method calls, nominal types, and ownership semantics (`sink`).
- Keeps typestate mostly a source-level UX feature.
- Keeps current backend/runtime unchanged via desugaring.

Given this source:

```machina
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

The user writes a typestate declaration, but the rest of the compiler sees
ordinary structs and functions.

## Validation Rules

Enforced in desugar + early semantic checks:

1. `typestate` must declare at least one `state`.
2. State names must be unique within the block.
3. `typestate.fields { ... }` may appear at most once.
4. In each `state`, `fields { ... }` may appear at most once.
5. State-local fields must not redeclare typestate-level field names.
6. Exactly one typestate-scope `new` method is required.
7. `new` return type must be a declared state, or `State | Error...` where the
   first type is a declared state.
8. Transition methods must not declare explicit `self`; it is implicit.
9. Transition return type must be a declared state, or `State | Error...` where
   the first type is a declared state.
10. Transition name collisions on same source state are disallowed.
11. External state literal construction is disallowed (must use `Typestate::new`
    for entry and transitions for evolution).
12. Implicit carried-field move is guaranteed when return is a direct state
    literal (`State { ... }`) or direct state shorthand (`State`).

## Semantic Invariants (Normative)

These invariants are the normative contract for all implementation work. If other
documents conflict, this section wins.

1. **Managed-mode exclusivity**
   - Registering a machine in managed mode consumes direct ownership.
   - Direct typestate transition calls on a managed instance are illegal.

2. **Dispatch model**
   - Handlers are synchronous, run-to-completion, one-at-a-time per machine.
   - Runtime dispatch is mailbox-driven only in managed mode.

3. **Transactional semantics**
   - Commit unit is `(next state, outbox effects, subscription updates)`.
   - If handler fails before successful completion, commit does not occur.

4. **Default handler precedence**
   - State-local handler first.
   - Typestate-level handler only as fallback when state-local is absent.

5. **Handle/capability surface**
   - Source-level addressing uses typed machine handles (`Machine<T>`).
   - Correlation is implicit by default; no raw correlation ids in source.
   - `Pending<RespSet>`/`ReplyCap<RespSet>` remain available as explicit
     advanced control forms.

6. **Reply linearity**
   - Request handlers expose an implicit reply capability by default.
   - On successful paths, exactly one reply must be produced.
   - Explicit `ReplyCap<RespSet>` binding (when used) remains linear and
     exactly-once on successful paths.

7. **Protocol conformance scope (V1)**
   - V1 enforces shape only.
   - Stronger protocol refinement/projection checks are explicitly deferred.

8. **Pattern handler determinism**
   - Multiple handlers for the same event selector are allowed only when their
     response patterns are non-overlapping.
   - Pattern-form handlers are syntax sugar over one canonical handler body.
   - When response provenance is ambiguous, `for RequestType(binding)` is
     required for deterministic dispatch.

## Diagnostics

Dedicated diagnostic codes:

- `MC-TYPESTATE-UNKNOWN-STATE`
- `MC-TYPESTATE-INVALID-SELF-PARAM`
- `MC-TYPESTATE-TRANSITION-RETURN`
- `MC-TYPESTATE-EXPLICIT-SELF-NOT-ALLOWED`
- `MC-TYPESTATE-PROTOTYPE-DIRECT-LITERAL-REQUIRED`
- `MC-TYPESTATE-DUPLICATE-STATE`
- `MC-TYPESTATE-DUPLICATE-TRANSITION`
- `MC-TYPESTATE-DUPLICATE-TYPESTATE-FIELDS-BLOCK`
- `MC-TYPESTATE-DUPLICATE-STATE-FIELDS-BLOCK`
- `MC-TYPESTATE-STATE-FIELD-SHADOWS-CARRIED-FIELD`
- `MC-TYPESTATE-MISSING-NEW`
- `MC-TYPESTATE-DUPLICATE-NEW`
- `MC-TYPESTATE-INVALID-NEW-RETURN`
- `MC-TYPESTATE-STATE-LITERAL-OUTSIDE-TYPESTATE`

All diagnostics use spans on source `typestate` declarations, not generated
symbol names.

## Feature Flag

Compiler flag: `--experimental typestate`

Behavior:
- Default mode: `typestate` syntax rejected with a clear "feature disabled"
  diagnostic.
- Experimental mode: parse + desugar enabled.
- LSP honors project/compiler option so diagnostics match batch mode.

## Direct vs Managed Mode

Two modes coexist for the same typestate definition:

1. **Direct mode**: value-level transitions (`let c1 = c0.connect(42)`).
   - No runtime scheduling required.
   - Standard ownership/move semantics apply.

2. **Managed mode**: runtime-owned machine, mailbox/event driven.
   - Selected by `Typestate::spawn(...)` and handle usage.
   - Runtime manages lifecycle, dispatch, and inter-machine communication.
   - See `machine-runtime-design.md` and `machine-surface-design.md`.
