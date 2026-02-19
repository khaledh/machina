# Machine Surface API and Developer Experience

## Overview

This document defines the user-facing API for managed machines — what using
machines should feel like in day-to-day Machina code. The goal is zero-ceremony
defaults where typical programs contain no explicit runtime setup code, while
preserving strict compile-time typestate/protocol safety.

For the core typestate language model, see `typestate-design.md`. For runtime
internals, see `machine-runtime-design.md`. For protocol contracts, see
`protocol-design.md`.

## Implementation Status

- `@machines` managed entrypoint opt-in: implemented.
- `Typestate::spawn(...) -> Machine<T> | ...`: implemented.
- Typed handle methods: `send(payload)` and `request(dst, payload)`: implemented.
- Typed request destinations: `u64` or `Machine<...>` handles: implemented.
- Handler sugar:
  - `send(dst, payload)` / `request(dst, payload)` lowering: implemented.
  - `cap.reply(payload)` lowering: implemented.
  - `reply(payload)` implicit-cap form: not implemented yet.
- Same-state ergonomics (`-> stay`, fieldless shorthand): implemented.
- Provenance binding (`for RequestType(binding)` + labels): implemented.

## Design Principles

1. **Zero-ceremony defaults** — simple machine programs compile and run without
   runtime setup code.
2. **Typed API only at source level** — no `kind/payload0/payload1` in user
   code.
3. **Direct + managed coexistence** — direct transitions stay valid for local
   deterministic usage.
4. **Explicit escape hatch for advanced control** — low-level runtime APIs for
   tests/embedding/perf control.
5. **No function coloring** — handlers stay synchronous run-to-completion.

## Target User Experience

```mc
requires {
    std::io::println
}

type Connect = { token: string }
type Ping = { id: u64 }

typestate Client {
    fn new() -> Disconnected {
        Disconnected {}
    }

    state Disconnected {
        on Connect(c: Connect) -> Connected {
            c;
            Connected {}
        }
    }

    state Connected {
        on Ping(p) -> stay {
            println(f"ping {p.id}");
        }
    }
}

@machines
fn main() -> () | MachineError {
    let client = Client::spawn()?;
    client.send(Connect { token: "secret" })?;
    client.send(Ping { id: 1 })?;
}
```

Key property: app code does not create or pass a runtime object.

---

## Surface API

### 1) `@machines` Entrypoint

Managed execution is visible at the entrypoint:

```mc
@machines
fn main() -> () | MachineError {
    let client = Client::spawn()?;
    client.send(Ping { id: 1 })?;
}
```

The `@machines` attribute wraps the user's `main()` with runtime lifecycle:

1. Bootstrap runtime (triggers `__mc_machine_bootstrap`).
2. Run user code (spawns machines, sends initial messages).
3. Auto-drive dispatch loop until idle or faulted.
4. Shutdown runtime.

Design intent:
- No explicit runtime object in app code.
- No callback-wrapper ceremony.
- Runtime model is still visible (not hidden compiler magic).

Runtime ownership rules:
- Binaries own runtime bootstrap/shutdown when opted in.
- Libraries never auto-bootstrap runtime.

Execution policy (V1):
- Single-thread cooperative dispatch by default.
- Deterministic drain policy on shutdown.

### 2) Machine Handles

`Typestate::spawn(...)` returns a typed machine handle:

```mc
let c: Machine<Client> = Client::spawn()?;
```

Handle capabilities in V1:
- `send(payload)`
- `request(dst, payload)` where `dst` may be `u64` or typed machine handle
- Optional lifecycle queries (future)

No descriptor ids, state tags, or runtime pointers are exposed.

### `new(...)` vs `spawn(...)` Contract

- `new(...) -> InitialState`: typestate-level initializer logic.
- `spawn(...) -> Machine<Typestate> | MachineError`: managed runtime
  constructor.

Required contract:
1. `spawn` mirrors `new` parameter list exactly.
2. `spawn(args...)` forwards arguments to `new(args...)`.
3. Runtime registration occurs using the initialized state from `new`.

Conceptual expansion:

```mc
let m = Connection::spawn(auth, "api");
// desugars to:
let s0 = Connection::new(auth, "api");
let m = __machine_spawn(Connection, s0);
```

Direct mode remains unchanged and uses `new` without runtime registration.

### 3) Typed Messaging

Handlers and callers use typed payloads only:
- `send(Msg { ... })` or `handle.send(Msg { ... })`
- `request(dst, Req { ... })` or `handle.request(Req { ... })`
- `cap.reply(Resp { ... })`
- `reply(Resp { ... })` (implicit cap — planned)

These desugar to runtime ABI calls through compiler-generated metadata.

### 4) Fallible Managed APIs

Managed spawn/send/request APIs are fallible:
- `Typestate::spawn(...) -> Machine<T> | MachineError`
- `send(...) -> () | MachineError`
- `request(...) -> u64 | MachineError` in default implicit-correlation form

No silent drop at source API boundary.

### 5) Step Status (Advanced Path)

`step()` returns a status enum:
- `Idle` — no runnable machines.
- `DidWork` — dispatched one envelope.
- `Faulted` — dispatched + faulted.

Explicit runtime API for tests/embedding:

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

---

## Handler Ergonomics

### 1) Same-State Transition (`stay`)

Explicit same-state marker:

```mc
state Connected {
    on Ping(p) -> stay {
        println(f"ping {p.id}");
    }
}
```

`stay` semantics:
- Preserves current state's fields after handler execution.
- Observes in-handler mutations to `self`.
- No manual reconstruction boilerplate.

Fieldless shorthand — omitting `-> ...` means implicit same-state:

```mc
state Connected {
    on Ping(p) {
        println(f"ping {p.id}");
    }
}
```

For states with fields, prefer/require `-> stay` for clarity. When handler
paths use error unions, require explicit return state form.

### 2) Payload Shorthand

For handlers where event selector and payload type are the same:
- `on Ping(p: Ping) { ... }` can be written as `on Ping(p) { ... }`.
- `on Ping { ... }` is allowed when payload is unused.

### 3) Correlation and Capability Model

Correlation and reply capabilities are handler context, not primary payload
parameters.

Default ergonomic model:
- Payload pattern first (`on AuthApproved(ok) { ... }`).
- Correlation implicit.
- Optional explicit provenance binding only when needed.

### 4) Request Provenance via `for`

When response handlers need the originating request payload:

```mc
on AuthApproved(ok) for AuthCheck(req) {
    println(f"approved conn={req.conn_id} user={ok.user_id}");
}
```

`for` binds the correlated originating request payload. Optional in the common
case where only response payload is needed.

Canonical meaning (conceptual desugar):

```mc
on Response(resp: AuthApproved | AuthDenied, origin: AuthorizeReq) -> ... {
    match (resp, origin) {
        (AuthApproved(ok), AuthorizeReq(req)) => ...,
        (AuthDenied(err), AuthorizeReq(req)) => ...,
    }
}
```

### 5) Implicit Correlation

`request(...)` creates an internal inflight correlation entry managed by
compiler/runtime metadata. Response routing is deterministic without
user-managed pending lists.

`Pending<T>` remains available as an advanced explicit control tool.

Ambiguity policy: if a response handler has multiple possible originating
request sites of the same request type, require explicit disambiguation via
request-site labels (e.g. `request:auth1(...)` and `for AuthCheck:auth1(req)`).

### 6) Inflight Correlation Cleanup

V1 baseline:
- Remove entry on successful response commit.
- Remove entry on requester machine fault/stop.
- Remove entry on timeout with dead-letter/fault telemetry.
- Expose diagnostics/hooks for timed-out/canceled inflight entries.

---

## Request/Reply Example (Full)

```mc
type Open = { conn_id: u64, token: string }
type AuthCheck = { conn_id: u64, token: string }
type AuthApproved = { user_id: u64 }
type AuthDenied = { reason: string }

typestate Connection {
    fields = { auth: Machine<AuthService> }

    fn new(auth: Machine<AuthService>) -> Running {
        Running {}
    }

    state Running {
        on Open(open) {
            request(self.auth, AuthCheck {
                conn_id: open.conn_id,
                token: open.token,
            });
        }

        on AuthApproved(ok) for AuthCheck(req) {
            println(f"conn {req.conn_id} approved user={ok.user_id}");
        }

        on AuthDenied(err) for AuthCheck(req) {
            println(f"conn {req.conn_id} denied: {err.reason}");
        }
    }
}

typestate AuthService {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthCheck(req, cap: ReplyCap<AuthApproved | AuthDenied>) {
            if req.token == "secret" {
                cap.reply(AuthApproved { user_id: req.conn_id + 1000 });
            } else {
                cap.reply(AuthDenied { reason: "invalid token" });
            }
        }
    }
}
```

---

## Diagnostics and Tooling

### Error Messages Use Source Terms

Diagnostics/hover/signature help use:
- `Connection::Disconnected`
- `Machine<Connection>`
- `Pending<AuthApproved | AuthDenied>`

Never leak synthetic/internal symbol names.

### Managed API Diagnostics

- Sending payload not accepted by target machine/protocol role.
- Replying with type outside allowed response set.
- Using a consumed or mismatched `Pending`/`ReplyCap` token.
- Ambiguous response handlers where `for` provenance is required.

### IDE Quick Fixes

Candidate quick fixes:
- Add missing `on` handler stub.
- Add missing response pattern arm.
- Convert raw runtime API usage to typed `spawn/send/request` wrappers.

---

## Shipping Strategy (Phased)

### Tier 1 (Ship First)

- `Machine<T>` typed handles and `Typestate::spawn(...)`.
- Handler payload shorthand (`on Ping(p)`, `on Ping`).
- Command sugar (`send/request/reply` in handlers).
- Explicit same-state marker (`-> stay`) with clear semantics.
- Explicit runtime opt-in entrypoint (`@machines`).

### Tier 2 (After Runtime/Metadata Hardening)

- Implicit correlation as default for request/reply.
- `for RequestType(binding)` provenance binding.
- Optional request-site labels for ambiguous concurrent same-type requests.
- Advanced diagnostics/quick-fixes around provenance and inflight tracking.

---

## Semantics Carried Forward

These invariants are unchanged by surface ergonomics:

1. Managed handlers remain transactional.
2. Reply capabilities remain linear.
3. Default handler precedence remains state-local first, typestate-level
   fallback.
4. Shape conformance remains minimum protocol checking level for V1.
