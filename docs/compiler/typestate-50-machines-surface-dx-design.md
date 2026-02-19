# Typestate Machines: Surface Ergonomics and Developer Experience (Draft)

## Status

Draft proposal focused on source-level ergonomics for typestate machines.

Implementation snapshot (current):
- `@machines` managed entrypoint opt-in: implemented.
- `Typestate::spawn(...) -> Machine<T> | ...`: implemented.
- typed handle methods: `send(payload)` and `request(dst, payload)`: implemented.
- typed request destinations: `u64` or `Machine<...>` handles: implemented.
- handler sugar:
  - `send(dst, payload)` / `request(dst, payload)` lowering: implemented.
  - `cap.reply(payload)` lowering: implemented.
  - `reply(payload)` implicit-cap form: not implemented yet.
- same-state ergonomics (`-> stay`, fieldless shorthand): implemented.
- provenance binding (`for RequestType(binding)` + labels): implemented.

This document complements:
- `docs/compiler/typestate-machines-async-events-design.md` (semantics/protocol model), and
- `docs/compiler/typestate-machines-runtime-execution-design.md` (compiler/runtime bridge).

Those documents define correctness and execution. This one defines what using
machines should feel like in day-to-day Machina code.

## Problem

The current managed-machine foundation is workable but too low-level for app code:
- explicit runtime creation,
- explicit descriptor binding/bootstrap,
- explicit step loops,
- low-level send/request payload ABI surfaces.

This creates friction and hides the flagship language value behind runtime plumbing.

## Goal

Make machines feel native and lightweight:
- `spawn` machines directly from app code,
- send/request/reply with typed values,
- no runtime object plumbing in normal programs,
- no callback ceremony as the default execution model,
- preserve strict compile-time typestate/protocol safety.

Specific ergonomic targets for the end-state surface:
- handlers should read like function callbacks over event payloads,
- same-state handlers should stay concise without hidden state-loss behavior,
- request/reply correlation should be implicit by default,
- advanced correlation access should be explicit but non-intrusive.

## Non-Goals (V1 DX)

1. No distributed runtime model.
2. No promise/future-based async language model.
3. No weakening of typestate state-transition guarantees.
4. No implicit global singleton runtime shared across processes.

## Design Principles

1. **Zero-ceremony defaults**
   - simple machine programs should compile and run without runtime setup code.
2. **Typed API only at source level**
   - no `kind/payload0/payload1` in user code.
3. **Direct + managed coexistence**
   - direct transitions stay valid for local deterministic usage.
4. **Explicit escape hatch for advanced control**
   - keep low-level runtime APIs for tests/embedding/perf control.
5. **No function coloring**
   - handlers stay synchronous run-to-completion.

## Shipping Strategy (Phased)

The feature should ship in two tiers to stay within the language "magic budget".

### Tier 1 (ship first)

- `Machine<T>` typed handles and `Typestate::spawn(...)`.
- handler payload shorthand (`on Ping(p)`, `on Ping`).
- command sugar (`send/request/reply` in handlers).
- explicit same-state marker (`-> stay`) with clear semantics.
- explicit runtime opt-in entrypoint (one visible line/attribute), no hidden
  compiler takeover of `main`.

### Tier 2 (after runtime/metadata hardening)

- implicit correlation as default for request/reply.
- `for RequestType(binding)` provenance binding.
- optional request-site labels for ambiguous concurrent same-type requests.
- advanced diagnostics/quick-fixes around provenance and inflight tracking.

## Target User Experience

### Desired app code shape

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

## Surface API Proposal

### 1) Explicit runtime opt-in for binaries

Managed execution is visible at the entrypoint via `@machines`:

```mc
@machines
fn main() -> () | MachineError {
    let client = Client::spawn()?;
    client.send(Ping { id: 1 })?;
}
```

Design intent:
- no explicit runtime object in app code,
- no callback-wrapper ceremony,
- runtime model is still visible (not hidden compiler magic).

Runtime ownership rules:
- binaries own runtime bootstrap/shutdown when opted in,
- libraries never auto-bootstrap runtime.

### Execution policy (V1)

- single-thread cooperative dispatch by default,
- deterministic drain policy on shutdown,
- bounded drain timeout (configurable later).

### 2) First-class machine handle API

`Typestate::spawn(...)` returns a typed machine handle.

```mc
let c: Machine<Client> = Client::spawn()?;
```

Handle capabilities in V1:
- `send(payload)`
- `request(dst, payload)` where `dst` may be `u64` or typed machine handle
- optional lifecycle queries (future)

No descriptor ids, state tags, or runtime pointers are exposed.

### `new(...)` vs `spawn(...)` contract

`new(...)` and `spawn(...)` serve different layers:

- `new(...) -> InitialState`: typestate-level initializer logic.
- `spawn(...) -> Machine<Typestate>`: managed runtime constructor.

Required source contract:

1. `spawn` mirrors `new` parameter list exactly.
2. `spawn(args...)` forwards arguments to `new(args...)`.
3. runtime registration occurs using the initialized state from `new`.

Conceptual expansion:

```mc
let m = Connection::spawn(auth, "api");
```

desugars to:

```mc
let s0 = Connection::new(auth, "api");
let m = __machine_spawn(Connection, s0);
```

Direct mode remains unchanged and uses `new` without runtime registration:

```mc
let s = Connection::new(auth, "api");
```

If runtime allocation/registration is fallible, `spawn` should return a
fallible type. Current surface:

```mc
Typestate::spawn(...) -> Machine<Typestate> | MachineError
```

### 3) Typed messaging surface

Handlers and callers use typed payloads only:
- `send(Msg { ... })`
- `request(dst, Req { ... })`
- `cap.reply(Resp { ... })`

These desugar to existing runtime ABI calls through compiler-generated metadata.

In ergonomic mode:
- `cap.reply(Resp { ... })` is preferred in request handlers,
- explicit `Pending<T>` / `ReplyCap<T>` forms remain available as escape hatches.

### 4) Keep direct typestate mode unchanged

Direct transitions remain available:
- value-level typestate programming (local/stateful APIs),
- no runtime scheduling required.

Managed mode is selected by `Typestate::spawn(...)` and handle usage.

### 5) Advanced explicit-runtime path (kept, de-emphasized)

Retain low-level runtime APIs for:
- deterministic step-driven tests,
- embedding Machina runtime into hosts,
- diagnostic/perf instrumentation scenarios.

But this is not the default app path.

## Ergonomic Sugar

### 1) Method-style handle operations

Allow both:
- `send(handle, msg)` and
- `handle.send(msg)`.

Prefer method style in docs/examples.

### 2) Handler-side command sugar

Inside `on` handlers, support direct forms:
- `send(to, Msg {})`
- `request(to, Req {})`
- `cap.reply(Resp {})`

No `emit ...` ceremony required in the common case.

`emit` may stay as canonical lower-level form for advanced scenarios.

## Handler Surface Simplifications

### 1) Same-state transition (`stay`)

Canonical same-state form should be explicit:

```mc
state Connected {
    on Ping(p) -> stay {
        println(f"ping {p.id}");
    }
}
```

`stay` semantics:
- preserves current state's fields after handler execution,
- observes in-handler mutations to `self`,
- does not require manual reconstruction boilerplate.

Optional shorthand for fieldless states:

```mc
state Connected {
    on Ping(p) {
        println(f"ping {p.id}");
    }
}
```

For states with fields, prefer/require `-> stay` for clarity.

When handler paths use error unions, require explicit return state form, e.g.
`-> stay | ErrorType`.

### 2) Same-type payload shorthand

For event handlers where event selector and payload type are the same:

- `on Ping(p: Ping) { ... }` can be written as `on Ping(p) { ... }`.
- `on Ping { ... }` is allowed when payload is unused.

This keeps handler syntax aligned with `match`-style destructuring intuition.

### 3) Keep correlation/capability out of payload arguments

Correlation and reply capabilities are modeled as handler context, not as
primary payload parameters. This avoids confusing handlers with payload tuple
destructuring.

Default ergonomic model:
- payload pattern first (`on AuthApproved(ok) { ... }`),
- correlation implicit,
- optional explicit provenance binding (below) only when needed.

### 4) Explicit request provenance via `for`

When response handlers need access to the originating request payload, use
`for`:

```mc
on AuthApproved(ok) for AuthCheck(req) {
    println(f"approved conn={req.conn_id} user={ok.user_id}");
}
```

`for` binds the correlated originating request payload for that response event.
It is optional in the common case where only response payload is needed.

### 5) Implicit correlation model

`request(...)` creates an internal inflight correlation entry managed by
compiler/runtime metadata. Response routing is deterministic against these
entries without requiring user-managed pending lists in normal code.

`Pending<T>` remains available as an advanced explicit control tool, but is not
required for basic request/reply flows.

Ambiguity policy:
- if a response handler has multiple possible originating request sites of the
  same request type, require explicit disambiguation.
- v1 disambiguation can use request-site labels, e.g. `request:auth1(...)` and
  `for AuthCheck:auth1(req)`.

### 6) Inflight correlation cleanup policy

Implicit correlation requires bounded lifecycle for inflight entries.

V1 baseline:
- remove entry on successful response commit,
- remove entry on requester machine fault/stop,
- remove entry on timeout with dead-letter/fault telemetry,
- expose diagnostics/hooks for timeouted/canceled inflight entries.

## Request/Reply Example (Implicit Correlation)

This example represents the Tier 2 target surface.

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

## Diagnostics and Tooling UX

### 1) Error messages should speak source terms

Diagnostics/hover/signature help should use:
- `Connection::Disconnected`
- `Machine<Connection>`
- `Pending<AuthApproved | AuthDenied>`

and avoid leaking synthetic/internal symbol names.

### 2) Managed API diagnostics

Add focused diagnostics for:
- sending payload not accepted by the target machine/protocol role,
- replying with type outside allowed response set,
- using a consumed or mismatched `Pending`/`ReplyCap` token (explicit forms),
- ambiguous response handlers where `for` provenance is required to disambiguate.

### 3) IDE quick fixes

Candidate quick fixes for V1/V1.1:
- add missing `on` handler stub,
- add missing response pattern arm,
- convert raw runtime API usage to typed `spawn/send/request` wrappers.

## Semantics and Safety Constraints (Carried Forward)

1. Managed handlers remain transactional.
2. Reply capabilities remain linear.
3. Default handler precedence remains state-local first, typestate-level fallback.
4. Shape conformance remains minimum protocol checking level for current V1.

This document changes source ergonomics, not those invariants.

## Implementation Strategy

### Phase A: remove user-facing runtime plumbing

1. Add explicit entrypoint opt-in for managed runtime in binaries.
2. Add source-level `Typestate::spawn(...)` to std+compiler lowering path.
3. Wire handles to runtime registration automatically.
4. Keep explicit runtime APIs for tests/embedding.

### Phase B: typed surface cleanup

1. Expose typed handle operations (`handle.send`, `handle.request`).
2. Replace/cover low-level message ABI calls behind generated wrappers.
3. Add handler-side sugar (`send/request/reply`) and lower to existing core forms.
4. Implement `-> stay` semantics and shorthand rules.
5. Implement shorthand payload forms (`on Ping(p)`, `on Ping`).

### Phase C: polish and hardening

1. Upgrade diagnostics to source-first naming.
2. Update docs/examples to zero-ceremony style.
3. Add e2e tests covering multi-inflight request/reply interaction.
4. Implement implicit correlation + `for` provenance with ambiguity diagnostics.

## Open Questions

1. What should default shutdown drain timeout be?
2. Should request-site labels be required syntax only on ambiguity, or always
   optional?
3. Should `emit` remain visible in user docs or be treated as advanced/internal
   syntax once sugar is available?
4. Should V1 expose a user-level timeout/cancel API for in-flight requests, or
   keep timeout policy runtime-config-only?

## Success Criteria

This proposal is successful when:

1. Typical machine programs contain no explicit runtime setup code.
   - but do include one visible managed-runtime opt-in marker at entrypoint.
2. Typical machine examples do not mention descriptor ids/state tags/payload words.
3. Two-machine request/reply examples are concise and readable without plumbing.
4. Compiler + IDE output uses source names and protocol/state language, not
   internal symbol forms.
