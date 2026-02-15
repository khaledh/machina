# Typestate

Typestate lets you encode lifecycle rules directly in types.

Instead of one mutable type with runtime guards, you model each lifecycle phase
as a distinct state type and define legal transitions between states.

> Status: experimental.
>
> CLI usage requires `--experimental typestate`.

## Why Use It

Typestate makes invalid flows fail at compile time.

Examples:
- Calling `send()` before `connect()`
- Calling `disconnect()` on an already disconnected value
- Constructing internal state values from outside the typestate

## Basic Shape

```mc
typestate Connection {
    fields {
        retries: u64,
    }

    fn new() -> Disconnected {
        Disconnected { retries: 0 }
    }

    state Disconnected {
        fn connect(addr: string) -> Connected {
            Connected { fd: 7 }
        }
    }

    state Connected {
        fields {
            fd: u64,
        }

        fn send(payload: string) -> u64 {
            payload.len
        }

        fn disconnect() -> Disconnected {
            Disconnected
        }
    }
}
```

And usage:

```mc
fn main() {
    let c0 = Connection::new();
    // c0.send("ping"); // compile error: send only exists in Connected
    let c1 = c0.connect("localhost");
    let sent = c1.send("ping");
    let c2 = c1.disconnect();
}
```

## Rules (V1)

1. A typestate must declare `fn new(...) -> <State>`.
2. Each `state` declares transition methods.
3. `self` is implicit in transition methods (do not write it explicitly).
4. Transition return type must be a state, or a union where the first variant is
   a state (`Connected | ConnectError`).
5. `on` handlers can return a state, `stay`, or `State | Error...`.
6. `on Event(payload)` and `on Event` shorthand forms are supported.
7. State literals are only valid inside typestate constructor/transition bodies.
8. Typestate-level `fields` are carried across transitions automatically.
9. State-local field names cannot shadow carried field names.
10. Transition names must be unique within a state.

## Error Handling in Transitions

Transitions can use error unions:

```mc
type ConnectError = { message: string }

typestate Client {
    fn new() -> Idle {
        Idle
    }

    state Idle {
        fn connect(addr: string) -> Connected | ConnectError {
            if addr.len == 0 {
                ConnectError { message: "empty address" }
            } else {
                Connected
            }
        }
    }

    state Connected {}
}
```

## Common Pattern

Use typestate for values with strict phases:
- network connections
- request builders
- startup/shutdown lifecycles
- handles/resources that must be opened before use

See `/examples/typestate/` for runnable examples.

## Managed Events And Protocol Flows

Machina also supports managed typestate machines with event handlers and
protocol/role flow declarations.

Tier 1 managed entrypoint model:
- mark binary entrypoint with `@[machines]`,
- spawn managed machines through `Typestate::spawn(...)`,
- drive event dispatch via `std::machine::step`.

`spawn(...)` mirrors constructor parameters exactly:
- `fn new(auth: u64) -> Idle` implies `AuthClient::spawn(auth)`.

```mc
requires {
    std::machine::managed_runtime
    std::machine::send
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

type AuthorizeReq = { user: string }
type AuthApproved = {}
type AuthDenied = {}
type Start = { user: string }

protocol Auth {
    role Client;
    role Server;
    flow Client -> Server: AuthorizeReq -> AuthApproved | AuthDenied;
}

typestate GatewayClient : Auth::Client {
    fields { auth_peer: u64 }

    fn new(auth_peer: u64) -> Idle {
        Idle { auth_peer }
    }

    state Idle {
        on Start(start) -> AwaitAuth {
            let pending = request(self.auth_peer, AuthorizeReq { user: start.user });
            AwaitAuth { pending }
        }
    }

    state AwaitAuth {
        fields { pending: Pending<AuthApproved | AuthDenied> }

        on Response(pending, AuthApproved) -> Connected { Connected {} }
        on Response(pending, AuthDenied) -> Idle { Idle {} }
    }

    state Connected {}
}

typestate AuthService : Auth::Server {
    fn new() -> Ready { Ready {} }

    state Ready {
        on AuthorizeReq(req: AuthorizeReq, cap: ReplyCap<AuthApproved | AuthDenied>) -> stay {
            if req.user.len > 0 {
                reply(cap, AuthApproved {});
            } else {
                reply(cap, AuthDenied {});
            };
        }
    }
}

@[machines]
fn main() {
    let auth: Machine = match AuthService::spawn() {
        m: Machine => m,
        _ => { return; },
    };
    let client: Machine = match GatewayClient::spawn(auth._id) {
        m: Machine => m,
        _ => { return; },
    };
    let client_id = client._id;

    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return; },
    };

    // Event kind ids are currently runtime ABI-level integers.
    match send(rt, client_id, 1, 0, 0) {
        ok: () => { ok; }
        _ => { return; },
    };
    step(rt);
    step(rt);
    step(rt);
}
```

Key ideas:
- `send(...)` and `request(...)` handler sugar lower to outbound effects checked against
  `flow` declarations for the implemented role.
- `request(...)` returns `Pending<...>` so later handlers can correlate
  responses.
- Pattern-form handlers (`on Response(pending, Variant) -> ...`) keep response
  routing explicit and checked.
- `reply(cap, value)` consumes `ReplyCap<...>` and enforces response-set safety.
- `@[machines]` is required to use `Typestate::spawn(...)` in binaries.

Examples:
- runnable single-role event flow:
  `/examples/typestate/machine_events_check.mc`
- runnable two-machine request/reply flow:
  `/examples/typestate/inter_machine_req_reply_check.mc`

Run them with:

```bash
cargo mcr --experimental typestate examples/typestate/machine_events_check.mc
cargo mcr --experimental typestate examples/typestate/inter_machine_req_reply_check.mc
```

## Managed Mode Limits (V1)

Current managed/event support is runnable but still early-stage.

- `Machine` handles currently expose machine id fields; ergonomic handle methods
  (`handle.send(...)`, `handle.request(...)`) are follow-up work.
- Event kind ids in `std::machine::send(...)` are still ABI-level integers.
- Emit payload ABI is currently minimal:
  - `emit Send/Request/reply` carries event kind + payload words.
  - rich payload boxing/unboxing and drop paths are still being expanded.
- Protocol conformance is shape-based for v1 (flows/handlers/payload families),
  without full per-state protocol projection checks yet.
