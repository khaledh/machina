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
- mark binary entrypoint with `@machines`,
- spawn managed machines through `Typestate::spawn(...)`,
- use typed handles (`Machine<Typestate>`) with method-style ops
  (`handle.send(...)`, `handle.request(...)`),
- let the compiler-managed runtime auto-drive dispatch after `main` returns.

```mc
requires {
    std::io::println
}

type OpenPressed = { id: u64 }
type OpenCmd = {}
type Opened = {}

typestate DoorActuator {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on OpenCmd(_cmd: OpenCmd, cap: ReplyCap<Opened>) -> stay {
            cap.reply(Opened {});
        }
    }
}

typestate DoorController {
    fields {
        door: Machine<DoorActuator>,
    }

    fn new(door: Machine<DoorActuator>) -> Closed {
        Closed { door: door }
    }

    state Closed {
        on OpenPressed(evt) -> Waiting {
            println(f"open event {evt.id}");
            let _pending: Pending<Opened> = request(self.door, OpenCmd {});
            Waiting
        }
    }

    state Waiting {
        on Opened(_evt) for OpenCmd(_origin) -> Open {
            println("door transitioned: Closed -> Waiting -> Open");
            Open
        }
    }

    @final
    state Open {}
}

@machines
fn main() -> () | MachineError {
    let actuator = DoorActuator::spawn()?;
    let controller = DoorController::spawn(actuator)?;
    controller.send(OpenPressed { id: 1 })?;
}
```

Key ideas:
- `send(...)` and `request(...)` handler sugar lower to outbound effects checked
  against `flow` declarations for the implemented role.
- Correlation is implicit by default. Use `for RequestType(binding)` on
  response handlers when you need provenance (or when disambiguation is
  required).
- Request-site labels (`request:label(...)`) disambiguate concurrent same-type
  inflight requests; handlers match them with `for RequestType:label(binding)`.
- `Pending<...>`/`ReplyCap<...>` remain available as explicit advanced forms.
- `cap.reply(value)` consumes `ReplyCap<...>` and enforces response-set safety.
- `@machines` is required to use `Typestate::spawn(...)` in binaries.

Examples:
- canonical managed interaction + state transitions:
  `/examples/typestate/managed_state_transitions.mc`
- focused labeled-provenance request/reply flow:
  `/examples/typestate/inter_machine_request_reply.mc`

Run them with:

```bash
cargo mcr --experimental typestate examples/typestate/managed_state_transitions.mc
cargo mcr --experimental typestate examples/typestate/inter_machine_request_reply.mc
```

## Managed Mode Limits (V1)

Current managed/event support is runnable but still early-stage.

- Raw ABI-level overloads still exist (`send(kind, payload0, payload1)` and
  `request(dst, kind, payload0, payload1)`), but normal app code should prefer
  typed forms.
- Emit payload ABI is still being expanded:
  - `emit Send/Request/reply` carries event kind + payload words.
  - rich payload boxing/unboxing and drop paths are still under active work.
- Protocol conformance is shape-based for v1 (flows/handlers/payload families),
  without full per-state protocol projection checks yet.
