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

        fn disconnect() -> Disconnected {
            Disconnected
        }
    }
}
```

And usage:

```mc
fn main() -> u64 {
    let c0 = Connection::new();
    let c1 = c0.connect("localhost");
    let c2 = c1.disconnect();
    c2.retries
}
```

## Rules (V1)

1. A typestate must declare `fn new(...) -> <State>`.
2. Each `state` declares transition methods.
3. `self` is implicit in transition methods (do not write it explicitly).
4. Transition return type must be a state, or a union where the first variant is
   a state (`Connected | ConnectError`).
5. State literals are only valid inside typestate constructor/transition bodies.
6. Typestate-level `fields` are carried across transitions automatically.
7. State-local field names cannot shadow carried field names.
8. Transition names must be unique within a state.

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
