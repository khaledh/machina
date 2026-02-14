# Machina

Machina is an early-stage systems programming language focused on a specific
question:

**Can the language make large stateful systems easier to design and safer to evolve, without becoming heavyweight?**

It combines explicit ownership and value semantics with a roadmap toward
first-class state-machine and typestate modeling.

## Why Machina

Most systems languages force a tradeoff:

- low-level control but weak design guidance, or
- strong safety but high conceptual overhead

Machina aims for a middle path:

- **Explicit by default**: ownership transfer and mutation are visible in code
- **Composable abstractions**: traits, methods, properties, algebraic data types
- **Design-level safety direction**: typestate and state-machine-oriented features

## Current Status

Machina is being actively developed and not stable yet.

- Compiler pipeline + ARM64 codegen
- Module/capsule model with `requires { ... }`
- Traits, properties, generics, unions for error handling, collections
- IDE/LSP support in progress

Current compiler target: **ARM64**.

## Example

```mc
requires {
    std::io::println
}

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

fn main() {
    let c0 = Connection::new();
    // c0.send("ping"); // compile error: send only exists in Connected

    let c1 = c0.connect("localhost");
    let sent = c1.send("ping");
    let c2 = c1.disconnect();

    println(f"sent={sent}, retries={c2.retries}");
}
```

Try it:

```sh
cargo mcc run --experimental typestate examples/typestate/connection.mc
```

Typestate is where Machina is heading: APIs that encode lifecycle/state
constraints directly in types.

## Also Nice Today

- **Error unions + `?`** for explicit, low-ceremony error handling
- **Traits + trait properties** for abstraction and dispatch
- **Growable arrays / sets / maps** as built-in collection types
- **Module imports with `requires`**
- **LSP/editor tooling** support

## Quickstart

```sh
git clone https://github.com/khaledh/machina.git
cd machina
cargo mcc run examples/quickstart/hello.mc
```

## Documentation

- [Getting started](docs/getting-started.md)
- [Language tour](docs/tour.md)
- [Guide](docs/guide/)
- [Language reference](docs/reference/)
- [Examples](examples/)

## Project Structure

- `src/core/` — compiler core stages and IR
- `src/driver/` — batch compiler driver (`mcc`)
- `src/services/` — analysis/query services for IDE features
- `tooling/lsp/` — `machina-lsp` language server
- `tooling/vscode/` — VS Code/Cursor extension
- `runtime/` — runtime support
- `std/` — Machina standard library modules

## Direction

Machina is being built as a long-horizon language project. The near-term goal is
solid core ergonomics and tooling; the long-term differentiator is first-class
support for modeling and verifying stateful protocols in code.

## License

Machina is licensed under the [MIT License](LICENSE).
