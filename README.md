# Machina

Machina is an early-stage systems programming language focused on a specific
question:

**Can the language make large stateful systems easier to design and safer to evolve, without becoming heavyweight?**

It combines explicit ownership and value semantics with a roadmap toward
first-class workflow and state-machine modeling.

## Why Machina

Most systems languages force a tradeoff:

- low-level control but weak design guidance, or
- strong safety but high conceptual overhead

Machina aims for a middle path:

- **Explicit by default**: ownership transfer and mutation are visible in code
- **Composable abstractions**: traits, methods, properties, algebraic data types
- **Design-level safety direction**: linear workflow types and hosted machines

## Current Status

Machina is being actively developed and not stable yet.

- Compiler pipeline + ARM64 codegen
- Module/capsule model with `requires { ... }`
- Traits, properties, generics, unions for error handling, collections
- `@linear type` for direct typestate-style values and hosted workflows
- IDE/LSP support in progress

Current compiler target: **ARM64**.

## Example

```mc
@linear
type Approval = {
    id: u64,

    states {
        Review,
        Approved,
    }

    actions {
        approve: Review -> Approved,
    }

    roles {
        Reviewer { approve }
    }
}

Approval :: {
    fn approve(self) -> Approved {
        println("approved");
        Approved {}
    }
}

machine ApprovalService hosts Approval(key: id) {
    fn new() -> Self { Self {} }
}

fn main() -> () | MachineError | SessionError {
    let service = ApprovalService::spawn()?;
    let review = service.create(Approval as Reviewer)?;
    let _approved = review.approve()?;
}
```

Try it:

```sh
cargo mcc run examples/linear/approval_hosted.mc
```

That example is intentionally small. For the more complete story — async events,
multiple actors over time, and resuming a long-lived entity by key — see
[`docs/why-machina.md`](docs/why-machina.md) and
`examples/linear/payment_lifecycle.mc`.

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
- [Why Machina](docs/why-machina.md)
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
support for modeling long-lived workflows, stateful services, and typed machine
interactions in code.

## License

Machina is licensed under the [MIT License](LICENSE).
