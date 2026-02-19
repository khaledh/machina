# Typestate Prototype Design (Feature-Flagged)

Issue: #79

## Goal
Ship a constrained typestate prototype that:
- is usable behind a feature flag,
- lowers to existing language constructs,
- does not destabilize default compilation mode,
- gives enough real examples to evaluate ergonomics.

## Non-Goals (V1)
- No full formal typestate model/proofs.
- No new runtime model.
- No borrow-checker/lifetime-style machinery.
- No separate compilation changes.

## Proposed Surface Syntax (Prototype)

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

Usage:

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

Notes:
- `typestate.fields { ... }` declares carried fields available to all states.
- `fn new(...) -> State` is required at typestate scope and defines the lifecycle entry state.
- `state.fields { ... }` declares state-local fields (optional).
- State methods are declared directly inside `state { ... }`.
- State methods have implicit `sink self: <CurrentState>`.
- State names are scoped to the `typestate` block in source; desugaring rewrites to generated nominal types.
- Typestate-level fields are available on every generated state type.
- State literal construction is restricted to typestate methods in V1; external construction must go through `Connection::new(...)`.

## Lowering Strategy (Desugar First)

Lower `typestate` into existing top-level constructs before resolve/typecheck:

1. Generate one struct type per state:
- `Connection$Disconnected`
- `Connection$Connected`

2. Flatten typestate-level fields into every generated state struct:
- `Connection$Disconnected { addr: string, retries: u64 }`
- `Connection$Connected { addr: string, retries: u64, fd: u64 }`

3. Rewrite state body methods into inherent methods on the source state type:
- `Connection$Disconnected :: { fn connect(sink self, fd: u64) -> Connection$Connected | ConnectError { ... } }`
- `Connection$Connected :: { fn close(sink self) -> Connection$Disconnected { ... } }`

4. Rewrite typestate-scope constructor:
- `fn new(...) -> State` is rewritten to `Connection :: { fn new(...) -> Connection$State { ... } }` (or equivalent generated namespace entrypoint).

5. Rewrite state references in signatures/bodies:
- `Disconnected` -> `Connection$Disconnected`
- `Connected` -> `Connection$Connected`

6. Apply implicit carried-field move on success-state literals:
- In state methods, `TargetState { ... }` auto-fills missing typestate-level fields from `self.<field>`.
- Carry is move/ownership transfer (not implicit copy).

7. (Optional in V1.1) Generate a dynamic sum enum for runtime state dispatch:
- `type Connection = enum { Disconnected(Connection$Disconnected), Connected(Connection$Connected) }`

This keeps backend/runtime unchanged.

## Why This Shape
- Minimal compiler risk: parser + desugar work, little change in resolver/typechecker core.
- Reuses existing method calls, nominal types, and ownership semantics (`sink`).
- Keeps typestate mostly a source-level UX feature in V1.

## Validation Rules (Prototype)

Enforced in desugar+early semantic checks:

1. `typestate` must declare at least one `state`.
2. State names must be unique within the block.
3. `typestate.fields { ... }` may appear at most once.
4. In each `state`, `fields { ... }` may appear at most once.
5. State-local fields must not redeclare typestate-level field names in V1.
6. Exactly one typestate-scope `new` method is required.
7. `new` return type must be:
- a declared state, or
- `State | Error...` where the first type is a declared state.
8. Transition methods must not declare explicit `self`; it is implicit.
9. Transition return type must be:
- a declared state, or
- `State | Error...` where the first type is a declared state.
10. Transition name collisions on same source state are disallowed.
11. External state literal construction is disallowed in V1 (must use `Typestate::new` for entry and transitions for evolution).
12. Prototype limitation (V1): implicit carried-field move is only guaranteed when success return is a direct state literal (`State { ... }`) or direct state shorthand (`State` for empty local fields).

## Diagnostics (Initial)

Add dedicated diagnostics:
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

Keep spans on source `typestate` declarations, not generated symbols.

## Feature Flag Plan

Compiler flag:
- `--experimental typestate` (or equivalent compile option `experimental_typestate: bool`).

Behavior:
- Default mode: `typestate` syntax rejected with a clear “feature disabled” diagnostic.
- Experimental mode: parse + desugar enabled.

LSP:
- Should honor project/compiler option so diagnostics match batch mode.

## Pipeline Placement

Add a small front-end prepass:
- Parse -> `TypestateDesugar` -> existing Resolve -> Typecheck -> Semck -> Elaborate...

Rationale:
- Keeps typestate-specific complexity out of resolver/typechecker internals for V1.
- Keeps current backend/runtime unchanged via desugaring.

## Example Set for Evaluation (5+)

Planned examples:
1. TCP connection lifecycle (`Disconnected -> Connected -> Disconnected`)
2. File handle lifecycle (`Closed -> Open -> Closed`)
3. Job execution (`Created -> Running -> Succeeded|Failed`)
4. Request builder (`Empty -> WithUrl -> Ready -> Sent`)
5. Service lifecycle (`Stopped -> Starting -> Running -> Stopping`)

Each should include at least one intentional invalid transition to test diagnostics.

## Risks and Guardrails

Risks:
- Surface syntax lock-in too early.
- Desugar name generation leaking into user-facing diagnostics.
- Partial/analysis mode divergence if gating is inconsistent.
- V1 direct-literal carried-field move restriction may feel limiting in complex control flow.

Guardrails:
- Keep feature off by default.
- Keep generated names internal; format diagnostics with source names.
- Add strict/partial parity tests for typestate-enabled files.
- Treat direct-literal requirement as an implementation limitation, not the long-term semantic model.
- Keep typestate rewrite in its own pass so later semantic-flow implementation can replace it without syntax churn.

## Success Criteria

Prototype is successful if:
- feature works behind flag without regressions in default mode,
- 5+ examples compile and expose meaningful ergonomic tradeoffs,
- diagnostics are understandable enough to guide iteration,
- we can decide what to keep/change before deep type-system investment.
