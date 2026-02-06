# Typecheck Rewrite Plan

This document defines the migration contract for the parallel `src/typecheck`
implementation. The rewrite is staged and non-disruptive: existing behavior is
kept through the legacy `src/typeck` path until parity is proven.

## Goals

- Split type checking into explicit phases with clear ownership.
- Isolate constraint collection from solving.
- Preserve downstream contracts:
  - `TypeMap`
  - `CallSigMap`
  - `GenericInstMap`
- Enable future features like inferred closure signatures without full HM.

## Phase Model

The target pipeline is:

1. `collect`: build symbol/signature environment.
2. `constrain`: walk AST and emit type constraints/obligations.
3. `solve`: solve constraints and call-overload obligations.
4. `validate`: run semantic checks that are not pure unification.
5. `finalize`: materialize side tables and typed tree inputs.

## Compatibility Contract

- Public function shape must stay:
  - `type_check(ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>>`
- Error type remains `TypeCheckError` / `TypeCheckErrorKind` during migration.
- Legacy `src/typeck` remains default checker until parity harness is green.
- New checker may be dual-run in tests before being user-visible by default.

## Runtime Toggle

`typecheck` is now the default public entrypoint and always runs first.
During migration, it still falls back to legacy `typeck` output/diagnostics in
compatibility mode.

### Strict Rewrite Mode

Output defaults to legacy-compatible behavior while parity is under
construction.

- To force strict rewrite results: `MACHINA_TYPECHECK_REWRITE_STRICT=1`

## Engine Boundaries

- `TcEnv`:
  - immutable shared inputs
  - collected symbols/signatures
- `TcState`:
  - mutable pass products and diagnostics
- `DiagCtx`:
  - compact contextual metadata for consistent diagnostics

## Migration Strategy

1. Scaffold `src/typecheck` with no behavioral change.
2. Implement passes incrementally behind stable interfaces.
3. Add dual-run parity tests.
4. Flip default when parity is sustained.
5. Remove legacy implementation after burn-in.

## Strict Parity Snapshot

Snapshot date: February 6, 2026.

Command:

```bash
MACHINA_TYPECHECK_REWRITE_STRICT=1 cargo test -q
```

Result:

- Passed: `422`
- Failed: `77`
- Baseline failure list: `docs/compiler/typecheck-strict-failures.txt`

### Gap Matrix

| Capability bucket                                    | Failed tests | Primary owner pass(es)                      | Main symptom                                                              |
|------------------------------------------------------|-------------:|---------------------------------------------|---------------------------------------------------------------------------|
| Final type materialization / no TyVar leaks          |           36 | `solve`, `finalize`                         | backend sees `Var(TyVarId(...))`, unknown fn types, shape panics          |
| Expression constraints and diagnostic specificity    |            6 | `constraints`, `solve`                      | generic `DeclTypeMismatch` instead of specific operator/index diagnostics |
| Aggregate and pattern shape checks                   |            9 | `constraints`, `validate`                   | missing tuple/array/struct/enum payload checks                            |
| Calls, overloads, and generic instantiation          |            5 | `constraints`, `solve`, `finalize`          | generic call inference/parsing gaps, method arg mismatch not rejected     |
| Property and iterable checks                         |            3 | `constraints`, `validate`                   | read-only/write-only and `for` iterable checks not enforced               |
| Refinement/range/nonzero semantics                   |            5 | `validate`                                  | range/bounds/nonzero acceptance and rejection mismatches                  |
| Semantics coupling (move/borrow/match/struct-update) |           13 | `validate` (+ downstream semck assumptions) | semck expected errors not produced due weak type facts                    |

Notes:

- `backend::lower` failures (`28`) and `backend::opt` failures (`7`) are mostly downstream effects of unresolved type vars and missing shape constraints.
- `semck` failures (`18`) are mostly downstream effects of missing strict typing guarantees before semantic checks.

## Completion Plan

### Milestone 1: Constraint Coverage

- Replace broad equality constraints with typed obligations:
  - operator obligations (`arith`, `logical`, `cmp`, `neg`)
  - index/slice obligations
  - aggregate obligations (array/tuple/struct literal/update)
  - enum/match obligations
  - property obligations
  - iterable obligations (`for`)
- Add unit tests in pass-local modules for obligation emission.

Exit gate:

- Strict failures in `typecheck::tests_typecheck::*` reduced by at least 70%.

### Milestone 2: Solver + Diagnostics

- Solve obligations by kind, not just `Type == Type`.
- Introduce obligation-kind to diagnostic-kind mapping so strict mode emits precise `TypeCheckErrorKind`.
- Keep unification errors as internal data; surface semantic diagnostics by rule.

Exit gate:

- Operator/index/type-arg expectation tests emit exact legacy diagnostic kinds.

### Milestone 3: Validation Semantics

- Expand `validate` to enforce:
  - payload arity/type rules
  - struct update field existence/duplicates/types
  - property read/write access guarantees
  - refinement bounds and nonzero logic
  - match target/type and arm consistency invariants
- Keep `validate` as the single owner for non-unification semantic rules.

Exit gate:

- All strict failures in `semck::tests::*` that assert rejection behavior are resolved.

### Milestone 4: Calls + Generics + Overloads

- Complete call resolution parity:
  - overload filtering/scoring
  - type arg inference from args and expected return
  - deterministic ambiguity/no-match diagnostics
  - method/self-mode handling
- Ensure `GenericInstMap` is populated whenever monomorphization expects it.

Exit gate:

- Strict `monomorphize` and call-related typecheck tests pass.

### Milestone 5: Finalize Invariants

- Enforce invariant: successful strict check cannot emit unresolved inference vars in output side tables.
- Add a finalize validation pass that rejects leaked vars early with compiler-bug diagnostics (during development), then convert to hard assert once green.

Exit gate:

- Strict `backend::lower::*` and `backend::opt::*` suites pass without type-shape panics.

### Milestone 6: Rewrite Completion

- Remove compatibility fallback path from `typecheck::type_check`.
- Delete `src/typecheck/legacy`.
- Remove `MACHINA_TYPECHECK_REWRITE_STRICT` toggle and keep one execution path.

Final gate:

- `cargo check -q`
- `cargo test -q`
- `cargo fmt`
- strict run is identical to default behavior (single path).
