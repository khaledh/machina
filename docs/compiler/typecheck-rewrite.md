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

Production defaults to legacy `typeck`.

- To opt into the rewrite path:
  - `MACHINA_TYPECHECK_BACKEND=rewrite`
  - or `MACHINA_TYPECHECK_REWRITE=1`
- Any other backend value falls back to legacy.

### Strict Rewrite Mode

When rewrite is selected, output still defaults to legacy-compatible behavior
while parity is under construction.

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
