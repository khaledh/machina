# Typecheck Architecture

This document describes the type checker architecture in `src/typecheck`.
It defines phase ownership, invariants, and quality gates for ongoing work.

## Goals

- Keep phase boundaries explicit and narrow.
- Separate syntax-driven collection from semantic solving.
- Preserve downstream contracts:
  - `TypeMap`
  - `CallSigMap`
  - `GenericInstMap`
- Support future features such as inferred closure signatures.

## Phase Model

The checker pipeline is:

1. `collect`: build symbol/signature environment.
2. `constraints`: walk AST and emit constraints/obligations/facts.
3. `solve`: solve constraints, resolve calls, and produce concrete types.
4. `validate`: enforce control-flow and non-unification semantic rules.
5. `finalize`: materialize side tables and typed tree inputs.

## Engine Boundaries

- `TcEnv`:
  - immutable shared inputs
  - collected symbols/signatures
- `TcState`:
  - mutable pass products and diagnostics
- `TypeVarStore`:
  - type-variable kinds
  - substitutions
  - fresh inference/meta variable generation

## Core Invariants

- Successful type check must not leak unresolved inference variables to outputs.
- `solve` owns type substitution and overload resolution.
- `validate` owns control-flow semantic checks (`return`, `break`, `continue`).
- `finalize` is the only phase that writes `TypeMap` / `CallSigMap` / `GenericInstMap`.

## Quality Gates

Every substantial checker change should pass:

- `cargo check -q`
- `cargo test -q`
- `cargo fmt`

For strict single-pipeline validation, run the strict-mode test command used
in the project tooling.

## Work Streams

### Constraint Coverage

- Prefer explicit obligation kinds over broad `Type == Type` constraints.
- Keep collection diagnostic-light; diagnostics belong in `solve`/`validate`.

### Solver Quality

- Keep overload selection deterministic and score-driven.
- Preserve refinement semantics while still allowing local inference progress.
- Ensure call-site resolution records deterministic `def_id` choices for finalize.

### Diagnostics

- Keep error kinds specific to rule category (operator/index/pattern/call/etc).
- Avoid generic mismatch diagnostics when a precise diagnostic is available.

### Maintainability

- Keep modules small and focused by phase responsibility.
- Document each module with `//!` explaining ownership and invariants.
- Add comments around dense orchestration code and non-obvious decisions.
