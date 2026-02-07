# Error Handling Design

## Overview

This document defines the initial error handling model for Machina:

- Error unions in return position: `T | E1 | E2`
- Postfix propagation operator: `?`
- Exhaustive `match` over union variants with type-pattern arms
- Regular structs/enums as error variants (no separate error type category)

The goal is to make fallible code ergonomic on the success path while keeping
type checker complexity bounded.

## Goals

- Keep success-path code linear and easy to read.
- Make recoverable errors explicit in function signatures.
- Require intentional propagation and handling.
- Reuse existing enum/match machinery where possible.

## Non-Goals (MVP)

- General-purpose union types in all type positions.
- Exception-like implicit error flow.
- Additional chaining constructs (`with`, `use`, `guard`) in v1.
- New wrapper-centric APIs (`Result<T, E>`) as the primary language mechanism.

## Language Semantics

### 1. Return-position error unions

Functions may declare return types as:

```mc
fn load_config(path: string) -> Config | IoError | ParseError
```

The first variant is always the success type. Remaining variants are
recoverable error variants.

### 2. Success-first rule

The success-first rule is mandatory:

- `fn f() -> T | E1 | E2` means success is `T`.
- `?` unwraps `T` and propagates `E1 | E2`.

For functions with no success payload, use explicit unit:

```mc
fn shutdown() -> () | IoError
```

### 3. Propagation operator (`?`)

Given `x: T | E1 | E2`, `x?`:

- Produces `T` on success.
- Early-returns `E1` or `E2` from the current function.
- Requires `E1` and `E2` to be present in the caller's declared error set.

### 4. Match arm binding

`match` supports:

- `Type => { ... }` (variant test only)
- `name: Type => { ... }` (variant test + value binding)

Example:

```mc
match load_config(path) {
    cfg: Config => cfg,
    err: IoError => default_config(),
    err: ParseError => default_config(),
}
```

### 5. Placement restrictions (MVP)

Allowed:

- Function/method/closure return types.
- Inferred local bindings (e.g. `let r = f();` where `r` infers to a union).

Disallowed in explicit type positions:

- Function parameter types.
- Struct fields.
- Type aliases.
- Generic arguments.

This restriction keeps unions from leaking broadly into the type system in v1.

## Type Rules

### Return conformance

In a function returning `T | E1 | E2`, each return expression must be
assignable to at least one union variant.

### Subset propagation

For `expr?` where `expr: T | E1 | E2`, caller must declare `E1 | E2` in its
own return error set. Otherwise, emit a compile error.

### Invalid try

Applying `?` to a non-union fallible type is a type error.

### Exhaustive matching

`match` on an error union must cover all variants (or a wildcard arm if
wildcards are supported for this form).

### Narrowing

Inside `name: Type => ...`, the bound variable is narrowed to the arm type.

## Runtime Representation

Error unions are represented as tagged sums (anonymous enum model):

- Discriminant tag
- Payload storage for the active variant

Lowering/codegen should reuse existing enum machinery where possible.

## Compiler Implementation Plan

### 1. Parser and AST

- Update grammar in `grammar.bnf` for return unions, postfix `?`, and
  union-type arm patterns.
- Extend parse modules under `src/parse/*`.
- Extend tree model in `src/tree/model.rs`.

### 2. Resolver

- Resolve union return variant types.
- Resolve `name: Type` match-arm bindings.

### 3. Types

- Add `Type::ErrorUnion` in `src/types/mod.rs`.
- Update formatting, hashing, equality, and relations helpers.

### 4. Typecheck: constraints

In `src/typecheck/constraints.rs`:

- Emit obligations for `Try` (`?`) expressions.
- Emit obligations for union-arm narrowing and exhaustiveness checks.
- Emit return conformance obligations against union variants.

### 5. Typecheck: solve

In `src/typecheck/solve.rs`:

- Enforce subset propagation for `?`.
- Resolve/narrow union arm types.
- Enforce exhaustiveness diagnostics.
- Check return expression compatibility with union variants.

### 6. Finalize and normalize

In `src/typecheck/finalize.rs` and `src/normalize/mod.rs`:

- Preserve union/try resolution metadata in side tables.
- Rewrite to backend-friendly explicit control-flow forms where needed.

### 7. Lowering and codegen

In `src/backend/lower/*` and `src/backend/codegen/*`:

- Lower error unions as tagged sums.
- Reuse enum branch and payload handling infrastructure.

### 8. Diagnostics

Add/extend diagnostics in `src/typecheck/errors.rs`:

- `TryOperandNotErrorUnion`
- `TryOutsideFunction`
- `TryReturnTypeNotErrorUnion`
- `TryErrorNotInReturn`
- `ReturnNotInErrorUnion`
- `JoinArmNotInErrorUnion`
- `UnionNotAllowedHere`

Add/extend diagnostics in `src/semck/errors.rs`:

- `NonExhaustiveUnionMatch`

## Testing Plan

Add tests under `src/tests/*`:

- Parse tests for return unions, `?`, and match type-arm binding.
- Typecheck tests for subset propagation and exhaustiveness.
- Negative tests for forbidden union positions.
- Normalize/lowering tests for union control-flow and payload behavior.
- End-to-end examples under `examples/*.mc`.

## Rollout

### Milestone 1

- Return unions + explicit `match` handling.
- No `?` yet.

### Milestone 2

- Add `?` with subset-propagation diagnostics.

### Milestone 3

- Diagnostic polish and documentation updates.

## Future Extensions

- Optional syntactic sugar for validation-heavy code (`guard`-like form).
- Controlled broadening of where union types may appear.
- Better IDE diagnostics and quick-fix suggestions for propagation mismatches.
