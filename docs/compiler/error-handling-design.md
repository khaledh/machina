# Error Handling Design

## Overview

This document defines the initial error handling model for Machina:

- Error unions in return position: `T | E1 | E2`
- A canonical `Try` construct with two surface forms:
  - Postfix propagation operator: `?`
  - Recovery operator with handler: `or |err| { ... }`
- Sugar forms over recovery handlers:
  - `expr or { ... }` (ignore error payload)
  - `expr or { E1 => ..., E2 => ... }` (arm-style recovery)
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

### 4. Recovery operator (`or`)

Given `x: T | E1 | E2`, users may handle the error path inline:

```mc
let cfg = parse_config(text) or |err| {
    match err {
        ParseError => default_config(),
        IoError => default_config(),
    }
};
```

`or` keeps success-path code linear while making recovery explicit.

#### Recovery sugar forms

Two sugar forms are supported and lower to the same canonical `Try` core:

```mc
// Ignore payload sugar
read(flag) or { 0 }
// Desugars to:
read(flag) or |err| {
    err;
    0
}

// Arm-style sugar
read(flag) or {
    io: IoError => io.code,
    parse: ParseError => parse.line,
}
// Desugars to:
read(flag) or |err| {
    match err {
        io: IoError => io.code,
        parse: ParseError => parse.line,
    }
}
```

### 5. Canonical internal form

Both `?` and `or` lower to one core construct in the tree model:

```text
Try { fallible_expr, on_error }
```

Where:

- `on_error = Propagate` for `expr?`
- `on_error = Handle(handler_expr)` for `expr or handler_expr`
- `expr or { ... }` is lowered to a synthetic closure handler.
- `expr or { arm => ... }` is lowered to a synthetic closure with an inner
  `match` on the error union.

`handler_expr` is expected to be callable with one argument (the error union).

### 6. Match arm binding

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

### 7. Placement restrictions (MVP)

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

For `Try { on_error = Propagate }` where `expr: T | E1 | E2`, caller must
declare `E1 | E2` in its own return error set. Otherwise, emit a compile error.

### Recovery handler typing

For `Try { on_error = Handle(h) }` and `expr: T | E...`:

- `h` must be callable with one argument of type `E...` (error-only union).
- `Try` expression result type is the join of:
  - success type `T`, and
  - handler return type.

In most cases, expected-context typing constrains this join to a single target
type (for example, assignment or function return context).

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

- Keep one canonical `Try` node in the AST/tree model.
- Parse `?` as `Try { on_error = Propagate }`.
- Parse `or` as contextual keyword in expression position:
  - `lhs or rhs` -> `Try { fallible_expr: lhs, on_error: Handle(rhs) }`.
- Parse sugar forms and desugar at parse-time into standard handler expressions:
  - `or { ... }` -> synthetic closure
  - `or { arm => ... }` -> synthetic closure with `match`

### 2. Resolver

- Resolve union return variant types.
- Resolve `name: Type` match-arm bindings.
- Resolve handler expression/bindings for `Try { on_error = Handle(..) }`.

### 3. Types

- Add `Type::ErrorUnion` in `src/types/mod.rs`.
- Update formatting, hashing, equality, and relations helpers.

### 4. Typecheck: constraints

In `src/typecheck/constraints.rs`:

- Emit obligations for canonical `Try` expressions:
  - propagate mode (`?`)
  - handle mode (`or ...`)
- Emit obligations for union-arm narrowing and exhaustiveness checks.
- Emit return conformance obligations against union variants.

### 5. Typecheck: solve

In `src/typecheck/solve.rs`:

- Enforce subset propagation for `Try::Propagate`.
- Enforce handler callable-arity/type constraints for `Try::Handle`.
- Compute/join result type for success and handler paths.
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
- `TryHandlerNotCallable`
- `TryHandlerArgMismatch`
- `ReturnNotInErrorUnion`
- `JoinArmNotInErrorUnion`
- `UnionNotAllowedHere`

Add/extend diagnostics in `src/semck/errors.rs`:

- `NonExhaustiveUnionMatch`

## Testing Plan

Add tests under `src/tests/*`:

- Parse tests for return unions, `?`, `or`, and closure-handler syntax reuse.
- Typecheck tests for subset propagation and exhaustiveness.
- Typecheck tests for handler callable typing and join typing.
- Negative tests for forbidden union positions.
- Normalize/lowering tests for union control-flow and payload behavior.
- End-to-end examples under subfolders in `examples/` (e.g. `examples/error_handling/*.mc`).
- Include sugar coverage via `/Users/khaled/src/khaledh/machina/examples/error_handling/try_or_sugar.mc`.

## Rollout

### Milestone 1

- Return unions + explicit `match` handling.
- No `?` yet.

### Milestone 2

- Add `?` with subset-propagation diagnostics.

### Milestone 3

- Diagnostic polish and documentation updates.

## Future Extensions

- Controlled broadening of where union types may appear.
- Better IDE diagnostics and quick-fix suggestions for propagation mismatches.
