# For-Loop Iteration Kernels Design

## Overview

Goal: keep user syntax minimal (`for item in collection`) while preserving fast code generation for built-in and standard-library collections.

This design introduces a **ForPlan + IterationKernel** abstraction so the compiler does not need to hardcode every collection type in multiple stages.

## Goals

- Keep one loop surface syntax: `for pat in expr { ... }`.
- Preserve efficient lowering for built-ins (no `iter()/next()` call overhead in generated code).
- Avoid compiler churn as new std collections are added.
- Keep backend model stable by continuing to lower loops to `while`.

## Non-Goals (V1)

- General auto-vectorization.
- User-defined custom intrinsic kernels.
- Replacing existing `for -> while` desugaring architecture.

## Core Idea

Split loop handling into:

1. **Planning (typecheck/finalize)**: choose a `ForPlan` for each `for` statement.
2. **Desugaring/lowering**: use the plan to emit one generic while-loop skeleton.

The skeleton is parameterized by a small kernel API:

- `init_cursor`
- `is_done`
- `load_item`
- `advance`

## ForPlan

Add a side-table entry keyed by `StmtId` (or equivalent):

```text
ForPlan {
  item_ty,
  kernel,
}

kernel =
  | Intrinsic(IntrinsicKernelId)
  | Protocol(ProtocolKernelSpec)
```

### Intrinsic kernel

Used for built-ins and selected std collections.

Examples:

- array/slice/dyn-array: index-based cursor
- range: numeric cursor
- set/map: slot-scan cursor

Lowering emits direct IR operations for these kernels.

### Protocol kernel

Fallback for user-defined iterables:

- `iter(self) -> Iter`
- `next(inout Iter) -> Item | IterDone`

Desugaring still emits `while`, but loop steps invoke protocol operations.

## Intrinsic Kernel Registry

To avoid per-feature compiler churn, centralize intrinsic loop metadata in one registry:

```text
IntrinsicKernelSpec {
  type_match,
  item_type_rule,
  cursor_layout,
  lower_init,
  lower_is_done,
  lower_load_item,
  lower_advance,
}
```

Adding a new fast-path collection should normally be:

1. add one kernel spec,
2. connect type-matching and lowering callbacks,
3. add tests.

No broad edits across parser/typecheck/desugar/backends.

## Desugaring Shape

`for pat in expr { body }` becomes:

```text
{
  let __src = expr;
  let __cursor = kernel.init_cursor(__src);
  while !kernel.is_done(__src, __cursor) {
    let __item = kernel.load_item(__src, __cursor);
    kernel.advance(__src, __cursor);
    let pat = __item;
    body
  }
}
```

`continue` behavior remains correct by preserving current increment-before-body semantics where required.

## Diagnostics

When no intrinsic or protocol kernel matches:

- `Type X is not iterable`
- help: `implement iter()/next()` or use a supported collection type

When protocol shape mismatches:

- wrong arity or mode on `next`
- `next` does not return `Item | IterDone`

## Why this avoids churn

Without this design, every new collection requires ad-hoc conditionals in multiple compiler stages.

With this design:

- parser is unchanged,
- loop syntax is unchanged,
- planning chooses kernel,
- desugar/lower uses the common kernel contract.

Most new collections only need one new intrinsic kernel spec and tests.

## Implementation sketch

1. Add `ForPlan` side table in typecheck/finalize output.
2. Add kernel selection in typecheck solver/finalize.
3. Update for-desugar to consume `ForPlan` instead of hardcoded type checks.
4. Add intrinsic kernel registry for existing built-ins.
5. Keep protocol fallback path for user iterables.
6. Add integration tests for:
   - built-in fast path still works,
   - protocol fallback works,
   - unknown iterable diagnostics.
