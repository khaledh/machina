# Dynamic Arrays Design (`T[*]`)

## Overview

This document defines v1 growable arrays for Machina as a language primitive:

- Primitive type form: `T[*]`
- Fixed arrays remain: `T[N]`
- Slices remain non-owning views: `T[]`
- Optional ergonomic alias in std: `List<T> = T[*]`

The goal is to make the common case simple (`append`, indexing, literals) while
preserving explicit low-level control through the existing type family.

## Goals

- Keep array syntax cohesive: `T[N]`, `T[]`, `T[*]`.
- Make growable arrays first-class and ergonomic.
- Reuse existing runtime support (`mc_dyn_array_t`) instead of inventing a new
  container runtime.
- Preserve current slice safety model when slicing growable arrays.
- Align mutating API naming with strings (`append`).

## Non-Goals (v1)

- General collection framework (iterators, map/filter/reduce, etc.).
- Full `List<T>` language keyword/type constructor.
- Reworking string representation.
- Changing runtime layout widths to `u64/u64/u64`.

## Type Model

Machina array family after this feature:

- `T[N]`: fixed-size array value.
- `T[]`: non-owning slice/view (`ptr + len`) with borrow-style safety checks.
- `T[*]`: owning growable array value (`ptr + len + cap`).

`T[*]` is move-only (like other owning resource values) and requires drop when
`T` needs drop.

## Syntax

### Type syntax

```mc
var xs: u64[*];
fn collect(xs: u64[]) -> u64[*] { ... }
```

### Construction

```mc
var a = u64[*]::new();
var b = u64[*]::with_capacity(16);
var c: u64[*] = [1, 2, 3];   // fixed literal coerced into growable
```

### Operations (v1)

```mc
a.append(4);
let n = a.len();
let cap = a.capacity();
let empty = a.is_empty();
let x = a.pop()?;            // pop: T | Empty
a.clear();
```

## Core Semantics

### Ownership and moves

- `T[*]` owns heap storage.
- Assignment transfers ownership (move semantics).
- Passing to `sink` transfers ownership.
- Implicit copying is not allowed.

### Drop behavior

On drop of `v: T[*]`:

1. Drop elements in index range `[0, len)`, when `T.needs_drop()`.
2. Free backing storage if owned and non-null.

### Runtime layout (v1)

`T[*]` lowers to runtime layout already implemented in `runtime/types.h`:

```c
typedef struct mc_dyn_array {
  uint64_t ptr;
  uint32_t len;
  uint32_t cap; // high bit is owned flag
} mc_dyn_array_t;
```

This mirrors `mc_string_t` and intentionally remains 16 bytes in v1.

### Coercions

Allowed implicit coercions:

- `T[N] -> T[*]` (alloc + copy; used by typed literal assignment)
- `T[*] -> T[]` (view of current elements)
- `T[N] -> T[]` (existing behavior)

### Slice safety with growable arrays

When a live slice aliases a growable array, operations that may invalidate the
backing pointer are rejected until borrow ends.

Example:

```mc
var arr: u64[*] = [1, 2, 3];
let s = arr[..];
// arr.append(4); // error: arr is borrowed by s
let x = s[0];     // last use
arr.append(4);    // ok
```

This extends existing `slice_borrow` rules to `T[*]` sources.

## API Surface (v1)

The v1 language/runtime surface for `T[*]`:

- `fn new() -> T[*]`
- `fn with_capacity(cap: u64) -> T[*]`
- `fn append(inout self, sink value: T)`
- `fn pop(inout self) -> T | Empty`
- `fn len(self) -> u64`
- `fn capacity(self) -> u64`
- `fn is_empty(self) -> bool`
- `fn clear(inout self)`

Notes:

- Method name is `append` to stay consistent with `string.append`.
- `pop` is fallible and uses error unions instead of panic.
- `Empty` is a lightweight std/core error type for now.

## Frontend/Typecheck Changes

### Parser

- Extend type parser to accept `[*]` in array-type position.
- Keep `[]` and `[N]` unchanged.

### Type representation

- Add a dedicated type form for growable arrays (`Type::DynArray` or
  equivalent), rather than overloading fixed arrays/slices.
- Update printers, hashing, equality, substitution, and assignability helpers.

### Constraint and solve rules

- Method resolution for `append/pop/len/...` on `T[*]`.
- Coercion rules:
  - literal/fixed array to `T[*]`
  - `T[*]` to `T[]`
- Borrow-safety integration:
  - treat `append`, `clear`, and potential reallocating operations as mutations
    blocked by live slice borrows.

### Diagnostics

Add specific diagnostics for:

- Invalid `[*]` type syntax usage.
- Missing `append`/`pop` compatibility on non-`T[*]`.
- Mutation while growable-array slice borrow is live.
- `pop?` misuse when return type omits `Empty`.

## Lowering/Backend/Runtime Changes

### Lowering

- Lower `T[*]` values as `mc_dyn_array_t`-compatible aggregate.
- Lower `append` to `__rt_dyn_array_append_elem`.
- Lower capacity growth preallocation (if needed) to `__rt_dyn_array_ensure`.
- Lower `pop` by:
  - checking `len == 0`,
  - constructing `Empty` error union variant,
  - otherwise moving out last element and decrementing len.

### Drop glue

- Extend drop-lowering for `T[*]`:
  - element drop loop when element type needs drop,
  - free backing storage if owned.

### Runtime

No new runtime layout required for v1. Reuse existing:

- `__rt_dyn_array_ensure`
- `__rt_dyn_array_append_elem`

Potential future runtime additions (optional):

- `__rt_dyn_array_pop_elem`
- `__rt_dyn_array_clear`

## Standard Library Surface

Provide optional naming alias without changing core type semantics:

```mc
type List<T> = T[*];
```

This alias belongs in std, not in the core language grammar.

## Testing Plan

### Parsing and type tests

- Parse `u64[*]` in local vars, params, and returns.
- Coercion tests:
  - `let x: u64[*] = [1, 2, 3]`
  - passing `u64[*]` to `u64[]` params.
- Negative tests for illegal mutations under live slice borrow.
- `pop` typeflow tests with error unions.

### Lowering/runtime tests

- `append` growth and capacity behavior.
- `pop` success and `Empty` path.
- drop correctness for `T[*]` with droppable element types.
- borrow safety interactions (`arr[..]` + `append`/`clear`).

### Example coverage

Add or extend examples under `examples/` with:

- dynamic array basics,
- coercion to slice,
- fallible pop with `?`,
- borrow conflict diagnostics.

## Rollout Plan

1. Parse/type representation for `T[*]`.
2. Type rules + method resolution + coercions.
3. Lowering/runtime wiring for `append` and basic construction.
4. Drop support + borrow-safety integration.
5. Fallible `pop` and diagnostics polish.
6. Std alias + docs/examples.

## Open Questions

- Public API widths: should `len/capacity` be `u64` (language-friendly) with
  checked conversion to runtime `u32`, or exposed as `u32` directly?
- Should `append` accept `in value: T` for copyable scalar `T`, with `sink`
  required only for non-copy types, or keep one uniform `sink` signature in v1?
- Whether to add `reserve` in v1 or keep it for v1.1.
