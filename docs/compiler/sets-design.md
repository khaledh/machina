# Sets Design (V1)

## Status

Proposed.

## Goals

- Add a built-in set type that is easy to use for common cases.
- Keep literal syntax lightweight and close to Python:
  - `let s = {1, 2, 3};`
- Keep compiler complexity bounded by using a narrow V1 key space.
- Reuse runtime/container patterns already used by `string` and dynamic arrays.

## Non-Goals (V1)

- User-defined hashing/equality traits.
- Full structural hashing for arbitrary structs/enums.
- Set algebra API (`union`, `intersection`, `difference`).
- Stable iteration order guarantees.

## Surface Syntax

### Types

Use a built-in generic type name:

- `set<T>`

Examples:

```machina
var ids: set<u64> = {1, 2, 3};
let names: set<string> = {"a", "b"};
```

### Literals

- Non-empty set literal: `{e1, e2, e3}`
- Single-element set literal must use a trailing comma:
  - `{e1,}`
- Empty set literal uses a typed built-in literal:
  - `set<T>{}`
  - example: `let s = set<u64>{};`
- Bare `{}` is always a block expression.

### Parser Disambiguation

`{ ... }` can denote a block expression, struct update, or a collection literal.

V1 rule in expression position:

- `TypeName { ... }` remains struct literal syntax.
- `set<T>{}` and `map<K, V>{}` are typed collection literals.
- For bare `{ ... }`:
  - If top-level `|` appears, parse as struct update.
  - Else if top-level `:` appears, parse as map literal (handled by map design).
  - Else if top-level `,` appears, parse as set literal.
  - Else parse as block expression.

This keeps `{1, 2, 3}` lightweight while preserving block syntax and avoids
ambiguity for singleton sets by requiring `{e,}`.

## Type Rules

- `{e1, e2, ...}` infers `set<T>` where all elements are assignable to one unified `T`.
- Duplicate elements are allowed syntactically; dedup is runtime behavior.
- Empty set literal is `set<T>{}`; bare `{}` never denotes a set.
- Keys allowed in V1:
  - integer types, `bool`, `char`, `string`
- Other element types produce a clear type error:
  - `set<T> is not supported for element type T yet`

## Ownership and Mutation

Set is an owning container value.

- Move semantics follow other owning aggregate values.
- `insert` takes ownership of inserted values when needed.
- `remove` returns only membership success in V1 (`bool`), not removed value.

## API (V1)

```machina
set<T> :: {
    fn new() -> set<T>;
    fn with_capacity(cap: u64) -> set<T>;

    fn insert(inout self, value: T) -> bool;
    fn remove(inout self, value: T) -> bool;
    fn contains(self, value: T) -> bool;
    fn clear(inout self);

    @[intrinsic]
    prop len: u64 { get; }
    @[intrinsic]
    prop capacity: u64 { get; }
    @[intrinsic]
    prop is_empty: bool { get; }
}
```

Notes:

- `len`, `capacity`, and `is_empty` are intrinsic properties (not methods).
- `insert/remove/contains/clear` are intrinsic methods lowered to runtime calls.

## Runtime Representation

Primary representation is an open-addressing hash table.

Suggested header:

- `ptr: u64` (table storage)
- `len: u32`
- `cap: u32` (high bit reserved for owned flag, same style as `string`/dyn-array)

Table internals:

- Control bytes for empty/tombstone/occupied.
- Probing strategy: linear probing in V1.

## Optional Optimization: Bitmap Backend

For eligible tiny domains (`bool`, small enums, bounded small integers), backend can use a compact bitmap representation.

- This is an internal optimization only.
- The language-level type remains `set<T>`.
- No API or semantic differences.

## Compiler Pipeline Integration

1. Parser
   - Add set literal AST node for brace expressions with commas and no colons.
   - Support typed empty literal `set<T>{}`.
2. Typecheck
   - Add `Type::Set { elem_ty }`.
   - Infer element type from set literal items.
   - Enforce V1 key-type eligibility.
   - Resolve intrinsic properties/methods.
3. Normalize/Elaborate
   - Lower set literals into `new/with_capacity + insert` plan.
4. Lowering
   - Emit runtime calls for insert/remove/contains/clear.
   - Emit intrinsic property loads for `len/capacity/is_empty`.
5. Drops
   - Drop elements when needed.
   - Free backing storage when owned.

## Diagnostics

- Unsupported key type:
  - `set<T> is not supported for element type T yet`
- Property called as method:
  - `Property called as method: len` (same style as existing property diagnostics)
- Mutation while borrowed (if set views are later introduced):
  - follow existing borrow conflict diagnostics.

## Testing Plan

- Parser tests:
  - set literal parsing (including `{e,}`) and block disambiguation.
- Typecheck tests:
  - inference, key constraints, property/method resolution.
- Lowering tests:
  - literal construction sequence, runtime calls.
- Runtime tests:
  - insert/remove/contains semantics, rehashing behavior, drop behavior.
