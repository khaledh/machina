# Maps Design (V1)

## Status

Proposed.

Companion foundation: `docs/compiler/hash-eq-design.md`.

## Goals

- Add a built-in hash table map with lightweight literal syntax:
  - `let t = {"one": 1, "two": 2};`
- Keep V1 small and predictable.
- Reuse as much runtime machinery as possible with sets.

## Non-Goals (V1)

- Ordered maps.
- User-defined hash/equality.
- Entry API (`entry`, `or_insert`, etc.).
- Multi-map semantics.

## Surface Syntax

### Types

- `map<K, V>`

Examples:

```machina
var ports: map<string, u16> = {"http": 80, "https": 443};
let counts: map<u64, u64> = {1: 10, 2: 20};
```

### Literals

- Non-empty map literal: `{k1: v1, k2: v2, ...}`
- Empty map literal uses a typed built-in literal:
  - `map<K, V>{}`
  - example: `let ports = map<string, u16>{};`
- Bare `{}` is always a block expression.

### Parser Disambiguation

In expression position, for `{ ... }`:

- `TypeName { ... }` remains struct literal syntax.
- `set<T>{}` and `map<K, V>{}` are typed collection literals.
- For bare `{ ... }`:
  - If top-level `|` exists, parse as struct update.
  - Else if top-level `:` exists, parse as map literal.
  - Else if top-level `,` exists, parse as set literal.
  - Else parse as block expression.

## Type Rules

- `{k1: v1, k2: v2}` infers `map<K, V>`:
  - unify all keys to `K`
  - unify all values to `V`
- Empty map literal is `map<K, V>{}`; bare `{}` never denotes a map.
- Key hash/equality capability and diagnostics follow `hash-eq-design.md`.
- V1 key constraints (same as set element constraints):
  - integer types, `bool`, `char`, `string`
- Value type `V` has no special restriction beyond normal type rules.

## Ownership and Mutation

Map is an owning container value.

- Keys and values are owned by the map.
- Mutating methods require `inout self`.
- Move semantics follow other owning aggregate values.

## API (V1)

```machina
map<K, V> :: {
    fn new() -> map<K, V>;
    fn with_capacity(cap: u64) -> map<K, V>;

    fn insert(inout self, key: K, value: V) -> bool;
    fn contains_key(self, key: K) -> bool;
    fn remove(inout self, key: K) -> bool;
    fn clear(inout self);

    @intrinsic
    prop len: u64 { get; }
    @intrinsic
    prop capacity: u64 { get; }
    @intrinsic
    prop is_empty: bool { get; }
}
```

Notes:

- `insert` returns whether key was newly inserted (`true`) vs updated (`false`).
- `remove` returns whether key existed.
- V1 intentionally omits reference-returning getters to avoid borrow complexity.

## Runtime Representation

Map and set should share the same table algorithm.

Header shape:

- `ptr: u64`
- `len: u32`
- `cap: u32` (owned bit in high bit)

Storage:

- slots hold `(K, V)` records
- control bytes for occupancy/tombstones
- open addressing probing (linear probing in V1)

## Compiler Pipeline Integration

1. Parser
   - Add map literal AST node for brace expressions with colon pairs.
2. Typecheck
   - Add `Type::Map { key_ty, value_ty }`.
   - Infer key/value types from literal entries.
   - Enforce V1 key-type eligibility.
   - Resolve intrinsic properties/method calls.
3. Normalize/Elaborate
   - Lower map literals into construction + insert sequence.
4. Lowering
   - Emit runtime calls for map intrinsics.
5. Drops
   - Drop keys and values as needed.
   - Free map storage when owned.

## Diagnostics

- Unsupported key type:
  - `map<K, V> is not supported for key type K yet`
- Property called as method:
  - existing property-call diagnostic.
- Invalid literal mix:
  - disallow mixed set/map entry forms in one literal.

## Testing Plan

- Parser:
  - map literal parsing, typed empty literal `map<K, V>{}`, disambiguation from blocks.
- Typecheck:
  - key/value inference, key constraints, method/property typing.
- Lowering:
  - literal lowering sequence and runtime-call shapes.
- Runtime:
  - insert/update/remove/contains behavior, rehashing, drop correctness.

## Relationship to Set

- A set is effectively a map with keys only.
- Implement map runtime core first (or shared core), then layer set on top.
- Keep user-facing APIs separate (`set<T>` and `map<K, V>`), but share internals.
