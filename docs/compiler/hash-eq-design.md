# Hash and Equality Design

## Status

Proposed.

## Goals

- Define a clear semantic model for equality (`==`, `!=`) and hashing.
- Unblock `set<T>` and `map<K, V>` with predictable key constraints.
- Keep V1 implementation simple and compatible with current compiler/runtime architecture.
- Preserve a clean migration path to user-facing traits (`Eq`, `Hash`) later.

## Non-Goals (V1)

- User-defined hash/equality implementations.
- Operator overloading through trait dispatch.
- Floating-point equality/hash semantics.
- Cryptographic hashing or hash-randomization for DoS resistance.

## Why This Comes First

`set<T>` and `map<K, V>` require:

1. A stable definition of key equality.
2. A coherent hash contract (`a == b => hash(a) == hash(b)`).
3. Compile-time diagnostics for unsupported key types.

Without this foundation, map/set behavior becomes ad-hoc and hard to evolve.

## Semantic Model

### Equality

`==` and `!=` are defined only for **equatable** types.

V1 equatable categories:

- Integers (`u*`, `i*`)
- `bool`
- `char`
- `string`
- Tuples whose fields are equatable
- Fixed arrays (`T[N]`) where `T` is equatable
- Structs where all fields are equatable
- Enums where all payload fields are equatable

V1 non-equatable categories:

- `^T` (heap-owning pointer types)
- Slices (`T[]`)
- Dynamic arrays (`T[*]`)
- `set<T>`, `map<K, V>`
- Function/closure types
- Type variables that do not resolve to an equatable concrete type

### Hashing

A type is **hashable** in V1 if:

- It is equatable, and
- It belongs to a V1 hashable subset.

V1 hashable categories:

- Integers (`u*`, `i*`)
- `bool`
- `char`
- `string`
- Tuples whose fields are hashable
- Fixed arrays (`T[N]`) where `T` is hashable
- Structs where all fields are hashable
- Enums where all payload fields are hashable

V1 non-hashable categories:

- `^T`
- Slices (`T[]`)
- Dynamic arrays (`T[*]`)
- `set<T>`, `map<K, V>`
- Function/closure types

## Laws and Invariants

For all supported types:

1. Reflexive: `x == x` is true.
2. Symmetric: `x == y` iff `y == x`.
3. Transitive: if `x == y` and `y == z`, then `x == z`.
4. Hash coherence: if `x == y`, then `hash(x) == hash(y)`.

Implementation requirement:

- Equality and hash synthesis must walk values with the same structural order.
- Enum hashing must include variant tag plus payload hash.

## Compiler Architecture

### Phase A (Immediate): Internal Capability Checks

Add internal capability queries in typecheck:

- `is_equatable(ty) -> Result<(), CapabilityErrorPath>`
- `is_hashable(ty) -> Result<(), CapabilityErrorPath>`

Use these in:

- `==`/`!=` operand validation.
- `set<T>` element constraints.
- `map<K, V>` key constraints.

No new user-facing trait system is required for this phase.

### Phase B: Structural Equality Lowering

Lower `==`/`!=` for aggregates via generated structural compare logic:

- Tuple/array: element-wise compare in order.
- Struct: field-wise compare in declaration order.
- Enum: compare tag, then variant payload fields.

This can be lowered inline or via generated helper functions per concrete type.

### Phase C: Structural Hash Synthesis

Generate monomorphized hash helpers for concrete hashable types used by set/map:

- Primitive leaf hash functions.
- Aggregate combine over ordered fields/elements.
- Enum hash includes tag and payload.

Set/map runtime receives concrete hash/eq hooks from lowering.

## Runtime Interface Direction

For hashed containers (future backend for set/map), runtime should accept:

- Equality callback
- Hash callback
- Element size/alignment metadata

Representative shape (conceptual):

```c
typedef uint8_t (*mc_eq_fn)(const void* a, const void* b);
typedef uint64_t (*mc_hash_fn)(const void* value);
```

V1 may keep current linear-scan set behavior while compiler/runtime hooks are introduced.

## Diagnostics

Diagnostics should identify the first failing subpath.

Examples:

- `Type 'Packet' is not equatable: field 'payload' (u8[]) is not equatable`
- `Type 'Config' is not hashable: field 'source' (^Source) is not hashable`
- `Operator '==' requires equatable operands, found set<u64>`
- `map<K, V> requires hashable key type K, found K = bytes[]`

## Trait Evolution (Future, Compatible)

After V1 is stable, expose public traits:

```machina
trait Eq {
    fn eq(self, other: Self) -> bool;
}

trait Hash {
    fn hash(self) -> u64;
}
```

Migration path:

1. Keep compiler-synthesized behavior as the default implementation source.
2. Add trait conformance checks/lookup.
3. Optionally add derive-style synthesis for user-defined types.
4. Allow manual impls later with explicit coherence requirements.

This keeps current V1 work valid and avoids re-architecting set/map.

## Rollout Plan

1. Add `is_equatable` / `is_hashable` capability checks and path-based diagnostics.
2. Enforce equality capability for `==` / `!=`.
3. Enforce hashable keys for map and hashable elements for set.
4. Add structural equality lowering for aggregates.
5. Add structural hash synthesis hooks for concrete types used in containers.
6. Switch map/set runtime from linear scan to hashed probing when ready.
7. Introduce public `Eq`/`Hash` traits on top of existing synthesis.

## Implementation Checklist

This checklist is ordered by dependency and intended to be landed in small,
reviewable commits.

### Milestone 1: Capability Analysis in Typecheck

- Add internal predicates:
  - `is_equatable(ty) -> Result<(), CapabilityErrorPath>`
  - `is_hashable(ty) -> Result<(), CapabilityErrorPath>`
- Implement recursive shape-walk with first-failure path reporting.
- Add canonical formatting for failure paths (`Foo.bar.baz`).
- Add diagnostics:
  - `TypeNotEquatable(ty, path, span)`
  - `TypeNotHashable(ty, path, span)`

Acceptance:

- `==`/`!=` over unsupported types emits one clear error.
- `set<T>` / `map<K, V>` unsupported key errors identify first failing field.

### Milestone 2: Enforce Equality Semantics for Operators

- Route `==` / `!=` obligation handling through `is_equatable`.
- Keep existing primitive fast paths; gate aggregate support behind capability.
- Ensure error suppression behavior stays stable (no duplicate follow-on errors).

Acceptance:

- Existing primitive comparison examples still compile.
- Non-equatable comparisons fail with targeted diagnostics.

### Milestone 3: Container Key Constraints on Hashability

- `set<T>` requires `T` hashable.
- `map<K, V>` requires `K` hashable.
- Keep value-type `V` unconstrained beyond normal typing rules.

Acceptance:

- `set<u64>`, `set<char>`, `set<string>` pass.
- Unsupported keys (`set<u8[]>`, `map<^X, Y>`) fail with path diagnostics.

### Milestone 4: Structural Equality Lowering

- Lower tuple/array/struct/enum equality structurally.
- Enum equality compares tag first, then payload.
- Reuse existing control-flow/value lowering patterns to avoid bespoke IR forms.

Acceptance:

- Add lowering tests for:
  - tuple equality
  - struct equality
  - enum equality (same variant, different variant)

### Milestone 5: Structural Hash Synthesis Hooks

- Introduce monomorphized hash helper generation for concrete hashable types.
- Ensure synthesis walk order mirrors equality walk order.
- Lower set/map hash calls to generated helpers.

Acceptance:

- Hash coherence spot tests (`a == b => hash(a) == hash(b)`).
- Distinct enum variants produce distinct hash streams by tag inclusion.

### Milestone 6: Hashed Container Runtime Backend

- Add hashed probing backend for map first (set can reuse map core or same table kernel).
- Preserve current API and ownership semantics.
- Keep linear-scan backend available during transition behind an internal switch.

Acceptance:

- Functional parity with existing set behavior.
- Collision handling, tombstone reuse, and growth covered in runtime tests.

### Milestone 7: Trait Surface (Future Compatibility Layer)

- Introduce public `trait Eq` / `trait Hash`.
- Bind synthesized implementations as default conformance source.
- Keep manual impls out of scope until coherence rules are finalized.

Acceptance:

- No behavior change to existing set/map programs.
- Trait surface is additive and forwards-compatible.

## Testing Plan

- Typecheck:
  - Equatable/hashable positive and negative matrices.
  - Field-path diagnostics for nested failures.
  - `==`/`!=` rejection for unsupported types.
- Lowering:
  - Aggregate equality lowering for tuple/struct/enum.
  - Enum tag + payload equality correctness.
- Runtime/container:
  - Set/map key behavior with collisions and duplicates.
  - Hash coherence checks for representative aggregate keys.
  - Deterministic behavior across runs for same inputs.
