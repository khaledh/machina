# Partial Semantic Analysis Design

This document defines how Machina should keep semantic analysis useful in
broken code by making resolve, typecheck, and semantic checks error-tolerant.

It complements `docs/compiler/analysis-db-design.md` and goes deeper on the
stage contracts needed for IDE/LSP behavior.

## Problem

Today, a single resolve/typecheck failure can drop the whole stage product for
the file/module in analysis mode. This causes tooling regressions such as:

- go-to-definition returning nothing for valid local symbols,
- hover/type queries failing outside the broken region,
- completion quality collapsing after unrelated mistakes.

This is an all-or-nothing failure model. It does not match IDE needs.

## Goals

1. Preserve as much semantic product as possible when errors exist.
2. Keep diagnostics accurate and explicit.
3. Ensure queries return best-effort answers for unaffected regions.
4. Keep batch compile behavior strict (errors still fail compile/build/run).
5. Keep stage boundaries clear and testable.

## Non-Goals

1. Making incorrect code compile.
2. Perfect recovery for every malformed construct in v1.
3. Changing language semantics.
4. Implementing full incremental compilation in this workstream.

## Core Principle

Each front-end stage must return:

1. `product` (possibly partial),
2. `diagnostics`,
3. `poisoned regions` that cannot be trusted downstream.

Downstream stages operate on non-poisoned regions and propagate poison where
needed, instead of aborting the entire module.

## Proposed Stage Contract

Introduce a common stage output shape:

```rust
struct StageOutput<T> {
    product: T,
    diagnostics: Vec<Diagnostic>,
    poisoned_nodes: FxHashSet<NodeId>,
}
```

Notes:

- `product` always exists for resolve/typecheck/semck in analysis mode.
- `poisoned_nodes` marks subtree roots that are semantically invalid.
- Queries may return `None`/empty for poisoned targets while still answering
  for healthy targets in the same file.

## Resolver Design

### Current Friction

- `resolve(...) -> Result<ResolvedContext, Vec<ResolveError>>` loses all context
  on any error in analysis flow.

### New Contract

- Add a tolerant resolver entrypoint:

```rust
resolve_partial(...) -> ResolveOutput {
    context: ResolvedContext,
    diagnostics: Vec<ResolveError>,
    poisoned_nodes: FxHashSet<NodeId>,
}
```

- Keep strict wrapper for compile path:
  - `resolve_strict(...) -> Result<ResolvedContext, Vec<ResolveError>>`
  - implemented by calling `resolve_partial` and failing if diagnostics exist.

### Recovery Rules (v1)

1. Unresolved name:
   - emit diagnostic,
   - mark the identifier node poisoned,
   - continue traversal.
2. Scope/duplicate errors:
   - emit diagnostic,
   - keep already-built symbol state where valid.
3. Keep `DefTable` for all successfully resolved defs/uses.

## Typecheck Design

### Current Friction

- typecheck failures can prevent typed product from being available for queries.

### New Contract

- Add tolerant entrypoint:

```rust
type_check_partial(...) -> TypecheckOutput {
    context: TypeCheckedContext,
    diagnostics: Vec<TypeCheckError>,
    poisoned_nodes: FxHashSet<NodeId>,
}
```

- Keep strict wrapper for batch compile.

### Recovery Rules (v1)

1. Type mismatch on a node:
   - emit diagnostic,
   - mark node poisoned,
   - assign a sentinel/unknown type for local continuation.
2. Constraint failures:
   - poison only dependent nodes, not whole function/module.
3. Keep call signatures and type map entries for valid nodes.

## Semantic Check Design

### Current Friction

- semantic checks may assume fully valid upstream state.

### New Contract

- `semck_partial(...) -> SemckOutput { diagnostics, poisoned_nodes }`
- Skip checks rooted in poisoned nodes from resolve/typecheck.
- Continue checks elsewhere.

### Recovery Rules (v1)

1. Control-flow checks run only on healthy regions.
2. Move/init/drop checks skip poisoned subtrees.
3. Emit diagnostics only when preconditions for a check are satisfied.

## Query Behavior

Queries must become poison-aware:

1. `def_at` / `definition`:
   - use resolved product even when resolve diagnostics exist,
   - return `None` only for poisoned target regions.
2. `hover`:
   - return best-effort symbol/type info for healthy regions,
   - `None` for poisoned region.
3. `type_at`:
   - return concrete type when available,
   - `None` for unknown/poisoned nodes.
4. `completion`:
   - return scope-derived items even if unrelated parts are broken.
5. `diagnostics`:
   - union of parse/resolve/typecheck/semck diagnostics, stable ordering.

## Module-Aware vs Partial Semantics

These are orthogonal:

1. Module-aware analysis fixes false unresolved symbols caused by missing
   dependency context.
2. Partial semantics fixes all-or-nothing collapse in presence of real errors.

Both are required for robust IDE behavior.

## Integration Points

Primary touchpoints:

- `src/analysis/pipeline.rs`
  - move from `Option<ResolvedContext>/Option<TypeCheckedContext>` on success-only
    to always-available stage outputs.
- `src/resolve/*`
  - add partial resolver output API.
- `src/typecheck/*`
  - add partial typecheck output API.
- `src/semck/*`
  - add partial semantic-check output API.
- `src/analysis/db.rs`
  - make lookup/query APIs poison-aware.

## Batch Compiler Compatibility

Batch compile/build/run remain strict:

1. Run partial stages internally.
2. If any diagnostics at strict phases, fail as today.
3. Backend/lowering never consumes poisoned stage products in strict mode.

This keeps current language/compiler guarantees unchanged.

## Migration Plan

1. Introduce shared `StageOutput<T>` and analysis-pipeline plumbing.
2. Implement `resolve_partial` + strict wrapper.
3. Wire query `def_at`/`definition` to partial resolve output.
4. Implement `type_check_partial` + strict wrapper.
5. Wire `type_at`/`hover`/`completion` to partial typed output.
6. Implement `semck_partial` with poison-aware skipping.
7. Add regression tests for mixed healthy/broken regions.

## Acceptance Criteria

1. A file with one unresolved symbol still supports go-to-definition for
   unrelated local functions/types.
2. Hover and type queries continue to work outside poisoned regions.
3. Completion still returns scope symbols in broken files.
4. Diagnostics remain deterministic and phase-tagged.
5. Compile/build/run strict behavior unchanged.

## Risks

1. Poison propagation too broad:
   - mitigation: poison only minimal subtree roots.
2. Hidden assumptions in downstream passes:
   - mitigation: explicit precondition checks and early skips.
3. Diagnostic duplication:
   - mitigation: stage ownership rules for diagnostics per failure kind.

## Open Questions

1. Sentinel representation for unknown/error type in type map:
   - dedicated `Type::Error` vs existing unknown form.
2. How much resolver recovery should happen per expression category in v1.
3. Whether semck should emit “skipped due to prior errors” telemetry in debug
   mode for troubleshooting.
