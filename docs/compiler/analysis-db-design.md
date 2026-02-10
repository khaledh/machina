# Analysis DB Design

This document defines a query-based incremental analysis architecture for
Machina that serves both CLI compilation and IDE tooling (LSP and JetBrains).

## Purpose

Machina currently uses a linear compiler pipeline. That works well for batch
compilation, but IDE workflows need:

- low-latency partial analysis on unsaved edits,
- cancellable concurrent requests,
- incremental recomputation,
- reusable semantic facts for navigation and refactoring.

This design introduces `AnalysisDb` as the semantic foundation for both batch
and interactive use.

## Goals

- Share one semantic implementation between CLI and IDE tooling.
- Support low-latency semantic queries on incomplete code.
- Preserve existing linear backend/codegen pipeline in V1.
- Remove cross-stage mutable side-table drift (boundary erosion).

## Non-Goals (V1)

- Rewriting backend/lowering/codegen into query form.
- Full persistent on-disk incremental cache.
- Perfectly stable IDs across arbitrary edits.

## Primary Tooling Targets

- VS Code and Cursor through LSP.
- JetBrains IDEs through:
  - LSP bridge mode (short path), and
  - native plugin mode using the same analysis API.

## Real-World IDE Use Cases

1. Edit-time diagnostics while typing in unsaved buffers.
2. Hover/type info in partially broken code.
3. Completion in invalid/incomplete contexts.
4. Go-to-definition and peek-definition across modules.
5. Workspace references and rename preview/edit plan.
6. Signature help and inlay-hint data for calls.
7. Semantic tokens and document symbols.
8. Code actions from structured diagnostics.
9. Background project checks with progressive updates.
10. Large-workspace responsiveness under frequent edits.

## Use Cases to Required Capabilities

1. Partial parse/resolve/typecheck with structured diagnostics.
2. Query-time symbol/type lookup by source span.
3. Incremental invalidation by module dependency closure.
4. Snapshot isolation for concurrent requests.
5. Request cancellation to avoid stale work.
6. Shared semantic query layer consumed by CLI + IDE adapters.

## Architectural Direction

Adopt a program-level `AnalysisDb` with memoized queries and dependency tracking.
Queries are pure from `(snapshot, inputs)` to outputs.

### Inputs

- file text (workspace + unsaved overlays)
- compiler options
- stdlib/module graph configuration

### Outputs

- parsed/resolved/typed artifacts
- phase diagnostics
- semantic navigation/type/refactoring facts

## High-Level Layering

1. Source Layer
- `FileId`, text storage, revisioned overlays.

2. Frontend Layer
- module discovery and graph construction.
- per-module parse artifacts and parse diagnostics.

3. Semantic Layer
- per-module resolve/typecheck artifacts.
- visibility/access checks across module graph.
- semantic diagnostics and symbol/type lookup data.

4. Batch Compilation Layer
- consumes query outputs and runs normalize/semck/elaborate/backend.

## Query Surface (Initial)

### Source/Graph

- `file_text(file_id) -> Arc<str>`
- `module_id_for_path(path) -> Option<ModuleId>`
- `module_graph(entry: ModuleId) -> Arc<ModuleGraph>`

### Syntax

- `parse_module(module_id) -> Arc<ParsedModuleResult>`

### Resolve

- `resolve_module(module_id) -> Arc<ResolvedModuleResult>`

### Typecheck

- `typecheck_module(module_id) -> Arc<TypedModuleResult>`

### Tooling

- `diagnostics(module_id) -> Arc<Vec<Diagnostic>>`
- `def_at(module_id, span) -> Option<DefId>`
- `type_at(module_id, span) -> Option<Type>`
- `hover(module_id, span) -> Option<HoverInfo>`
- `references(def_id) -> Arc<Vec<Location>>`
- `completions(module_id, span) -> Arc<Vec<CompletionItem>>`
- `signature_help(module_id, span) -> Option<SignatureHelp>`
- `document_symbols(module_id) -> Arc<Vec<DocumentSymbol>>`
- `semantic_tokens(module_id) -> Arc<Vec<SemanticToken>>`
- `rename_plan(def_id, new_name) -> Arc<RenamePlan>`
- `code_actions(module_id, range) -> Arc<Vec<CodeAction>>`

## Data Ownership Contract

### Problem

`DefTable` and `TypeMap` have drifted toward mutable cross-stage state.
This weakens stage boundaries and complicates incremental invalidation.

### Rule

Each query result owns immutable products. Later stages do not mutate earlier
stage outputs.

- Resolve outputs own source definition and name-resolution facts.
- Typecheck outputs own node/def type facts and call/generic side tables.
- Later semantic/elaboration outputs own lowering/drop plans.

## Synthetic Entity Strategy

Synthetic entities are first-class and provenance-tagged:

- `EntityOrigin::Source`
- `EntityOrigin::Synthetic { stage, reason }`

Synthetic defs/types are stored in stage-owned overlays. Lookup APIs merge base
and overlay views by precedence.

## Incrementality Model

- Query cache keys include input identities and revision.
- Query dependency edges drive invalidation.
- V1 invalidation granularity is module-level.
- Future refinement: item-level invalidation and more stable semantic IDs.

## Incremental Compilation Compatibility

This architecture is intentionally designed to stay compatible with future
module-level incremental compilation. To avoid painting the implementation into
a corner, keep these guardrails:

1. Module-scoped determinism
- Query outputs for a module must be deterministic for the same
  `(snapshot, options, dependency facts)`.
- Avoid hidden global mutable state in query evaluation.

2. Immutable phase products
- Parse/resolve/typecheck products remain immutable outputs.
- Later stages add overlays instead of mutating base products.

3. First-class dependency data
- Keep module graph edges and query dependency edges explicit and testable.
- Invalidation logic must remain closure-based, not ad-hoc.

4. Fingerprint-ready outputs
- Query products should be hashable/serializable in principle.
- This enables future artifact reuse keyed by source/options/API fingerprints.

5. Interface vs implementation separation
- Preserve clear exported API facts per module.
- Dependents should only need rebuild when relevant interface facts change.

6. Generic ownership policy
- Keep a clear policy for cross-module generic instantiation ownership so
  incremental recompilation boundaries stay predictable.

## Error-Tolerant Analysis Contract

- Parse returns partial AST + diagnostics.
- Resolve/typecheck are best-effort and return partial semantic products.
- Tooling queries prefer partial answers over hard failure.
- Diagnostics include stable codes and structured metadata for code actions.

## Concurrency and Cancellation

- Queries can execute in parallel along module DAG boundaries.
- Requests are cancellable; stale work should be dropped quickly.
- Snapshot isolation: each request reads a consistent revision view.

## IDE Integration Model

### VS Code / Cursor (LSP)

`machina-lsp` maps protocol handlers directly to `AnalysisDb` queries:

- diagnostics publication
- hover, go-to-definition, references
- completion, signature help
- semantic tokens, document symbols
- rename, code actions

### JetBrains

Two supported adapter modes:

1. LSP bridge mode calling `machina-lsp`.
2. Native plugin mode calling shared analysis APIs directly.

Both modes must reuse the same semantic engine; no duplicated compiler logic.

## Compiler Context Implications

Introduce query-native result products:

- `ParsedModuleResult`
- `ResolvedModuleResult`
- `TypedModuleResult`

Batch contexts should become thin wrappers around these products.

## Migration Plan

1. Add `AnalysisDb` facade over current frontend/resolve/typecheck.
2. Add snapshot/overlay and module-level invalidation.
3. Make resolve/typecheck outputs immutable by contract.
4. Move synthetic writes into stage-owned overlays.
5. Expose initial tooling queries (`diagnostics`, `def_at`, `type_at`, `hover`).
6. Route batch compile/check through query outputs.
7. Expand query surface for completion/rename/code actions/tokens.

## Execution Plan

Feature work depends on foundational work.

### Foundational Work

1. Query runtime core (keys, memoization, dependency graph).
2. Source snapshot and overlay model.
3. Module graph and invalidation propagation.
4. Immutable semantic products and synthetic overlays.
5. Unified diagnostics model with machine-readable metadata.
6. Shared query result schemas and lookup APIs.
7. Batch compatibility through query outputs.

### Feature Work

1. Diagnostics publishing.
2. Navigation (`def_at`, go-to-def, references).
3. Type/hover queries.
4. Completion and signature help.
5. Rename planning.
6. Semantic tokens and document symbols.
7. Code actions.
8. IDE adapters (VS Code/Cursor and JetBrains).

### Dependency Gates

- Gate A: Foundational 1-3 before interactive feature endpoints.
- Gate B: Foundational 4-6 before semantic features (navigation/type/completion/rename).
- Gate C: Foundational 7 before query mode becomes default in batch compile/check.

## Risks and Mitigations

- Risk: duplicate models between query products and batch contexts.
  - Mitigation: contexts are wrappers, not separate truth.

- Risk: hidden mutable state paths remain.
  - Mitigation: explicit ownership assertions and tests per stage.

- Risk: coarse invalidation hurts responsiveness.
  - Mitigation: start module-level, profile, then refine granularity.

## Success Criteria

- One semantic engine used by CLI, VS Code/Cursor, and JetBrains adapters.
- Typical edits avoid full-program re-typecheck.
- `diagnostics`/`type_at`/`def_at` on unsaved buffers are low latency.
- No cross-stage mutation of resolve/typecheck base outputs.

## Appendix: Out of Scope but Planned

The following items are intentionally out of scope for this phase, but are
expected follow-ups once the query architecture is stable.

1. Module artifact cache
- Cache per-module backend artifacts (`.o`, metadata) keyed by fingerprints.

2. API fingerprinting
- Distinguish interface hash changes from implementation-only changes to
  minimize dependent recompilation.

3. Incremental linker orchestration
- Re-link using a mix of reused and newly produced module artifacts.

4. Persistent cache storage
- Move from in-memory query caching to optional on-disk cache reuse between
  compiler invocations.

5. Fine-grained invalidation
- Evolve invalidation from module-level to item-level where it provides
  measurable wins.

6. Cross-module generic instantiation strategy
- Finalize ownership/placement policy for monomorphized instances to preserve
  predictable incremental rebuild boundaries.
