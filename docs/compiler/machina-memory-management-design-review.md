# Memory Management Design: Confined Value Semantics (CVS) - Panel Review

Review of the proposal in
[`machina-memory-management-design.md`](machina-memory-management-design.md).

## Status

Review completed February 2026.

Three independent expert panels evaluated the design using different model
architectures.

| Panel | Model | Primary Lens | Rating |
|-------|-------|--------------|--------|
| A | Claude Opus 4.6 | Language design coherence, specification rigor, rollout risk | 8.0/10 |
| B | GPT-5.3-Codex | Compiler/runtime feasibility, static invariants, diagnostics | 8.0/10 |
| C | Gemini 3 Pro | Teachability, hidden complexity cliffs, long-term ergonomics | 6.5/10 |

Cross-model average: **7.5/10** - strong architecture, but several semantics are
still underspecified and should be resolved before implementation.

## Executive Verdict

The CVS direction is strong and aligned with Machina's "explicit by default"
goal. The layered model (value/COW -> second-class refs -> regions ->
per-machine heaps -> shared immutables) is credible and differentiating.

The core blocker is not architecture; it is **spec precision**. Before coding,
the proposal needs explicit rules for (1) aliasing and mutation inside regions,
(2) `nocopy` interaction boundaries, (3) transactional transfer rollback
ownership state, and (4) `shared` deep-immutability eligibility.

## Consensus Strengths (3/3)

1. **Progressive disclosure is well-designed.** Most code can stay in Layers 1-2,
   with Layers 3-5 as opt-in power tools.
2. **Confinement is conceptually clean.** Scope-bounded ownership constraints map
   naturally to compiler checks and machine isolation.
3. **Typestate alignment is excellent.** Transfer-on-send and per-machine heaps
   fit managed-mode semantics and transactional handlers.
4. **Non-goals are disciplined.** Deferring GC, lifetime parameters, and
   cross-process transport implementation keeps V1 tractable.

## Consensus Risks (3/3)

### 1) Region Safety Contract Needs Tightening

Current text says region checks are "structural/lexical" and avoid borrow
checking. All panels agreed this is too loose for mutation + interior refs.

- If a reference into a region-backed collection exists, mutations that may
  invalidate storage must be constrained.
- Without this, region code can admit iterator invalidation/dangling-reference
  behavior.

**Action:** define a minimal region-local aliasing/loan rule set (no lifetime
syntax required), or explicitly scope the feature to immutable interior refs in
V1.

### 2) `nocopy` Is Underspecified

The proposal states "implicit copy is an error," but it is unclear for:

- generic instantiation and helper functions,
- implicit temporaries/desugaring,
- nested structs with `nocopy` fields,
- COW handle sharing vs true deep copy.

**Action:** add a formal "what counts as copy" section and exhaustive legal/
illegal contexts.

### 3) Transactional Transfer Rollback Must Define Ownership States

"Moved values are restored (or never transferred)" is directionally right but
implementation-critical details are missing.

**Action:** define staging states for payloads (`available -> staged ->
committed`), drop behavior on failure, and no-double-drop guarantees.

### 4) `shared T` Deep Immutability/Eligibility Needs a Checker Contract

Need explicit exclusion of non-freezable content (region refs, mutable cells,
unsafe pointer-containing types without proof).

**Action:** define a `Freezable`-style structural predicate in typecheck.

## Key Disagreements

1. **How strict Layer 3 should be in V1**
   - Opus/GPT: proceed with conservative but usable region refs + strong escape
     and alias checks.
   - Gemini: current model risks pushing users into a hidden complexity cliff;
     suggests stronger primitives (`Box<T>`, explicit iterator strategy) earlier.

2. **`shared T` type ergonomics**
   - Opus/GPT: keep `shared T` distinct and explicit, add good diagnostics.
   - Gemini: warns about API coloring pressure; suggests a read-only abstraction
     path to reduce duplicated APIs.

## Prioritized Recommendations

### P0 (Resolve Before Implementation Starts)

1. **Region aliasing contract:** define when mutation is illegal with live
   interior refs, including closure/iterator captures.
2. **`nocopy` formal semantics:** classify copy-triggering operations and
   transitivity behavior on aggregates and generics.
3. **Transfer staging model:** specify rollback/commit ownership transitions and
   drop invariants.
4. **`shared` freeze rules:** codify deep immutability eligibility and rejection
   diagnostics with field paths.
5. **Payload eligibility rule:** forbid region/second-class refs from
   `send/request/reply` payload graphs.

### P1 (Implement in V1)

1. Add dedicated diagnostics families for CVS (`RegionEscape`,
   `CrossRegionRefForbidden`, `NocopyImplicitCopy`, `SharedFreezeRejected`,
   `TransferRollbackUseAfterStagedMove`).
2. Add COW observability tooling (optional warnings/counters at detach sites).
3. Add strict tests for nested regions, closure captures, staged transfer faults,
   and `shared` freeze rejection paths.
4. Keep region rules conservative until full confidence (prefer false positives
   over unsound acceptance).

### P2 (Post-V1 / Evolution)

1. Region-polymorphic function design sketch (lightweight syntax, no Rust-level
   lifetime complexity).
2. Performance mitigations for `shared` RC contention (batched decrements,
   cache-line-aware layout, profiling hooks).
3. Evaluate library ergonomics for `shared T` read-only APIs to avoid excessive
   API duplication.

## Proposed Spec Amendments (Concrete)

1. **Region provenance rule:** any value derived from `r.alloc(...)` is tagged
   with region `r`; tagged values cannot be stored/returned/sent beyond `r`.
2. **Region mutation safety rule:** operations that may relocate/invalidate a
   region-backed object are rejected while interior references to that object are
   live.
3. **`nocopy` copy taxonomy:** explicitly separate handle-sharing (not a copy)
   from deep-copy-producing operations (copy).
4. **Transactional transfer state machine:** define staged effect ownership,
   rollback restoration semantics, and exactly-once drop guarantees.
5. **`shared` freeze eligibility:** freeze fails if any reachable field is
   mutable-by-construction, region-bound, or unsafe without explicit opt-in.

## Compiler/Runtime Mapping (Machina Codebase)

The review cross-check identified practical integration points:

- **Semantics checks:** `src/core/semck/move_check.rs`,
  `src/core/semck/slice_borrow.rs`, `src/core/semck/slice_escape.rs`
- **Type contracts:** `src/core/typecheck/*`, `src/core/types/*`
- **Typestate + machine planning:** `src/core/typestate/mod.rs`,
  `src/core/elaborate/machine_plan.rs`, `src/core/backend/lower/machine.rs`
- **Drop/ownership lowering:** `src/core/backend/lower/drops.rs`,
  `src/core/backend/lower/drop_glue.rs`
- **Runtime transaction semantics:** `runtime/machine_runtime.h`,
  `runtime/machine_runtime.c`
- **Diagnostic UX:** `src/services/analysis/diagnostics.rs`

## External Validation Notes (Librarian Pass)

The panel compared CVS assumptions to known production patterns:

- **Swift COW:** practical and ergonomic, but can hide detach cliffs and has
  thread-safety caveats under unsynchronized mutation.
- **MLKit/Cyclone regions:** region systems are viable, but precision/usability
  depend heavily on conservative escape/alias rules.
- **BEAM actor heaps:** per-actor heaps scale conceptually; large payload copying
  and shared-data mechanisms require careful thresholds.
- **Arc-style sharing:** deterministic and practical, but RC contention needs
  visibility and optimization paths.

These references support the CVS direction, while reinforcing the need to lock
formal invariants before implementation.

## Ship Gates

CVS should not be considered implementation-ready until these are true:

1. P0 semantic contracts are written in the design spec.
2. Each contract is mapped to a concrete compiler pass and runtime invariant.
3. Negative tests exist for every forbidden escape/alias/transfer/freeze case.
4. Transaction rollback paths prove no use-after-move and no double-drop.
5. Diagnostics include actionable fix guidance for each new error family.

---

Overall recommendation: **Proceed with CVS**, but treat this as a
spec-hardening-first milestone before parser/runtime implementation begins.
