# Frontend Trees/Contexts Collapse Evaluation

## Goal
Evaluate reducing near-identical frontend tree/context representations while preserving stage invariants and keeping IDE/batch behavior stable.

Issue: #78

## Current Reality
The frontend already has substantial physical sharing:

- `resolved::Module` = `tree::model::Module<DefId>`
- `typed::Module` = `tree::model::Module<DefId, TypeId>`
- `normalized::Module` = `tree::model::Module<DefId, TypeId>`

So the main remaining duplication is **logical stage boundaries and conversion flow**, not separate AST struct definitions.

## Invariants To Preserve

1. Resolver output must preserve DefId ownership/mapping as the source of truth.
2. Typecheck output must preserve node/def typing side tables (`TypeMap`, `CallSigMap`, generic insts).
3. Normalize must be the only phase inserting explicit coercions/property rewrites.
4. Semcheck must continue to consume normalized semantics (explicit coercions present).
5. Batch and analysis/LSP strict-vs-partial behavior must remain unchanged.

## Candidate Collapses Considered

### A) Full tree-count collapse (resolved/typed/normalized -> one tree type)
- Benefit: smaller conceptual surface.
- Cost: weakens phase contracts and makes pass ownership less explicit.
- Risk: boundary erosion across resolver/typecheck/normalize/semck becomes harder to reason about.

### B) Keep logical stage boundaries, remove redundant materialization/copy boundaries
- Benefit: preserves contracts while reducing churn/copy overhead.
- Cost: minimal.
- Risk: low (because representation is already shared).

## Spike Implemented

### Spike: remove typed->normalized rebuild in semcheck normalize prepass
File: `src/core/semck/normalize.rs`

Change:
- Before: `build_module(&module)` cloned/rebuilt typed tree into normalized tree.
- After: move owned module directly:
  - `let mut module: norm::Module = module;`

Why this is valid:
- `typed::Module` and `normalized::Module` are aliases of the same underlying generic model type.
- `normalize` already consumes stage input ownership; no downstream user requires the pre-normalized typed module instance.

Validation run:
- `cargo check -q`
- `cargo test -q typecheck_policy_strict_vs_partial`
- `cargo test -q elaborate_output_has_no_for_statements_after_syntax_desugar_pass`

## Spike Outcome

Result: **Positive**.

- Ergonomics: simpler; one fewer conversion step.
- Risk: low; contracts unchanged.
- Behavioral parity: validated by targeted tests.

## Recommendation

## Go/No-Go
**GO** for incremental collapse of redundant conversion/materialization boundaries.

**NO-GO** for collapsing logical stage contracts (resolved/typed/normalized semantics) into a single unconstrained phase model at this time.

Rationale:
- We can get real simplification wins without sacrificing ownership boundaries.
- Full contract collapse would save little structurally (trees are already shared) while increasing semantic coupling risk.

## Phased Plan (with guardrails)

### Phase 1 (done by spike)
- Remove typed->normalized module rebuild in semcheck normalization.
- Guardrail: targeted parity tests around strict/partial and semcheck/elaborate path.

### Phase 2
- Audit and remove other no-op tree adaptation helpers where source/target tree aliases are identical.
- Guardrail: keep stage input/output types unchanged; only internal conversion cleanup.

### Phase 3
- Evaluate context payload flattening where wrappers are pass-through only.
- Guardrail: API-level stage entrypoints remain stable; analysis and batch callsites unchanged.

### Phase 4
- Re-evaluate whether any remaining frontend tree wrappers add invariant value vs naming noise.
- Guardrail: do not merge phase responsibilities; only remove representational redundancy.

## Estimated Churn/Risk

- Near-term code churn: low (small, local edits in normalization and context wiring).
- Near-term deletion potential: moderate in adapter/conversion glue; low in core model definitions.
- Risk profile: low for conversion elimination, medium-high for contract collapse.
