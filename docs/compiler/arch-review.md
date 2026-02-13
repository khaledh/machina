# Architecture Review

_Date: 2025-02-13_ · _Codebase: ~96K lines, 305 files_

This review incorporates the prior review's findings, re-evaluates each against
the current state of the codebase, and adds new findings from a multi-model
expert panel analysis (three specialist perspectives stress-tested by a
contrarian reviewer focused on single-developer momentum).

## What's Working Well

These are not generic compliments. Each is a specific architectural choice that
is better than what most compilers at this stage have.

**1. Staged context design with typed contracts.** The `ParsedContext →
ResolvedContext → TypeCheckedContext → … → AnalyzedContext` chain with type
aliases (`TypecheckStageInput = ResolvedContext`) makes stage boundaries explicit
and compiler-enforced. Each stage's inputs and outputs are statically known.
_(Unchanged from prior review — still a core strength.)_

**2. Five-phase constraint-based typechecker.** Collect → Constraints → Solver →
Validate → Finalize is clean, well-decomposed, and separates constraint
_generation_ from _solving_. This is the right architecture for overload
resolution, generics, and eventually typestate. Still zero TODOs in the
typecheck module. _(Unchanged — still excellent.)_

**3. Immutable trees + side tables.** Information accumulates in keyed side
tables (`TypeMap`, `DefTable`, `CallSigMap`), not by mutating the tree. This
enables clean separation across the pipeline. _(Unchanged.)_

**4. SemCheck modularity.** Independent checkers (move restrictions, borrow
scope, init tracking, slice escape, call overlap, closure capture) composed at
the top level. Easy to add new checks — and typestate transition rules will
have a natural home here. _(Unchanged.)_

**5. Elaboration as "make implicit explicit."** Pre-computing lowering plans,
drop plans, match decision trees, and closure lifting _before_ IR lowering keeps
the backend semantics-free. This is a maintainable design choice that most hobby
compilers skip. _(Unchanged.)_

**6. SSA IR with block parameters + explicit drops.** Modern design (aligned with
MLIR, Cranelift). The hybrid scalar-SSA / place-based-aggregate approach is
pragmatic and correct. _(New observation.)_

**7. Query-based analysis layer with snapshot isolation.** `QueryRuntime` with
dependency tracking, `CancellationToken`, source overlays, and transitive
invalidation. Production-grade IDE infrastructure. _(New observation.)_

**8. Compile driver decoupled from AnalysisDb.** The prior review flagged
`compile_with_path()` depending on the query/caching infrastructure. **This has
been fixed.** The compile path now calls pure pipeline functions via `core::api`
directly. `AnalysisDb` is no longer in the batch compilation hot path.
_(Resolved since prior review.)_

**9. Optimization pipeline.** Clear tier structure (CFG cleanup → CFG-free →
dataflow → module DCE) with fixpoint iteration and a trait-based `Pass`
interface. Easy to add new passes. _(Unchanged.)_

**10. Code quality.** 96K LOC with only 5 TODO markers (2 in code-action
templates, 3 legitimate). No FIXME/HACK/XXX. Exceptional for any codebase, let
alone a hobby project.

## Status of Prior Review Items

| # | Prior Finding | Status | Notes |
|---|---------------|--------|-------|
| 1 | Tree layer: too many near-identical representations | **Still applies** | 5 trees; `TypedModule` and `NormalizedModule` are both `Module<DefId, TypeId>` — literally identical types. ~5,500 lines of tree infra (visitor/mapper/fold/format) only work on `Module<D,T>`, not on the semantic tree. |
| 2 | Monomorphization: full re-typecheck | **Still applies** | Same pattern: clone generic functions, rebuild context, re-run entire typechecker. No memo table. |
| 3 | Elaborate: kitchen-sink stage | **Still applies** | 4,668 lines, 6 responsibilities. Still the only stage that mutates `DefTable` and `TypeMap` (`elaborator.rs:258,286` and `value_for_loop.rs:207`). |
| 4 | Backend lowering: drop handling fragmentation | **Still applies** | 8,320 lines. Drop logic still spread across `drops.rs` (865 lines), `drop_glue.rs`, and `calls.rs` (1,068 lines). |
| 5 | Context accretion: 11+ fields | **Still applies** | `SemanticCheckedContext` now has 12 fields. Each new semantic fact adds another. |
| 6 | Compile driver coupling to AnalysisDb | **Resolved** ✅ | `compile_with_path()` now calls `core::api` directly. No `AnalysisDb` import in `src/driver/`. |
| 7 | Normalize: too thin to be a stage | **Still applies** | Still 260 lines. Still produces its own context type (`NormalizedContext`) that is structurally identical to `TypeCheckedContext`. |

## New Findings

### N1. Backend Correctness Around Calls — Highest Priority

The IR verifier (`backend/verify.rs`) checks structural invariants (block
uniqueness, value uniqueness, use-before-def, type consistency) but does **not**
verify ABI/calling-convention correctness: stack alignment at call sites,
caller-saved register preservation, argument passing conventions, or spill slot
correctness.

Linear-scan without coalescing or live-range splitting is fine for now, but
combined with the lack of ABI verification, this is the most likely source of
silent miscompiles as IR complexity grows.

**Recommendation (P0).** Add verifier checks specifically for call-site ABI
invariants. Add a "call torture test" suite: many arguments, nested calls, mixed
types, high register pressure. Run verification in CI. This is the
highest-leverage correctness investment.

### N2. Strict/Partial Semantic Drift Risk

The strict and partial paths sometimes call different functions
(`type_check_with_imported_facts` vs `type_check_partial_with_imported_facts`).
If these diverge beyond error tolerance, users get "works in IDE but fails on
build" bugs — the most trust-destroying class of compiler issues.

**Recommendation (P1).** Add golden tests: same source file through both
policies, asserting that partial produces a superset of strict's diagnostics,
never different semantics. Cheap insurance. Long-term, refactor toward a single
engine parameterized by policy.

### N3. Non-Interned Language Types

`Type` uses `Box<Type>` for recursion with structural equality. Every comparison
is a deep walk, every clone is a deep copy. No canonical representation for type
identity at the language level (the IR level has `IrTypeCache` interning, but
the frontend does not).

**Recommendation (P2).** Don't do full interning yet — the type representation
isn't stable and typestate will change it. Instead, replace `Box<Type>` with
`Arc<Type>` in recursive positions. This gives cheap cloning and
pointer-equality fast-path without the interning commitment. Migrate to full
`TypeId` interning after the type representation stabilizes.

### N4. Monomorphization Memo Table

The current full-program re-typecheck can be meaningfully improved with a simple
memo table — `(DefId, Vec<Type>) → TypeCheckedResult` — to avoid re-checking
identical instantiations. This is a ~50-line change, not a redesign.

**Recommendation (P2).** Add a memo table inside monomorphization to deduplicate
identical instantiations. Defer demand-driven instantiation until compile times
are measurably dominated by mono.

### N5. Deterministic Output

IR and assembly dumps should be deterministic — same input must produce
identical output. Stable iteration order (e.g., sorted keys where `HashMap`
iteration order leaks into output) and stable symbol/label naming are
prerequisites for reproducible debugging and diff-based regression detection.

**Recommendation (P1).** Enforce stable ordering and naming. Add a
"deterministic dump" test (same input ⇒ identical IR/asm).

### N6. Typestate — The Vision Needs a Prototype

The entire language vision centres on state machines and typestate, but there is
no surface syntax or lowering path yet. The existing machinery (parameter modes
like `sink`, semck, elaboration) is _well-suited_ for this — `sink self` already
encodes ownership transfer between states.

**Recommendation (P3).** Implement typestate as _desugaring_ first — a surface
feature that lowers to existing constructs (enums + match + generics + semck
transition rules). Keep it behind a feature flag. Iterate on examples until the
shape feels inevitable. Don't commit to a formal model until you've written 5+
real state machines with it.

## Prior Recommendations (Still Applicable)

The following recommendations from the prior review remain valid and are
incorporated here with updated priority assignments.

### P1. Tree Layer: Too Many Near-Identical Representations

There are 5 tree representations. The first 4 (`parsed`, `resolved`, `typed`,
`normalized`) are thin type aliases over a generic `Module<D, T>` model (946
lines). The 5th (`semantic`) is a completely separate structure (571 lines).
Supporting all this: ~5,500 lines of tree infrastructure
(visitor, visitor_mut, mapper, fold, format, format_compact) — all of which only
work on the generic `Module<D, T>` and don't apply to the semantic tree.

`TypedModule` and `NormalizedModule` are both `Module<DefId, TypeId>` — literally
the same type with different aliases. The normalize pass (260 lines) that
produces one from the other inserts coercion annotations but doesn't change the
tree's type parameters.

**Recommendation.** Collapse the first 4 trees to 2: `ParsedModule`
(identifiers are strings) and `ResolvedModule` (identifiers are `DefId`s). Types
and coercions become side-table annotations, not tree parameters. This eliminates
`TypedModule` and `NormalizedModule` as distinct types, simplifies the tree
infrastructure, and keeps the semantic tree as the one genuinely different
representation.

### P2. Elaborate: The "Kitchen Sink" Stage

Elaborate handles 6 distinct responsibilities: closure lifting (struct types +
invoke methods), call planning (argument passing modes), match planning (decision
trees), for-loop desugaring (→ while loops), string format planning, and
place/value separation (explicit loads/moves).

It is also the only stage that mutates `DefTable` and `TypeMap` (via direct
inserts in `elaborator.rs` and `value_for_loop.rs`), breaking the otherwise
clean immutability pattern.

**Recommendation.** Split into 2–3 passes:

- **Desugar** (for-loops, f-strings) — purely syntactic, operates on the
  normalized tree.
- **Closure conversion** (captures, type registration) — the only thing allowed
  to extend DefTable/TypeMap.
- **Place/value lowering** (explicit loads/moves, call planning, match planning)
  — produces the semantic tree.

### P3. Backend Lowering: Drop Handling Fragmentation

Backend lowering is 8,320 lines — the largest module. Drop-related logic is
spread across three files:

- `drops.rs` (865 lines) — drop scope management
- `drop_glue.rs` (195 lines) — generated drop functions
- `calls.rs` (1,068 lines) — drop flags at call sites

**Recommendation.** Introduce a `DropManager` that owns all drop scheduling. All
drop-related decisions (scope entry/exit, drop flags at calls, glue generation)
go through this single API. The rest of lowering calls `drop_mgr.enter_scope()`,
`drop_mgr.drop_value()`, etc.

### P4. Context Accretion: 12+ Fields by Elaborate

By the time you reach Elaborate, the input context (`SemanticCheckedContext`) has
12 fields that get destructured. Every new semantic fact adds another field,
touching context types, stage inputs/outputs, and builder methods.

**Recommendation.** Bundle related artifacts:

- `Tables { def_table, type_map, call_sigs, generic_insts, symbols, def_owners }`
  — shared compiler state.
- `SemFacts { implicit_moves, init_assigns, full_init_assigns, closure_captures }`
  — semantic analysis results.

Contexts become `(module, tables, sem_facts)` instead of 12 loose fields.

### P5. Normalize: Too Thin to Be a Stage

Normalize is 260 lines. Its sole job is inserting explicit array-to-slice
coercions at call sites. It produces a new tree type (`NormalizedModule`) and
context (`NormalizedContext`) that are structurally identical to their typed
equivalents.

**Recommendation.** Fold coercion insertion into the type checker's finalize
phase (as a side table of coercion sites), or fold it into the semantic check
stage as a pre-step. This eliminates one tree type and one context type.

## Consolidated Priority Table

| Pri | Item | Source | Impact | Effort |
|-----|------|--------|--------|--------|
| **P0** | ABI/call verification + call torture tests | New (N1) | Existential correctness | 1–2 days |
| **P1** | Strict/partial golden tests | New (N2) | Trust / bug prevention | 2–4 hours |
| **P1** | Deterministic IR/asm dumps | New (N5) | Debugging / reproducibility | 2–4 hours |
| **P1** | Context bundling (`Tables` + `SemFacts`) | Prior #5 (P4) | Maintainability | 1–2 days |
| **P2** | Normalize → fold into adjacent stage | Prior #7 (P5) | Removes dead weight | 1 day |
| **P2** | Monomorphization memo table | Prior #2 + New (N4) | Scaling | Half day |
| **P2** | `Box<Type>` → `Arc<Type>` in recursive positions | New (N3) | Performance headroom | 1 day |
| **P3** | Elaborate split (desugar / closure / place-value) | Prior #3 (P2) | Modularity | 2–3 days |
| **P3** | Drop handling consolidation (`DropManager`) | Prior #4 (P3) | Maintainability | 2–3 days |
| **P3** | Tree collapse (4 → 2 representations) | Prior #1 (P1) | Reduces ~5.5K infra lines | 3+ days |
| **P3** | Typestate prototype behind feature flag | New (N6) | Vision alignment | 1–2 weeks |
| **Defer** | Full type interning with `TypeId` arena | | Wait for type repr stability | |
| **Defer** | Demand-driven monomorphization | | Wait for scaling evidence | |
| **Defer** | Separate compilation artifacts | | Wait for module API stability | |
| **Defer** | Live-range splitting in regalloc | | Wait for spilling evidence | |

## Caveats

- **Tree collapse requires invariant discipline.** If you collapse trees, be
  explicit about which passes may mutate structure vs only add side tables. The
  current 4-tree system is _safe_ even if redundant — collapsing it incorrectly
  would be worse than the status quo.

- **Splitting Elaborate means choosing where "planning" lives.** The current
  approach (plans as semantic-tree artifacts) works. If the semantic tree keeps
  growing new plan types, consider pushing plans into lowering-time helpers
  rather than semantic artifacts, making the semantic tree a leaner MIR-like
  representation.

- **Normalize elimination requires a home for coercions.** Folding normalize
  into typecheck's finalize phase (as a side table of coercion sites) or into
  semcheck as a pre-step are both viable. The key constraint: coercions must be
  visible to semantic checks.

- **Don't refactor ahead of pain.** At 96K lines with a single developer, the
  biggest risk isn't technical debt — it's loss of momentum from
  over-engineering. Measure before refactoring (add compile-time/memory counters
  for the cloning, types, and mono paths). Protect the clean architecture with
  _tests and instrumentation_, not premature abstraction layers.

- **What compilers actually die from.** Not from uninterned types or cloned
  contexts. They die from (1) backend miscompiles that destroy user trust, (2)
  semantic divergence between IDE and build, and (3) chasing infrastructure
  instead of language features. Address those three, and the rest can evolve.
