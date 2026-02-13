# Architecture Review

_Date: 2025-02-12_

The compiler is remarkably well-engineered for its stage of maturity — 70k LOC
of non-test Rust, only 3 TODOs in the entire codebase, clean phase boundaries,
immutable trees + side tables, and a well-decomposed type checker. That said,
there are several areas where architectural complexity is self-inflicted and
could be addressed.

## What's Working Well

- **Type checker design.** The 5-phase pipeline (collect → constrain → solve →
  validate → finalize) is clean and well-decomposed. Zero TODOs in the entire
  module. Separating constraint emission from solving is textbook compiler design
  and pays dividends for overload resolution and generics.

- **Immutable trees + side tables.** The core pattern of "trees are immutable,
  information accumulates in side tables keyed by NodeId" is sound and enables
  good separation of concerns across the pipeline.

- **SemCheck modularity.** 10 independent checkers that each return their own
  results, composed at the top level. Easy to add new checks without disturbing
  existing ones.

- **Optimization pipeline.** Clear tier structure (CFG cleanup → CFG-free →
  dataflow → module DCE) with fixpoint iteration. Easy to add new passes.

- **Overall code quality.** 70k LOC with only 3 TODOs and no HACK/FIXME markers
  is exceptional.

## Areas for Improvement

### 1. Tree Layer: Too Many Near-Identical Representations

**Situation.** There are 5 tree representations. The first 4 (`parsed`,
`resolved`, `typed`, `normalized`) are thin type aliases over a generic
`Module<D, T>` model (946 lines). The 5th (`semantic`) is a completely separate
structure (571 lines). Supporting all this: ~4,100 lines of tree infrastructure
(visitor, visitor_mut, mapper, fold, format, format_compact) — all of which only
work on the generic `Module<D, T>` and don't apply to the semantic tree.

**Problem.** Heavy infrastructure tax (visitor/mapper/fold/format) to support 4
trees that are structurally identical — they differ only in what `D` and `T`
are. `TypedModule` and `NormalizedModule` in particular add very little
structural novelty. Meanwhile, the semantic tree, which is genuinely different,
gets no benefit from this infrastructure and needs its own traversal code.

**Recommendation.** Collapse the first 4 trees down to 2: `ParsedModule`
(pre-resolution, identifiers are strings) and `ResolvedModule` (identifiers are
`DefId`s). Types and coercions become side-table annotations, not tree
parameters. This would eliminate `TypedModule` and `NormalizedModule` as distinct
types, simplify or halve the tree infrastructure, and keep the semantic tree as
the one genuinely different "MIR-like" representation.

### 2. Monomorphization: Full Re-Typecheck Is Architecturally Expensive

**Situation.** When generics are discovered during type checking, the entire
module is monomorphized and then re-typechecked from scratch:

```rust
// In compile_with_path():
if !typed_result.generic_insts.is_empty() {
    let monomorphized = monomorphize(resolved_context, &generic_insts);
    typed_result = batch::query_typecheck(..., monomorphized);  // full re-run
}
```

**Problem.** This re-runs type checking for the entire module, not just the
newly instantiated functions. As the language grows and programs get larger, this
will become a scaling bottleneck. The compiler does work, discovers it needs to
do more work, then redoes all the work.

**Recommendation.** Move toward per-item instantiation + per-item type checking.
The `AnalysisDb` query infrastructure already exists — leverage it so generic
instantiation triggers typecheck only for newly instantiated functions (and their
dependencies), not a full re-run. Minimal first step: at least skip re-checking
functions that weren't affected by monomorphization.

### 3. Elaborate: The "Kitchen Sink" Stage

**Situation.** Elaborate handles 6 distinct responsibilities:

1. Closure lifting (struct types + invoke methods)
2. Call planning (argument passing modes)
3. Match planning (decision trees)
4. For-loop desugaring (→ while loops)
5. String format planning
6. Place/value separation (explicit loads/moves)

It's also the only stage that mutates DefTable and TypeMap (via overlays),
breaking the otherwise clean immutability pattern.

**Problem.** These responsibilities have different concerns and different
"audiences." Desugaring (for-loops, f-strings) is syntactic; closure lifting is
semantic and extends the type system; place/value separation is a lowering
concern. Bundling them means any change to one aspect risks touching the others.

**Recommendation.** Split into 2–3 passes:

- **Desugar** (for-loops, f-strings) — purely syntactic, operates on the
  normalized tree.
- **Closure conversion** (captures, type registration) — the only thing allowed
  to extend DefTable/TypeMap.
- **Place/value lowering** (explicit loads/moves, call planning, match planning)
  — produces the semantic tree.

### 4. Backend Lowering: Drop Handling Fragmentation

**Situation.** Backend lowering is 8,320 lines — the largest module. Drop-related
logic is spread across three files:

- `drops.rs` (865 lines) — drop scope management
- `drop_glue.rs` (195 lines) — generated drop functions
- `calls.rs` (1,068 lines) — drop flags at call sites

**Problem.** Drop semantics are a cross-cutting concern that currently leaks
across module boundaries. Understanding "what happens when a value is dropped"
requires reading three files in different contexts.

**Recommendation.** Introduce a `DropManager` that owns all drop scheduling. All
drop-related decisions (scope entry/exit, drop flags at calls, glue generation)
go through this single API. The rest of lowering calls `drop_mgr.enter_scope()`,
`drop_mgr.drop_value()`, etc.

### 5. Context Accretion: 11+ Fields by Elaborate

**Situation.** By the time you reach Elaborate, the input context has 11 fields
that get destructured:

```rust
let ElaborateStageInput {
    module, def_table, def_owners, type_map, call_sigs,
    generic_insts, symbols, node_id_gen, implicit_moves,
    init_assigns, full_init_assigns, closure_captures,
} = ctx;
```

**Problem.** Every new semantic fact adds another field to the context chain.
Adding a new analysis (e.g., a new borrow check variant) means touching context
types, stage inputs/outputs, and all the builder methods.

**Recommendation.** Bundle related artifacts:

- `Tables { def_table, type_map, call_sigs, generic_insts, symbols, def_owners }`
  — shared compiler state.
- `SemFacts { implicit_moves, init_assigns, full_init_assigns, closure_captures }`
  — semantic analysis results.

Contexts become `(module, tables, sem_facts)` instead of 11 loose fields.

### 6. Services/Analysis Coupling with Core Compile Path

**Situation.** The main `compile_with_path()` creates an `AnalysisDb` and uses
`batch::query_parse_resolve_typecheck()` even for non-IDE compilation. This
means the compile path depends on the query/caching/cancellation infrastructure.

**Problem.** IDE concerns (partial execution, cancellation tokens, query
memoization) shouldn't be in the hot path of a simple `cargo mcc build`. Today
it works because `AnalysisDb` is lightweight, but it's an architectural smell —
the core compiler depends on a service layer.

**Recommendation.** The core compile path should call pure pipeline functions
directly. The `AnalysisDb` / batch layer should be a wrapper on top of those
pure functions. Extract the resolve+typecheck sequence into a direct function in
`core::api`, have `batch::query_*` call that, and have `compile_with_path` call
it directly.

### 7. Normalize: Too Thin to Be a Stage

**Situation.** Normalize is 260 lines. Its sole job is inserting explicit
array-to-slice coercions at call sites.

**Problem.** It produces a new tree type (`NormalizedModule`) and context
(`NormalizedContext`) for very little structural novelty.

**Recommendation.** Either fold coercion insertion into the type checker's
finalize phase (as a side table of coercion sites), or fold it into the semantic
check stage as a pre-step. This eliminates one tree type and one context type.

## Priority

| # | Area | Impact | Effort |
|---|------|--------|--------|
| 1 | Context bundling (Tables + SemFacts) | High | Small (1–2 days) |
| 2 | Compile driver decoupling from AnalysisDb | Medium | Small (hours) |
| 3 | Normalize → fold into adjacent stage | Medium | Small (1 day) |
| 4 | Elaborate split (desugar / closure / place-value) | High | Medium (2–3 days) |
| 5 | Drop handling consolidation | Medium | Medium (2–3 days) |
| 6 | Tree collapse (4 → 2 representations) | High | Large (3+ days) |
| 7 | Incremental monomorphization | Medium | Large (3+ days) |

## Caveats

- **Tree collapse requires invariant discipline.** If you collapse trees, be
  explicit about which passes are allowed to mutate structure vs only add side
  tables; otherwise later passes may depend on earlier structural edits in
  non-obvious ways.

- **Splitting Elaborate means choosing where "planning" lives.** Keep plans
  either purely lowering-local (preferred) or purely semantic-tree-local, but
  not both.

- **Incremental monomorphization will surface dependency-order issues.** You'll
  want a clear "instantiate → typecheck → (maybe instantiate more)" loop rather
  than a single re-run.

- **If the semantic tree keeps growing new "plan" types**, consider making it
  explicitly a MIR with a small number of orthogonal node kinds (places,
  rvalues, statements, terminators), and push all "plans" into lowering-time
  helpers rather than semantic artifacts.
