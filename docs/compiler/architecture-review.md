# Compiler Architecture Review

An analysis of the Machina compiler architecture covering code metrics,
structural patterns, and refactoring opportunities. The review covers
production code (~77K lines Rust), test code (~31K lines), a C runtime
(~5.9K lines), and IDE tooling (~12K lines).

## Table of Contents

- [Codebase Metrics](#codebase-metrics)
- [Pipeline Overview](#pipeline-overview)
- [10K-Foot Assessment](#10k-foot-assessment)
- [Area 1: AST and Semantic Tree Representations](#area-1-ast-and-semantic-tree-representations)
- [Area 2: Backend Lowering (backend/lower/)](#area-2-backend-lowering-backendlower)
- [Area 3: Visitor Infrastructure and Pass Repetition](#area-3-visitor-infrastructure-and-pass-repetition)
- [Area 4: Type Checker Internals (typecheck/)](#area-4-type-checker-internals-typecheck)
- [Area 5: IR Type Representation](#area-5-ir-type-representation)
- [Area 6: IDE/LSP Integration (services/, tooling/, core/api/)](#area-6-ideslsp-integration)
- [Area 7: C Runtime (runtime/)](#area-7-c-runtime)
- [Area 8: Multi-Module Support (capsule/)](#area-8-multi-module-support-capsule)
- [Summary of Recommendations](#summary-of-recommendations)
- [Strategic Observations](#strategic-observations)

---

## Codebase Metrics

| Language       | Files | Blank  | Comment | Code    |
|----------------|-------|--------|---------|---------|
| **Rust**       | 373   | 12,008 | 3,796   | 110,281 |
| Markdown       | 44    | 2,721  | 0       | 7,900   |
| C              | 31    | 775    | 508     | 5,906   |
| C/C++ Header   | 10    | 170    | 277     | 780     |
| TypeScript     | 1     | 50     | 8       | 427     |
| Other          | 112   | 721    | 28      | 4,159   |
| **Total**      | **571** | **16,445** | **4,617** | **129,453** |

### Rust Code Split

| Category                         | Files | Code   | % of Rust |
|----------------------------------|-------|--------|-----------|
| Production (`src/` excl. tests)  | 279   | 76,523 | 71%       |
| Tests (`tests/` + `src/tests/`)  | 88    | 31,155 | 29%       |

Test-to-production ratio: **0.41** (1 line of test per 2.5 lines of production).

### Largest Production Modules

| Module       | Files | ~LoC   | Purpose                                      |
|--------------|-------|--------|----------------------------------------------|
| backend/     | 68    | 14,500 | SSA lowering, optimization, ARM64 codegen    |
| typecheck/   | 42    | 13,200 | Type inference, unification, constraint solver|
| semck/       | 22    | 6,100  | Move/borrow checking, pattern exhaustiveness |
| elaborate/   | 18    | 6,300  | Semantic tree construction, plan extraction   |
| tree/        | 17    | 5,500  | AST definition + semantic tree + visitors     |
| typestate/   | 11    | 3,500  | Machine/typestate protocol generation         |
| parse/       | 10    | 3,900  | Lexer and recursive descent parser            |
| resolve/     | 7     | 3,100  | Name resolution, definition table             |
| services/    | 40+   | 8,600  | IDE analysis engine                           |

---

## Pipeline Overview

```
Source → Lex → Parse → Resolve → Typecheck → Monomorphize
  → SemCheck → Elaborate → NRVO → Lower → Optimize → CodeGen → Link
```

Each stage produces append-only output consumed by later stages:

| Stage        | Key Output                                          |
|--------------|-----------------------------------------------------|
| Parse        | AST (Module)                                        |
| Resolve      | DefTable, symbol tables                             |
| Typecheck    | TypeMap (NodeId→Type), CallSigMap, GenericInstMap    |
| Monomorphize | Specialized function clones with concrete types     |
| SemCheck     | ImplicitMoves, InitAssigns, ClosureCaptures         |
| Elaborate    | Semantic tree + CallPlans, MatchPlans, DropPlans    |
| NRVO         | Return-value optimization eligibility flags         |
| Lower        | SSA IR (Functions, Blocks, Instructions)            |
| Optimize     | CFG simplification, DCE, copy propagation           |
| CodeGen      | ARM64 assembly                                      |

---

## 10K-Foot Assessment

**The overall architecture is sound.** The stage ordering is well-justified,
data flow between stages is clean and append-only, and the separation between
checking passes and transformation passes is correct.

**The typestate desugaring strategy is the best design decision in the
compiler.** By lowering typestates to ordinary structs + methods before resolve
runs, ~90% of the compiler has zero knowledge of typestates. Direct-mode
typestates become zero-cost abstractions. Managed-mode machines generate
dispatch metadata consumed by the C runtime. The compiler-runtime boundary is
clean.

**No fundamental rethinking of the pipeline is needed.** The opportunities
identified below are implementation-level simplifications within a sound
architecture.

Two things to watch going forward:

1. **Monomorphization placement.** Currently happens after typecheck but before
   semcheck. Generic code is only validated when instantiated (C++ template
   model). If Machina adds trait-bounded generics checked in the abstract
   (Rust model), monomorphization would need to move after semantic checking,
   and the type checker would need to verify generic bodies against trait
   bounds.

2. **Linear pipeline vs. demand-driven.** The IDE layer adds caching, but the
   underlying model is "rerun the pipeline." A query-based architecture (like
   Salsa) would compute only what's needed for a specific IDE query. The
   current clean stage separation makes a future migration feasible.

---

## Area 1: AST and Semantic Tree Representations

### Current Design

The compiler maintains three expression representations:

1. **AST** (`tree/model.rs`, 1,114 lines): Monolithic `ExprKind` enum with
   ~40 variants mixing places and values.
2. **Semantic tree** (`tree/semantic/model.rs`, 600 lines): Split into
   `PlaceExpr` and `ValueExpr` with explicit Load/Move nodes, desugared
   control flow, and lifted closures.
3. **SSA IR** (`ir/model.rs`, 538 lines): Explicit-memory SSA with blocks,
   instructions, and terminators.

The elaboration pass rebuilds the entire AST into the semantic tree while also
computing six plan types stored in side tables:

| Plan Type              | Purpose                                    |
|------------------------|--------------------------------------------|
| LoweringPlan           | Linear vs. branching classification        |
| CallPlan               | Call dispatch strategy, arg passing modes   |
| MatchPlan              | Pattern-matching decision trees            |
| IndexPlan              | Array/slice dimension info                 |
| DropPlan               | Scope-based cleanup sequences              |
| MachineDescriptorPlan  | Typestate dispatch and state/event mapping |

### Analysis

The semantic tree is structurally very similar to the AST. The main differences
are:

- Place/value annotation (which expressions are lvalues vs. rvalues)
- Explicit Load/Move/ImplicitMove wrapper nodes
- Desugared for-loops (→ while), defer/using (→ cleanup sequences)
- Lifted closures (→ struct types with invoke methods)

Meanwhile, plans are already stored in side tables keyed by NodeId. The
lowerer walks the semantic tree and consults these side tables.

Additional duplication:
- `StmtExprKind` is nearly identical between AST and semantic tree.
- `BindPatternKind` is nearly identical (semantic adds `def_id`, `field_index`).
- `CallArg` exists in both AST (struct with mode) and semantic (enum with
  variants) forms.
- Operator enums differ in naming (`LtEq` vs. `Le`) and splitting (AST
  `BinaryOp` vs. IR `BinOp` + `CmpOp`).
- Every AST node embeds `pub span: Span` (~75 occurrences); a `NodeId → Span`
  side table would reduce memory and struct sizes.

### Recommendation

**Eliminate the semantic tree as a separate data structure.** Instead:

- Annotate the AST in-place (or via side tables) with place/value
  classification.
- Perform desugaring as a rewriting pre-pass on the AST itself.
- Have the lowerer walk the AST directly, consulting plan side tables.

This would eliminate `tree/semantic/model.rs`, the tree-rewriting half of the
elaborate pass, and the duplicated enums between AST and semantic tree.

**Estimated impact: 4,000-6,000 lines removed.**

---

## Area 2: Backend Lowering (backend/lower/)

### Current Design

The lowering module is the largest in the compiler at ~10,600 lines across
24 files. It converts the semantic tree to SSA IR via `FuncLowerer`, a struct
with 13 fields and 130+ methods spread across 6 major files:

| File           | Lines | Purpose                                    |
|----------------|-------|--------------------------------------------|
| linear.rs      | 1,409 | Direct value expression lowering           |
| calls.rs       | 1,091 | Call resolution, argument materialization   |
| drops.rs       | 1,049 | Drop scope tracking, liveness management   |
| util.rs        | 1,018 | Type coercion, bounds checks, view helpers |
| branching.rs   | 673   | Multi-block lowering (if, match, loops)    |
| lowerer.rs     | 596   | Core state, initialization, finalization   |

Plus 18 smaller files (slots, globals, bind, proj, join, match, etc.).

### Analysis

**~50% of the code is plumbing.** Three patterns repeat throughout:

1. **Control-flow splitting** (~15 lines each occurrence): Create then/else/join
   blocks, emit CondBr, lower branches, emit branch-to-join with args, restore
   local snapshots. Appears 10+ times.

2. **Aggregate materialization** (~8 lines each): Allocate slot, store fields,
   load result. Used for structs, tuples, enums, error unions, coercions.

3. **Runtime guards** (~12 lines each): Emit condition, branch to trap block or
   continue block. Used for bounds checks, division-by-zero, nonzero refinement,
   range checks. Appears 10+ times with slight variations.

`FuncLowerer` is a god object. Every method touches shared state
(builder, locals, drop_manager, type_lowerer, loop_stack). There is no clear
separation of concerns within the struct.

Six files are under 100 lines (`slots.rs` 51, `globals.rs` 52,
`machine_layout.rs` 51, `proj.rs` 88, `bind.rs` 94, `mapping.rs` 32) and
contribute one or two methods each, creating unnecessary file fragmentation.

### Recommendation

1. **Extract an IR builder abstraction** that hides the repetitive patterns:
   - `emit_branch(cond) -> (ThenBuilder, ElseBuilder, JoinBuilder)` — hides
     block creation, terminators, join threading.
   - `materialize_aggregate(ty, fields) -> ValueId` — hides alloc/store/load.
   - `emit_guard(cond, trap_kind, trap_args)` — replaces 10 near-identical
     runtime-check functions.

2. **Consolidate small files** by concern: all place-related lowering into one
   file, all binding/mapping into another.

3. **Modularize FuncLowerer** by extracting sub-contexts (e.g., `IrContext`
   for builder + type_lowerer, `LoweringState` for locals + drops + loops) to
   make dependencies explicit.

**Estimated impact: 2,000-3,000 lines removed.**

---

## Area 3: Visitor Infrastructure and Pass Repetition

### Current Design

The compiler defines two visitor traits (`Visitor` and `VisitorMut`) in
`tree/visit.rs` (879 lines) and `tree/visit_mut.rs` (867 lines). These
provide ~50 method pairs for traversing AST nodes.

27 files implement one of these traits, creating 15+ independent tree-walking
passes. Many passes in `semck/` do related analyses:

| Pass                | File             | Lines | Analysis                      |
|---------------------|------------------|-------|-------------------------------|
| Definite init       | def_init.rs      | 1,303 | Variable initialization       |
| Slice borrow        | slice_borrow.rs  | 660   | Borrow conflict detection     |
| Move checking       | move_check.rs    | 555   | Use-after-move                |
| Structural check    | structural.rs    | 524   | Enum/struct validation        |
| Value checking      | value.rs         | 474   | Constant evaluation           |
| Normalization       | normalize.rs     | 468   | Coercion insertion            |

Each pass independently walks the entire expression tree with its own
`visit_expr` containing a large match on `ExprKind` variants. Each has its own
context struct, error collection, and scope management.

### Analysis

- `visit.rs` and `visit_mut.rs` are 1,746 lines of nearly identical code.
  The mutable variant differs only in `&` vs. `&mut` references.

- The dataflow passes (`def_init`, `move_check`, `slice_borrow`) share
  identical patterns: scope tracking, snapshot/restore of state at branches,
  CFG-like walking. They could share a common dataflow framework.

- 15+ independent traversals of the same expression tree means the AST is
  walked at least 15 times during compilation.

### Recommendation

1. **Generate visitor traits from a single source** via a declarative macro,
   eliminating ~900 lines of duplication between `visit.rs` and `visit_mut.rs`.

2. **Merge related dataflow passes** (`def_init`, `move_check`,
   `slice_borrow`) into a single framework with pluggable analysis lattices.
   These passes share identical scope-tracking, CFG-walking, and
   snapshot/restore patterns.

**Estimated impact: 2,000-3,000 lines removed.**

---

## Area 4: Type Checker Internals (typecheck/)

### Current Design

The type checker is 13,176 lines across 42 files, organized into five phases:

| Phase      | Lines | Purpose                                        |
|------------|-------|------------------------------------------------|
| Collect    | 1,002 | Scan module for type/function definitions       |
| Constrain  | 2,136 | Walk AST bodies, emit type constraints          |
| Solve      | 2,379 | Iteratively solve constraints, discharge obligations |
| Validate   | 1,318 | Protocol shape conformance, reply cap tracking  |
| Finalize   | 3,361 | Materialize TypeMap, CallSigMap from solver     |

The constraint-based approach is **genuinely necessary** for Machina. Protocol/
typestate features require tracking constraints across function boundaries,
generic instantiation needs deferred resolution, and control flow creates
branches with different type assumptions.

### Analysis

**Two unification systems.** `unify.rs` (323 lines) provides core Robinson
unification. `infer_unify.rs` (512 lines) appears to be a higher-level wrapper
that may be redundant — the solver uses `TcUnifier` from `unify.rs` directly.

**Duplicate type-structure walking.** `type_map.rs`'s
`resolve_type_expr_with_params_and_args()` and `finalize.rs`'s
`match_template_type()` both recursively walk all type constructors (Struct,
Enum, Tuple, Array, Fn, etc.) for different purposes — resolution vs. pattern
matching. The structural traversal logic is duplicated.

**Obligation dispatch fragmentation.** Nine solver sub-modules (`index.rs`,
`collections.rs`, `control.rs`, `nominal.rs`, `expr_ops.rs`, `patterns.rs`,
`calls.rs`, `assignability.rs`, `joins.rs`) each implement their own error
handling and type normalization. A trait-based single dispatch could reduce
this by ~30%.

**Finalize scope creep.** `finalize.rs` (1,146 lines) does substitution
application, call signature resolution, AND nominal key inference (400 lines of
structural template matching). The latter should be a separate pass or folded
into constraint collection.

**Protocol validation coupling.** `validate/protocol.rs` (765 lines) and reply
capability tracking (382 lines) are language-specific semantic rules, not core
type checking. They depend on type information but don't contribute to type
inference.

### Recommendation

1. **Determine if `infer_unify.rs` is dead code** — delete it or make it the
   sole unification interface.

2. **Extract a shared `StructuralTypeWalker` trait** to eliminate duplicate
   type-structure traversal between `type_map.rs` and `finalize.rs`.

3. **Consolidate obligation dispatch** using a trait-based checker with single
   dispatch point, reducing per-module error handling duplication.

4. **Split finalize.rs**: Move nominal key inference to a separate pass or
   into constraint collection. Keep finalize focused on substitution
   application and output materialization.

5. **Move protocol validation out of typecheck** into a standalone analysis
   pass alongside `semck/`, aligning with the pattern where semantic checks
   happen post-typing.

**Estimated impact: 2,000-3,000 lines removed.**

---

## Area 5: IR Type Representation

### Current Design

The IR has its own type representation (`IrTypeKind` with 9 variants in
`ir/types.rs`) that is a strict subset of the semantic `Type`. A dedicated
translation module (`backend/lower/types.rs`, 599 lines) converts between
them.

```
Type (20 variants) → IrTypeKind (9 variants)
  Int(signed, bits)    Int(signed, bits)     -- identical
  Bool                 Bool                  -- identical
  Unit                 Unit                  -- identical
  Struct(...)          Struct(...)           -- simplified
  ...                  Blob(size, align)     -- catch-all for unresolved
```

### Analysis

Since Machina has a single frontend and single backend, there is no need for a
separate IR type system. The translation layer exists only to strip information
that the backend doesn't use, but this stripping provides no benefit and costs
599 lines of conversion code plus the `IrTypeKind` definition.

### Recommendation

**Use `Type` directly in the IR** (or a thin wrapper adding layout info).
Eliminate `IrTypeKind` and the translation layer.

**Estimated impact: 800-1,200 lines removed.**

---

## Area 6: IDE/LSP Integration

*Files: `src/services/`, `tooling/lsp/`, `src/core/api/`*

### Current Design

The IDE integration spans ~12,000 lines across three layers:

```
LSP Server (tooling/lsp/, 2,858 lines)
  → AnalysisSession (document lifecycle, version guards)
    → AnalysisDb (services/analysis/, 8,659 lines)
      → QueryRuntime (memoization, dependency tracking, cancellation)
        → Pipeline (stage sequencing)
          → Core API (src/core/api/, 625 lines)
            → Compiler stages
```

The QueryRuntime provides incremental caching with dependency tracking. The
`FrontendPolicy` enum cleanly models batch (Strict) vs. IDE (Partial) modes.

### Analysis

**The architecture is well-designed.** Each layer has a clear reason to exist.
The query-based analysis with overlay support and version guards is correct.

Minor issues:

- **API surface explosion.** Many stage functions have 3 variants
  (`resolve_stage`, `resolve_stage_with_imports`, `resolve_stage_partial`).
  The 13 public functions could be reduced to ~5 using `Options` structs.

- **Query entry point duplication.** Every lookup has `_at_path`, `_at_file`,
  and `_at_program_file` wrappers (~28 methods). A `QueryScope` enum would
  eliminate ~18 wrappers.

- **The `src/core/api/` layer is thin.** It mostly delegates to stage
  implementations with a `FrontendPolicy` dispatch. Could be folded into the
  stages themselves.

### Recommendation

1. Collapse stage function variants into single functions with `Options`
   structs.
2. Introduce a `QueryScope` enum parameter to unify `_at_path`/`_at_file`/
   `_at_program_file` entry points.
3. Consider folding the API facade into the stage implementations.

**Impact: Minor cleanup, not a priority. No architectural changes needed.**

---

## Area 7: C Runtime

*Files: `runtime/` (~5,900 lines C)*

### Current Design

The runtime has two layers:

- **Core runtime** (~2,400 lines): Memory allocation with header-based
  tracking, string operations, dynamic arrays, hash table kernel (shared by
  maps and sets), file I/O, formatting, trap handling.

- **Machine runtime** (~3,500 lines): Actor-like machine lifecycle, mailbox
  system, single-envelope dispatch, request/reply correlation, descriptor
  parsing, thunk registry.

Every public function has a dual entry point: `__mc_*` (internal, structured
types) and `__rt_*` (public, raw u64 parameters). This is an intentional ABI
boundary between compiler-generated code and the runtime.

### Analysis

**The runtime is clean and well-scoped.** The compiler-runtime division of
labor is correct: the runtime handles what must be dynamic (allocation, hashing,
scheduling), the compiler generates the rest.

Minor duplication:
- `mc_bytes_eq()` and `mc_copy_bytes()` are defined identically in 3 files
  (`hash_table.c`, `map_table.c`, `set.c`).
- `mc_next_cap()` (capacity doubling) is identical in `dyn_array.c` and
  `string.c`.
- `runtime/machine/runtime.c` at ~1,500 lines is the largest file and could
  be split into mailbox, dispatch, and slot management.

### Recommendation

1. Extract `mc_bytes_eq`, `mc_copy_bytes`, `mc_next_cap` to a shared
   `util.h` header.
2. Consider splitting `machine/runtime.c` by concern for maintainability.

**Impact: ~50 lines saved, improved maintainability. No architectural changes
needed.**

---

## Area 8: Multi-Module Support (capsule/)

*Files: `src/core/capsule/`, `src/core/resolve/`, `src/core/context/capsule.rs`*

### Current Design

Multi-module compilation uses a flattening approach:

1. **Discover** all modules reachable from entry via `requires` statements
   (BFS, cycle detection).
2. **Flatten** all modules into a single `Module` by rewriting qualified names,
   validating visibility, and mangling conflicting symbols.
3. **Compile** the flattened module through the standard single-file pipeline.

### Analysis

**The system works correctly** but carries accidental complexity from three
parallel import representations:

1. **Capsule layer**: `RequireSpec` with module paths and member names.
2. **Resolver layer**: `ImportedModule`, `ImportedSymbol`,
   `ImportedCallableSig` — reconstructed signature types that duplicate what
   the `Type` system already knows.
3. **Context layer**: `ModuleExportFacts`, `ImportEnv`,
   `ImportedSymbolBinding` with `GlobalDefId` references.

These represent the same information in different encodings with conversion
logic between them.

Additional issues:
- `resolve_program()` in `resolver.rs` (lines 2054-2156) implements a
  module-aware resolution approach that is **never called**. Appears to be an
  abandoned alternative to flattening.
- Flattening loses module ownership information, requiring a
  `top_level_owners: HashMap<NodeId, ModuleId>` hack threaded through later
  phases.

### Recommendation

1. **Unify import representations around `GlobalDefId`.** When module A imports
   `foo` from module B, store B's `DefId` for `foo`. Let the type checker look
   up the actual signature from the DefTable. This eliminates
   `ImportedCallableSig`, `ImportedSymbol`, `ImportedFacts`, and the conversion
   logic between all three representations.

2. **Delete the unused `resolve_program()` function.**

**Estimated impact: 500-800 lines removed, meaningful reduction in conceptual
complexity.**

---

## Summary of Recommendations

| #  | Area                            | Change                                          | Lines Saved   | Priority |
|----|---------------------------------|-------------------------------------------------|---------------|----------|
| 1  | AST / Semantic tree             | Eliminate semantic tree; lower from annotated AST | 4,000-6,000 | High     |
| 2  | Backend lowering                | IR builder abstraction; consolidate god object    | 2,000-3,000 | High     |
| 3  | Visitor infrastructure          | Generate visitors; merge dataflow passes          | 2,000-3,000 | Medium   |
| 4  | Type checker                    | Deduplicate unification, type walking, dispatch   | 2,000-3,000 | Medium   |
| 5  | IR types                        | Eliminate IrTypeKind translation layer             | 800-1,200   | Medium   |
| 6  | Multi-module                    | Unify import representations around DefId         | 500-800     | Low      |
| 7  | IDE/LSP                         | Collapse API variants, unify query entry points   | ~200        | Low      |
| 8  | C Runtime                       | Extract duplicated utilities to shared header     | ~50         | Low      |

**Total estimated reduction: 12,000-17,000 lines from ~77K production Rust
(15-22%).**

Items 1 and 2 are the highest-impact changes and can be done independently.

---

## Strategic Observations

### What Should Not Change

- **Pipeline stage ordering** — justified, well-ordered, clean data flow.
- **Typestate desugaring strategy** — the best design decision in the compiler;
  keeps ~90% of the compiler generic.
- **Plan-based elaboration** — good separation between deciding how to lower
  and emitting IR.
- **Ownership model** — parameter modes (in/inout/out/sink) are the right
  abstraction.
- **Compiler-runtime boundary** — clean and appropriately scoped.

### What to Watch

- **Generics model.** If Machina adds trait-bounded generics checked in the
  abstract, the monomorphization placement (after typecheck, before semcheck)
  will need to change.
- **IDE scalability.** If programs grow large, the linear pipeline will
  benefit from a demand-driven (Salsa-style) architecture. The current clean
  stage separation makes this migration feasible.
