# Compiler Architecture

This document describes the architecture of the Machina compiler, from
high-level structure down to individual subsystems. It is intended as a
living reference — update it as the codebase evolves.

## High-Level Overview

Machina is a systems programming language with first-class linear types and
hosted machines. The compiler (`mcc`) is written in Rust, targets ARM64, and
ships with a C runtime library and an LSP-based language server.

```
┌──────────────────────────────────────────────────────────────────┐
│                        machina (Rust)                            │
│                                                                  │
│  src/core/         Compiler pipeline (parse → codegen)           │
│  src/driver/       CLI entry point & orchestration               │
│  src/services/     IDE analysis layer                            │
│                                                                  │
├──────────────────────────────────────────────────────────────────┤
│  tooling/lsp/      Language server (LSP over stdio)              │
│  tooling/vscode/   VS Code extension                             │
├──────────────────────────────────────────────────────────────────┤
│  runtime/          C runtime linked into compiled programs       │
│  std/              Standard library (prelude)                    │
└──────────────────────────────────────────────────────────────────┘
```

## Compilation Pipeline

The compiler is a linear, append-only pipeline. Each stage consumes the
output of the previous stage and produces immutable results for later stages.
No feedback loops exist between stages.

```
Source (.mc)
  │
  ▼
Parse ─────────────── Module (AST)
  │
  ▼
Linear Validate ───── LinearIndex + diagnostics
  │
  ▼
Linear Desugar ────── Module (AST rewritten: @linear type → enum + methods)
  │
  ▼
Resolve ───────────── DefTable, SymbolIds
  │
  ▼
Typecheck ─────────── TypeMap, CallSigMap, GenericInstMap
  │
  ▼
Monomorphize ──────── Specialized generic instances
  │
  ▼
Semcheck ──────────── ImplicitMoves, InitAssigns, ClosureCaptures
  │
  ▼
Elaborate ─────────── Plans (call, match, drop, lowering, linear machine)
  │
  ▼
NRVO Analysis ─────── Return-value optimization eligibility
  │
  ▼
Lower ─────────────── SSA IR (Functions, Blocks, Instructions)
  │
  ▼
Optimize ──────────── Simplified IR (CFG simplification, DCE, copy prop)
  │
  ▼
Register Alloc ────── Allocation map (Value → Register or Stack)
  │
  ▼
Codegen ───────────── ARM64 assembly
  │
  ▼
Link ──────────────── Executable (via system C compiler + runtime)
```

The batch driver (`src/driver/compile.rs`) orchestrates this pipeline. The
IDE path (`src/services/analysis/`) runs the same stages with a "partial"
policy that tolerates errors for best-effort results.

## Module Map

### `src/core/` — Compiler Pipeline

| Module | Purpose |
|---|---|
| `lexer.rs` | Tokenizer |
| `parse/` | Recursive-descent parser → `Module` AST |
| `ast/` | AST node definitions, visitors, formatting |
| `linear/` | `@linear type` validation, desugaring, machine surface generation |
| `resolve/` | Name binding → `DefTable` |
| `typecheck/` | Constraint-based type inference → `TypeMap` |
| `monomorphize/` | Generic function specialization |
| `semck/` | Move checking, init checking, pattern exhaustiveness, closures |
| `elaborate/` | Enrich AST with lowering plans (calls, matches, drops) |
| `nrvo.rs` | Named return value optimization analysis |
| `backend/lower/` | AST → SSA IR lowering |
| `backend/opt/` | IR optimization passes |
| `backend/regalloc/` | Register allocation (ARM64) |
| `backend/codegen/` | IR → ARM64 assembly emission |
| `backend/verify.rs` | IR well-formedness checker |
| `backend/analysis/` | CFG and liveness analysis |
| `ir/` | SSA IR data model, builder, layout, formatting |
| `types/` | `Type` enum, assignability, rendering |
| `plans/` | Plan data structures consumed by lowering |
| `context/` | Stage input/output type contracts |
| `api/` | Stable stage facade (strict vs. partial policies) |
| `capsule/` | Multi-module support (module graph, imports) |
| `machine/` | Shared machine helpers (runtime intrinsics, request sites) |
| `analysis/` | Generic dataflow framework and fact overlays |
| `diag.rs` | `Span`, `Position`, `CompileError` |
| `symbol_id.rs` | `SymbolIdTable` for IDE symbol identity |
| `codegen_names.rs` | DefId → output symbol name mapping |

### `src/driver/` — CLI & Orchestration

| File | Purpose |
|---|---|
| `compile.rs` | `compile_with_path()` — batch pipeline orchestration |
| `query.rs` | IDE-style queries from CLI (`--kind hover`, etc.) |

### `src/services/analysis/` — IDE Analysis

| Module | Purpose |
|---|---|
| `db.rs`, `db/` | `AnalysisDb` — query memoization and version tracking |
| `pipeline.rs` | Stage sequencing with fact overlays |
| `program_pipeline.rs` | Multi-file program-level analysis |
| `snapshot.rs` | Document version tracking |
| `lookups/` | Definition, hover, semantic tokens, signature help |
| `completion/` | Context-aware code completion |
| `diagnostics/` | Error reporting for IDE |
| `code_actions.rs` | Quick-fix suggestions |
| `rename.rs` | Rename refactoring |
| `syntax_index.rs` | AST span queries (node-at-position, call-site-at-span) |
| `results.rs` | IDE result types (HoverInfo, Location, etc.) |
| `query/` | Query request/response types |

### `tooling/`

| Directory | Purpose |
|---|---|
| `lsp/` | LSP server (Rust): message dispatch, handlers, session management |
| `vscode/` | VS Code extension (TypeScript): syntax highlighting, LSP client |

### `runtime/` — C Runtime

| File(s) | Purpose |
|---|---|
| `alloc.c` | Header-based memory allocation with tracking |
| `mem.c` | Memory operations (copy, zero, compare) |
| `string.c`, `print.c` | String manipulation and formatted output |
| `dyn_array.c` | Dynamic array operations |
| `hash_table.c`, `map_table.c`, `set.c` | Hash-based collections |
| `io.c` | File I/O |
| `conv.c` | Type conversions |
| `trap.c` | Panic/trap handling |
| `machine/` | Actor/machine runtime (mailbox, dispatch, descriptors) |

---

## Subsystem Details

### Parse (`src/core/parse/`, `src/core/lexer.rs`)

Recursive-descent parser. The lexer produces tokens; the parser builds a
`Module` containing top-level items (`FuncDef`, `TypeDef`, `TraitDef`,
`LinearTypeDef`, `MachineHostDef`, etc.).

Every AST node carries a unique `NodeId` (allocated by `NodeIdGen`) and a
`Span` (source location). `NodeId` is the universal key used by later stages
to attach type information, plans, and other metadata without mutating the
AST.

### AST (`src/core/ast/`)

Core types:

- `Module` — top-level container (requires, items)
- `Expr` / `ExprKind` — expressions (~40 variants)
- `StmtExpr` / `StmtExprKind` — statements
- `TypeExpr` / `TypeExprKind` — type syntax
- `BindPattern` / `BindPatternKind` — destructuring patterns

Infrastructure:

- `visit.rs` / `visit_mut.rs` — immutable and mutable visitor traits with
  default walk functions. Most analysis passes are implemented as visitors.
- `format.rs` — pretty-printing for `--dump ast`

### Linear Types (`src/core/linear/`)

Machina's distinguishing feature. An `@linear type` declaration combines
struct fields, enum states, transition rules, and role-based access control
into a single construct.

The linear subsystem runs **before name resolution** so the rest of the
compiler sees only ordinary structs, enums, and methods.

| File | Responsibility |
|---|---|
| `validate.rs` | Check declarations for well-formedness (unknown states, missing actions, etc.) |
| `index.rs` | Build `LinearIndex` — metadata about each linear type (states, actions, fields, roles) |
| `rewrite.rs` | Desugar `@linear type` → enum + struct + methods in the AST |
| `machine.rs` | Generate hosted-machine surface: spawn/create/resume helpers, descriptors |

This pre-resolve desugaring means ~90% of the compiler is generic — it has
no special knowledge of linear types.

### Resolve (`src/core/resolve/`)

Walks the AST to collect definitions and bind name references to `DefId`s.

Key types:

- `Def` — a named definition (kind, name, visibility, node ID)
- `DefId` — unique definition identifier
- `DefTable` — the name → `DefId` registry
- `SymbolIdTable` — stable symbol identity for IDE features

Multi-module support: the capsule system (`src/core/capsule/`) discovers
and parses dependent modules, then resolve runs with imported facts from
other modules.

### Typecheck (`src/core/typecheck/`)

Constraint-based bidirectional type inference.

**Flow:**

1. **Collect** (`collect.rs`) — scan type/function definitions, register
   signatures
2. **Constrain** (`constraints.rs`, `constraints/`) — walk AST bodies,
   emit type constraints
3. **Solve** (`solver/`) — iterative fixpoint: unification (Robinson
   algorithm) + obligation dispatch (9 handler modules)
4. **Finalize** (`finalize.rs`) — materialize `TypeMap` from solver state

**Output:** `TypeMap` (NodeId → Type), `CallSigMap` (call-site signatures),
`GenericInstMap` (generic instantiation records).

### Monomorphize (`src/core/monomorphize/`)

Specializes generic function definitions for each concrete set of type
arguments. Runs after typecheck, before semcheck. Currently follows C++
template semantics — only instantiated generics are validated.

### Semantic Checking (`src/core/semck/`)

Post-typing validation of runtime safety properties. Each check is an
independent AST walk:

| File | Check |
|---|---|
| `move_check.rs` | Use-after-move detection |
| `def_init.rs` | Definite initialization |
| `lvalue_overlap.rs` | Overlapping mutable borrows |
| `slice_borrow.rs` | Slice borrow conflicts |
| `slice_escape.rs` | Slice escape detection |
| `match_check.rs` | Pattern exhaustiveness |
| `structural.rs` | Enum/struct validation |
| `value.rs` | Constant evaluation |
| `closure/` | Closure capture analysis |

Output: `ImplicitMoves`, `InitAssigns`, `ClosureCaptures`.

### Elaborate (`src/core/elaborate/`)

Enriches the AST with side-table "plans" that tell the lowering stage *how*
to emit IR, separating semantic decisions from code generation.

| File | Plan type |
|---|---|
| `calls.rs` | Call dispatch strategy (intrinsic, direct, trait method) |
| `match_plan.rs` | Pattern-match decision trees |
| `drop_plan.rs` | Scope-based cleanup sequences |
| `lowering_plan.rs` | Linear vs. branching context classification |
| `linear_machine_plan.rs` | Machine state/event dispatch tables |
| `closure.rs` | Closure lifting to struct + invoke method |
| `index_plan.rs` | Array dimension metadata |

The main orchestrator is `elaborator.rs`, with expression-level logic split
across `value_expr.rs`, `value_block.rs`, `place.rs`, and `value.rs`.

### IR (`src/core/ir/`)

Explicit-memory SSA intermediate representation.

- `Function` → `Block` → `Instruction` + `Terminator`
- `ValueDef` — SSA value definitions
- `IrTypeKind` — simplified type system (9 variants vs. `Type`'s ~20)
- `builder.rs` — `FunctionBuilder` API for IR construction
- `layout.rs` — memory layout calculations (size, alignment)

### Lowering (`src/core/backend/lower/`)

Converts the elaborated AST into SSA IR. The `FuncLowerer` struct is the
main workhorse, maintaining SSA state, drop scopes, and block structure.

Split across ~13 files by domain: `linear.rs` (value expressions),
`branching.rs` (if/match/loops), `calls.rs` (call materialization),
`drops.rs` (cleanup), `bind.rs` (let bindings), `match.rs` (pattern
matching), `error.rs` (error unions), etc.

### Optimization (`src/core/backend/opt/`)

IR-level passes: CFG simplification, dead code elimination, copy
propagation. Currently minimal — designed to grow.

### Register Allocation (`src/core/backend/regalloc/`)

ARM64-specific graph-coloring register allocator. Uses liveness analysis
from `backend/analysis/` to build interference graphs.

### Code Generation (`src/core/backend/codegen/`)

Lowers IR to ARM64 assembly text.

1. **Traverse** (`traverse.rs`) — linearize IR blocks
2. **Moves** (`moves.rs`) — schedule register-move operations for conflicts
3. **Emit** (`arm64.rs`) — ARM64 instruction emission

### IDE Analysis (`src/services/analysis/`)

Query-based incremental analysis powering LSP features. The `AnalysisDb`
caches stage results per-file and invalidates on edits. The pipeline runs
the same compiler stages as batch mode but with a "partial" policy that
tolerates errors.

**Key design:** Fact overlay maps let the IDE attach synthetic facts (e.g.,
from incomplete edits) without mutating base compilation results.

Features: hover, go-to-definition, references, completion, signature help,
rename, document symbols, semantic tokens, code actions.

### Machine Runtime (`runtime/machine/`)

C-based actor runtime linked into compiled programs that use hosted
machines.

| File | Responsibility |
|---|---|
| `runtime.c` | Machine table, mailbox FIFO, scheduler step loop |
| `bridge.c` | Compiler ↔ runtime ABI boundary (`__mc_*` entry points) |
| `descriptor.c` | Binary descriptor parsing and dispatch-row lookup |
| `hosted_instance.c` | Instance lifecycle (create, bind, start, stop) |
| `emit.c` | Event emission and routing |
| `pending.c` | Pending request tracking |

The compiler emits binary descriptors encoding each machine's state/event
dispatch table. The runtime parses these at startup and uses them to route
messages to the correct handler thunks.

---

## Two Execution Modes

The compiler stages run in two modes via `src/core/api/`:

| | Batch (Strict) | IDE (Partial) |
|---|---|---|
| **Driver** | `src/driver/compile.rs` | `src/services/analysis/pipeline.rs` |
| **Error handling** | Fail-fast on first stage error | Best-effort, accumulate diagnostics |
| **Scope** | Full pipeline through codegen | Frontend stages only (parse → typecheck) |
| **Caching** | None | Per-file memoization in `AnalysisDb` |
| **Multi-module** | Capsule discovery + compose | Program-level imports via `program_pipeline.rs` |

Both modes share the same stage implementations — the difference is in
orchestration and error tolerance.
