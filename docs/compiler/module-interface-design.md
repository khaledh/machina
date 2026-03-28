# Module Interface and Separate Compilation -- Design

## Problem

Machina currently uses a flattened compilation model: all modules discovered
through `requires` are parsed, merged into one flat module, and compiled
together. The standard library is compiled from source on every user build.

This creates several problems:

- Every user build re-parses, re-resolves, and re-typechecks all imported stdlib
  code.
- Flattening into one namespace requires name-mangling workarounds to avoid
  symbol collisions (the `prelude_only_keep_sets` mechanism in `compose.rs`).
- The prelude system has three files (`prelude.mc`, `prelude_impl.mc`,
  `prelude_requires.mc`) with overlapping and unclear responsibilities.
- Runtime C source files are recompiled on every build.
- There is no path toward incremental compilation or user-defined libraries.

## Goal

Establish a compilation model where:

1. The **language surface** (prelude) is injected declarations, not a library.
2. The **runtime** is a separately built native archive.
3. **Stdlib modules** are separately compiled Machina libraries with interface
   metadata and object code.
4. The **user program** is the final compilation unit and owns generic
   monomorphization.

## Current Boundary Decision

For the current implementation phase:

- `.mci` is a **metadata artifact**, not a generic-body transport format.
- Generic monomorphization continues to use the **source file** containing the
  generic definition when the body is needed.
- Generic body serialization in `.mci` is explicitly **out of scope for now**.

Rationale:

- stdlib source already ships with the compiler and is already the source of
  truth for generic bodies.
- Serializing generic bodies into `.mci` would duplicate that source in a
  second compiler-owned IR before we have removed the source dependency.
- The metadata-only `.mci` path already pays for itself in resolve, tooling,
  and exported-surface queries without taking on that extra complexity.

Longer term, if Machina moves to source-independent compiled libraries, generic
body transport can be revisited deliberately. That is a future step, not part
of the current module-interface rollout.

## Current Transitional State

The implementation is now intentionally split into three active paths:

1. **Runtime archive**: native C support in `libmachina_rt.a`
2. **Stdlib archive**: concrete Machina stdlib code in `libmachina_std_<subset>.a`
3. **Source flattening**: remaining stdlib/user source still merged for strict
   frontend stages that need AST-level access

Today, the stdlib archive always includes `std::builtin`, and may also include
referenced concrete modules such as `std::parse`, `std::env`, and `std::io`.
At the same time, `.mci` already serves as the metadata boundary for exported
surface queries, resolve import facts, and tooling.

This is a deliberate transition state, not the final architecture. In
particular:

- `.mci` is already authoritative for metadata
- the stdlib archive already owns some concrete code
- source flattening still remains for generic bodies and a few semantic hooks

## Why Flattening Still Exists

Stdlib source flattening remains in the strict compile path for a small number
of real reasons:

- generic body access for source-backed monomorphization
- semantic/elaboration hooks that still inspect stdlib AST shape
- the current strict frontend still hands later stages one merged module

This means flattening is no longer the ownership mechanism for all stdlib code;
it is currently a frontend compatibility mechanism.

## Revisit Triggers

The strict frontend should only be reworked into a more capsule-aware,
less-flattened shape when there is a concrete driver, for example:

- stdlib or package growth makes source flattening measurably costly
- third-party libraries make the current model awkward to scale
- distribution goals require reducing source participation in normal builds
- generic-body transport becomes necessary for source-independent libraries

Until one of those triggers exists, the current split is considered acceptable:
keep improving artifact ownership and metadata boundaries, but do not force a
large strict-frontend refactor just for architectural tidiness.

## Artifact Model

A compiled Machina module produces two artifacts:

### `.mci` -- Module Compiled Interface

Long-term target: contains everything a consumer needs to typecheck and
monomorphize against the module without access to its source.

Current phase: `.mci` carries metadata only and does **not** carry generic
bodies.

Long-term contents:

- Public type definitions (structs, enums, traits, linear types)
- Public function/method signatures
- Public trait property declarations
- Generic function/method bodies in resolved, typed IR form
- Reachable dependency closure for exported generic bodies (see below)
- Export metadata: `SymbolId` for each exported definition
- Link symbol names for non-generic exported functions/methods

### `.o` -- Object Code

Contains compiled machine code for all non-generic public and private functions.
Generic functions produce no object code in the library -- they are instantiated
by the consumer.

### Example

```
std/io.mc     -->  std/io.mci   +  std/io.o
std/iter.mc   -->  std/iter.mci +  std/iter.o  (may be nearly empty -- mostly generics)
std/parse.mc  -->  std/parse.mci + std/parse.o
```

## Three-Layer Build Model

### Layer 1: Language Surface (Prelude)

The prelude contains declarations that are part of the language itself:

- Runtime FFI declarations (`@runtime fn __rt_print(...)`)
- Intrinsic declarations (`@intrinsic fn type_of<T>()`)
- Built-in types (`IterDone`, etc.)
- Always-available functions (`print`, `println`)

The prelude is:

- Declarations only -- no implementations
- Compiler-injected -- no `requires` needed by user code
- A single file (`std/prelude.mc`, renamed from `prelude.mc`)

The prelude is NOT a module. It is not separately compiled. It is merged into
every compilation unit by the compiler frontend, the same way C compilers
implicitly declare `__builtin_*` functions.

`prelude_requires.mc` is eliminated. If `print` and `println` are always
available, they belong in the prelude declarations, not in an auto-imported
module.

`prelude_impl.mc` is eliminated as a separate concept. Its Machina
implementations become a normal stdlib module (`std::prelude_support` or
similar) that is compiled to `.mci` + `.o` like any other module.

Always-available names like `print` and `println` are declared in the prelude
and resolved to fixed `SymbolId`s. Their implementations live in
`std::prelude_support` (or directly in the C runtime). The prelude declaration
establishes the name in every compilation unit; the link symbol resolves to the
implementation at link time. They are language-surface names with
library-backed implementations -- not compiler builtins, and not auto-imported
library functions.

### Layer 2: Runtime

The native support library, written in C:

```
runtime/*.c  -->  libmachina_rt.a
```

Built once. Linked into every program. Not rebuilt on user builds unless the
runtime source changes.

### Layer 3: Stdlib Modules

Standard library modules written in Machina:

```
std/io.mc      -->  std/io.mci + std/io.o
std/iter.mc    -->  std/iter.mci + std/iter.o
std/parse.mc   -->  std/parse.mci + std/parse.o
std/env.mc     -->  std/env.mci + std/env.o
std/format/csv.mc --> std/format/csv.mci + std/format/csv.o
```

These can be bundled into a single archive:

```
libmachina_std.a   (all stdlib .o files)
std/*.mci          (interface files, read by the compiler)
```

### Link Command

The final link for a user program:

```
cc -o program  user.o  libmachina_std.a  libmachina_rt.a
```

## Module Interface Format (`.mci`)

### Design Principles

- **Post-resolve, typed IR -- not raw AST.** The consumer performs no name
  resolution and no type inference against interface contents. The only
  operations the consumer performs on interface IR are: type parameter
  substitution (monomorphization), typecheck of instantiated bodies, and
  lowering to backend IR. If the consumer needs to re-resolve or re-infer, the
  interface format is wrong.
- Keyed by `SymbolId` (from `symbol-identity-design.md`), not by `DefId`.
- Versioned per compiler version. Not stable across compiler releases. The
  stdlib `.mci` files are rebuilt when the compiler is rebuilt.
- Self-contained for its exported surface: a consumer should never need the
  module's source to compile against it.

### Contents

```
ModuleInterface {
    // Module identity
    module_path: ModulePath,
    compiler_version: String,

    // Exported definitions
    exports: Vec<ExportedDef>,

    // Hidden definitions reachable from exported generic bodies
    // (see "Reachable Dependency Closure" below)
    closure_defs: Vec<ClosureDef>,
}
```

#### Exported Definitions

Each exported definition carries:

```
ExportedDef {
    symbol_id: SymbolId,
    visibility: Public | Opaque,
    kind: ExportedDefKind,
}

ExportedDefKind =
    | TypeDef { fields, variants, ... }
    | TraitDef { methods, properties }
    | FuncDef { signature, link_symbol, body: Option<TypedIR> }
    | MethodDef { parent_type, signature, link_symbol, body: Option<TypedIR> }
    | PropertyDef { parent_type, accessors }
```

- Non-generic functions/methods: `body` is `None`, `link_symbol` is present.
  The consumer emits extern calls to the link symbol.
- Generic functions/methods: `body` is `Some(TypedIR)`, `link_symbol` is absent.
  The consumer instantiates from the body.

#### Opaque Types

For `@opaque` types, the `.mci` export section includes:

- The type name and public method/property signatures
- NOT the internal field layout

The consumer can call methods and access properties but cannot construct or
destructure the type.

Note: if an exported generic body's reachable closure requires a private or
opaque type's internal layout (e.g., to construct or destructure it during
instantiation), that layout appears in `closure_defs` as a `CompileTime`
dependency. This does not change the public API contract -- closure contents are
compiler-internal material, not part of the module's public surface. A consumer
cannot use closure-included layouts to construct or inspect opaque types in
user code; they exist only to support instantiation of generic bodies that were
written inside the module.

## Reachable Dependency Closure

### The Problem

A public generic function may depend on private definitions:

```mc
// std/iter.mc

fn private_helper(x: u64) -> u64 { x + 1 }

type PrivateState<T> = { value: T, count: u64 }

@public
fn transform<S, T>(source: S, f: fn(T) -> T) -> TransformIter<S, T> {
    // body references private_helper and PrivateState
    let state = PrivateState { value: ..., count: 0 };
    ...
}
```

When the consumer monomorphizes `transform<MyIter, u64>`, the compiler needs
access to `private_helper` and `PrivateState<T>`. These are not exported, but
they are reachable from an exported generic body.

### The Rule

For every exported generic body included in a `.mci`, the interface must also
include the **reachable compilation closure** -- all definitions transitively
referenced by that body that the consumer needs to instantiate it.

### Two Categories of Reachable Dependencies

**Compile-time dependencies** -- needed by the consumer to typecheck,
monomorphize, and lower the instantiated body:

- Private generic function/method bodies
- Private type definitions (needed for layout and typecheck)
- Private trait/method signatures
- Private method implementations used by trait-driven dispatch

These are included in `closure_defs` with their full typed IR.

**Link-time dependencies** -- concrete (non-generic) private functions that the
instantiated body calls, but that are already compiled into the module's `.o`:

- Private non-generic helper functions
- Private non-generic methods

These are included in `closure_defs` with:

- Signature (for typecheck)
- `SymbolId` (for identity)
- Link symbol name (so the consumer can emit an extern call)
- NO body (the implementation lives in `stdlib.o`)

### Closure Def Format

```
ClosureDef {
    symbol_id: SymbolId,     // internal identity, not publicly exported
    kind: ClosureDefKind,
}

ClosureDefKind =
    | CompileTime {
        // Consumer needs the full body to instantiate
        def: TypeDef | FuncDef | MethodDef | TraitDef
    }
    | LinkTime {
        // Consumer only needs signature + link symbol
        signature: FuncSignature,
        link_symbol: String,
    }
```

### Closure Computation

The interface builder computes the closure by:

1. Starting from each exported definition that carries a body (generic
   functions/methods).
2. Walking the typed IR body, collecting all referenced `SymbolId`s.
3. For each referenced definition that is not itself exported:
   - If it is generic or a type definition: include as `CompileTime`
   - If it is a concrete function/method: include as `LinkTime`
4. Recurse into newly added `CompileTime` bodies.
5. Deduplicate by `SymbolId`.

This walk is done by symbol identity (`SymbolId`/`DefId` in the producing
module), not by name -- ensuring robustness against renaming or shadowing.

### Policy: Private Helpers in Generic Bodies

Public generic bodies are allowed to depend on arbitrary private definitions.
The closure mechanism makes this safe for consumers. Restricting this
artificially would force awkward stdlib style just to satisfy packaging rules.

### Invariant: Link-Time Closure Defs Must Survive DCE

Any definition included in `closure_defs` as `LinkTime` must be treated as
**externally reachable** during library-side dead code elimination and object
emission. If the library compiler optimizes away a private concrete helper that
an exported generic body depends on, the consumer will emit an extern call to a
symbol that does not exist in the library's `.o`.

The interface emitter and the codegen/linker must agree: if a symbol appears as
a `LinkTime` closure def, its compiled body survives into the archive.

## Tooling Use of `.mci`

### Two Consumers, One Artifact

The `.mci` format serves two consumers:

- **Compiler** -- reads types, signatures, and generic bodies for compilation.
- **LSP/tooling** -- reads types, signatures, doc comments, and source
  locations for editor intelligence.

Both consume the same file. The compiler ignores tooling metadata; the LSP
ignores generic body IR. No separate artifact needed.

### Boundary: Interface vs Live Pipeline

`.mci` does not replace the compiler pipeline for tooling. It complements it:

- **Active/edited source modules** -- the LSP uses the live analysis pipeline
  (parse, resolve, typecheck) for diagnostics, semantic tokens, and
  incremental re-analysis. This is the only way to give correct feedback on
  code the user is currently editing.
- **Imported compiled modules** (stdlib, libraries) -- the LSP reads `.mci`
  for all metadata about these modules. No parsing, no re-analysis.

This gives the best of both: live correctness for the file being edited, fast
metadata lookup for everything it depends on.

### What `.mci` Supports for Tooling

| Operation | Source | Sufficient from `.mci`? |
|---|---|---|
| Hover (type, signature, docs) | Exported signatures + doc comments | Yes |
| Completion | Exported names, kinds, signatures | Yes |
| Signature help | Parameter names, types, param docs | Yes |
| Go-to-definition | Source file path + definition span | Yes (opens source if available) |
| Outline/symbol list | Exported definitions | Yes |
| Find references | All use sites across modules | No -- whole-program |
| Rename | All use sites + definition site | No -- whole-program |
| Diagnostics for edited code | Live analysis | No -- pipeline only |

### Tooling Metadata in `.mci`

Each `ExportedDef` carries optional tooling metadata alongside its compilation
metadata:

```
ExportedDef {
    symbol_id: SymbolId,
    visibility: Public | Opaque,
    kind: ExportedDefKind,

    // Tooling metadata (ignored by compilation)
    doc: Option<String>,
    source_location: Option<SourceLocation>,
}

SourceLocation {
    file: PathBuf,
    name_span: Span,       // span of the definition name, for precise jumps
    decl_span: Span,       // span of the full declaration, for context
}
```

Design choices:

- **Compilation metadata is required; tooling metadata is optional.** A
  minimal `.mci` with no docs and no source locations is still a valid
  compilation artifact. Tooling metadata improves the editor experience but
  is never needed for correctness. This keeps the format resilient -- a
  stripped `.mci` still compiles; a full one gives better tooling.
- **Source locations are best-effort.** They may be absent (e.g., a
  third-party library that ships `.mci` + `.o` without source) or stale
  (source modified after the interface was compiled). Tooling should degrade
  gracefully: show the signature from `.mci` if the source span cannot be
  resolved. Source locations are advisory, not authoritative.
- **Doc comments** are stored as raw text (markdown/plain as authored). No
  pre-rendering. Tooling renders as needed. Start with a single `doc` string;
  structured sections (`@param`, `@return`, examples) can be added later.
- **Source locations** include both the definition name span (for precise
  go-to-definition jumps) and the full declaration span (for contextual
  display). If the source file is available, the LSP opens it. If not (future:
  third-party compiled libraries), the LSP shows a synthetic read-only view
  built from the signature and doc comment.
- **Parameter names** are already part of function/method signatures. They are
  compilation metadata, but they also make signature help and hover much
  better. No duplication needed.
- **Display-quality type parameter names** (e.g., `T`, `S`, `In`, `Out`) are
  preserved in the signature IR. The compiler could survive without them, but
  tooling needs them to render human-readable signatures.

### Future: Doc Generation

A doc generation tool can read all `.mci` files and produce API documentation
from exported definitions + doc comments without running the compiler. This
falls out naturally from the format carrying docs and signatures.

## Symbol and Link Name Ownership

### Who Emits What

| Symbol kind | Object file | Named by |
|---|---|---|
| Runtime C functions | `libmachina_rt.a` | C names (`__rt_*`, `__mc_machine_*`) |
| Non-generic stdlib functions | `libmachina_std.a` | Module-qualified link name |
| Non-generic stdlib methods | `libmachina_std.a` | Type + method link name |
| Private concrete helpers | `libmachina_std.a` | Internal link name (in closure as `LinkTime`) |
| Generic instantiations | `user.o` | Module path + concrete type encoding |
| User functions | `user.o` | Module-qualified link name |

### Link Name Derivation

Link names are derived from `SymbolId`, not computed ad hoc:

- `SymbolId { module: std::io, path: println, ns: Value }` -->
  `__mc_std_io_println`
- `SymbolId { module: std::iter, path: MapIter.next, ns: Method }` -->
  `__mc_std_iter_MapIter_next` (generic -- only instantiated forms are emitted)
- Monomorphized instance: `__mc_std_iter_map__MyIter_u64_string`

The `CodegenNameTable` (from `symbol-identity-design.md`) remains the single
place where `SymbolId` is translated to emitted link labels.

### Generic Deduplication

For now, Machina performs whole-program monomorphization in a single pass. If
two user modules both use `map<Foo, u64, string>`, the monomorphizer
deduplicates naturally.

When independent per-module compilation is introduced later, monomorphized
generics should be emitted as weak/COMDAT symbols so the linker deduplicates
them. This is deferred -- not needed for the stdlib separation milestone.

## Import Resolution Changes

### Current: Source Flattening

```
user.mc  --requires--> std/io.mc
         parser reads std/io.mc from disk
         capsule flattener merges into one module
         single-module resolve/typecheck/codegen
```

### Target: Interface-Based

```
user.mc  --requires--> std/io.mci  (read interface metadata)
         resolve against exported SymbolIds and signatures
         typecheck against exported type definitions
         monomorphize generics from interface IR bodies
         codegen emits extern calls to link symbols
         linker resolves against libmachina_std.a
```

The capsule loader and resolver need to support two module source kinds:

- **Source module**: parsed from `.mc`, used for user code and during stdlib
  build
- **Interface module**: loaded from `.mci`, used when consuming a compiled
  library

The consumer reconstructs a semantically equivalent imported-module
representation from the `.mci` contents. This is not byte-identical to the
source-backed in-memory shape -- interface-loaded defs will not have source
`NodeId`s, original spans, or source-local ownership structure. But the
representation is equivalent for the purposes of resolve, typecheck, and
monomorphization. The difference between source and interface loading is in
how the module enters the compiler, not in how downstream passes consume it.

## Migration Plan

### Phase 1: Mechanical Cleanup (no design risk)

1. **Build runtime as `libmachina_rt.a`.** Compile the 17 C files once, cache
   the archive. Only rebuild when source changes.

2. **Collapse prelude to one file.** Merge `prelude.mc` and
   `prelude_requires.mc` into a single `prelude.mc` containing all
   declarations. Move `print`/`println` declarations into the prelude directly.

3. **Cache `prelude_impl.o`.** Only recompile when source changes (timestamp or
   content hash).

### Phase 2: Module Interface Format

1. **Define the `.mci` binary format.** Serialized typed IR + export metadata +
   closure defs. Use the compiler's internal IR representation -- no need for a
   separate stable format yet.

2. **Build the interface emitter.** Given a compiled module, walk exports,
   compute reachable closures, serialize to `.mci`.

3. **Build the interface loader.** Read `.mci`, reconstruct typed IR and
   export tables into the same internal representation the resolver/typechecker
   expects.

### Phase 3: Stdlib Compilation

1. **Compile each stdlib module to `.mci` + `.o`.** Build as a separate step
   before user compilation.

2. **Bundle stdlib objects into `libmachina_std.a`.** Keep `.mci` files
   alongside for compiler consumption.

3. **Modify the capsule loader.** When a `requires { std::... }` path resolves
   to a module with a `.mci` file, load the interface instead of parsing
   source.

4. **Remove source flattening for stdlib.** The flattening path in
   `compose.rs` is no longer used for stdlib modules. Keep it temporarily for
   user multi-module compilation until Phase 4.

### Phase 4: User Libraries (Future)

1. Extend `.mci` + `.o` compilation to user-defined library modules.
2. Support incremental builds: recompile only modules whose source or
   dependencies changed.
3. Introduce weak/COMDAT symbols for independently compiled generic
   instantiations.

## What This Does Not Cover

- Stable ABI across compiler versions
- Package management or external library distribution
- Dynamic linking
- Cross-compilation (currently ARM64 only)
- Generic instantiation deduplication across independently compiled modules
  (deferred to Phase 4)

## Relationship to Existing Design Documents

- **`symbol-identity-design.md`**: The `.mci` format is keyed by `SymbolId`.
  Link names are derived through the `CodegenNameTable` model. The symbol
  identity model is a prerequisite for this design.
- **`modules-visibility.md`**: `@public` and `@opaque` visibility determine
  what appears in the export section of `.mci`. `@opaque` types export
  signatures but not field layouts.
- **Capsule model** (`src/core/capsule/`): The capsule loader gains a new
  module source kind (interface) alongside existing source parsing. The
  dependency graph and `requires` resolution remain the same.
