# Symbol Identity Design

## Summary

Machina currently uses two different notions of definition identity:

- `DefId`: a local numeric identifier unique only within one module `DefTable`
- `GlobalDefId`: `{ module_id, def_id }`, a program-unique wrapper around a local `DefId`

This has been sufficient for basic resolve/typecheck/codegen plumbing, but it is
starting to show strain in cross-module tooling and overloaded-call resolution.
A concrete example is language-server hover on imported overloaded functions:
we can know that a call resolved successfully, but still lack a direct,
canonical identity for the exact source definition that was selected.

This document proposes a long-term symbol identity model for Machina based on a
canonical, path-based global symbol identifier, while keeping numeric local ids
as internal storage keys where they are still useful.

## Implementation Status

Current status as of March 1, 2026:

- Phase 1 is complete: `SymbolTable` was renamed to `CodegenNameTable`.
- Phase 2 is largely complete: `SymbolId`/`SymbolIdTable` exist and loaded module contexts can map local defs to canonical symbol ids.
- Phase 3 is substantially implemented: selected callable facts now prefer canonical symbol ids, including overloaded imported call sites.
- Phase 4 is substantially implemented: import/export facts now carry symbol-aware exported entries and canonical symbol ids for callable/type/trait payloads.
- Phase 5 is substantially implemented for tooling lookup: hover, signature help, definition, references, and rename now flow through `DefTarget` and canonical symbol lookup helpers.
- Phase 6 is underway: `GlobalDefId` has been removed from selected-call handling and most active tooling/import lookup paths, but it still remains in provenance-oriented export/import metadata and a few bridge APIs.

The remaining high-value work is mostly consolidation:

- make `SymbolId -> source definition/render` the single obvious tooling API
- attach doc comments through canonical symbol lookup
- continue shrinking `GlobalDefId` to provenance/internal bridging only, or decide deliberately to keep it as the explicit provenance type

## Goals

- Define one long-term stable semantic identifier for definitions.
- Make cross-module tooling queries resolve to the actual source definition.
- Support overloaded callable identity cleanly.
- Keep source-level symbol paths human-readable.
- Preserve efficient local indexing inside per-module compiler tables.
- Clarify the distinction between semantic identity and codegen/link names.

## Non-Goals

- Do not redesign the entire resolver/typechecker in one step.
- Do not remove `DefId` immediately from all compiler internals.
- Do not define persistent on-disk symbol ids for external package registries yet.
- Do not solve generic monomorphized-instance identity here; this document is
  about source definitions.

## Current State

### Local definition identity

`src/core/resolve/def.rs` defines:

```rust
pub struct DefId(pub u32);

pub struct GlobalDefId {
    pub module_id: ModuleId,
    pub def_id: DefId,
}
```

This gives Machina:

- cheap local lookup via `DefId`
- a globally unique program-level id via `GlobalDefId`

But `GlobalDefId` is still:

- opaque
- tied to allocation order inside a module-local `DefTable`
- not naturally human-readable
- not suitable as the primary identity we want to surface in tooling

### Module identity

`src/core/capsule/mod.rs` defines:

- `ModuleId(pub u32)`
- `ModulePath { segments: Vec<String> }`

`ModulePath` is already the source-level namespace path we want to build on,
for example:

- `std::io`
- `app::net::client`

### Definition and use tracking today

`src/core/resolve/def_table.rs` tracks:

- `node_id -> DefId` for both defs and uses
- `DefId -> node_id`
- `DefId -> span/location`

So the resolver already answers the question:

- “which local definition does this syntax node refer to?”

For overloaded calls, however, that is not enough, because the final selected
callee is only known after type checking.

### Current “SymbolTable” is not a semantic symbol table

`src/core/symtab.rs` currently defines:

```rust
pub struct SymbolTable {
    pub def_names: HashMap<DefId, String>,
}
```

This is used primarily by:

- backend codegen
- IR formatting
- driver compile output
- generated closure/lifted symbol naming

Its real job is assigning emitted names such as:

- unsuffixed names for non-overloaded callables
- `$0`, `$1`, ... suffixes for overloaded codegen labels
- explicit `link_name` overrides

So despite the name, this is really a codegen/link-name table, not a canonical
semantic symbol table.

## Problem Statement

Machina needs a single, canonical symbol identity model that can answer:

- What exact definition does this use refer to?
- Which source module owns it?
- What source-level path should tooling display?
- For overloaded functions, which overload was selected?

Today, local numeric ids solve only part of this.

The missing piece is especially visible for imported overloaded call sites:

- resolve identifies an imported alias / overload set
- typecheck chooses one overload for a specific call site
- analysis/tooling still has to recover the actual source definition

That recovery should not require heuristic re-matching in the long term.

## Proposed Model

Introduce a canonical semantic identifier:

```rust
pub struct SymbolId {
    pub module: ModulePath,
    pub path: SymbolPath,
    pub ns: SymbolNs,
    pub disambiguator: Option<SymbolDisambiguator>,
}
```

### Path

```rust
pub struct SymbolPath {
    pub segments: Vec<SymbolSegment>,
}

pub struct SymbolSegment {
    pub name: String,
}
```

This path is source-level and human-readable. Examples:

- `std::io::println`
- `std::io::ReadFile::text`
- `app::auth::AuthServer`
- `app::auth::AuthServer::Ready`
- `app::auth::Auth::Server`

### Namespace

Different symbol spaces need explicit distinction.

```rust
pub enum SymbolNs {
    Value,
    Type,
    Trait,
    Variant,
    Field,
    Method,
    Property,
    State,
    Role,
    Handler,
}
```

This keeps identity explicit even when paths are visually similar.

Examples:

- `std::io::ReadFile` is `Type`
- `std::io::ReadFile::text` is `Method`
- `Color::Red` is `Variant`
- `Connection::Connected` is `State`

### Overload disambiguator

For non-overloaded symbols, path + namespace is enough.
For overloaded callables, we need one extra piece.

```rust
pub enum SymbolDisambiguator {
    Callable(CallableSigKey),
}
```

```rust
pub struct CallableSigKey {
    pub type_param_count: u16,
    pub self_mode: Option<CallableSelfKey>,
    pub params: Vec<ParamKey>,
}

pub enum CallableSelfKey {
    In,
    InOut,
    Sink,
}

pub struct ParamKey {
    pub mode: ParamMode,
    pub ty: TypeKey,
}
```

The key idea is:

- overload identity is based on the declared source signature
- generic parameter names are normalized away
- the identity stays semantic, not tied to local numeric ids

Examples:

- `std::io::println#fn(string)`
- `std::io::println#fn(u64)`
- `std::io::println#fn()`

The `#fn(...)` portion is conceptual/internal. Tooling can render a friendlier
form while still using the canonical identity.

### Type key for callable signatures

A callable disambiguator needs a stable way to talk about parameter types.
That implies a normalized `TypeKey`.

```rust
pub enum TypeKey {
    Unit,
    Bool,
    Char,
    Int { signed: bool, bits: u8, nonzero: bool },
    String,
    Named { module: ModulePath, path: SymbolPath, args: Vec<TypeKey> },
    Tuple(Vec<TypeKey>),
    Array { elem: Box<TypeKey>, dims: Vec<usize> },
    Slice(Box<TypeKey>),
    DynArray(Box<TypeKey>),
    Heap(Box<TypeKey>),
    Set(Box<TypeKey>),
    Map { key: Box<TypeKey>, value: Box<TypeKey> },
    Fn { params: Vec<ParamKey>, ret: Box<TypeKey> },
    GenericParam(u32),
    ErrorUnion { ok: Box<TypeKey>, errs: Vec<TypeKey> },
}
```

This is not meant to replace `Type` immediately.
It is a normalized identity/rendering key for symbol identities.

## Human-Facing Names

Tooling should expose both:

- the canonical source path
- the actual source-level signature for callables

Examples:

- path: `std::io::println`
- signature: `fn println(s: string) -> ()`

This matters because hover and documentation should come from the real source
definition, not from reconstructed call-site types.

## Relationship to Existing IDs

### `DefId`

Keep `DefId` as an internal per-module storage key.

Use it for:

- indexing local `DefTable` rows
- compact module-local maps
- internal traversal and builder code

Do **not** treat it as the long-term semantic identity.

### `GlobalDefId`

Treat `GlobalDefId` as a transitional compiler-internal bridge.
It remains useful while migrating from local numeric ids to canonical symbol ids.

Long term, most cross-module and tooling-facing APIs should prefer `SymbolId`
over `GlobalDefId`.

### `SymbolId`

Make `SymbolId` the canonical semantic identity for:

- selected overloaded call targets
- imported definition facts
- hover/definition/signature-help cross-module lookup
- references and rename planning
- documentation attachment

## Definition Ownership and Lookup

A definition should support these relationships:

- local table row: `DefId`
- canonical identity: `SymbolId`
- source location: file path + span
- optional codegen name: backend/link label

So a loaded module should be able to answer:

- `DefId -> SymbolId`
- `SymbolId -> DefId` (within that loaded module)

This suggests extending the definition metadata surface rather than replacing
`DefTable` storage all at once.

## Source Signature Rendering

Hover/signature help should render callable signatures from the owning source
callable definition, not from reconstructed call-site types.

Desired flow:

1. A call site resolves to a selected callable `SymbolId`.
2. That `SymbolId` identifies the owning module and exact source definition.
3. Tooling loads that module state.
4. Signature rendering walks the source callable AST node and its resolved/typed
   metadata.
5. Later, documentation comments attach naturally to that same definition.

This avoids:

- fake parameter names like `arg0`
- wrong overload display for imported call sites
- duplication between hover and future docs support

## Overloaded Call Resolution

Resolver and typechecker have different responsibilities:

- resolver: identifies candidate symbols / overload sets
- typechecker: chooses the concrete callable for a specific call site

The end result of type checking should therefore include:

```rust
call_node_id -> SelectedCallable
```

During migration, this should use an explicit transitional enum rather than a
struct with multiple optional identities:

```rust
pub enum SelectedCallable {
    Local {
        def_id: DefId,
    },
    Global {
        global_def_id: GlobalDefId,
    },
    Canonical {
        symbol_id: SymbolId,
    },
}
```

This keeps migration states explicit and avoids invalid combinations such as
"both local and canonical are missing" or "all three are present".

The desired end state is simpler:

```rust
pub struct SelectedCallable {
    pub symbol_id: SymbolId,
}
```

Any local `DefId` or transitional `GlobalDefId` lookups should then live in
separate caches or mapping tables, not inside the selected-call semantic fact
itself.

In the long term, tooling should not need to re-match selected overloads by
parameter types once this fact exists.

## Imported Definitions

Imported symbols currently carry a mix of:

- local synthetic alias defs
- source `GlobalDefId`s
- reconstructed imported callable/type/trait payloads

That should evolve toward:

- alias/local binding for syntax and scope management
- canonical source identity via `SymbolId`
- optional local cached payloads for performance

This keeps imported aliases and source definitions distinct:

- alias def: local scope object
- symbol id: actual source definition identity

## Codegen Name Table

The existing `SymbolTable` should be renamed because it is not a semantic symbol
system.

Recommended rename:

- `CodegenNameTable`

Rationale:

- it maps local defs to emitted labels / link names
- it handles overload label suffixing
- it is backend-specific
- it should remain separate from semantic symbol identity

This separation should be explicit:

- `SymbolId` answers “what definition is this?”
- `CodegenNameTable` answers “what symbol label should backend/codegen emit?”

## Proposed Rust Surface

### New semantic identity types

```rust
pub struct SymbolId {
    pub module: ModulePath,
    pub path: SymbolPath,
    pub ns: SymbolNs,
    pub disambiguator: Option<SymbolDisambiguator>,
}

pub struct SymbolPath {
    pub segments: Vec<SymbolSegment>,
}

pub struct SymbolSegment {
    pub name: String,
}

pub enum SymbolNs {
    Value,
    Type,
    Trait,
    Variant,
    Field,
    Method,
    Property,
    State,
    Role,
    Handler,
}

pub enum SymbolDisambiguator {
    Callable(CallableSigKey),
}
```

### Transitional selection facts

```rust
pub enum SelectedCallable {
    Local {
        def_id: DefId,
    },
    Global {
        global_def_id: GlobalDefId,
    },
    Canonical {
        symbol_id: SymbolId,
    },
}
```

### Final selection fact

```rust
pub struct SelectedCallable {
    pub symbol_id: SymbolId,
}
```

### Definition metadata extension

Add the ability for loaded module contexts / def tables to answer:

- `lookup_symbol_id(def_id) -> Option<&SymbolId>`
- `lookup_local_def_id(symbol_id) -> Option<DefId>`

## Migration Plan

### Phase 1: Naming cleanup

1. Rename `SymbolTable` to `CodegenNameTable`.
2. Update comments and stage payload names accordingly.
3. Keep behavior unchanged.

### Phase 2: Introduce `SymbolId`

1. Add `SymbolId`, `SymbolNs`, and callable disambiguator types.
2. Teach resolver/capsule code to derive module-qualified symbol paths.
3. Build `DefId -> SymbolId` mapping for source definitions.

### Phase 3: Selected callable identity

1. Extend selected-call facts to include `GlobalDefId` and then `SymbolId`.
2. Prefer that fact in hover/signature/definition lookups.
3. Remove analysis-side overload-target re-matching.

### Phase 4: Import fact convergence

1. Extend import facts to carry canonical `SymbolId`s for source defs.
2. Keep local alias defs only for scope binding.
3. Gradually shift analysis/typecheck lookups to canonical source identities.

### Phase 5: Tooling and docs

Status: substantially implemented for hover, signature help, definition,
references, and rename. Documentation comments are still pending.

1. Make hover/signature help render from `SymbolId` target definitions.
2. Attach documentation comments by `SymbolId`.
3. Use `SymbolId` in references/rename planning.

### Phase 6: Reduce `GlobalDefId` surface

Status: underway. `GlobalDefId` no longer drives selected-call identity or most
analysis/tooling lookup paths. Its remaining role is mainly provenance in
export/import metadata plus a handful of bridge APIs.

1. Keep `GlobalDefId` only where it still helps as a compact bridge.
2. Move public/compiler-service APIs to `SymbolId`.
3. Reassess whether `GlobalDefId` remains worthwhile at all.

## Design Constraints

- `SymbolId` must describe source definitions, not monomorphized clones.
- Local generated/internal defs may need a separate naming policy.
- The semantic identifier must remain stable under innocent local `DefId`
  renumbering.
- The source path representation must align with module/capsule naming.
- Overload disambiguation must not depend on codegen label suffixes.

## Open Questions

### 1. Do we need `Field` in `SymbolNs` if fields are not first-class defs?

Probably yes for tooling/display consistency, but this can stay provisional.

### 2. Should linear type handlers/states/roles use the same path model?

Yes. They are source definitions and should participate in the same scheme.
They may live in specialized namespaces such as `State`, `Role`, and `Handler`.

### 3. Do we want `ModulePath` display to use `.` or `::`?

User-facing symbol rendering should use `::`.
`ModulePath` can keep its current internal representation and display policy if
needed, but tooling should prefer the symbol-style path rendering.

### 4. Are methods/properties separate namespaces from values?

Long term, yes. That keeps symbol identity explicit and avoids subtle collisions.

## Recommendation

Adopt this long-term direction:

- `DefId`: local storage key
- `GlobalDefId`: transitional bridge
- `SymbolId`: canonical semantic identity
- `CodegenNameTable`: backend naming only

This gives Machina a clean path to:

- correct imported-overload tooling
- source-based hover/docs/signature rendering
- less identity translation across resolve/typecheck/analysis
- clearer architecture between semantic symbols and emitted labels
