# Modules and Visibility Design (V1)

## Goals

1. Keep everyday code simple and explicit.
2. Make module boundaries obvious.
3. Keep resolver/typechecker complexity low.
4. Support opaque abstractions cleanly.

## Module Model

1. One source file is one module.
2. Module path is file path relative to project root or `std/`.
3. No `mod` declarations in source.
4. Cyclic module dependencies are rejected.

Examples:

- `std/io.mc` -> module path `std.io`
- `app/config/parser.mc` -> module path `app.config.parser`

## Imports (`requires`)

At file top, an optional `requires` block declares module dependencies:

```mc
requires {
    std.io
    std.parse as parse
    std.parse.u64 as parse_u64
    app.config.loader as cfg
}
```

Rules:

1. Aliases use postfix `as`: `module.path as alias`.
2. If `as` is omitted, the default alias is the last path segment.
3. Bound aliases must be unique in file scope.
4. `requires` imports modules only in V1 (not individual symbols).
5. No glob imports in V1.
6. No grouped imports in V1 (`std.io.{stream, file}` is out of scope).
7. Unused `requires` entries may become warnings (later).
8. Resolver only sees names brought in via `requires` plus local scope.

Example usage:

```mc
requires {
    std.io
    std.parse as parse
}

fn load(path: string) -> u64 | IoError {
    let text = io.read_file(path)?;
    parse.parse_u64(text)
}
```

## Visibility (Item-Annotated)

Default visibility is module-private.

Attributes:

1. `@public` for externally visible symbols.
2. `@opaque` for public type identity with hidden representation.

Examples:

```mc
@public
fn load(path: string) -> Config | IoError { ... }

@public
type Config = {
    host: string,
    port: u16,
}

@opaque
type Buffer = {
    data: ^u8[],
    len: u64,
}
```

Semantics:

1. `@public type`:
   - Type name is visible outside module.
   - Struct literal construction and field access are allowed outside module.
2. `@opaque type`:
   - Type name is visible outside module.
   - No external struct literal construction.
   - No external direct field access.
   - Access only through `@public` methods/properties.

## Methods, Properties, Traits Visibility

1. Methods/properties default private even inside public/opaque types.
2. Mark member with `@public` to expose it outside module.
3. Trait definitions can be `@public` or private.
4. Trait impl blocks themselves are not exported objects; visibility is determined by trait/member visibility.
5. `@opaque` values must be constructed externally through `@public` factory/constructor APIs.

Example:

```mc
@opaque
type Point = { _x: f64, _y: f64 }

Point :: {
    @public fn new(x: f64, y: f64) -> Point { ... }

    @public
    prop x: f64 {
        get { self._x }
        set(v) { self._x = v; }
    }
}
```

## Field/Property Namespace Rule

1. Fields and properties share one namespace per type.
2. Name collisions are illegal (`PropertyConflictsWithField`).
3. Backing-field naming is convention only, not a language rule.

## Name Resolution Rules

1. Unqualified names resolve local scope first.
2. `requires` aliases are top-level module names in current file.
3. For `alias.name`, resolver first checks whether `alias` is a `requires` alias:
   - if yes, treat as module-qualified lookup;
   - otherwise, resolve as ordinary value/member access.
4. External access requires symbol visibility checks.
5. Accessing a private symbol from another module is a hard error.

## Diagnostics to Add or Standardize

1. `UnknownModule(path)`
2. `DuplicateRequireAlias(alias)`
3. `PrivateSymbolAccess(module, symbol)`
4. `OpaqueTypeConstruction(type_name)`
5. `OpaqueFieldAccess(type_name, field)`
6. `ModuleDependencyCycle(cycle_path)`

## Implementation Plan

1. Parser:
   - Add `requires` block AST.
   - Add `@public` / `@opaque` item attributes in AST where missing.
2. Context:
   - Build module graph from file paths.
   - Build program-level bindings (`frontend::bind`) that map each module's
     `requires` aliases to dependency export surfaces.
3. Resolver:
   - Consume flattened module plus program metadata.
   - Resolve module aliases after frontend rewrite.
   - Resolve item references across module boundaries.
4. Def metadata:
   - Store visibility and opacity flags.
   - Track top-level owner module ids as side metadata for future
     per-module checks/diagnostics.
5. Typecheck:
   - Enforce opacity/visibility access rules.
6. Diagnostics:
   - Emit errors listed above with spans on offending sites.
7. Examples/tests:
   - Positive and negative tests for cross-module access, opaque construction, and cycles.

## Out of Scope (V1)

1. Wildcard imports.
2. Grouped imports (`std.io.{stream, file}`).
3. Specific symbol imports from modules.
4. Re-export syntax.
5. Package manager/external dependencies.
6. Friend/internal visibility tiers.
