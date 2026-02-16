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

- `std/io.mc` -> module path `std::io`
- `app/config/parser.mc` -> module path `app::config::parser`

## Imports (`requires`)

At file top, an optional `requires` block declares module dependencies:

```mc
requires {
    std::io
    std::io::println
    std::parse as parse
    app::config::loader as cfg
}
```

Rules:

1. Aliases use postfix `as`: `module::path as alias`.
2. If `as` is omitted, the default alias is the last path segment.
3. Bound aliases must be unique in file scope.
4. `requires` supports module imports (`std::io`) and symbol imports (`std::io::println`).
5. No glob imports in V1.
6. No grouped imports in V1 (`std::io::{stream, file}` is out of scope).
7. Unused `requires` entries may become warnings (later).
8. Resolver only sees names brought in via `requires` plus local scope.
9. `::` is the only namespace separator. `.` is field/property access only.

Example usage:

```mc
requires {
    std::io::println
    std::io as io
    std::parse as parse
}

fn load(path: string) -> u64 | IoError {
    println("loading");
    let text = io::read_file(path)?;
    parse::parse_u64(text)
}
```

Symbol import notes:

1. `module::symbol` imports one exported top-level symbol.
2. Symbol-import aliasing (`module::symbol as alias`) is currently rejected.
3. Symbol imports still add a dependency edge on the owning module.

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
    data: u8[]^,
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
3. `alias::name` is module-qualified lookup through a module-import alias.
4. Symbol imports introduce unqualified names directly.
5. External access requires symbol visibility checks.
6. Accessing a private symbol from another module is a hard error.

## Diagnostics (Current V1 Surface)

1. `UnknownModule(path)` (frontend discovery/loader)
2. `DuplicateRequireAlias(module, alias, span)` (resolver)
3. `UnknownRequireAlias(alias, span)` (flatten/rewrite)
4. `RequireMemberUndefined(alias, module, member, expected_kind, span)` (flatten/rewrite)
5. `RequireMemberPrivate(alias, module, member, expected_kind, span)` (flatten/rewrite)
6. `OpaqueTypeConstruction(type_name, span)` (typecheck)
7. `OpaqueFieldAccess(type_name, field, span)` (typecheck)
8. `OpaquePatternDestructure(type_name, span)` (typecheck)
9. `CallableNotAccessible(name, span)` (typecheck)
10. `PropertyNotAccessible(name, span)` (typecheck)

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
2. Grouped imports (`std::io::{stream, file}`).
3. Re-export syntax.
4. Package manager/external dependencies.
5. Friend/internal visibility tiers.
