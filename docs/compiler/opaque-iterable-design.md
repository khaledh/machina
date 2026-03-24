# Opaque `Iterable<T>` Protocol Type — v1 Design

## Motivation

Iterator adapter composition produces deeply nested concrete types:

```
CsvFormatIter<MapIter<CsvParseIter<InputRow, ParseError>, InputRow, OutputRow>, OutputRow>
```

Users reason at the `Iterable<string>` level, but the compiler exposes the full
concrete type in hover, signatures, and diagnostics. This creates a gap between
the abstraction the user intends and the type the compiler reports.

v1 introduces `Iterable<T>` as an **opaque static protocol type** in return
position and immutable local annotations. The concrete type is hidden at the
source level but preserved internally for zero-cost monomorphization.

## Allowed Positions

| Position            | v1      | Notes                                        |
|---------------------|---------|----------------------------------------------|
| Function parameter  | Yes     | Already works                                |
| Function return     | **Yes** | New                                          |
| Local `let`         | **Yes** | New                                          |
| Local `var`         | No      | Reassignment with different witness is existential |
| Struct field        | No      | Requires storage layout / boxing             |
| Type alias          | No      |                                              |
| Collection element  | No      |                                              |

## Core Semantic Rule

A binding or return annotated as `Iterable<T>`:

- Has one hidden concrete witness type, fixed at the annotation site.
- Exposes only the `Iterable<T>` protocol surface to source-level code.
- Is transparent to internal codegen and monomorphization.

## Single Hidden Witness

All paths producing a value for an `Iterable<T>` annotation must produce the
**same concrete type**. This falls out of existing join semantics — different
concrete iterator types already fail to join.

```machina
// OK — single concrete type
fn build(lines: string[*]) -> Iterable<string> {
    lines
        |> from_csv(parse_row, opts)
        |> map(grade_row)
        |> to_csv(format_row, fmt_opts)
}

// ERROR — different concrete types in branches (join fails)
fn build(flag: bool) -> Iterable<string> {
    if flag { iter_a } else { iter_b }
}
```

## Protocol Surface

An opaque `Iterable<T>` binding exposes a deliberately small usage surface in
v1. The allowed surface is:

- `for` loops — desugar internally to iterable protocol operations without exposing concrete iterator details at source level.
- Passing to a parameter typed `Iterable<T>`.
- Passing to a generic parameter `S` (binds the concrete witness internally).

Direct method calls are intentionally **not** part of the v1 surface, including
`iter()` and `next()`. Allowing those directly would leak the hidden concrete
witness type or its hidden `next()` error shape, which cuts against the opacity
boundary.

Everything outside the allowed surface is rejected.

### What Callers Can Do

```machina
let p: Iterable<string> = build_pipeline();

for line in p { ... }              // OK — for loop uses protocol
write_lines(writer, p)             // OK — Iterable<T> parameter
map(p, f)                          // OK — generic S binds to concrete witness
```

### What Callers Cannot Do

```machina
let p: Iterable<string> = build_pipeline();

p.wrote_header                     // ERROR — field access on opaque type
p.some_concrete_method()           // ERROR — not on Iterable<T> surface
let q: CsvFormatIter<...> = p      // ERROR — cannot assign opaque to concrete
```

## Interaction with Generics

When an `Iterable<T>` value flows into a generic type parameter, the compiler
binds the **concrete witness type** internally for monomorphization. This does
not make the original binding concrete again in source-level typing; it is an
internal instantiation rule used to preserve zero-cost code generation:

```machina
let p: Iterable<string> = build_pipeline();
let mapped = map(p, to_upper);
// S binds to CsvFormatIter<MapIter<...>>, not Iterable<string>
// mapped: MapIter<CsvFormatIter<...>, string, string>
```

Opacity is at the binding boundary, not contagious. To re-hide:

```machina
let mapped: Iterable<string> = map(p, to_upper);
```

## Interaction with Error Unions

`Iterable<T>` can appear as the ok type in an error union return:

```machina
fn build() -> Iterable<string> | IoError {
    let text = read_file(path)?;
    text.lines()
        |> from_csv(...)
        |> map(grade_row)
        |> to_csv(...)
}
```

The compiler verifies the ok-path concrete type satisfies `Iterable<string>`.

## Implementation

### 1. Shared Protocol Satisfaction Helper

Extract from `src/core/plans/for_plan.rs` into a shared helper:

```rust
fn check_iterable_protocol(
    ty: &Type,
    expected_item_ty: &Type,
    method_sigs: &...,
) -> Result<(), ProtocolError>
```

Used by:
- `for` loop planning (existing consumer).
- `Iterable<T>` annotation checking (new consumer).

### 2. Opaque Binding Metadata

Keep `TypeMap` concrete. Add a binding-level side table keyed by `DefId`:

```rust
struct OpaqueBinding {
    exposed_ty: Type,   // Iterable<string>
    witness_ty: Type,   // CsvFormatIter<MapIter<...>, OutputRow>
}

opaque_bindings: HashMap<DefId, OpaqueBinding>
```

Keying by `DefId` (not just `NodeId`) lets the type checker enforce opacity on
later uses of a binding — field access, method resolution, and assignability
checks look up the binding's `DefId` to determine whether it is opaque.

For display purposes (hover, diagnostics), a mirrored `NodeId -> Type` mapping
may also be useful so the analysis layer can resolve exposed types for arbitrary
expression nodes.

- Type checker populates when it encounters `Iterable<T>` in return position or
  `let` annotation.
- Hover and diagnostics consult exposed type first, fall back to concrete type.
- Monomorphizer and backend ignore the side table entirely.

### 3. Type Checker Changes

**Return position.** When a function's return type is `Iterable<T>`, infer the
concrete return type from the body, run `check_iterable_protocol`, record the
exposed type in the side table, and record the concrete type in the type map as
usual.

**Local `let`.** When the annotation is `Iterable<T>`, infer the RHS concrete
type, run `check_iterable_protocol`, record the exposed type in the side table.
Downstream uses of the variable are checked against the `Iterable<T>` surface
(only `iter` and `next`), not the concrete type.

**Assignability.** A concrete type satisfying the protocol is assignable TO
`Iterable<T>`. `Iterable<T>` is NOT assignable to a concrete type annotation.

**Visibility enforcement.** When resolving field access or method calls on a
variable with an exposed type, only allow operations visible through the
protocol surface.

### 4. Parser

`Iterable<T>` is already parseable in type expressions. No parser changes
expected — the type checker handles the new semantics.

### 5. Analysis / LSP

Hover: if a node has an exposed type in the side table, show that. Otherwise
show the concrete type.

## Diagnostics

| Situation                                | Message                                                                            |
|------------------------------------------|------------------------------------------------------------------------------------|
| Concrete type doesn't satisfy protocol   | `type Foo does not implement Iterable<string>: missing next() method`              |
| Field access on opaque binding           | `cannot access field 'wrote_header' on Iterable<string>`                           |
| Method not on protocol surface           | `method 'some_method' is not available on Iterable<string>`                        |
| Assigning opaque to concrete annotation  | `cannot assign Iterable<string> to CsvFormatIter<...>`                            |
| Different witnesses across branches      | `Iterable<string> return requires a single concrete type across all return paths` |

## Non-goals for v1

- `var` bindings with `Iterable<T>` annotation.
- Field and storage positions.
- Generalization to arbitrary protocols (future: `Hashable<T>`, etc.).
- Method-style chaining (`.map()`, `.filter()`).
- Automatic protocol display without user annotation.
