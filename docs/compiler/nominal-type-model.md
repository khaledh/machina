# Nominal Type Model

This document defines the canonical type-model architecture for Machina's type
checker and backend consumers.

## Problem

Today, nominal types (`struct`/`enum`) can appear in multiple structural shapes
at different points in the pipeline (for example shallow recursive placeholders
vs. richer expanded forms). This is enough for many passes, but it creates
ordering-sensitive behavior in caches and consumers that need structural data
(e.g. drop lowering).

## Goals

- Keep nominal identity canonical and stable.
- Separate identity from structural expansion.
- Make recursive expansion deterministic.
- Keep existing passes working while migrating incrementally.

## Core Model

### 1) Canonical identity

Nominal type identity is represented by:

- `NominalKey { def_id, type_args }`

where:

- `def_id` identifies the nominal definition,
- `type_args` are canonicalized type arguments.

Two nominals are equal if and only if their keys are equal.

### 2) Structural view

Structural data is represented separately from identity:

- `TypeView::Struct(StructView)`
- `TypeView::Enum(EnumView)`

The view layer is read-only and resolver-backed. It is not itself the nominal
identity.

### 3) Expansion state

Structural expansion is modeled explicitly:

- `ExpansionState::Shallow` for recursion boundaries,
- `ExpansionState::Expanded` when full payload is available.

Consumers can require expanded views where needed and handle shallow nodes
explicitly when recursion boundaries are hit.

## Invariants

1. Nominal identity is canonical by `(def_id, type_args)`.
2. Structural expansion never changes identity.
3. Cache keys use canonical identity, not structural payload shape.
4. Recursive expansion is guarded and deterministic.
5. Backend lowering requiring layout/drop data consumes view APIs, not ad-hoc
   embedded nominal payloads.

## Migration Strategy

1. Introduce nominal key and view data types in `src/typecheck`.
2. Add resolver-backed `TypeViewResolver` with recursion guards/memoization.
3. Switch one backend path (drop/lower nominal metadata) to use type views.
4. Add determinism tests for recursive nominal expansion order.
5. Gradually move other structural consumers to view APIs.
6. Optionally introduce `Type::Nominal` once view-based consumers are in place.

## Non-goals (for this phase)

- Large-scale replacement of all `Type::Struct`/`Type::Enum` pattern matches in
  one change.
- Changing user-facing syntax or diagnostics.

## Validation

Each migration slice must keep:

- `cargo check -q` passing,
- `cargo test -q` passing,
- `cargo fmt` clean.
