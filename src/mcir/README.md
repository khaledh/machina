# MCIR (Machina Core IR)

MCIR is a place-based, typed, low-level IR used after AST lowering and before
regalloc/codegen. It models scalar values as SSA-like temporaries and aggregates
as addressable places, with explicit control flow.

## Design Goals

- Make illegal states unrepresentable in the type system where possible.
- Separate scalar values from aggregate locations (places).
- Keep lowering simple and explicit (no hidden copies).
- Preserve a straightforward mapping to machine code.

## Core Concepts

### Types

Types are stored in a per-function `TyTable`. Each `TyId` refers to a `TyKind`:
`Unit`, `Bool`, `Int`, `Tuple`, `Array`, and user-defined structs.

### Locals

Locals are function-scoped and can be:
- `Return`
- `Param`
- `Temp`
- `User`

Each local has a `TyId` and optional name.

### Places

A `Place<K>` is an addressable location with a base `LocalId` and a projection
path (fields/indices). Places are typed, with a marker `K`:
- `Place<Scalar>` for scalar l-values
- `Place<Aggregate>` for aggregate l-values

`PlaceAny` is an enum for cases where either kind is allowed.

### Operands and Rvalues

- `Operand` represents a scalar value: `Copy(place)`, `Move(place)`, or `Const`.
- `Rvalue` represents computed scalar values: `Use`, `BinOp`, or `UnOp`.
  `Const::GlobalAddr` is treated as an address-sized `u64` constant.

### Statements

Statements encode side effects and writes:
- `CopyScalar { dst, src }`
- `CopyAggregate { dst, src }`
- `Call { dst, callee, args }`

### Terminators

Blocks end in:
- `Return`
- `Goto`
- `If { cond, then_bb, else_bb }`
- `Switch { discr, cases, default }`
- `Trap { kind }`
- `Unterminated` (placeholder during building)

## Control Flow

Functions are lists of basic blocks. Each block owns a list of statements and a
single terminator. There is an `entry` block and a `ret_local` for returns.

## Place Projections

Projections are structural:
- `Field { index }` for tuples/structs (lowered to an index)
- `Index { index }` for arrays (index is an operand)

Lowering computes offsets as needed during codegen; MCIR keeps the projection
structure explicit.

## Calls

Calls use a `Callee::Def(DefId)` so codegen can resolve symbol names via the
symbol table (DefId -> name mapping).
