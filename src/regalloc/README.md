# Regalloc (MCIR Register Allocator)

This module implements a simple register allocator for MCIR based on linear
scan with a small amount of move/constraint handling. It consumes a lowered
`FuncBody` and produces a map from locals to machine locations plus stack
layout details.

## High‑level Flow

1. **Constraint analysis** (`constraints.rs`)
   - Determines calling‑convention requirements for args/returns.
   - Marks locals that must be in registers or stack addresses.
   - Produces a `ConstraintMap` keyed by `LocalId`.

2. **Liveness analysis** (`liveness.rs`)
   - Computes live‑in/live‑out sets per block.
   - Builds live intervals over instruction positions (`InstPos`).
   - Provides the data needed for linear scan.

3. **Allocation** (`alloc.rs`)
   - Uses a linear scan algorithm across live intervals.
   - Assigns locals to registers or stack slots.
   - Inserts spill/reload moves using `FnMoveList`.
   - Reserves scratch registers used by codegen (x14/x15).

4. **Move emission** (`moves.rs`)
   - Stores per‑instruction move lists (before/after).
   - Stores return moves for ABI‑required return placement.

## Key Data Structures

### Locations

`MappedLocal` is the final allocation result per local:
- `Reg(Arm64Reg)`
- `Stack(StackSlotId)`
- `StackAddr(StackSlotId)` for aggregate by‑address values

`Location` in `moves.rs` describes move sources/targets (registers, stack, and
places), used to emit spill/reload and ABI moves.

### Constraints

Constraints are derived from call/return ABI rules and the MCIR type:
- Scalars prefer registers.
- Aggregates use stack addresses (by‑address passing).
- Parameters/returns may require fixed registers based on ABI.

### Stack Layout

`StackAllocator` assigns `StackSlotId` offsets. These offsets are later used by
codegen to compute SP‑relative loads/stores.

## Notes

- This allocator is intentionally simple and pragmatic; it is not SSA‑based.
- Phi‑like moves are not modeled in MCIR yet, so edge moves are not required.
- If MCIR grows more complex control‑flow joins, `moves.rs` can be extended.
