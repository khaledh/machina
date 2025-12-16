# IR for Machina

## Concepts:

- Functions are the basic units of lowering.
- Blocks are the basic units of control flow within a function.
- Instructions are the basic units of computation within a block.
- Temps are register-sized units of data within a function.
- Addresses are the basic units of memory within a function.

## Functions

Each function has a name, parameters, return type, blocks, temps, and addresses.
Functions are lowered from a function AST node using an IR function builder.

## Blocks

 Each block has an id, name, instructions, and a terminator. Block ids are
 assigned sequentially starting from 0 within a function. The function builder
 uses a vector to store the blocks, indexed by block id.

## IR Function Builder

- Supports creation of blocks, temps, and addresses.
- Always starts with an entry block.
- Other blocks can be selected for emission of instructions at any time
  (although only one block can be selected at a time).
- Blocks must be terminated with a terminator instruction (e.g. `br`, `condbr`,
  `ret`).
- Returns an immutable `IrFunction` when finished.

## Instructions

Each instruction has its own representation in the IR. Depending on the
instruction, it may have operands, a result, and a type.

## Terminators

Each block must be terminated with a terminator instruction. The terminator
instruction is the last instruction in the block. The terminator instruction
determines the next block to execute, or the end of the function if the block is
the last block in the function.

## Temps

Each temp has a type. Temps are assigned sequentially starting from 0 within a
function. Temps are used to represent the result of an instruction or the value
of a variable. The function builder uses a vector to store the temps, indexed by
temp id.

## Addresses

Each address has a type. The address type includes its size and alignment. The
function builder uses a vector to store the addresses, indexed by address id.

## Types

The IR uses the `IrType` enum to represent the type of a value. The type enum is
defined in the `types` module.

## Load and Store Instructions

The IR supports load and store instructions for variables. The load instruction
reads the value from an address and stores it in a temp. The store instruction
writes the value from a temp to an address.

## Constants

The IR supports constants for the following types: u64, bool, and unit.

## Operations

The IR supports binary operations (e.g. add, sub, mul, div) and unary operations
(e.g. neg).

## Calls

The call instruction takes a function name, arguments, and a return type.

## Phi Instructions

The IR supports phi instructions for the control flow of the function. The phi
instruction is used to merge the values of the incoming blocks into a single
value.
