# Machina IR Redesign Sketch

This document sketches a direction for a cleaner, more optimizable IR for Machina,
focusing on function calls and aggregates. It is intentionally higher-level than the
current implementation.

The goals:

- Make *semantic* call sites simple (by-value args only).
- Represent *places* (addresses / l-values) explicitly and cheaply.
- Defer ABI details (registers, stack layout, memcpy) to a late pass.
- Make optimizations like "avoid memcpy for `arr[0]`" fall out naturally.

---

## 1. Two-Layer Model: Values vs Places

We distinguish **values** from **places**:

- **Values** are SSA-like temporaries of scalar or aggregate type.
- **Places** are typed memory locations: stack slots and sub-objects (fields, array
  elements).

### 1.1 Value IR

Value-level concepts:

- `v := op(v1, v2, ...)` for arithmetic, logical, and other pure computations.
- `v := call f(v1, v2, ...)` with **by-value** arguments.
- `phi` for control-flow merges.

Values have IR types (e.g. `u64`, `bool`, tuple, array) and follow SSA where feasible.

### 1.2 Place IR

Place-level concepts (a place is roughly `base_slot + byte_offset + type`):

- `p := alloca T` — allocate a stack slot holding a value of type `T`.
- `p_field := field_addr p, field_index` — address of struct/tuple field.
- `p_elem  := index_addr p, index, elem_size` — address of array element.
- `v := load p` — read value from place.
- `store v -> p` — write value into place.
- `memcpy dst_p <- src_p, size` — bulk copy between places.

Places are *not* SSA; they capture memory structure and layout. Values can be loaded from
and stored to places.

This design makes sub-objects (`arr[0]`, `t.field`) first-class, addressable entities
without forcing immediate materialization into temps.

---

## 2. Instruction Set (Sketch)

This section lists a possible instruction set, split into value and place instructions.
Names are illustrative.

### 2.1 Types

- Scalar: `Unit`, `Int{ bits, signed }`, `Bool`.
- Pointer: `Ptr(T)`.
- Aggregates: `Tuple{ fields: [T] }`, `Array{ elem: T, dims: [usize] }`.

### 2.2 Value Instructions

```text
v := const_int(value, bits, signed)
v := const_bool(value)
v := const_unit()

v := binop(op, v1, v2)     ; add, sub, mul, udiv, cmp, etc.
v := unop(op, v1)          ; neg, etc.

v := load p                ; load from a place p
store v -> p               ; store into a place p

v := call f(v1, v2, ...)   ; semantic call (by-value args)

v := phi( (b1, v1), (b2, v2), ... )

br block
condbr v_cond, then_block, else_block
ret v?                      ; optional return value
```

At this *semantic* level:

- Calls know nothing about registers or stack.
- Aggregate arguments (`Array`, `Tuple`) are conceptually passed by value.

### 2.3 Place Instructions

```text
p := alloca T              ; stack slot for a T

p_field := field_addr p_base, field_index
p_elem  := index_addr p_base, v_index, elem_size

memcpy dst_p <- src_p, size
```

`field_addr` and `index_addr` are pure address calculations; they **do not** read or write
memory.

Implementation-wise, a place can be lowered to `(base_stack_slot, constant_byte_offset,
T)`, but the IR exposes it as an opaque handle with these operations.

---

## 3. Calls and the ABI

Key idea: separate **semantic calls** from **ABI-lowered calls**.

### 3.1 Semantic Call IR

At the semantic level, every call is of the form:

```text
v_ret := call f(v_arg0, v_arg1, ..., v_argN) : RetType
```

- Each `v_argK` is a value of the declared parameter type.
- There are no explicit caller-side `memcpy` instructions mandated by the IR.
- Conceptually, parameters are passed by value.

### 3.2 ABI Description

A target-specific ABI describes, per parameter and return type, how values are passed:

- In a value register (e.g. `x0`, `x1`).
- By pointer to a stack object (indirect result / indirect parameter).
- Split across multiple registers (for some aggregates).

The ABI is **not** encoded in the semantic `call` instruction. Instead, a later **ABI
Lowering** pass:

- Inspects each `call` and the types of its args/return.
- Decides where to place arguments (registers, stack, indirect pointers).
- Inserts the minimal required `memcpy`, load, and store instructions.

This keeps the call IR simple and makes the ABI details explicit and local to one pass.

---

## 4. Aggregate Argument Lowering

This section focuses on how to lower aggregate arguments (`Tuple`, `Array`, `struct`) to
match the ABI while enabling optimizations like avoiding unnecessary `memcpy`.

### 4.1 Inputs to the Aggregate-Arg Pass

For each call site, the pass sees:

- The semantic `call` instruction with arguments `v0, v1, ..., vN`.
- A mapping from values to origin information:
  - For scalars: they may live in registers, be constants, or load results.
  - For aggregates: they may be:
    - Materialized temporaries occupying their own stack slots.
    - Views of larger aggregates (e.g. `arr[0]`, `t.field`) represented as places.
- Target ABI rules for the callee.

### 4.2 Strategy per Argument

Given parameter type `T` and argument value `v`:

1. **If `T` is scalar**:
   - Ensure `v` is materialized as a scalar value (possibly via `load p` if it originated
     from memory).
   - The ABI pass then moves that value into the appropriate register/stack space.

2. **If `T` is aggregate** (tuple, array, struct):

   - Determine a *place* representing the source of `v` if possible:
     - If `v` is the result of `load p_src`, then `p_src` is the natural place.
     - If `v` is produced directly by constructing into a place (`store` chain into an
       `alloca`), then that allocation is the place.
     - If `v` is a sub-object expression (`arr[0]`, `t.field`), then we expect it to be
       available as a place via `index_addr` / `field_addr`.
   - Decide whether we must copy:
     - If language semantics require a distinct by-value copy and analysis shows the
       callee may mutate/escape the param, allocate a param scratch place `p_param` and
       `memcpy p_param <- p_src, sizeof(T)`.
     - If an analysis (escape or side-effect analysis) says it is safe to alias the caller
       storage (e.g., params are immutable by spec, or the callee does not mutate them),
       we can pass a pointer to `p_src` without a copy.

3. **Map to ABI representation**:
   - If ABI expects a pointer to aggregate:
     - Pass `addr(p_param)` (if we had to copy) or `addr(p_src)` (if aliasing is allowed).
   - If ABI expects split scalars:
     - Optionally scalarize `T` into fields and load them individually for passing.

### 4.3 Where memcpy-Avoidance Happens

The optimization you care about (avoiding `memcpy` for `arr[0]`, etc.) becomes a local
decision in this pass:

- If `v` corresponds to a place like `p_elem = index_addr p_arr, const_index, elem_size`,
  then:
  - The pass can map the parameter directly to `p_elem` when aliasing is allowed.
  - No new aggregate temp is created.
  - No explicit `memcpy` is emitted at the call site.

This does *not* require any special operand form like "temp+offset" in the general IR; it
just requires that places and address calculations are first-class and cheap.

---

## 5. Example: `add_points(arr[0], arr[1])`

Consider:

```machina
let arr = [Point { x: 10, y: 20 }, Point { x: 30, y: 40 }, Point { x: 50, y: 60 }];
let sum1 = add_points(arr[0], arr[1]);
```

Assume `Point` is laid out as two `u64`s (`x`, `y`) and the array is contiguous.

### 5.1 High-Level Lowering (Sketch)

We might lower `arr` allocation and initialization roughly as:

```text
p_arr := alloca [3 x Point]
; store all three points into p_arr[0], p_arr[1], p_arr[2]
```

Field and element addresses:

```text
p_p0 := index_addr p_arr, 0, sizeof(Point)
p_p1 := index_addr p_arr, 1, sizeof(Point)
```

The call in **semantic IR** is:

```text
v0 := load p_p0           ; Point value (semantics)
v1 := load p_p1
v_sum := call add_points(v0, v1) : Point
```

No memcpy is mandated yet.

### 5.2 ABI Lowering With memcpy (Conservative)

A conservative ABI-lowering implementation might do:

```text
p_arg0 := alloca Point
p_arg1 := alloca Point

memcpy p_arg0 <- p_p0, sizeof(Point)
memcpy p_arg1 <- p_p1, sizeof(Point)

; ABI says: pass pointer to Point in x0, x1
x0 := addr(p_arg0)
x1 := addr(p_arg1)
call _add_points
```

This matches what the current implementation effectively does: it always allocates
argument slots and copies into them, even when the source was already in memory.

### 5.3 ABI Lowering With memcpy-Avoidance (Optimized)

With the improved design, the same pass could instead recognize that:

- `p_p0` and `p_p1` are already stack-resident contiguous points.
- Parameters are immutable (by language spec) or proven not to be mutated/escaped by the
  callee.

Then it can choose to alias the caller storage:

```text
; ABI: pass pointer to Point in x0, x1
x0 := addr(p_p0)
x1 := addr(p_p1)
call _add_points
```

No new stack slots for args, no `memcpy`.

The IR around the call would still be explicit, for example:

```text
; semantic
v0 := load p_p0
v1 := load p_p1
v_sum := call add_points(v0, v1) : Point

; after ABI lowering (roughly)
; compute param pointers and perform the call
x0 := addr(p_p0)
x1 := addr(p_p1)
call _add_points

; v_sum may be in registers or in an alloca, depending on ABI
```

The key part is that the decision to reuse `p_p0`/`p_p1` in place of new arg slots is
entirely within the ABI-lowering / aggregate-arg pass and does *not* require special cases
in codegen.

---

## 6. Relationship to the Current IR

This redesign preserves many good aspects of the existing IR while sharpening
responsibilities:

- **Keep**:
  - Temps (`IrTempId`) for SSA-like values.
  - `MemCopy`, `LoadAtByteOffset`, `StoreAtByteOffset` as low-level building blocks.
  - Basic blocks, CFG, and terminators.

- **Refine**:
  - Introduce an explicit place/address abstraction (rather than encoding `base+offset` in
    ad hoc ways).
  - Treat `Call` as a semantic instruction with by-value args, with ABI/aggregate details
    handled in a distinct pass.

- **Enable**:
  - A principled, local optimization in the aggregate-arg lowering pass that avoids
    redundant `memcpy` when the source is already an addressable sub-object.
  - Later introduction of scalar replacement of aggregates (SROA), more advanced escape
    analysis, and target-specific ABI tweaks without disturbing the core IR.

This is intended as a design direction, not a concrete migration plan. A practical path
forward could start with:

1. Adding a lightweight place/address abstraction (possibly reusing the existing `Place`
   logic).
2. Refactoring `Call` lowering to go through a small, localized aggregate-arg lowering
   phase.
3. Gradually moving memcpy insertion out of expression lowering into that phase, while
   preserving current semantics.
