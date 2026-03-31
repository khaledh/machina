# Low-Level Primitives Design: Limine Bootstrapping

## Status

Proposed. Partially implemented; this document reflects the target
design, which extends beyond what the compiler currently supports.

Scoped to what Machina needs to parse Limine boot protocol responses.
Volatile access, MMIO registers, and bitfield types are out of scope
(separate future design).

## Goals

- Enable Machina to implement a Limine-protocol kernel entry point.
- Provide typed, safe-by-default access to foreign memory structures.
- Keep raw pointers as an escape hatch, not the primary user model.
- Ground every design choice in a concrete Limine structure.

## Non-Goals (this doc)

- Volatile/MMIO access (separate track for device drivers).
- Bitfield types or register modeling.
- Inline assembly or platform intrinsics.
- Async or interrupt-driven I/O.
- Full allocator or memory management design.
- Region/lifetime system for view validity (see safety model below).

## Motivating Example

The Limine memory map response in C:

```c
struct limine_memmap_entry {
    uint64_t base;
    uint64_t length;
    uint64_t type;
};

struct limine_memmap_response {
    uint64_t revision;
    uint64_t entry_count;
    struct limine_memmap_entry **entries;
};

struct limine_memmap_request {
    uint64_t id[4];
    uint64_t revision;
    struct limine_memmap_response *response;
};
```

The kernel places a request struct in its binary. The bootloader scans
for it by magic bytes, processes it, and fills in the `response`
pointer. The response contains a pointer to an array of pointers to
entry structs.

All response pointers are virtual addresses with the HHDM offset
applied. The data lives in bootloader-reclaimable RAM — not MMIO, not
volatile.

A Machina kernel needs to:
1. Declare the request as a mutable global with known layout.
2. Place the request in a named linker section.
3. Read the response pointer (which may be null).
4. Iterate over the entries array (double indirection).
5. Read entry fields (base address, length, type).

---

## Feature 1: Fixed-Layout Types

### Problem

Machina's default struct layout is unspecified — the compiler may
reorder fields, insert padding, or change alignment for optimization.
Foreign data structures require exact control over field order, size,
and alignment.

### Design

A `@layout(fixed)` attribute on a type declaration guarantees:

- Fields are laid out in **declaration order**.
- No implicit padding is inserted between fields.
- No field reordering.
- The type's size is the sum of its field sizes, rounded up to its
  alignment.

```machina
@layout(fixed, size: 24)
type LimineMemmapEntry = {
    base: paddr,     // offset 0, size 8
    length: u64,     // offset 8, size 8
    typ: u64,        // offset 16, size 8
}
// total size: 24 bytes, alignment: 8
```

### Transparency rule

The in-memory representation of a fixed-layout type must be fully
determined by reading its field declarations. Every field's type must
have a known, fixed size. The rules are:

- **Plain value fields** contribute their natural size and alignment
  (e.g., `u64` = 8 bytes, `u8` = 1 byte, `paddr` = 8 bytes).
- **View fields** (`view<T>?`, `view<T[]>?`, `view<view<T>[]>?`)
  always occupy **8 bytes** (one pointer-sized slot). The view type
  describes the *meaning* of the stored pointer, not an expanded
  in-memory value.
- **`@count(sibling)`** is field metadata — it tells the compiler
  which sibling field provides the element count for a sequence view
  field. It does not affect the field's storage size.

This keeps layout computation straightforward: read the fields, sum
the sizes using the rules above.

### Typed view fields

View fields encode the pointee type directly in the declaration
rather than using untyped `vaddr?`:

```machina
@layout(fixed)
type LimineMemmapResponse = {
    revision: u64,
    entry_count: u64,
    @count(entry_count)
    entries: view<view<LimineMemmapEntry>[]>?,
}

@layout(fixed)
type LimineMemmapRequest = {
    id: u64[4],
    revision: u64,
    response: view<LimineMemmapResponse>?,
}
```

This eliminates manual `view_at` / `view_slice_at` calls — the struct
declaration is the single source of truth for both layout and access
semantics.

### View field access

Reading a nullable view field yields a value that can be unwrapped
via `or` or `match some/none`:

```machina
let response = memmap_request.response or {
    println("no response");
    return;
};
// response: view<LimineMemmapResponse>
```

Reading a counted view field (with `@count`) automatically pairs the
stored pointer with the sibling count to produce a sequence view:

```machina
let entries = response.entries or {
    println("no entries");
    return;
};
// entries: view<view<LimineMemmapEntry>[]>
// iteration yields view<LimineMemmapEntry> per element
```

### View type composition

The view type family uses composition rather than separate named types:

| Type | Meaning | Storage | Indexing |
|------|---------|---------|----------|
| `view<T>` | Single foreign struct | 8 bytes (ptr) | Field access |
| `view<T[]>` | Contiguous foreign array | 8 bytes (ptr) + count from `@count` | Direct index |
| `view<view<T>[]>` | Pointer table (T\*\*) | 8 bytes (ptr) + count from `@count` | Double deref |
| `view<T>?` | Nullable pointer to T | 8 bytes | Unwrap then access |

`view<view<T>[]>` reads as: "a foreign view over a contiguous array
of foreign views of T." The outer view handles the array bounds; each
inner `view<T>` is a pointer to a T. This is exactly the Limine
`T**` pattern — honest about the double indirection without exposing
raw pointers.

### `@count` rules

- `@count(field_name)` may only appear on `view<T[]>?` or
  `view<view<T>[]>?` fields.
- The referenced field must be a preceding field in the same struct
  with an unsigned integer type (`u64`, `u32`, etc.).
- The compiler verifies that the referenced field exists and has a
  valid type.
- `@count` does not affect storage size — it is access metadata only.

### Padding and Alignment

When padding is needed (e.g., for mixed field sizes), the programmer
declares it explicitly:

```machina
@layout(fixed)
type LimineFramebuffer = {
    address: vaddr,          // 8 bytes
    width: u64,              // 8 bytes
    height: u64,             // 8 bytes
    pitch: u64,              // 8 bytes
    bpp: u16,                // 2 bytes
    memory_model: u8,        // 1 byte
    red_mask_size: u8,       // 1 byte
    red_mask_shift: u8,      // 1 byte
    green_mask_size: u8,     // 1 byte
    green_mask_shift: u8,    // 1 byte
    blue_mask_size: u8,      // 1 byte
    _unused: u8[7],          // 7 bytes explicit padding
    edid_size: u64,          // 8 bytes
    edid: vaddr,             // 8 bytes
}
```

### Per-field alignment

When a field requires alignment beyond its natural alignment:

```machina
@layout(fixed)
type Example = {
    flags: u8,
    @align(8) data: u64,    // force 8-byte alignment
}
// compiler error: 7 bytes of implicit padding between flags and data
```

The compiler rejects implicit gaps. The programmer must fill them:

```machina
@layout(fixed)
type Example = {
    flags: u8,
    _pad: u8[7],
    data: u64,
}
```

### Per-type alignment

```machina
@layout(fixed)
@align(4096)
type PageTable = {
    entries: u64[512],
}
```

### Size assertions

An optional size constraint catches layout mistakes at compile time:

```machina
@layout(fixed, size: 24)
type LimineMemmapEntry = {
    base: paddr,
    length: u64,
    typ: u64,
}
// compiler error if fields don't sum to 24 bytes
```

### Interaction with normal types

Fixed-layout types can be used as fields in other fixed-layout types
(as long as the nested type also has a known fixed size). They can
also be used in normal (non-fixed) code — the layout guarantee only
constrains the in-memory representation, not how the type is used.

---

## Feature 2: Address Types

### Problem

Kernel code works with two address spaces: physical and virtual.
Confusing them is a common source of bugs. Using `u64` for both loses
type safety.

### Design

Two built-in primitive types:

```machina
paddr    // physical address
vaddr    // virtual address
```

These are 64-bit value types — copyable, comparable, printable — but
distinct from `u64` and from each other.

Both `paddr` and `vaddr` are valid field types in `@layout(fixed)`
structs. They occupy 8 bytes and have 8-byte alignment, identical to
`u64` in layout but distinct in the type system.

### Nullable addresses

Foreign data structures frequently use null (zero) to indicate "not
present." Rather than dropping back to `u64` for these fields, address
types support a nullable form:

```machina
paddr?   // nullable physical address (0 = none)
vaddr?   // nullable virtual address (0 = none)
```

In memory, `paddr?` and `vaddr?` are stored as plain 8-byte values,
identical to `paddr`/`vaddr`. The `?` is a type-level annotation that
tells the compiler the zero value represents "absent." Accessing the
address requires an explicit null check:

```machina
match some_field {
    some(addr) => {
        // addr is vaddr (non-nullable)
    }
    none => {
        // field was null
    }
}
```

Or using `or` for early bail:

```machina
let addr = some_field or {
    println("not present");
    return;
};
// addr is vaddr (non-nullable)
```

### Construction

```machina
let base = paddr(0x1000)
let virt = vaddr(0xFFFF_8000_0000_0000)
```

### Arithmetic

Minimal operator set — only what address manipulation actually needs:

```machina
addr + u64  -> same addr type    // offset forward
addr - u64  -> same addr type    // offset backward
addr - addr -> u64               // distance (same type only)

addr == addr -> bool             // equality
addr != addr -> bool
addr < addr  -> bool             // ordering
addr <= addr -> bool
addr > addr  -> bool
addr >= addr -> bool
```

Bitwise operators (`&`, `|`, `^`, `>>`, `<<`) are **not** provided
initially. Common operations that would use them get explicit helpers:

```machina
addr.align_down(alignment: u64) -> same addr type
addr.align_up(alignment: u64) -> same addr type
addr.is_aligned(alignment: u64) -> bool
addr.offset() -> u64             // extract raw value
```

### Conversion

No implicit conversion between `paddr`, `vaddr`, and `u64`.

Explicit conversions require the HHDM offset:

```machina
fn to_vaddr(p: paddr, hhdm_offset: u64) -> vaddr
fn to_paddr(v: vaddr, hhdm_offset: u64) -> paddr
```

Raw value extraction when needed:

```machina
let raw: u64 = addr.offset()
let addr = paddr(raw)
```

---

## Feature 3: Foreign Views

### Problem

Limine response data lives in bootloader-provided memory. The kernel
needs to read structures at specific virtual addresses without taking
ownership or managing their lifetime.

Raw pointers solve this but expose too much — pointer arithmetic,
arbitrary casts, manual null checks. Most kernel code just wants "read
the struct at this address."

### Design

A `view<T>` type represents a non-owning, typed handle to a
fixed-layout value at a specific address.

**Properties:**
- Non-owning: no drop, no deallocation.
- Read-through: field access reads from the underlying memory.
- Not volatile: reads are normal memory loads (suitable for RAM, not
  MMIO).
- Copyable: views are just typed addresses, can be freely copied.
- Typed: only works with `@layout(fixed)` types.

### As field types

View types are valid field types in `@layout(fixed)` structs (see
Feature 1). When used as fields, they occupy 8 bytes (pointer-sized)
and encode the pointee type in the declaration. Reading a view field
produces a `view<T>` (or `view<T>?` if nullable) that the programmer
can access immediately without manual `view_at` construction.

### Standalone construction

For cases where views are not declared as struct fields (e.g.,
bootstrapping the initial request struct), views can be constructed
explicitly:

```machina
let response: view<LimineMemmapResponse> = unsafe {
    view_at(response_addr)
};
```

Creating a view is unsafe. The caller must guarantee:

- `addr` points to a region of readable memory at least
  `size_of<T>()` bytes long.
- `addr` is aligned to `align_of<T>()`.
- The memory at `addr` contains a valid representation of `T`.
- The backing memory remains valid for the entire duration that the
  view (or any copy of it) is used.

Violating any of these is undefined behavior.

Standalone construction of sequence views:

```machina
// Contiguous array (single indirection)
let items: view<SomeType[]> = unsafe {
    view_array_at(addr, count)
};

// Pointer table (double indirection)
let entries: view<view<LimineMemmapEntry>[]> = unsafe {
    view_slice_at(entries_addr, entry_count)
};
```

These constructors are also unsafe with the same preconditions plus
count validity and pointee validity for the entire sequence.

### Safety model

Views do not carry lifetime or region information. The compiler does
**not** track whether the backing memory is still valid. This is a
deliberate V1 simplification.

The programmer is responsible for ensuring that a view's backing
memory remains valid for the duration of use. For Limine boot
structures, the contract is:

- Bootloader-reclaimable memory is valid from kernel entry until the
  kernel explicitly reclaims it (e.g., by adding it to the free page
  list).
- The programmer must not use views into bootloader memory after
  reclamation.

This is analogous to how C kernels use bootloader-provided pointers —
the validity is a protocol-level invariant, not a compiler-enforced
one. A future region or lifetime system could tighten this, but it is
not required for the Limine use case.

### Field access

```machina
let rev = response.revision;         // reads u64 from addr+0
let count = response.entry_count;    // reads u64 from addr+8
```

Field access through a view looks identical to field access on a
normal struct value. The difference is that the read comes from the
view's backing address rather than a stack/heap location.

---

## Feature 4: Global Statics and Section Placement

### Problem

The Limine protocol requires the kernel to place request structures in
its binary image so the bootloader can find them by scanning for magic
bytes. This requires:

1. Mutable global variables (the bootloader writes the `response`
   pointer).
2. Named section placement (the bootloader scans a specific section or
   address range).

### Design

#### Global statics

Machina adds `static var` declarations at module scope:

```machina
@section(".limine_requests")
static var memmap_request = LimineMemmapRequest {
    id: [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b,
         0x67cf3d9d378a806f, 0xe304acdfc50c3c62],
    revision: 0,
    response: None,
};
```

**Properties:**
- Initialized at load time (no runtime constructor).
- Mutable: the bootloader or hardware may write to them before kernel
  entry.
- Lifetime: program lifetime (never dropped).
- Access: reading and writing a `static var` is `unsafe` in general
  code, since the compiler cannot reason about concurrent access or
  external mutation. In the kernel entry path (single-threaded, before
  any scheduler), this is acceptable.

**Immutable globals** use `static let`:

```machina
static let MAX_ENTRIES: u64 = 256;
```

These are safe to read from any context.

#### Section placement

A `@section` attribute on a global places it in a named linker section:

```machina
@section(".limine_requests")
static var memmap_request = LimineMemmapRequest { ... };
```

The compiler emits the global into the specified section in the object
file. The linker script controls where that section appears in the
final binary.

**Constraints:**
- `@section` only applies to `static var` and `static let`
  declarations.
- The section name is a string literal.
- The compiler does not validate the section name against any linker
  script — it is the programmer's responsibility to ensure the name
  matches.

---

## Feature 5: Raw Pointer Escape Hatch

### Problem

The abstractions above cover the structured cases. Some kernel code
needs genuinely unstructured memory access — custom allocators, page
table construction, C FFI, etc.

### Design

A raw pointer type `*T` for when views don't suffice:

```machina
let ptr: *u64 = unsafe { ptr_at<u64>(addr) };
let value = unsafe { ptr.read() };
unsafe { ptr.write(42) };
```

**Properties:**
- Non-owning, no drop
- Nullable
- Explicit read/write (no implicit dereferencing)
- All operations are `unsafe`
- Can be obtained from `paddr`/`vaddr` via explicit cast

**When to use raw pointers vs views:**

| Situation | Use |
|-----------|-----|
| Reading a known-layout struct at an address | `view<T>` field or `view_at` |
| Iterating a foreign array | `view<T[]>` field with `@count` |
| Following a nullable pointer to a struct | `view<T>?` field, unwrap via `or` |
| Building a page table | `*T` |
| Writing to arbitrary memory | `*T` |
| C FFI | `*T` |
| Custom allocator internals | `*T` |

---

## Limine Example: Target Design

The following shows the end-state design — how a Machina kernel
**will** read the Limine memory map once all planned phases are
implemented. Typed view fields (Phase 4 remaining), nullable-view
`or` recovery (Phase 6), and safe static-var reads in kernel-entry
context are not yet implemented.

```machina
// --- Type declarations ---

@layout(fixed, size: 24)
type LimineMemmapEntry = {
    base: paddr,
    length: u64,
    typ: u64,
}

@layout(fixed)
type LimineMemmapResponse = {
    revision: u64,
    entry_count: u64,
    @count(entry_count)
    entries: view<view<LimineMemmapEntry>[]>?,
}

@layout(fixed)
type LimineMemmapRequest = {
    id: u64[4],
    revision: u64,
    response: view<LimineMemmapResponse>?,
}

// --- Request global ---

@section(".limine_requests")
static var memmap_request = LimineMemmapRequest {
    id: [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b,
         0x67cf3d9d378a806f, 0xe304acdfc50c3c62],
    revision: 0,
    response: None,
};

// --- Kernel entry ---

fn kernel_main() {
    let response = memmap_request.response or {
        println("no response");
        return;
    };

    let entries = response.entries or {
        println("no entries");
        return;
    };

    println(response.entry_count);
    for entry in entries {
        println(entry.length);
    }
}
```

No raw pointers. No manual `view_at` calls. No nested `match`. The
struct declarations carry both layout and access semantics — every
pointer field declares what it points to, and counted fields declare
where their length comes from.

**Note on `static var` access:** The target example reads
`memmap_request.response` without `unsafe`. The current implementation
requires `unsafe` for all `static var` reads. The target design would
allow safe reads of `static var` in kernel-entry context
(single-threaded, no scheduler), but the exact scoping rule for this
exemption is an open question. Until then, the kernel entry path
would use `unsafe` around static reads.

---

## Open Questions

1. **Mutability of views**: Should views support writes? For Limine,
   read-only is sufficient. For page tables, you'd want writable
   views. Could split into `view<T>` (read) and `view_mut<T>`
   (read-write), or defer mutability to raw pointers for now.

2. **Endianness**: Limine mandates little-endian. Should fixed-layout
   types have an endianness annotation, or assume native (which is
   little-endian on all Limine-supported architectures)?

3. **Static initialization**: `static var` initializers must be
   evaluable at compile time (no runtime expressions). This matches
   the Limine use case (magic constants, None pointers) but may need
   expansion for other kernel globals.

4. **General nullable type**: This design introduces `paddr?` and
   `vaddr?` as nullable address types, and `view<T>?` as nullable
   view fields. Whether Machina adds a general `T?` optional type is
   a separate design question.

5. **Standalone constructors vs field types**: The current
   implementation uses `view_at` / `view_slice_at` / `view_array_at`
   as standalone constructors. The target design moves toward typed
   view fields in struct declarations. Both paths should coexist —
   standalone constructors remain useful for bootstrapping (e.g., the
   initial request struct read) and for ad-hoc foreign memory access.

## Implementation Plan

### Phase 1 — Fixed-layout types (done)
- `@layout(fixed)` attribute
- Explicit padding requirement (reject implicit gaps)
- Optional `size` assertion
- Optional `@align` per-type and per-field

### Phase 2 — Address types (done)
- `paddr` and `vaddr` as built-in primitives
- `paddr?` and `vaddr?` nullable forms
- Arithmetic: `+ u64`, `- u64`, `- same`, comparisons
- Helper methods: `align_down`, `align_up`, `is_aligned`, `offset`
- Nullable methods: `is_some`, `is_none`, `unwrap`
- Pattern matching: `match some/none`
- Inline recovery: `addr or { ... }`

### Phase 3 — Globals and section placement (done)
- `static var` and `static let` declarations
- `@section` attribute for linker section placement
- Compile-time initializer evaluation

### Phase 4 — Foreign views (partially done)
- `view<T>` type for fixed-layout types
- Standalone constructors: `view_at`, `view_slice_at`, `view_array_at`
  (done — these require `unsafe`)
- Safe field-read access through views (done)
- Indexing and iteration on view sequences (done)
- **Remaining**: typed view fields in fixed-layout structs
  (`view<T>?`, `view<T[]>?` with `@count`)
- **Done**: sequence views use the unified spellings `view<view<T>[]>` and `view<T[]>`; the older `view_slice<T>` / `view_array<T>` forms remain compatibility aliases for now

### Phase 5 — Raw pointer escape hatch (done)
- `*T` raw pointer type
- `ptr_at<T>(addr)` construction (unsafe)
- `.read()`, `.write(val)` (unsafe)
- `unsafe { ... }` block syntax with validation

### Phase 6 — Inline recovery for nullable types
- `or { ... }` on nullable addresses: `paddr?`, `vaddr?` (done)
- Extend `or { ... }` to nullable view fields: `view<T>?`
  (required by target example)
