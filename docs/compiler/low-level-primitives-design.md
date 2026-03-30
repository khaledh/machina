# Low-Level Primitives Design: Limine Bootstrapping

## Status

Proposed.

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

The kernel places a request struct in its binary. The bootloader scans for
it by magic bytes, processes it, and fills in the `response` pointer. The
response contains a pointer to an array of pointers to entry structs.

All response pointers are virtual addresses with the HHDM offset applied.
The data lives in bootloader-reclaimable RAM — not MMIO, not volatile.

A Machina kernel needs to:
1. Declare the request as a mutable global with known layout.
2. Place the request in a named linker section.
3. Read the response pointer (which may be null).
4. Iterate over the entries array (double indirection).
5. Read entry fields (base address, length, type).

---

## Feature 1: Fixed-Layout Types

### Problem

Machina's default struct layout is unspecified — the compiler may reorder
fields, insert padding, or change alignment for optimization. Foreign
data structures require exact control over field order, size, and
alignment.

### Design

A `@layout(fixed)` attribute on a type declaration guarantees:

- Fields are laid out in **declaration order**.
- No implicit padding is inserted between fields.
- No field reordering.
- The type's size is the sum of its field sizes, rounded up to its
  alignment.

```machina
@layout(fixed)
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
have a known, fixed size. No field may have hidden storage or depend on
sibling fields for its representation.

This means `view<T>`, `view_slice<T>`, and similar abstract handle types
are **not** valid field types in a `@layout(fixed)` struct. Foreign
indirection is modeled at the usage site, not inside the type
declaration (see Feature 3).

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
(as long as the nested type also has a known fixed size). They can also
be used in normal (non-fixed) code — the layout guarantee only
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

This keeps foreign pointer fields typed as addresses rather than raw
`u64`, preserving the type distinction at the point where it matters
most.

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

**`view<T>` is not a field type.** It does not appear inside
`@layout(fixed)` struct declarations. It is constructed at the usage
site from raw address values read out of fixed-layout fields. This
keeps fixed-layout types fully transparent — every field has a known,
fixed-size representation.

### Creation

```machina
unsafe {
    let response = view_at<LimineMemmapResponse>(addr);
}
```

Creating a view is unsafe. The caller must guarantee all of the
following preconditions:

- `addr` points to a region of readable memory at least
  `size_of<T>()` bytes long.
- `addr` is aligned to `align_of<T>()`.
- The memory at `addr` contains a valid representation of `T`
  (correct field values for the declared layout).
- The backing memory remains valid for the entire duration that the
  view (or any copy of it) is used.

Violating any of these is undefined behavior.

### Safety model

Views do not carry lifetime or region information. The compiler does
**not** track whether the backing memory is still valid. This is a
deliberate V1 simplification.

The programmer is responsible for ensuring that a view's backing memory
remains valid for the duration of use. For Limine boot structures, the
contract is:

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

Field access through a view looks identical to field access on a normal
struct value. The difference is that the read comes from the view's
backing address rather than a stack/heap location.

### Nullable views

Limine response pointers may be null (if the bootloader doesn't support
a request). Since pointer fields are declared as `vaddr?`, the null
check happens naturally through pattern matching before a view is
constructed:

```machina
match request.response_ptr {
    some(addr) => {
        // addr is vaddr (non-nullable) — safe to create a view
        let response = unsafe { view_at<LimineMemmapResponse>(addr) };
        // use response
    }
    none => {
        // bootloader didn't support this request
    }
}
```

The `vaddr?` type ensures the null check is required before the
address can be passed to `view_at`, which only accepts non-nullable
`vaddr`.

### Foreign slices

Limine's `entries` field uses double indirection: a pointer to an
array of pointers, with a separate `entry_count` field.

Since `view_slice<T>` is **not** a field type (it would break layout
transparency), foreign slices are constructed at the usage site from
the raw fields:

```machina
match response.entries_ptr {
    some(entries_addr) => {
        // Construct an iterable foreign slice (unsafe: trusting the pointer)
        let entries = unsafe {
            view_slice_at<LimineMemmapEntry>(entries_addr, response.entry_count)
        };

        // Iterate safely — bounds-checked by count
        for entry in entries {
            // entry is view<LimineMemmapEntry>
            let base = entry.base;       // paddr
            let len = entry.length;      // u64
        }
    }
    none => {
        // entries pointer was null
    }
}
```

`view_slice_at<T>(addr, count)` creates a `view_slice<T>` that
represents a counted, iterable sequence of foreign views. It handles
the double indirection internally: the address points to an array of
pointers, each pointer is followed to produce a `view<T>`.

Creating a view slice is unsafe. The caller must guarantee all of the
following preconditions:

- `addr` points to a readable, pointer-aligned region of memory
  containing at least `count` contiguous pointer-sized (8-byte)
  entries.
- Each of the `count` pointer entries is itself a valid, non-null,
  properly aligned address pointing to a readable region at least
  `size_of<T>()` bytes long containing a valid representation of `T`.
- All backing memory (both the pointer array and every pointee)
  remains valid for the entire duration that the slice (or any view
  produced by iterating it) is used.

Violating any of these is undefined behavior. Once constructed,
iteration is safe — it is bounds-checked by `count` and each element
access follows validated pointers.

For single-indirection patterns (pointer to contiguous array of
structs), a separate constructor could be provided:

```machina
let items = unsafe {
    view_array_at<SomeType>(vaddr(ptr), count)
};
```

The distinction between `view_slice_at` (double indirection, `T**`)
and `view_array_at` (single indirection, `T*`) is explicit at the
construction site.

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
static var memmap_request = LimineMemmapRequest {
    id: [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b,
         0x67cf3d9d378a806f, 0xe304acdfc50c3c62],
    revision: 0,
    response_ptr: 0,
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
| Reading a known-layout struct at an address | `view<T>` |
| Iterating a known-layout array at an address | `view_slice<T>` |
| Following a nullable pointer to a struct | `view<T>` + null check |
| Building a page table | `*T` |
| Writing to arbitrary memory | `*T` |
| C FFI | `*T` |
| Custom allocator internals | `*T` |

---

## Limine Example: Complete

Putting it all together — a Machina kernel that reads the Limine
memory map:

```machina
// --- Type declarations (fixed layout matching C structs) ---

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
    entries_ptr: vaddr?,     // nullable pointer to entries array
}

@layout(fixed)
type LimineMemmapRequest = {
    id: u64[4],
    revision: u64,
    response_ptr: vaddr?,    // nullable pointer to response
}

// --- Request global (placed in kernel binary for bootloader to find) ---

@section(".limine_requests")
static var memmap_request = LimineMemmapRequest {
    id: [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b,
         0x67cf3d9d378a806f, 0xe304acdfc50c3c62],
    revision: 0,
    response_ptr: none,
};

// --- Kernel entry ---

fn kernel_main() {
    // Read the response pointer (bootloader may have set it)
    let response_addr = unsafe { memmap_request.response_ptr };

    match response_addr {
        some(addr) => {
            // Create a typed view over the response struct
            let response = unsafe { view_at<LimineMemmapResponse>(addr) };

            // Read the entries pointer (also nullable)
            match response.entries_ptr {
                some(entries_addr) => {
                    // Create an iterable slice (double indirection)
                    let entries = unsafe {
                        view_slice_at<LimineMemmapEntry>(
                            entries_addr,
                            response.entry_count,
                        )
                    };

                    // Iterate — no raw pointers, bounds-checked by count
                    for entry in entries {
                        let base = entry.base;       // paddr
                        let length = entry.length;   // u64
                        let typ = entry.typ;         // u64
                        // ... process entry ...
                    }
                }
                none => {
                    // entries pointer was null
                }
            }
        }
        none => {
            // bootloader didn't provide memory map
        }
    }
}
```

The unsafe points are explicit and narrow:
1. Reading the global static (externally mutated by bootloader).
2. Creating a view from a bootloader-provided address (caller
   guarantees the address points to valid, aligned, readable memory
   containing a `LimineMemmapResponse`).
3. Creating a view slice from a bootloader-provided address and count
   (caller guarantees the pointer array contains `entry_count` valid,
   non-null, aligned pointers to `LimineMemmapEntry` structs, and all
   backing memory remains valid during iteration).

Once constructed, field access and iteration are safe — the type
system guarantees the layout and iteration is bounds-checked. The
fixed-layout types are fully transparent — every field has a declared
size and offset.

---

## Open Questions

1. **Mutability of views**: Should views support writes? For Limine,
   read-only is sufficient. For page tables, you'd want writable views.
   Could split into `view<T>` (read) and `view_mut<T>` (read-write),
   or defer mutability to raw pointers for now.

2. **View creation syntax**: `view_at<T>(addr)` is functional but
   verbose. Could be a method on vaddr: `addr.view<T>()`. Or a cast
   syntax.

3. **Single vs double indirection**: `view_slice_at` handles `T**`
   (Limine's pattern). `view_array_at` would handle `T*` (contiguous
   array). Both are needed; the naming should make the distinction
   clear.

4. **Endianness**: Limine mandates little-endian. Should fixed-layout
   types have an endianness annotation, or assume native (which is
   little-endian on all Limine-supported architectures)?

5. **Static initialization**: `static var` initializers must be
   evaluable at compile time (no runtime expressions). This matches
   the Limine use case (magic constants, none pointers) but may need
   expansion for other kernel globals.

6. **General nullable type**: This design introduces `paddr?` and
   `vaddr?` as nullable address types with zero-is-none semantics.
   Whether Machina adds a general `T?` optional type is a separate
   design question. The address-specific nullable types stand on their
   own regardless.

## Implementation Plan

### Phase 1 — Fixed-layout types
- `@layout(fixed)` attribute
- Explicit padding requirement (reject implicit gaps)
- Optional `size` assertion
- Optional `@align` per-type and per-field
- Transparency rule enforcement (no abstract handle types in fields)

### Phase 2 — Address types
- `paddr` and `vaddr` as built-in primitives
- Arithmetic: `+ u64`, `- u64`, `- same`, comparisons
- Helper methods: `align_down`, `align_up`, `is_aligned`, `offset`
- Explicit construction from `u64` literal
- Explicit `paddr`/`vaddr` conversion with HHDM offset
- f-string formatting support

### Phase 3 — Globals and section placement
- `static var` and `static let` declarations
- `@section` attribute for linker section placement
- Compile-time initializer evaluation
- Unsafe access to `static var`

### Phase 4 — Foreign views
- `view<T>` type for fixed-layout types
- Unsafe `view_at<T>(addr)` construction
- Safe field-read access through views
- `view_slice_at<T>(addr, count)` for double-indirection arrays
- `view_array_at<T>(addr, count)` for single-indirection arrays
- Iteration support for view slices/arrays

### Phase 5 — Raw pointer escape hatch
- `*T` raw pointer type
- `ptr_at<T>(addr)` construction (unsafe)
- `.read()`, `.write(val)` (unsafe)
- Pointer arithmetic: `ptr + u64`, `ptr - u64`
- Cast from/to `paddr`/`vaddr`
