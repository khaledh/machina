# `borrow<T>` Type Design

Status: **Proposal**

## Problem

Stack-backed f-strings produce a temporary string view whose backing storage
lives on the caller's stack frame. Passing this to a callee is safe only if
the callee does not let the value escape the call. Today there is no way to
express or enforce that contract, so all dynamic f-strings are forced through
the heap-allocated owned-string path — even when the callee only needs to
read the bytes and discard them.

This blocks f-string usage on bare-metal targets that have no heap allocator.

## Solution

Introduce `borrow<T>` as a built-in wrapper type that restricts how a value
may be used. The restriction is about **escape**, not ownership or mutability:
a `borrow<T>` value may be used locally but must not outlive the scope that
created it.

## Typing rules

### Construction

- `T` implicitly converts to `borrow<T>` (you can always borrow a value)
- `borrow<T>` does **not** implicitly convert to `T`

### Usage restrictions

A value of type `borrow<T>` may:

- be bound to a local variable
- be read (field access, method calls that take `borrow<T>`)
- be passed as an argument to a parameter of type `borrow<T>`

A value of type `borrow<T>` may **not**:

- be returned from a function
- be stored in a struct field
- be stored in a global or static variable
- be captured by a closure
- be inserted into a collection (array, set, map)
- be implicitly widened to `T`

These rules are enforced statically by the compiler.

### Lifetime scope (V1 rule)

A `borrow<T>` value is valid for the **lexical block** in which it is
created. Concretely:

- A `borrow<T>` parameter is valid for the function body.
- A `borrow<T>` local is valid for its enclosing block.
- A temporary `borrow<T>` (e.g., an f-string passed directly as a call
  argument) is valid for the enclosing statement.

Assignment between `borrow<T>` locals in the same block is allowed — it does
not extend the lifetime. The value remains bound to the scope of the
**original** binding.

This is intentionally conservative. It avoids Rust-style lifetime parameters
while still providing the key guarantee: a `borrow<T>` value cannot outlive
the stack storage it may reference.

### Nesting

- `borrow<borrow<T>>` is flattened to `borrow<T>` (borrowing is idempotent)
- `borrow<T>` where `T` is a value type like `u64` is legal but rarely useful

### Generality

The rules above apply uniformly for all `T`. The compiler does not restrict
which types may appear inside `borrow<>`. In practice, the useful cases
cluster around reference-like types:

- `borrow<string>` — the primary motivating case
- `borrow<u8[]>` — borrowed slice
- `borrow<SomeLargeStruct>` — non-escaping access to a struct

## Representation

`borrow<T>` has the **same runtime representation** as `T`. The distinction
is purely static — it affects what the compiler allows, not the generated
code layout or ABI. In particular, `borrow<T>` does **not** imply
pass-by-reference semantics. If `T` is passed by value, so is `borrow<T>`.

For `borrow<string>` specifically, the value is a non-owning string view
(ptr + len, cap = 0) — exactly the same as a string literal's representation.

## F-string lowering

The `borrow<T>` type enables a clean rule for f-string lowering strategy:

| Context expects     | F-string strategy         | Allocation |
|---------------------|---------------------------|------------|
| `borrow<string>`    | `FmtKind::View` — stack buffer | none       |
| `string`            | `FmtKind::Owned` — heap       | yes        |

The compiler inspects the expected type at the f-string expression site:

- If the target type is `borrow<string>`, the f-string is lowered using the
  stack-backed view path (`FmtInit` → `FmtAppendBytes` → `FmtAppendU64` →
  `FmtFinish`). The result is a string view into a caller-allocated stack
  buffer. This is safe because `borrow<string>` guarantees the callee will
  not let the value escape.

- If the target type is `string`, the f-string is lowered using the owned
  path as today.

### Segment restriction for view lowering

`FmtKind::View` requires all segments to have statically known maximum
lengths. This means:

- Literal segments: exact length known
- Integer segments: bounded (max 20 decimal digits for u64)
- Bool segments: bounded (max 5 chars for "false")
- **String interpolands: not allowed** in view-lowered f-strings

An f-string with a `{some_string}` interpoland in a `borrow<string>` context
is a compile error, because the total buffer size cannot be determined at
compile time. The user must either:
- Use the owned path (`string` context)
- Break the output into multiple calls

## Kernel example

```machina
@link_name("fusion_debugcon_write")
fn debugcon_write(text: borrow<string>);

@noreturn
fn kmain() {
    let count = memmap_total();

    // f-string lowered to stack-backed view — no heap needed
    debugcon_write(f"memmap entries: {count}\n");

    // string literal also works — implicitly borrows
    debugcon_write("Fusion: done\n");

    halt_forever();
}
```

## Conversion rules

```
string          → borrow<string>       ✓  implicit
"literal"       → borrow<string>       ✓  implicit (literal is already a view)
f"..."          → borrow<string>       ✓  triggers view lowering
borrow<string>  → string               ✗  compile error
borrow<string>  → borrow<string>       ✓  identity
```

More generally:
```
T               → borrow<T>            ✓  implicit
borrow<T>       → T                    ✗  compile error
```

## Escape analysis as optimization

For **internal** functions (body visible to the compiler), the compiler can
perform escape analysis on parameters. If a parameter of type `T` is proven
not to escape, the compiler may silently treat it as `borrow<T>` at call
sites for optimization purposes (e.g., passing a stack-backed f-string
without the user annotating the parameter).

This is an optimization, not a correctness requirement. The `borrow<T>` type
provides the correctness guarantee; escape analysis extends the benefit to
unannotated internal code.

## Interaction with existing features

### `view<T>` (foreign memory views)
Orthogonal. `view<T>` describes **what** the memory is (foreign, typed,
non-owning). `borrow<T>` describes **how long** a value may live (must not
escape). They could theoretically compose (`borrow<view<T>>`) but that is
not a priority.

### Closures
A closure that captures a `borrow<T>` value is a compile error. The closure
may outlive the borrowed value's scope.

### Parameter modes (`in`, `out`, `inout`)
`borrow<T>` is orthogonal to parameter modes. A `borrow<string>` parameter
in `in` mode means: borrowed, read-only, must not escape. Other combinations
are legal but unlikely to be useful initially.

## Implementation phases

### Phase 1: Core type + restrictions
- Parse `borrow<T>` in type positions
- Add `Type::Borrow(Box<Type>)` variant
- Enforce escape restrictions in semantic checking
- Implicit `T → borrow<T>` conversion in type unification

### Phase 2: F-string view lowering
- Select `FmtKind::View` when target type is `borrow<string>`
- Validate no `StringValue` segments in view context
- Link portable formatting runtime for bare targets

### Phase 3: Escape analysis (optional)
- Infer non-escaping parameters for internal functions
- Apply view lowering optimization at unannotated call sites

## Open questions

1. **Syntax** — is `borrow<T>` the right surface syntax? Alternatives:
   `&T`, `ref T`, `borrowed T`, `temp T`. `borrow<T>` is consistent with
   Machina's `view<T>` syntax for wrapper types.

2. **Explicit conversion** — should there be a way to explicitly copy a
   `borrow<T>` into a `T`? e.g., `string.copy(borrowed_value)` or
   `T.from(borrowed_value)`. This would be safe (creates independent owned
   copy) but adds API surface.

3. **Method calls on `borrow<T>`** — should `borrow<string>` support all
   `string` methods, or only a read-only subset? Since `borrow` is about
   escape not mutability, all non-escaping methods should work.
