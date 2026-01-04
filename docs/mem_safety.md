# Machina Memory Safety (User Guide)

This document explains Machina's memory safety rules in a practical way. It
starts with the basics and builds up to more advanced cases.

## Values by default

Machina treats most things as *values*. When you assign or pass a value around,
you are not creating hidden aliases. This keeps reasoning simple:

- Changing one value does not secretly change another.
- The compiler is free to optimize copies behind the scenes, but you should not
  rely on that for correctness.

Example: assignments do not alias.

```
fn main() {
    var a = [1, 2, 3];
    let b = a;     // b is a value copy of a
    a[0] = 99;
    // b[0] is still 1
}
```

Example: parameters are read-only by default (no `in` keyword).

```
fn read(xs: u64[]) -> u64 {
    xs[0]
}

fn main() {
    var arr = [10, 20, 30];
    let first = read(arr);
    arr[0] = 99;
    // first is still 10
}
```

## Parameter modes express intent

Machina uses explicit parameter modes to make ownership and mutation clear.
Read-only parameters are the default (no `in` keyword). For `inout`, `out`, and
`sink`, the call site must also be explicit.

| Mode    | Meaning                          | Caller writes   | Callee can...           |
|---------|----------------------------------|-----------------|-------------------------|
| default | Read-only borrow                 | nothing         | Read only               |
| `inout` | Mutable borrow                   | `inout arg`     | Read and mutate         |
| `out`   | Uninitialized output             | `out arg`       | Must initialize         |
| `sink`  | Ownership transfer               | `move arg`      | Move, mutate, or drop   |

Examples:

_Note: `^T` means "owned heap value of type T" (explained later)._

```
fn read(p: Point) { /* read only */ }
fn tweak(inout p: Point) { p.x = p.x + 1; }
fn fill(out p: Point) { p = Point { x: 1, y: 2 }; }
fn consume(sink p: ^Point) { /* owns it now */ }
```

At the call site:

```
var pt = Point { x: 1, y: 2 };
read(pt);           // ok: read-only is default
tweak(inout pt);    // ok: inout is explicit
fill(out pt);       // ok: out is explicit

let hp = ^Point { x: 3, y: 4 };
consume(move hp);   // ok: sink requires explicit move
// consume(hp);     // error: sink requires move

let hp2 = ^Point { x: 5, y: 6 };
consume(move hp2);  // ok: explicit move
// consume(hp2);    // error: use after move
```

### Type restrictions on modes

- `inout` requires aggregate or heap types (not plain scalars).
- `out` requires aggregate types (not heap or plain scalars).
- `sink` requires types that need dropping (heap values or aggregates containing them).

```
fn bad(inout x: u64) { }    // error: inout requires aggregate or heap type
fn bad(out p: ^Point) { }   // error: out requires aggregate type
fn bad(sink x: u64) { }     // error: sink requires owned type
```

### Arguments must be mutable lvalues

For `inout` and `out`, the argument must be a mutable variable:

```
fn tweak(inout p: Point) { p.x = p.x + 1; }

fn main() {
    let pt = Point { x: 1, y: 2 };
    tweak(inout pt);  // error: pt is not mutable

    var pt2 = Point { x: 1, y: 2 };
    tweak(inout pt2); // ok
}
```

### Moving from parameters

Regular parameters cannot be moved from. Only `sink` parameters own their value:

```
fn bad(p: ^Point) {
    let q = move p;  // error: cannot move from parameter
}

fn ok(sink p: ^Point) {
    let q = move p;  // ok: sink owns the value
}
```

## No overlapping mutable arguments

When calling a function, you cannot pass the same memory location (or
overlapping locations) to multiple arguments if any of them is mutated:

```
type Pair = { x: u64, y: u64 }

fn swap(inout a: Pair, inout b: Pair) {
    let tmp = a;
    a = b;
    b = tmp;
}

fn main() {
    var arr = [
        Pair { x: 1, y: 2 },
        Pair { x: 3, y: 4 },
        Pair { x: 5, y: 6 }
    ];
    swap(inout arr[0], inout arr[1]);  // ok: different indices
    swap(inout arr[0], inout arr[0]);  // error: overlapping arguments
}
```

This prevents subtle bugs where mutation through one parameter affects another.
The check is conservative: if indices are not compile-time constants, the
compiler assumes they might overlap.

```
fn main() {
    var arr = [
        Pair { x: 1, y: 2 },
        Pair { x: 3, y: 4 },
        Pair { x: 5, y: 6 }
    ];
    let i = 0;
    let j = 1;
    swap(inout arr[i], inout arr[j]);  // error: indices are not constant
}
```

Read-only aliasing is fine:

```
fn sum(a: u64, b: u64) -> u64 { a + b }

fn main() {
    let x = 5;
    let y = sum(x, x);  // ok: both are read-only
}
```

## Heap values are explicit

Heap allocation is explicit with the `^` type constructor and `^` allocation
expression. A `^T` means "this is an owned heap value of type T".

```
type Point = { x: u64, y: u64 }

fn main() {
    let p = ^Point { x: 1, y: 2 };
    // p owns that heap allocation and is dropped at scope end.
}
```

### Ownership transfer with `move`

Owned heap values must be moved when you transfer ownership:

```
fn main() {
    let p = ^Point { x: 1, y: 2 };
    let q = move p;  // ownership moves to q
    // use(p);       // error: use after move
}
```

If you try to use `p` after the move, the compiler rejects it.

### Implicit moves at last use

When a heap value is used for the last time, the compiler can insert an implicit
move. This avoids requiring `move` everywhere:

```
fn main() {
    let p = ^Point { x: 1, y: 2 };
    let q = p;   // implicit move: p is not used after this
}
```

Implicit moves do not apply to sink arguments (those must use `move`). And if
the value is used multiple times or is still live afterward, you must be
explicit:

```
fn main() {
    let p = ^Point { x: 1, y: 2 };
    let a = p;       // error: p is used again below
    let b = p;
}

fn main() {
    let p = ^Point { x: 1, y: 2 };
    consume(move p); // ok: explicit move
    consume(move p); // error: use after move
}
```

### You can only move whole variables

Moves must target entire variables, not projections like fields or indices:

```
fn main() {
    let pair = (^Point { x: 1, y: 2 }, ^Point { x: 3, y: 4 });
    let p = move pair.0;  // error: cannot move from projection
    let q = move pair;    // ok: move the whole tuple
}
```

This restriction keeps ownership tracking simple and predictable.

## Moves, drops, and scope

Owned heap values are dropped when they go out of scope, unless they were
moved. This is deterministic and works like RAII:

```
fn main() {
    let p = ^Point { x: 1, y: 2 };
    // p is dropped here at scope end
}
```

If control flow is conditional, Machina performs path-sensitive drop checks so
it doesn't double-free or leak on different branches:

```
fn main() {
    let p = ^Point { x: 1, y: 2 };
    if cond {
        consume(move p);  // p moved on this path
    } else {
        // no move on this path
    }
    // ok: p is dropped only on the path where it wasn't moved
    // use(p);            // error: p may have been moved
}
```

The fix is to ensure all paths handle the value consistently:

```
fn main() {
    let p = ^Point { x: 1, y: 2 };
    if cond {
        consume(move p);
    } else {
        consume(move p);  // both paths move p
    }
}
```

## Slices: safe views into existing storage

Slices are non-owning views into arrays or strings. They are deliberately
restricted to keep them safe and simple.

### Rule 1: Slice targets must be lvalues

You can only slice locals, params, or their fields/indices. This prevents
slicing a temporary that would die immediately:

```
fn ok() -> u64 {
    let arr = [1, 2, 3];
    let s = arr[0..2];  // ok: arr is a local
    s[0]
}

fn bad() -> u64 {
    let s = [1, 2, 3][0..2];  // error: target is a temporary
    s[0]
}
```

### Rule 2: Slices cannot escape

You cannot return a slice or store it inside an aggregate (tuple/array/struct/
enum):

```
fn bad(arr: u64[3]) -> u64[] {
    arr[0..2]  // error: slice cannot be returned
}

fn also_bad() {
    let arr = [1, 2, 3];
    let s = arr[0..2];
    let pair = (s, s);  // error: slice cannot be stored in tuple
}
```

### Rule 3: No mutation while a slice is live

The base value cannot be mutated, reassigned, or moved while its slice is
still in use:

```
fn bad() -> u64 {
    var arr = [1, 2, 3];
    let s = arr[0..2];
    arr[0] = 4;  // error: arr is borrowed by s
    s[0]
}
```

Mutation is allowed after the slice's last use:

```
fn ok() {
    var arr = [1, 2, 3];
    let s = arr[0..2];
    use(s[0]);   // last use of s
    arr[0] = 4;  // ok: s is no longer live
}
```

### Subslicing

You can slice a slice. The borrow extends back to the original base:

```
fn ok() {
    let arr = [1, 2, 3, 4, 5];
    let s1 = arr[0..4];
    let s2 = s1[1..3];  // s2 borrows from arr via s1
    use(s2[0]);
}
```

## Initialization rules

Machina tracks initialization so you cannot read uninitialized data:

- `let` must be initialized at declaration.
- `var` may be declared without initialization, but must be initialized before
  use.
- `out` parameters are treated as uninitialized on entry and must be filled by
  the callee.

```
fn fill(out p: Point) {
    p = Point { x: 1, y: 2 };
}

fn ok() {
    var p: Point;
    fill(out p);
    use(p);  // ok: p was initialized by fill
}

fn bad() -> Point {
    var p: Point;
    p  // error: use before initialization
}
```

### Initialization across branches

Initialization is tracked across all control flow paths. A variable is only
considered initialized if it is initialized on *all* paths:

```
fn bad(cond: bool) -> Point {
    var p: Point;
    if cond {
        p = Point { x: 1, y: 2 };
    } else {
        // left uninitialized
    }
    p  // error: p might not be initialized
}

fn ok(cond: bool) -> Point {
    var p: Point;
    if cond {
        p = Point { x: 1, y: 2 };
    } else {
        p = Point { x: 3, y: 4 };
    }
    p  // ok: initialized on all paths
}
```

### Partial initialization

If you initialize only part of an aggregate, Machina tracks that. The full
value must be initialized before the scope ends or the value is used:

```
type Pair = { a: ^Point, b: ^Point }

fn main() {
    var pair: Pair;
    pair.a = ^Point { x: 1, y: 2 };
    // error: pair.b is not initialized
}
```

This is particularly useful with `out` parameters for field-by-field
initialization:

```
fn fill_pair(out p: Pair) {
    p.a = ^Point { x: 1, y: 2 };
    p.b = ^Point { x: 3, y: 4 };
    // ok: both fields initialized
}

fn bad_fill(out p: Pair) {
    p.a = ^Point { x: 1, y: 2 };
    // error: out parameter p is not fully initialized
}
```

### Out parameters must be initialized

Every `out` parameter must be initialized before the function returns:

```
fn fill(out p: Point) {
    // error: out parameter p is not initialized on all paths
}

fn ok_fill(out p: Point) {
    p = Point { x: 1, y: 2 };  // ok
}
```

## Why this is safe without a full borrow checker

Machina's safety comes from a small set of strong guarantees:

- **Explicit heap ownership**: `^` types are move-only with deterministic drops.
- **Explicit mutation**: requires `inout` or `out` mode.
- **No hidden aliasing**: overlapping mutable arguments are rejected.
- **Restricted slices**: cannot escape, cannot outlive their source.
- **Tracked initialization**: prevents uninitialized reads and resource leaks.

These rules are intentionally conservative. They keep the language safe and
predictable while leaving room for more precision later (for example, allowing
slices to escape when the compiler can prove it is safe).

## Summary

| Feature | Rule |
|---------|------|
| Values | By default, no hidden aliasing |
| Heap (`^T`) | Explicit allocation, move-only ownership |
| `move` | Transfers ownership, explicit for sink calls, implicit at last use elsewhere |
| default | Read-only borrow |
| `inout` | Mutable borrow, must be mutable lvalue |
| `out` | Uninitialized, callee must initialize |
| `sink` | Ownership transfer, callee owns and drops |
| Overlapping args | Rejected if any is mutated |
| Slices | Lvalue targets only, no escape, no mutation while live |
| Initialization | Tracked across all paths, partial init must complete |

If you ever hit a restriction, the compiler is telling you where safety is at
risk. As the language evolves, many of these rules can become more precise
without changing the core mental model.
