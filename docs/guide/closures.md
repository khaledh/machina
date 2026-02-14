# Closures

Closures are anonymous functions that can capture variables from their
surrounding scope.

> Note: snippets that call `println` assume:
>
> ```mc
> requires {
>     std::io::println
> }
> ```

## Basic Syntax

A closure is written with parameters in `||` and a body in `{}`:

```
let add = |a: u64, b: u64| -> u64 { a + b };
let sum = add(2, 3);    // 5
```

For single-expression bodies, the braces can be omitted:

```
let double = |x: u64| -> u64 x * 2;
```

## Closures Without Parameters

Use empty `||` for closures with no parameters:

```
let get_value = || -> u64 { 42 };
let v = get_value();    // 42
```

## Capturing Variables

Closures can access variables from their enclosing scope.

### Borrow Captures (Default)

Without an explicit capture list, closures borrow variables:

```
var counter = 0;
let bump = || -> u64 {
    counter = counter + 1;    // borrows counter mutably
    counter
};

let c1 = bump();    // 1
let c2 = bump();    // 2
```

Immutable variables are borrowed immutably:

```
let base = 10;
let add = |x: u64| -> u64 { base + x };    // borrows base immutably
```

### Move Captures

Use `[move var]` to move a variable into the closure:

```
let base = 10;
let add = [move base] |x: u64| -> u64 {
    base + x    // base is moved into the closure
};

// base cannot be used after this point
let result = add(5);    // 15
```

Multiple variables can be moved:

```
let x = 1;
let y = 2;
let sum = [move x, y] || -> u64 { x + y };
```

_Note:_ the capture list is currently **move-only**. Any variables listed after
`move` are moved into the closure. Borrow captures are still inferred from the
closure body.

If a name appears in the capture list, it must be used in the closure body
(unused explicit captures are an error).

## Capture Behavior

### Borrow Capture Rules

When a closure borrows a variable:

1. The variable cannot be mutated while the closure is live
2. The variable cannot be moved while the closure is live
3. After the closure's last use, the variable can be used again

```
var n = 1;
let get = || -> u64 n;
let v = get();          // last use of closure
n = n + 1;              // ok: closure is no longer live
```

### Move Capture Rules

When a closure moves a variable:

1. The variable cannot be used after the closure is created
2. The closure owns the captured value
3. For heap values, the value is dropped when the closure is dropped

```
let moved = 5;
let bad = [move moved] || -> u64 moved;
// let again = moved;    // error: use after move
```

## Restrictions

### Closures Cannot Escape

Closures with captures cannot be returned, stored in aggregates, or passed as
arguments:

```
fn make_adder(base: u64) -> fn(u64) -> u64 {
    // error: captured closure cannot be returned
    [move base] |x: u64| -> u64 { base + x }
}

fn apply(f: fn(u64) -> u64) { }

fn bad() {
    let base = 10;
    let add = [move base] |x: u64| -> u64 { base + x };
    // let arr = [add, add];    // error: cannot store in array
    // apply(add);              // error: cannot pass captured closure
}
```

### Borrow-Mutation Conflicts

Cannot mutate a borrowed variable while the closure is still live:

```
var y = 1;
let read_y = || -> u64 y;
// y = 2;        // error: y is borrowed by closure
// read_y();     // closure still live
```

Fix by ensuring the closure's last use comes first:

```
var y = 1;
let read_y = || -> u64 y;
let val = read_y();    // last use
y = 2;                 // ok now
```

### Borrow-Move Conflicts

Cannot move a borrowed variable while the closure is live:

```
let h = ^Point { x: 9, y: 9 };
let read_h = || -> u64 h.x;
// let moved_h = move h;    // error: h is borrowed
// read_h();
```

## Function Types

Closures can be assigned to function type variables:

```
type BinaryOp = fn(u64, u64) -> u64

let add: BinaryOp = |a: u64, b: u64| -> u64 { a + b };
let result = add(2, 3);
```

However, closures with captures cannot be stored in function type arrays or
returned from functions.

## Examples

### Counter

```
fn main() {
    var count = 0;
    let next = || -> u64 {
        count = count + 1;
        count
    };

    println(next());    // 1
    println(next());    // 2
    println(next());    // 3
}
```

### Accumulator

```
fn main() {
    var total = 0;
    let add = |n: u64| -> u64 {
        total = total + n;
        total
    };

    add(5);
    add(10);
    add(3);
    println(total);    // 18
}
```

### Move Capture with Heap

```
type Point = { x: u64, y: u64 }

fn main() {
    let hp = ^Point { x: 3, y: 4 };
    let sum = [move hp] || -> u64 {
        hp.x + hp.y
    };

    println(sum());    // 7
    // hp is dropped when sum goes out of scope
}
```
