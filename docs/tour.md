# Language Tour

This tour gives you a quick overview of Machina's features. Each section links
to a more detailed guide.

## Hello World

```
fn main() {
    println("Hello, Machina!");
}
```

Every program needs a `main` function. `println` outputs text followed by a
newline.

## Variables

Use `let` for immutable bindings and `var` for mutable ones:

```
let x = 42;        // immutable
var y = 10;        // mutable
y = y + 1;         // ok
// x = 0;          // error: x is immutable
```

Type annotations are optional when the type can be inferred:

```
let a: u64 = 100;
let b = 100;       // inferred as u64
```

See [Variables](guide/variables.md) for destructuring and scope rules.

## Types

### Scalar Types

```
let i: i64 = -42;       // signed integers: i8, i16, i32, i64
let u: u64 = 42;        // unsigned integers: u8, u16, u32, u64
let b: bool = true;     // boolean
let c: char = 'a';      // character
```

### Numeric Literals

```
let dec = 1_000_000;    // decimal with digit grouping
let bin = 0b1010_0110;  // binary
let oct = 0o52;         // octal
let hex = 0x2a;         // hexadecimal
```

### Arrays

```
let arr = [1, 2, 3];           // array of 3 elements
let zeros = [0; 10];           // 10 zeros
let matrix = [[1, 2], [3, 4]]; // 2D array
let first = arr[0];            // indexing (bounds-checked)
```

### Tuples

```
let pair = (42, true);
let x = pair.0;        // 42
let y = pair.1;        // true
```

### Strings

```
let s = "hello";
let name = "world";
let msg = f"hello, {name}!";   // formatted string
```

See [Types](guide/types.md) for the complete type system.

## Control Flow

### Conditionals

`if` is an expression that returns a value:

```
let max = if a > b { a } else { b };
```

### Loops

```
// while loop
var i = 0;
while i < 10 {
    i = i + 1;
}

// for loop over range (half-open: includes 0, excludes 10)
for i in 0..10 {
    println(i);
}

// for loop over array
let items = [10, 20, 30];
for x in items {
    println(x);
}
```

### Pattern Matching

```
let x = 2;
let result = match x {
    0 => "zero",
    1 => "one",
    _ => "other",
};
```

See [Control Flow](guide/control-flow.md) for more patterns.

## Functions

```
fn add(a: u64, b: u64) -> u64 {
    a + b    // implicit return (last expression)
}

fn greet(name: string) {
    println(f"Hello, {name}!");
}
```

Use `return` for early exits or explicit returns:

```
fn clamp(n: i64) -> i64 {
    if n < 0 {
        return 0;
    } else {
        n
    }
}
```

### Parameter Modes

Machina uses explicit parameter modes to control how arguments are passed:

| Mode | Meaning | Call site |
|------|---------|-----------|
| (default) | Read-only borrow | `f(x)` |
| `inout` | Mutable borrow | `f(inout x)` |
| `out` | Uninitialized output | `f(out x)` |
| `sink` | Ownership transfer | `f(move x)` |

```
type Counter = { value: u64 }

fn double(inout c: Counter) {
    c.value = c.value * 2;
}

fn main() {
    var c = Counter { value: 5 };
    double(inout c);   // c.value is now 10
}
```

See [Functions](guide/functions.md) for overloading and more details.

## User-Defined Types

### Type Aliases

```
type Size = u64
```

### Structs

```
type Point = { x: u64, y: u64 }

let p = Point { x: 10, y: 20 };
let q = { p | x: 30 };           // struct update (new value)
```

### Enums

```
type Color = Red | Green | Blue

type Shape = Circle(u64) | Rect(u64, u64)

let c = Color::Red;
let s = Shape::Circle(10);
```

### Pattern Matching on Enums

```
fn area(s: Shape) -> u64 {
    match s {
        Shape::Circle(r) => r * r * 3,
        Shape::Rect(w, h) => w * h,
    }
}
```

See [Structs and Enums](guide/structs-enums.md) for destructuring and more.

## Methods

Methods are defined in a block associated with a type:

```
type Counter = { value: u64 }

Counter :: {
    fn increment(inout self) {
        self.value = self.value + 1;
    }

    fn get(self) -> u64 {
        self.value
    }
}

fn main() {
    var c = Counter { value: 0 };
    c.increment();
    println(c.get());   // 1
}
```

See [Methods](guide/methods.md) for more details.

## Closures

Closures are anonymous functions that can capture variables from their
environment:

```
let add = |a: u64, b: u64| -> u64 { a + b };
let sum = add(2, 3);   // 5
```

### Capture Modes

By default, closures borrow variables. Use `[move var]` to move ownership:

```
var counter = 0;
let bump = || -> u64 {
    counter = counter + 1;   // borrows counter mutably
    counter
};

let base = 10;
let add = [move base] |x: u64| -> u64 {
    base + x   // base is moved into the closure
};
```

See [Closures](guide/closures.md) for capture rules and restrictions.

## Heap Allocation

The `^` prefix allocates on the heap and denotes ownership:

```
type Point = { x: u64, y: u64 }

let p = ^Point { x: 1, y: 2 };   // heap-allocated Point
println(p.x);                     // implicit dereference
```

Heap values are automatically dropped when they go out of scope.

### Ownership Transfer

Heap values must be moved when transferring ownership:

```
let p = ^Point { x: 1, y: 2 };
let q = move p;    // ownership moves to q
// use(p);         // error: use after move
```

See [Memory Safety](guide/mem-safety.md) for ownership rules, borrowing, and
slices.

## Slices

Slices are non-owning views into arrays:

```
let arr = [10, 20, 30, 40, 50];
let mid = arr[1..4];    // [20, 30, 40]
let head = arr[..2];    // [10, 20]
let tail = arr[3..];    // [40, 50]
let all = arr[..];      // entire array
```

Slices cannot escape their scope and cannot outlive their source.

See [Arrays and Slices](guide/arrays-slices.md) for indexing and bounds
checking.

## What's Next?

- [Guide](guide/) — In-depth coverage of each topic
- [Reference](reference/) — Operators, keywords, grammar
- [Examples](examples/annotated-examples.md) — Annotated code samples
