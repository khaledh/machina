# Annotated Examples

This page presents curated examples with explanations. For more examples, see
the [`examples/`](../../examples/) directory.

## Hello World

The simplest Machina program:

```
fn main() {
    println("Hello, Machina!");
}
```

Every program needs a `main` function. `println` outputs text with a newline.

## Factorial

A recursive function computing factorial:

```
fn factorial(n: u64) -> u64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() {
    println(factorial(5));    // 120
}
```

**Key points:**
- `if` is an expression that returns a value
- `return` is available for early exits, but the last expression is still returned implicitly
- Recursion works naturally

## Sum of Array

Iterating over an array and accumulating a result:

```
fn sum(xs: u64[]) -> u64 {
    var total = 0;
    for x in xs {
        total = total + x;
    }
    total
}

fn main() {
    let numbers = [1, 2, 3, 4, 5];
    println(sum(numbers[..]));    // 15
}
```

**Key points:**
- `u64[]` is a slice type (non-owning view)
- `for x in xs` iterates over elements
- `numbers[..]` creates a slice of the entire array

## Structs and Methods

Defining a type with associated methods:

```
type Counter = { total: u64, last: u64 }

Counter :: {
    fn add(inout self, value: u64) {
        self.total = self.total + value;
        self.last = value;
    }

    fn reset(inout self) {
        self.total = 0;
        self.last = 0;
    }

    fn snapshot(self) -> (u64, u64) {
        (self.total, self.last)
    }
}

fn main() {
    var counter = Counter { total: 0, last: 0 };
    counter.add(10);
    counter.add(5);
    let (total, last) = counter.snapshot();
    println(f"total={total}, last={last}");    // total=15, last=5
}
```

**Key points:**
- Methods are defined in a `Type :: { }` block
- `inout self` allows mutation
- Read-only `self` for accessors
- Tuple destructuring in `let`
- F-strings for formatted output

## Enums and Pattern Matching

Using enums to model different shapes:

```
type Shape = Circle(u64) | Rect(u64, u64)

fn area(s: Shape) -> u64 {
    match s {
        Shape::Circle(r) => r * r * 3,
        Shape::Rect(w, h) => w * h,
    }
}

fn describe(s: Shape) -> string {
    match s {
        Shape::Circle(_) => "circle",
        Shape::Rect(_, _) => "rectangle",
    }
}

fn main() {
    let shapes = [
        Shape::Circle(10),
        Shape::Rect(4, 5),
    ];

    for s in shapes {
        println(f"{describe(s)}: area = {area(s)}");
    }
}
```

**Key points:**
- Enum variants can carry data
- `match` extracts payload values
- `_` ignores values in patterns
- Each match arm returns a value

## Event Processing

Combining enums, methods, and loops:

```
type Event = Add(u64) | Multiply(u64) | Reset

type Accumulator = { value: u64 }

Accumulator :: {
    fn process(inout self, ev: Event) {
        match ev {
            Event::Add(n) => self.value = self.value + n,
            Event::Multiply(n) => self.value = self.value * n,
            Event::Reset => self.value = 0,
        };
    }
}

fn main() {
    let events = [
        Event::Add(5),
        Event::Multiply(3),
        Event::Add(10),
        Event::Reset,
        Event::Add(7),
    ];

    var acc = Accumulator { value: 0 };
    for ev in events {
        acc.process(ev);
    }
    println(acc.value);    // 7
}
```

**Key points:**
- Events modeled as enum variants
- State updated through `inout self`
- Match handles each event type

## Heap Allocation and Ownership

Working with heap-allocated values:

```
type Node = { value: u64, next: NodeRef }
type NodeRef = Some(Node^) | None

fn sum_list(node: NodeRef) -> u64 {
    match node {
        NodeRef::None => 0,
        NodeRef::Some(n) => n.value + sum_list(n.next),
    }
}

fn main() {
    // Build a simple linked list: 1 -> 2 -> 3
    let list = NodeRef::Some(^Node {
        value: 1,
        next: NodeRef::Some(^Node {
            value: 2,
            next: NodeRef::Some(^Node {
                value: 3,
                next: NodeRef::None,
            }),
        }),
    });

    println(sum_list(list));    // 6
    // list is automatically dropped here
}
```

**Key points:**
- `T^` is an owned heap type, and `^expr` performs allocation
- Optional links are modeled with an enum
- Implicit dereference for field access
- Heap values automatically dropped at scope end

## Closures

Closures that capture variables:

```
fn main() {
    // Borrow capture (default)
    var counter = 0;
    let bump = || -> u64 {
        counter = counter + 1;
        counter
    };
    println(bump());    // 1
    println(bump());    // 2

    // Move capture
    let base = 100;
    let add = [move base] |x: u64| -> u64 {
        base + x
    };
    println(add(5));    // 105
    println(add(10));   // 110
}
```

**Key points:**
- Without capture list, closures borrow
- `[move var]` moves ownership into closure
- Borrow captures allow mutation
- Move captures freeze the captured value

## Parameter Modes

Demonstrating all parameter modes:

```
type Point = { x: u64, y: u64 }

// Default: read-only borrow
fn distance(p: Point) -> u64 {
    p.x + p.y
}

// inout: mutable borrow
fn translate(inout p: Point, dx: u64, dy: u64) {
    p.x = p.x + dx;
    p.y = p.y + dy;
}

// out: initialize uninitialized value
fn make_origin(out p: Point) {
    p = Point { x: 0, y: 0 };
}

// sink: take ownership
fn consume(sink p: Point^) {
    println(f"consumed point at ({p.x}, {p.y})");
    // p is dropped here
}

fn main() {
    var p = Point { x: 10, y: 20 };
    println(distance(p));       // 30

    translate(inout p, 5, 5);
    println(distance(p));       // 40

    var q: Point;
    make_origin(out q);
    println(distance(q));       // 0

    let hp = ^Point { x: 1, y: 2 };
    consume(move hp);
    // hp cannot be used after move
}
```

**Key points:**
- Default is read-only (no keyword)
- `inout` requires `inout` at call site
- `out` for initializing uninitialized variables
- `sink` requires `move` at call site

## Slices and Bounds

Working with slices safely:

```
fn print_slice(name: string, xs: u64[]) {
    print(f"{name}: [");
    var first = true;
    for x in xs {
        if first {
            first = false;
        } else {
            print(", ");
        };
        print(x);
    }
    println("]");
}

fn main() {
    let arr = [10, 20, 30, 40, 50];

    let all = arr[..];      // entire array
    let head = arr[..3];    // first 3
    let tail = arr[2..];    // from index 2
    let mid = arr[1..4];    // indices 1, 2, 3

    print_slice("all", all);
    print_slice("head", head);
    print_slice("tail", tail);
    print_slice("mid", mid);
}
```

Output:
```
all: [10, 20, 30, 40, 50]
head: [10, 20, 30]
tail: [30, 40, 50]
mid: [20, 30, 40]
```

**Key points:**
- `arr[..]` slices entire array
- `arr[..n]` from start to n (exclusive)
- `arr[n..]` from n to end
- `arr[m..n]` from m to n (exclusive)
- Slices are bounds-checked at runtime

## Struct Update

Creating modified copies of structs:

```
type Config = {
    debug: bool,
    verbose: bool,
    max_retries: u64,
}

fn with_debug(c: Config) -> Config {
    { c | debug: true }
}

fn with_retries(c: Config, n: u64) -> Config {
    { c | max_retries: n }
}

fn main() {
    let default = Config {
        debug: false,
        verbose: false,
        max_retries: 3,
    };

    let dev = { default | debug: true, verbose: true };
    let prod = { default | max_retries: 5 };

    println(f"dev.debug = {dev.debug}");           // true
    println(f"prod.max_retries = {prod.max_retries}"); // 5
}
```

**Key points:**
- `{ base | field: value }` creates a new struct
- Original struct unchanged
- Multiple fields can be updated
