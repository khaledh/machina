# Functions

Functions are the primary way to organize code in Machina.

## Defining Functions

A function has a name, parameters, an optional return type, and a body:

```
fn add(a: u64, b: u64) -> u64 {
    a + b
}
```

The return type can be omitted if the function returns `()`:

```
fn greet(name: string) {
    println(f"Hello, {name}!");
}
```

## Calling Functions

Call a function by name with arguments:

```
let sum = add(10, 20);
greet("Alice");
```

## Return Values

The last expression in a function body is implicitly returned:

```
fn square(x: u64) -> u64 {
    x * x    // returned implicitly
}
```

Use `return` for early exits or explicit returns:

```
fn abs(x: i64) -> i64 {
    if x < 0 {
        return 0 - x;
    } else {
        x
    }
}
```

In `()` functions, `return;` is allowed but optional.

## Parameter Modes

Machina uses explicit parameter modes to control how arguments are passed. This
makes ownership and mutation visible at both the definition and call site.

### Default (Read-Only)

Parameters without a mode keyword are read-only borrows:

```
fn length(s: string) -> u64 {
    // can read s, but not modify it
    ...
}

let n = length("hello");    // no keyword at call site
```

### inout (Mutable Borrow)

The `inout` mode allows the function to modify the argument:

```
type Counter = { value: u64 }

fn double(inout c: Counter) {
    c.value = c.value * 2;
}

fn main() {
    var c = Counter { value: 5 };
    double(inout c);    // explicit at call site
    // c.value is now 10
}
```

Restrictions:
- The argument must be a mutable variable (`var`)
- Only aggregate or heap types (not plain scalars like `u64`)

### out (Uninitialized Output)

The `out` mode is for parameters that the function will initialize:

```
fn make_point(out p: Point, x: u64, y: u64) {
    p = Point { x: x, y: y };
}

fn main() {
    var p: Point;           // uninitialized
    make_point(out p, 10, 20);
    // p is now initialized
}
```

The callee must initialize the parameter before returning. Restrictions:
- Only aggregate types (not heap or plain scalars)
- The argument must be a mutable variable

### sink (Ownership Transfer)

The `sink` mode transfers ownership to the function:

```
fn consume(sink p: ^Point) {
    // p is owned here and will be dropped at function exit
}

fn main() {
    let p = ^Point { x: 1, y: 2 };
    consume(move p);    // explicit move at call site
    // p can no longer be used
}
```

Restrictions:
- Only types that need dropping (heap values or aggregates containing them)
- The call site must use `move`

## Type Restrictions on Modes

| Mode | Allowed Types |
|------|---------------|
| default | Any type |
| `inout` | Aggregate or heap types |
| `out` | Aggregate types only |
| `sink` | Types requiring drop (heap or containing heap) |

```
fn bad(inout x: u64) { }      // error: inout requires aggregate or heap
fn bad(out p: ^Point) { }     // error: out requires aggregate type
fn bad(sink x: u64) { }       // error: sink requires owned type
```

## Overloading

Functions can be overloaded by parameter types:

```
fn print_value(x: u64) {
    println(x);
}

fn print_value(x: bool) {
    if x { println("true"); } else { println("false"); }
}

fn main() {
    print_value(42);      // calls u64 version
    print_value(true);    // calls bool version
}
```

The compiler selects the matching overload at compile time based on argument
types.

## Recursion

Functions can call themselves:

```
fn factorial(n: u64) -> u64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

## External Functions

Declare a function without a body to reference an external symbol:

```
fn external_function(x: u64) -> u64;
```

This is used for linking with code from other sources.

## First-Class Functions

Functions can be stored in variables and passed as arguments:

```
type BinaryOp = fn(u64, u64) -> u64

fn apply(f: BinaryOp, a: u64, b: u64) -> u64 {
    f(a, b)
}

fn add(a: u64, b: u64) -> u64 { a + b }
fn mul(a: u64, b: u64) -> u64 { a * b }

fn main() {
    let sum = apply(add, 2, 3);    // 5
    let prod = apply(mul, 2, 3);   // 6
}
```

See [Closures](closures.md) for anonymous functions that capture variables.
