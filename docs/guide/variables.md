# Variables

Variables in Machina are bindings that associate names with values. Machina
distinguishes between immutable and mutable bindings.

## Immutable Bindings

Use `let` to create an immutable binding:

```
let x = 42;
let name = "Alice";
```

Immutable bindings cannot be reassigned:

```
let x = 10;
// x = 20;    // error: cannot assign to immutable binding
```

## Mutable Bindings

Use `var` to create a mutable binding:

```
var counter = 0;
counter = counter + 1;    // ok
```

Mutable bindings can be declared without initialization:

```
var result: u64;          // uninitialized
// use(result);           // error: use before initialization
result = compute();
use(result);              // ok
```

The compiler tracks initialization and ensures variables are initialized before
use.

## Type Annotations

Type annotations are optional when the type can be inferred:

```
let x = 42;               // inferred as u64
let y: u64 = 42;          // explicit annotation
var z: i32;               // required when uninitialized
```

## Destructuring

Destructuring extracts values from compound types into separate bindings.

### Tuple Destructuring

```
let pair = (10, 20);
let (x, y) = pair;        // x = 10, y = 20
```

### Array Destructuring

```
let arr = [1, 2, 3];
let [a, b, c] = arr;      // a = 1, b = 2, c = 3
```

### Struct Destructuring

```
type Point = { x: u64, y: u64 }

let p = Point { x: 10, y: 20 };
let Point { x, y } = p;   // x = 10, y = 20
```

### Nested Destructuring

Patterns can be nested:

```
let nested = ((1, 2), (3, 4));
let ((a, b), (c, d)) = nested;
```

## Scope

Variables are scoped to the block in which they are declared:

```
fn main() {
    let x = 1;
    {
        let y = 2;
        // x and y are both visible here
    }
    // only x is visible here
    // y is out of scope
}
```

Variables are dropped (cleaned up) when they go out of scope. For heap values,
this means the memory is freed.

## Shadowing

A new binding can shadow an earlier one with the same name:

```
let x = 10;
let x = x + 1;    // shadows the previous x
// x is now 11
```

Shadowing creates a new binding rather than mutating the old one. This is
useful for transforming a value while keeping the same name.

## Initialization Tracking

The compiler tracks whether variables are initialized across all control flow
paths:

```
fn example(cond: bool) -> u64 {
    var x: u64;
    if cond {
        x = 10;
    } else {
    };
    // x might not be initialized here
    // x    // error: use of possibly uninitialized variable
    0
}
```

To fix this, initialize on all paths:

```
fn example(cond: bool) -> u64 {
    var x: u64;
    if cond {
        x = 10;
    } else {
        x = 20;
    };
    x    // ok: initialized on all paths
}
```

## Partial Initialization

For aggregates, the compiler tracks initialization of individual fields:

```
type Point = { x: u64, y: u64 }

fn main() {
    var p: Point;
    p.x = 10;
    // p.y is not initialized
    // use(p);    // error: p is not fully initialized
    p.y = 20;
    use(p);       // ok: fully initialized
}
```

See [Memory Safety](mem-safety.md) for more on initialization rules and `out`
parameters.
