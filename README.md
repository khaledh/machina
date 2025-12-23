# Machina Programming Language

Machina is a modern systems programming language that is still in early
development. The compiler currently targets only ARM64 assembly.

## Current features

- Expression oriented syntax
- Bindings: `let` (immutable), `var` (mutable)
- Arithmetic and comparison operators
- Blocks (last expression is the block value)
- Lexical scoping

### Types

- Basic types: `u64`, `bool`, `()`
- Arrays: `u64[N]`, multi-dimensional arrays: `u64[M, N]`, `let a = [1, 2, 3]`
- Tuples: `(u64, bool)`, `let t = (10, true)`
- Structs: `type Point = { x: u64, y: u64 }`, `let p = Point { x: 10, y: 20 }`
- Enums: `type Color = Red | Green | Blue`, `let c = Color::Green`
- Enum payloads: `type Shape = Circle(u64) | Rect(u64, u64)`, `let s = Shape::Circle(10)`
- Type aliases: `type Size = u64`, `let s: Size = 10`
- Array destructuring: `let [a, b, c] = [1, 2, 3]`
- Tuple destructuring: `let (x, y) = (1, true)`
- Struct destructuring: `let Point { x, y } = p`
- Struct update: `{ base | x: 10 }`

### Control flow

- `if`/`else`
- `while`

### Functions

- `fn foo(arg: arg_type, ...) -> return_type { body }`
- Recursion
- Pass-by-value and return-by-value (optimized)

### Code generation

- Code generation to ARM64 assembly

## Example

```
// Type Alias
type Coord = u64

// Enums
type Shade = Light | Dark
type Color = Red(Shade) | Green | Blue(u64, Shade)

// Struct
type Point = {
  x: Coord,
  y: Coord,
  color: Color,
}

fn main() -> u64 {
    // Struct literals
    let a = Point { x: 10, y: 20, color: Color::Red(Shade::Light) };
    let b = Point { x: 5, y: 40, color: Color::Blue(20, Shade::Dark) };

    // Function calls (pass and return by value)
    // (Optimized to pass by reference and RVO/NRVO internally)
    let c = scale(a, 2, 3);
    let d = change_color(b, Color::Green);

    // Tuple destructuring
    let (dx, dy) = delta(c, d);

    if same_delta_x(c, d) {
        42
    } else {
        21
    }
}

fn scale(p: Point, dx: u64, dy: u64) -> Point {
    // Struct destructuring
    let Point { x, y, color } = p;

    // Struct update (creates a new struct value)
    { p | x: x * dx, y: y * dy }
}

fn change_color(p: Point, color: Color) -> Point {
    { p | color: color }
}

fn same_delta_x(p1: Point, p2: Point) -> bool {
    p1.x == p2.x
}

fn delta(p1: Point, p2: Point) -> (u64, u64) {
    (p2.x - p1.x, p2.y - p1.y)
}
```

## Compiling and running

Change the `SOURCE` constant in `src/main.rs` to your source code and run:
```
cargo run
```

This will compile the source code to arm64 assembly in `output.s` in the root
directory. Use clang to assemble and link:
```
clang output.s -o output
```

Then run the executable:
```
./output; echo $?  # or ./output; echo $status (for fish shell)
```

This should print the return value of the `main` function.
