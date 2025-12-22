# Machina Programming Language

Machina is a modern systems programming language that is still in early
development. The compiler currently targets only ARM64 assembly.

## Current features

- Expression oriented syntax
- Variables: `let` (immutable), `var` (mutable) with lexical scoping
- Arithmetic and comparison operators
- Basic types: `u64`, `bool`, `()`
- Compound types: tuples `(u64, bool)`, arrays `u64[N]`, multi-dimensional arrays `u64[M, N]`
- Type aliases: `type Size = u64`
- Enums: `type Color = Red | Green | Blue`
- Array destructuring: `let [a, b, c] = [1, 2, 3]`
- Tuple destructuring: `let (x, y) = (1, true)`
- Struct destructuring: `let Point { x, y } = p`
- Blocks (last expression is the block value)
- Control flow: `if`/`else`, `while`
- Functions and function calls (recursion supported)
- Code generation to ARM64 assembly

## Example

```
// Type Alias
type Coord = u64

// Enum
type Color = Red | Green | Blue

// Struct
type Point = {
  x: Coord,
  y: Coord,
  color: Color,
}

fn main() -> u64 {
    // Struct literals
    let a = Point { x: 10, y: 20, color: Color::Green };
    let b = Point { x: 5, y: 40, color: Color::Blue };

    // Function calls (pass and return by value)
    // (Optimized to pass by reference and RVO/NRVO internally)
    let c = scale(a, 2, 3);
    let d = change_color(b, Color::Green);

    // Tuple destructuring
    let (dx, dy) = delta(c, d);

    if same_color(c, d) {
        42
    } else {
        21
    }
}

fn scale(p: Point, dx: u64, dy: u64) -> Point {
    // Struct destructuring
    let Point { x, y, color } = p;
    Point { x: x * dx, y: y * dy, color: color }
}

fn change_color(p: Point, color: Color) -> Point {
    Point { x: p.x, y: p.y, color: color }
}

fn same_color(p1: Point, p2: Point) -> bool {
    p1.color == p2.color
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
