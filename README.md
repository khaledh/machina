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

**Basic types**
- `u64`, `bool`, `char`, `()`

**Arrays**
- Single-dimensional: `u64[N]`, `let a = [1, 2, 3]`
- Multi-dimensional: `u64[M, N]`, `let a = [[1, 2], [3, 4]]`
- Array destructuring: `let [a, b, c] = [1, 2, 3]`

**Tuples** (fixed-size, homogeneous)
- Tuples: `(u64, bool)`, `let t = (10, true)`
- Tuple destructuring: `let (x, y) = (1, true)`

**Structs**
- `type Point = { x: u64, y: u64 }`, `let p = Point { x: 10, y: 20 }`
- Struct destructuring: `let Point { x, y } = p`
- Struct update: `{ base | x: 10 }`

**Enums**
- `type Color = Red | Green | Blue`, `let c = Color::Green`
- Enum payloads: `type Shape = Circle(u64) | Rect(u64, u64)`, `let s = Shape::Circle(10)`
- Enum pattern matching: `match color { Color::Green(val) => val, _ => 0 }`

**Type aliases**
- `type Size = u64`, `let s: Size = 10`

### Control flow

- `if`/`else`
- `while`

### Functions

- `fn foo(arg: arg_type, ...) -> return_type { body }`
- Recursion
- Pass and return by value (optimized)

### Code generation

- Code generation to ARM64 assembly

## Example

```
// Type Alias
type Coord = u64

type Shade = Light | Dark

// Enum
type Color = Red(Shade) | Green(u64) | Blue(u64, Shade, bool)

// Struct
type Point = {
  x: Coord,
  y: Coord,
  color: Color,
}

fn main() -> u64 {
    // Struct literals
    let a = Point { x: 10, y: 20, color: Color::Red(Shade::Light) };
    let b = Point { x: 5, y: 40, color: Color::Blue(20, Shade::Dark, true) };

    // Function calls (pass and return by value)
    // (Optimized to pass by reference and RVO/NRVO internally)
    let c = scale(a, 2, 3);
    let d = change_color(b, Color::Blue(20, Shade::Dark, false));

    // Tuple destructuring
    let (dx, dy) = delta(c, d);

    if is_blue(d) {
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

fn is_blue(p: Point) -> bool {
   // pattern matching on enum
    match p.color {
        Color::Blue(val, shade, b) => true,
        _ => false,
    }
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
