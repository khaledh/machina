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

**Strings**
- `string` values (literals + variables)

**Arrays**
- Single-dimensional: `u64[N]`, `let a = [1, 2, 3]`
- Multi-dimensional: `u64[M, N]`, `let a = [[1, 2], [3, 4]]`
- Array destructuring: `let [a, b, c] = [1, 2, 3]`
- Array indexing includes runtime bounds checks (traps on failure)
- Array slicing: `let s: u64[] = a[1..3]`, `a[..]` (produces a slice)
- Typed array literals: `let a = u8[1, 2, 3]` (only for primitive types)
- Array repeat literals: `let a = [0; 32]`, `let a = u8[0; 32]`

**Tuples** (fixed-size, homogeneous)
- Tuples: `(u64, bool)`, `let t = (10, true)`
- Tuple destructuring: `let (x, y) = (1, true)`

**Structs**
- `type Point = { x: u64, y: u64 }`, `let p = Point { x: 10, y: 20 }`
- Struct destructuring: `let Point { x, y } = p`
- Struct update: `{ base | x: 10 }`

**Enums**
- `type Color = Red | Green | Blue`, `let c = Color::Green` - Enum payloads:
`type Shape = Circle(u64) | Rect(u64, u64)`, `let s = Shape::Circle(10)`
- Enum pattern matching: `match color { Color::Green(val) => val, _ => 0 }`

**Type aliases**
- `type Size = u64`, `let s: Size = 10`

**Range types**
- `range(max)` and `range(min, max)` (half-open, `[min, max)`)

### Control flow

- `if`/`else`
- `while`
- `for <pattern> in <start>..<end> { ... }` (range)
- `for <pattern> in <expr> { ... }` where `<expr>` is an array

### Functions

- `fn foo(arg: arg_type, ...) -> return_type { body }`
- `inout` parameters for aggregate types (args must be mutable lvalues)
- `fn foo(arg: arg_type, ...) -> return_type;` declares an external function
- Recursion
- Pass and return by value (optimized)
- Function overloading (internal symbols use `name$n`, may change)

### Code generation

- Code generation to ARM64 assembly

### Runtime checks

- Division by zero
- Index out of bounds
- Value out of range

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

fn main() {
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
        println(42);
    } else {
        println(21);
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

During development, run the compiler via cargo (prefix `cargo mcc` or `cargo run --`):
```
cargo mcc run examples/for_array.mc
```

The compiler supports three modes:
```
cargo mcc compile input.mc          # produces input.o
cargo mcc build input.mc            # produces input (executable)
cargo mcc run input.mc              # builds + runs input
```

You can override outputs with `-o`:
```
cargo mcc compile -o output.o input.mc
cargo mcc build -o output input.mc
```

Use `--emit` to keep intermediate artifacts (otherwise `.s` is a temp file):
```
cargo mcc build --emit asm,mcir input.mc
```

## Testing

Unit tests only:
```
cargo test --lib
```

Integration tests only:
```
cargo test --test '*'
```

## Compiler Design

The compiler is a multi-stage pipeline written in Rust:

```
┌─────────────────────────────────────────────────────────────────┐
│ Frontend                                                        │
│   Source → Lexer → Parser → Resolver → Type Check → Sem Check   │
└────────────────────────────────────┬────────────────────────────┘
┌────────────────────────────────────▼────────────────────────────┐
│ Middle End                                                      │
│   MCIR Lowering → Optimizer → Liveness                          │
└────────────────────────────────────┬────────────────────────────┘
┌────────────────────────────────────▼────────────────────────────┐
│ Backend                                                         │
│   Register Allocation → Code Generation → ARM64                 │
└─────────────────────────────────────────────────────────────────┘
```

**Frontend** produces progressively richer context:

- **Lexer/Parser**: Hand-written recursive descent with Pratt parsing for
  operators
- **Resolver**: Builds scope tree, records definitions and uses
- **Type Checker**: Infers and validates types across expressions
- **Semantic Check**: Enforces value rules (mutability, pattern completeness,
  range bounds)

**Middle End** operates on MCIR (Machina IR), a typed, place-based
representation:

- Scalars as SSA-like temporaries, aggregates as addressable places
- Explicit control flow graphs with basic blocks and terminators
- **Optimizer**: Constant folding, identity simplification, constant branch
  elimination, self-copy removal, last-use copy elision, NRVO
- **Liveness**: Computes live-in/live-out sets per basic block

**Backend** allocates registers and emits machine code:

- **Register Allocation**: Linear scan with AAPCS calling convention
- **Code Generation**: ARM64 assembly with prologue/epilogue handling

Use `--dump` flags to inspect any stage: `tokens`, `ast`, `defmap`, `typemap`,
`nrvo`, `ir`, `liveness`, `intervals`, `regalloc`, `asm`.
