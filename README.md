# Machina Programming Language

Machina is a modern systems programming language that is still in early
development. The compiler currently targets only ARM64 assembly.

## Vision

Machina is exploring how to make **state machines** and **typestate**
first-class concepts in a systems language. The end goal: describe a system as a
composition of state machines with well-defined transitions, and have the
compiler verify that your code respects those constraints.

### What this might look like

*Envisioned future syntax — not yet implemented*

```
protocol ConnectionOps {
    state Disconnected;
    state Connecting;
    state Connected;

    // Commands (caller-initiated)
    fn connect(self: Disconnected, addr: Address) -> Connecting;
    fn send(self: inout Connected, data: u8[]);
    fn disconnect(self: Connected) -> Disconnected;

    // Events (environment-initiated)
    on Established(self: Connecting) -> Connected;
    on Received(self: inout Connected, data: u8[]);
    on Lost(self: Connected) -> Disconnected;
}

type TcpClient: ConnectionOps {
    state Disconnected
    state Connecting { addr: Address }
    state Connected { socket: Socket }

    fn new() -> Disconnected {
        Disconnected
    }

    fn connect(self: Disconnected, addr: Address) -> Connecting {
        sys_connect(addr);
        Connecting { addr }
    }

    fn send(self: inout Connected, data: u8[]) {
        sys_send(self.socket, data);
    }

    fn disconnect(self: Connected) -> Disconnected {
        sys_close(self.socket);
        Disconnected
    }

    on Established(self: Connecting, socket: Socket) -> Connected {
        Connected { socket }
    }

    on Received(self: inout Connected, data: u8[]) {
        // handle incoming data
    }

    on Lost(self: Connected) -> Disconnected {
        Disconnected
    }
}
```

The protocol defines the state machine contract: states, commands (`fn`), and
events (`on`). The type implements it with concrete data per state. The compiler
verifies that transitions only happen from valid states, and that all events are
handled.

## Today

The vision above is the destination. Today, Machina is a working compiler with
foundational features: expression-oriented syntax, algebraic data types, pattern
matching, and mutable value semantics—building blocks for the stateful
abstractions to come.

### Features

- Mutable value semantics
- Expression oriented syntax
- Bindings: `let` (immutable), `var` (mutable)
- Explicit `move` for ownership transfer
- Mutable parameters via `inout` mode
- Blocks (last expression is the block value)
- Pattern matching
- Function overloading
- Runtime safety checks

### Types

**Basic types**
- Integers: `u8`/`u16`/`u32`/`u64`, `i8`/`i16`/`i32`/`i64`
- Integer literals: decimal, `0b` (binary), `0o` (octal), `0x` (hex); `_` is allowed for grouping
- Booleans: `bool`
- Characters: `char`
- Unit: `()`

**Strings**
- `string` values (literals + variables)
- Formatted strings: `f"..."` with `{expr}` (integers and string literals for now; string variables are not yet supported)
- Escape braces in f-strings with `{{` and `}}`

**Arrays**
- Single-dimensional: `u64[N]`, `let a = [1, 2, 3]`
- Multi-dimensional: `u64[M, N]`, `let a = [[1, 2], [3, 4]]`
- Array destructuring: `let [a, b, c] = [1, 2, 3]`
- Array slicing: `let s: u64[] = a[1..3]`, `a[..]` (produces a slice)
- Typed array literals: `let a = u8[1, 2, 3]` (only for primitive types)
- Array repeat literals: `let a = [0; 32]`, `let a = u8[0; 32]`
- Array indexing is bounds-checked at runtime (compile time for constant
  indices)

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
- Range expressions are bounds-checked at runtime (compile time for constant
  ranges)

### Control flow

- `if`/`else`
- `while`
- `for <pattern> in <start>..<end> { ... }` (range)
- `for <pattern> in <expr> { ... }` where `<expr>` is an array

### Operators

- Arithmetic operators (`+`, `-`, `*`, `/`, `%`)
- Comparison operators ( `==`, `!=`, `>`, `>=`, `<`, `<=`)
- Bitwise operators (`&`, `|`, `^`, `~`, `<<`, `>>`)
- Logical operators (`&&`, `||`, `!`)

### Functions

- `fn foo(arg: arg_type, ...) -> return_type { body }`
- `inout` parameters for aggregate types (args must be mutable lvalues)
- `fn foo(arg: arg_type, ...) -> return_type;` declares an external function
- Recursion
- Pass and return by value (optimized where possible)
- Function overloading

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

During development, run the compiler via cargo (prefix `cargo mcc` or
`cargo run --`):
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
- **Type Checker**: Infers and validates types across expressions, resolves
  function overloading
- **Semantic Check**: Enforces value-based rules and structural rules

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
