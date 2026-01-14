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
    fn open(sink self: Disconnected, addr: Address) -> Connecting;
    fn send(inout self: Connected, data: u8[]) -> Connected;
    fn close(sink self: Connected) -> Disconnected;

    // Events (environment-initiated)
    on Established(self: Connecting, sock: Socket) -> Connected;
    on Received(inout self: Connected, data: u8[]) -> Connected;
    on Lost(sink self: Connected) -> Disconnected;
}

machine TcpClient<Net: Net>: ConnectionOps {
    // Common fields shared by all states
    net: Net;

    state Disconnected { }
    state Connecting { addr: Address }
    state Connected { socket: Socket }

    fn new(net: Net) -> Disconnected {
        Disconnected { net }
    }

    fn open(sink self: Disconnected, addr: Address) -> Connecting {
        self.net.connect(addr);
        Connecting { net: self.net, addr }
    }

    fn send(inout self: Connected, data: u8[]) {
        self.net.send(self.socket, data);
    }

    fn close(sink self: Connected) -> Disconnected {
        self.net.close(self.socket);
        Disconnected { net: self.net }
    }

    on Established(self: Connecting, sock: Socket) -> Connected {
        Connected { net: self.net, socket: sock }
    }

    on Received(inout self: Connected, data: u8[]) {
        self.net.on_data(self.socket, data);
    }

    on Lost(sink self: Connected) -> Disconnected {
        Disconnected { net: self.net }
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

## Example

```
type Counter = { total: u64, last: u64 }
type Event = Add(u64) | Reset

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

fn main() -> u64 {
    let events = [Event::Add(2), Event::Add(3), Event::Reset, Event::Add(5)];
    var counter = Counter { total: 0, last: 0 };

    for ev in events {
        match ev {
            Event::Add(n) => counter.add(n),
            Event::Reset => counter.reset(),
        }
    }

    let (total, last) = counter.snapshot();
    println(f"total={total}, last={last}");
    total
}
```

### Features

- Mutable value semantics
- Owned heap values (`^T`) with automatic drops
- Explicit ownership transfer (`move`)
- Parameter modes (`inout`, `out`, `sink`)
- Borrow rules for slices and `inout` args
- Function overloading
- Method blocks and method calls
- Closures (non-capturing)
- Pattern matching

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
- `type Color = Red | Green | Blue`, `let c = Color::Green`
- Payloads: `type Shape = Circle(u64) | Rect(u64, u64)`, `let s = Shape::Circle(10)`
- Pattern matching: `match color { Color::Green(val) => val, _ => 0 }`

**Type aliases**
- `type Size = u64`, `let s: Size = 10`

**Range types**
- `range(max)` and `range(min, max)` (half-open, `[min, max)`)
- Range expressions are bounds-checked at runtime (compile time for constant
  ranges)

**Ownership**
- `^T` dynamically allocates a value of type `T` and returns an owning handle
- The owner must move (`move`) when transferring ownership
- Owned values are dropped automatically when they go out of scope
- Access fields/indexes directly (`p.x`, `arr[i]`); deref is implicit

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

### Pattern Matching

- Enum patterns: `match color { Color::Green(val) => val, _ => 0 }`
- Tuple patterns: `match t { (0, Flag::On, true) => 10, (x, _, _) => x }`
- Literal matches: `match n { 0 => 10, 1 => 20, _ => 99 }`, `match b { true => 1, false => 0 }`

### Functions

- Function definitions: `fn foo(arg: arg_type, ...) -> ret_type { body }`
- External declarations: `fn foo(arg: arg_type, ...) -> ret_type;`
- Methods via method blocks: `Type :: { fn name(self, ...) { ... } }`
- Overloading and recursion
- Pass/return by value (optimized where possible)
- Function types: `fn(u64, u64) -> u64`
- Closures: `let add = |a: u64, b: u64| -> u64 { a + b }` (non-capturing today)

**Parameter modes**
- default (no keyword): immutable borrow of the argument
- `inout`: mutable borrow (arg must be a mutable lvalue)
- `out`: write-only parameter; callee must fully initialize it
- `sink`: ownership transfer to the callee (caller must use `move`)

### Code generation

- Code generation to ARM64 assembly

### Runtime checks

- Division by zero
- Index out of bounds
- Value out of range

### Optimizations

- RVO/NRVO (copy elision for return values)
- Last-use copy elision for aggregates
- Rvalue simplification
- Constant branch elimination
- Self-copy removal
- Stack slot reuse for non-overlapping lifetimes

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
┌───────────────────────────────────────────────────────────────────────┐
│ Frontend                                                              │
│   Lex → Parse → Resolve → Type Check → Normalize                      │
│       → Semantic Check → Elaborate → NRVO Analysis                    │
└───────────────────────────────────────┬───────────────────────────────┘
┌────────────────────────────────────▼──────────────────────────────────┐
│ Middle End                                                            │
│   MCIR Lowering → CFG-Free Opt → Liveness → Dataflow Opt → Liveness   │
└────────────────────────────────────┬──────────────────────────────────┘
┌────────────────────────────────────▼──────────────────────────────────┐
│ Backend                                                               │
│   Register Allocation → Code Generation → ARM64                       │
└───────────────────────────────────────────────────────────────────────┘
```

**Frontend** produces progressively richer context:

- **Lex/Parse**: Hand-written lexer and recursive descent parser with Pratt
  parsing for operators precedence. Produces AST.
- **Resolve**: Builds scope tree, records definitions and uses. Produces HIR.
- **Type Check**: Infers and validates types across expressions, resolves
  function overloading. Produces TIR.
- **Normalize**: Converts the typed tree into normalized IR and inserts
  explicit coercions (e.g., array-to-slice at call sites). Produces NIR.
- **Semantic Check**: Enforces value rules, structural rules, move restrictions,
  borrow restrictions, definition-before-use (including partial init), slice
  escape/borrow-scope rules, call-argument overlap checks.
- **Elaborate**: Converts NIR into SIR (semantic IR) and makes implicit
  operations explicit (e.g., implicit moves, place/value split). Produces SIR.
- **NRVO Analysis**: Marks safe copy-elision for aggregate returns.

**Middle End** operates on MCIR (Machina IR), a typed, place-based
representation:

- Scalars as SSA-like temporaries, aggregates as addressable places
- Explicit control flow graphs with basic blocks and terminators
- **CFG-free optimization**: Constant folding, identity simplification, constant
  branch elimination, self-copy removal
- **Dataflow optimization**: Last-use copy elision for aggregates
- **Liveness**: Computes live-in/live-out sets per basic block (used for
  optimization + register allocation)

**Backend** allocates registers and emits machine code:

- **Register Allocation**: Linear scan with AAPCS calling convention
- **Code Generation**: ARM64 assembly with prologue/epilogue handling

Use `--dump` flags to inspect any stage: `tokens`, `ast`, `deftab`, `typemap`,
`nrvo`, `ir`, `liveness`, `intervals`, `regalloc`, `asm`.
