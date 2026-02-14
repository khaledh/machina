# Language Tour

This tour gives a quick pass over Machina's core syntax and features.

## Hello World

```mc
requires {
    std::io::println
}

fn main() {
    println("Hello, Machina!");
}
```

Programs import symbols with a top-level `requires` block.

## Variables and Types

Use `let` for immutable bindings and `var` for mutable ones.

```mc
let x = 42;
var y = 10;
y = y + 1;
```

Common types:

- Integers: `u8/u16/u32/u64`, `i8/i16/i32/i64`
- `bool`, `char`, `string`, `()`
- Arrays: `u64[4]`
- Slices: `u64[]`
- Dynamic arrays: `u64[*]`
- Heap-owned values: `Point^`
- Sets/maps: `set<u64>`, `map<string, u64>`

## Control Flow

`if` and `match` are expressions.

```mc
let max = if a > b { a } else { b };

let label = match code {
    0 => "ok",
    _ => "error",
};
```

Loops:

```mc
for i in 0..10 {
    // ...
}

while cond {
    // ...
}
```

## Functions and Parameter Modes

```mc
type Counter = { value: u64 }

fn double(inout c: Counter) {
    c.value = c.value * 2;
}

fn consume(sink c: Counter^) {
    // takes ownership
}
```

Call-site modifiers mirror the parameter mode:

- default: `f(x)`
- `inout`: `f(inout x)`
- `out`: `f(out x)`
- `sink`: `f(move x)`

## Structs, Enums, and Methods

```mc
type Point = { x: u64, y: u64 }
type Shape = Circle(u64) | Rect(u64, u64)

Point :: {
    fn translate(inout self, dx: u64, dy: u64) {
        self.x = self.x + dx;
        self.y = self.y + dy;
    }
}
```

## Traits

```mc
trait Runnable {
    fn run(self);
}

type Job = { id: u64 }

Job :: Runnable {
    fn run(self) {
        // ...
    }
}
```

Traits can also declare properties.

## Error Unions and `?`

Machina supports ergonomic error unions in return types:

```mc
type IoError = { message: string }

fn read_config(path: string) -> string | IoError {
    // ...
}

fn load(path: string) -> string | IoError {
    let text = read_config(path)?;
    text
}
```

`?` propagates non-success union variants to the caller.

## Collections

```mc
var xs: u64[*] = [1, 2, 3];
xs.append(4);

var s = {1, 2, 3};
s.insert(4);

var m = map<u64, string>{1: "one"};
let v = m.get(1);
```

## Modules and Imports

```mc
requires {
    std::io::println
    app::config::Config
    app::config::load as load_config
}
```

Imported symbols are used directly (`println`, `Config`, `load_config`).

## Typestate (Experimental)

Machina includes an experimental typestate model for lifecycle-safe APIs.
Enable it through the compiler/LSP experimental feature settings.
See [Typestate](guide/typestate.md) for details.

## Next

- [Guide](guide/) for topic-focused docs
- [Reference](reference/) for grammar/operators/keywords
- [Annotated examples](examples/annotated-examples.md) for runnable samples
