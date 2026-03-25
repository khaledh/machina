# Language Tour

This tour gives a quick pass over Machina's core syntax and features.

## Hello World

```mc
fn main() {
    println("Hello, Machina!");
}
```

`print` and `println` are available by default.

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

## Generics

Functions and types can be parameterized over types:

```mc
fn id<T>(x: T) -> T { x }

type Option<T> = Some(T) | None

fn make_some<T>(x: T) -> Option<T> { Some(x) }
```

Type arguments are inferred from usage. See [Generics](guide/generics.md).

## Traits

```mc
trait Runnable {
    fn run(self) -> u64;
}

type Job = { id: u64 }

Job :: Runnable {
    fn run(self) -> u64 { self.id }
}

fn execute<T: Runnable>(value: T) -> u64 {
    value.run()
}
```

Traits can also declare properties. See [Traits](guide/traits.md).

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
    app::config::Config
    app::config::load as load_config
}
```

Imported symbols are used directly (`println`, `Config`, `load_config`).

## Resource Management

The `using` block scopes a resource and closes it automatically on block exit:

```mc
requires { std::io::open_write, std::io::IoError }

fn main() -> () | IoError {
    using writer = open_write("/tmp/out.txt")?.text() {
        writer.write_all("hello\n")?;
    }
    // writer is automatically closed here
}
```

## Pipe Operator

The pipe operator `|>` chains a value as the first argument to a function call:

```mc
requires { std::iter::map }

let result = text.lines()
    |> from_csv(parse_row, opts)
    |> map(grade_row)
    |> to_csv(format_row, fmt_opts);
```

`x |> f(a, b)` desugars to `f(x, a, b)`. See [Iterators and Pipe
Operator](guide/iterators.md).

## Linear Types and Hosted Machines

Machina's primary stateful-modeling surface is `@linear type`.

Without roles or triggers, a linear type is a direct-mode lifecycle-safe value:

```mc
@linear
type Door = {
    states {
        Closed,
        Open,
    }

    actions {
        open: Closed -> Open,
        close: Open -> Closed,
    }
}

Door :: {
    fn open(self) -> Open {
        Open {}
    }

    fn close(self) -> Closed {
        Closed {}
    }
}
```

Add roles and a hosting machine, and the same model becomes a long-lived hosted
workflow with `create(...)`, `resume(...)`, `send(...)`, `wait()`, and
`lookup(...)`.

See [Linear Types](guide/linear-types.md) for the practical guide, and
[Why Machina](why-machina.md) for the full payment workflow story.

## Next

- [Guide](guide/) for topic-focused docs
- [Reference](reference/) for grammar/operators/keywords
- [Annotated examples](examples/annotated-examples.md) for runnable samples
