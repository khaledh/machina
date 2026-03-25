# Iterators and Pipe Operator

Machina supports iteration over custom types through a protocol, and provides
the pipe operator `|>` for composing iterator pipelines.

## The Iterable Protocol

Any type can be used with `for` loops if it follows the iterable protocol:

1. The type has an `iter(self)` method returning an iterator.
2. The iterator has a `next(inout self)` method returning `T | IterDone`.

`IterDone` is a built-in type that signals the end of iteration.

### Custom Iterator Example

```mc
type Counter = { start: u64, end: u64 }

Counter :: {
    fn iter(self) -> CounterIter {
        CounterIter { cur: self.start, end: self.end }
    }
}

type CounterIter = { cur: u64, end: u64 }

CounterIter :: {
    fn next(inout self) -> u64 | IterDone {
        if self.cur < self.end {
            let value = self.cur;
            self.cur += 1;
            value
        } else {
            IterDone {}
        }
    }
}

fn main() {
    let counter = Counter { start: 2, end: 5 };
    for n in counter {
        println(n);    // prints 2, 3, 4
    }
}
```

### Built-in Iterables

These types already support `for` loops:

- **Arrays** (`T[N]`): `for x in [1, 2, 3] { ... }`
- **Slices** (`T[]`): `for x in arr[1..3] { ... }`
- **Dynamic arrays** (`T[*]`): `for x in xs { ... }`
- **Ranges**: `for i in 0..10 { ... }`
- **Strings** (via `.lines()`, `.split()`): `for line in text.lines() { ... }`
- **Maps**: `for (key, value) in m { ... }`

## Iterable<T>

`Iterable<T>` is a protocol type that hides the concrete iterator type. Use it
in function parameters to accept any iterable, and in `let` annotations to hide
complex pipeline types:

```mc
fn write_lines(writer: TextWriter, lines: Iterable<string>) -> () | IoError {
    for line in lines {
        writer.write_all(line)?;
        writer.write_all("\n")?;
    }
}
```

As a local annotation:

```mc
let pipeline: Iterable<string> =
    text.lines()
    |> from_csv(parse_row, opts)
    |> map(grade_row)
    |> to_csv(format_row, fmt_opts);

for line in pipeline {
    println(line);
}
```

The concrete type is preserved internally for zero-cost monomorphization, but
source-level code only sees `Iterable<string>`.

## Pipe Operator

The pipe operator `|>` chains a value as the first argument to a function call:

```mc
// These are equivalent:
let result = to_csv(map(from_csv(lines, parse, opts), transform), format, fmt_opts);

let result = lines
    |> from_csv(parse, opts)
    |> map(transform)
    |> to_csv(format, fmt_opts);
```

`x |> f(a, b)` desugars to `f(x, a, b)`.

Rules:

- The right side must be a function call with parentheses.
- Left-associative: `a |> f() |> g()` is `g(f(a))`.
- Method calls are not supported on the right side -- use free functions.

### Iterator Composition

The pipe operator is especially useful for composing iterator adapters from
`std::iter`:

```mc
requires {
    std::iter::map
    std::format::csv::from_csv
    std::format::csv::to_csv
}

let pipeline: Iterable<string> =
    text.lines()
    |> from_csv(parse_row, csv_parse_opts)
    |> map(grade_row)
    |> to_csv(format_row, csv_format_opts);
```

Each adapter wraps the previous iterator. The pipe operator makes the data flow
read top-to-bottom instead of inside-out.

## Standard Iterator Adapters

The `std::iter` module provides:

- `map(source, f)` -- transforms each item with `f`
- `try_map(source, f)` -- like `map`, but `f` returns `Out | E` (fallible)

## Runnable Examples

- `examples/basics/counter_iter_small.mc` -- custom iterator
- `examples/basics/map_iter_small.mc` -- `map` adapter
- `examples/basics/csv_grade_rewrite_small.mc` -- pipe operator with CSV pipeline
