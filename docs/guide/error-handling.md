# Error Handling

Machina uses union return types for fallible operations.

## Return Unions

A fallible function returns `Success | ErrorA | ErrorB`.

```mc
type IoError = { message: string }
type ParseError = { message: string }

fn parse_number(text: string) -> i32 | ParseError {
    // ...
}

fn read_file(path: string) -> string | IoError {
    // ...
}
```

The first variant is the success type by convention.

## Propagation with `?`

Use postfix `?` to propagate non-success variants to the caller.

```mc
fn load_number(path: string) -> i32 | IoError | ParseError {
    let text = read_file(path)?;
    let value = parse_number(text)?;
    value
}
```

`?` is allowed only when the surrounding function's return union can represent
all propagated error variants.

## Handling with `match`

Use `match` with typed bindings to handle variants explicitly.

```mc
fn main() {
    match load_number("n.txt") {
        value: i32 => {
            // success path
            value
        }
        err: IoError => {
            // handle IO failure
            0
        }
        err: ParseError => {
            // handle parse failure
            0
        }
    };
}
```

If you do not need the payload, match on the type only:

```mc
match load_number("n.txt") {
    i32 => 0,
    IoError => 1,
    ParseError => 2,
};
```

## Pattern

A practical style is:

1. Validate/parse in small functions returning unions.
2. Use `?` to keep the success path linear.
3. Convert to user-facing behavior in one outer `match`.
