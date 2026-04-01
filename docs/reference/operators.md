# Operators

This reference lists Machina operators and postfix forms.

## Precedence

From lowest to highest precedence:

| Precedence   | Operators                    | Associativity | Description                                                |
|--------------|------------------------------|---------------|------------------------------------------------------------|
| 0 (lowest)   | `\|>`                        | Left          | Pipe (first-argument composition)                          |
| 1            | `\|\|`                       | Left          | Logical OR                                                 |
| 2            | `&&`                         | Left          | Logical AND                                                |
| 3            | `\|`                         | Left          | Bitwise OR / type unions (in type position)                |
| 4            | `^`                          | Left          | Bitwise XOR                                                |
| 5            | `&`                          | Left          | Bitwise AND                                                |
| 6            | `==` `!=` `<` `<=` `>` `>=`  | Left          | Comparisons                                                |
| 7            | `<<` `>>`                    | Left          | Bit shifts                                                 |
| 8            | `+` `-`                      | Left          | Addition/subtraction                                       |
| 9            | `*` `/` `%`                  | Left          | Multiplication/division/remainder                          |
| 10           | unary `-` `!` `~` `^` `move` | Right         | Unary ops                                                  |
| 11           | ternary `?:`, `or`          | Right-ish     | Conditional / inline recovery forms                        |
| 12 (highest) | `[]` `.` `()` `?`            | Left          | Index/slice, field/method access, calls, postfix try       |

## Arithmetic

| Operator  | Meaning          | Example |
|-----------|------------------|---------|
| `+`       | Addition         | `a + b` |
| `-`       | Subtraction      | `a - b` |
| `*`       | Multiplication   | `a * b` |
| `/`       | Integer division | `a / b` |
| `%`       | Remainder        | `a % b` |
| unary `-` | Negation         | `-x`    |

Division by zero is a runtime error.

## Comparison

| Operator          | Meaning               |
|-------------------|-----------------------|
| `==` `!=`         | Equality / inequality |
| `<` `<=` `>` `>=` | Ordering              |

## Logical

| Operator | Meaning                     |
|----------|-----------------------------|
| `&&`     | Logical AND (short-circuit) |
| `\|\|`   | Logical OR (short-circuit)  |
| `!`      | Logical NOT                 |

## Bitwise

| Operator  | Meaning          |
|-----------|------------------|
| `&`       | Bitwise AND      |
| `\|`      | Bitwise OR       |
| `^`       | Bitwise XOR      |
| `~`       | Bitwise NOT      |
| `<<` `>>` | Left/right shift |

## Assignment

| Operator | Meaning    |
|----------|------------|
| `=`      | Assignment |

Compound assignment is supported:

| Operator | Meaning |
|----------|---------|
| `+=` `-=` `*=` `/=` `%=` | Arithmetic compound assignment |
| `&=` `\|=` `^=` | Bitwise compound assignment |
| `<<=` `>>=` | Shift compound assignment |

## Access and Postfix

| Form              | Meaning                 | Example                   |
|-------------------|-------------------------|---------------------------|
| `obj.field`       | Field/property access   | `p.x`, `cfg.port`         |
| `obj.method(...)` | Method call             | `arr.append(4)`           |
| `arr[i]`          | Indexing                | `arr[0]`                  |
| `arr[a..b]`       | Slicing                 | `arr[1..3]`, `arr[..]`    |
| `f(x)`            | Function call           | `add(1, 2)`               |
| `expr?`           | Error-union propagation | `let text = read(path)?;` |

Notes:
- Properties are accessed as fields (`obj.len`), not as method calls.
- `expr?` requires the operand to be an error union and propagates non-success
  variants to the caller.
- In type position, `?` is currently only accepted for low-level nullable forms
  such as `paddr?`, `vaddr?`, and `view<...>?`.

## Pipe Operator

| Operator | Meaning              | Example                        |
|----------|----------------------|--------------------------------|
| `\|>`    | Pipe (first-arg composition) | `x \|> f(a, b)` desugars to `f(x, a, b)` |

The pipe operator has the lowest precedence and is left-associative:

```mc
text.lines()
    |> from_csv(parse_row, opts)
    |> map(grade_row)
    |> to_csv(format_row, fmt_opts)
```

The right side must be a function call with parentheses. Method calls are not
supported on the right side.

## Recovery Forms

| Form | Meaning | Example |
|------|---------|---------|
| `expr or { ... }` | Inline recovery in the current scope | `read() or { return 0; }` |
| `expr or \|err\| { ... }` | Callable recovery handler | `read() or \|err\| { err.code }` |

Notes:
- Block-form `or { ... }` is inline recovery, so `return`, `break`, and
  `continue` affect the enclosing scope.
- The closure form remains useful when you want the error payload.

## Other Syntax Operators

| Operator/Form               | Meaning                     | Example                          |
|-----------------------------|-----------------------------|----------------------------------|
| prefix `^`                  | Heap allocation expression  | `^Point { x: 1, y: 2 }`          |
| postfix `^` (type position) | Heap-owned type             | `Point^`, `u8[]^`                |
| prefix `*` (type position)  | Raw pointer type            | `*u64`, `*Header`                |
| `move`                      | Explicit ownership transfer | `consume(move v)`                |
| `..`                        | Range (in loops/slices)     | `0..10`, `arr[1..]`              |
| `::`                        | Scope/path resolution       | `Color::Red`, `std::io::println` |

## Operator Overloading

Machina does not support user-defined operator overloading.
