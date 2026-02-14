# Control Flow

Machina provides standard control flow constructs. Most are expressions that
return values.

> Note: snippets that call `print`/`println` assume:
>
> ```mc
> requires {
>     std::io::print
>     std::io::println
> }
> ```

## Conditionals

### if/else

The `if` expression evaluates a condition and executes one of two branches:

```
if x > 0 {
    println("positive");
} else {
    println("not positive");
}
```

Both the `then` and `else` bodies are blocks. The `else` branch is optional.
If you omit it, the `if` expression has type `()` and is mainly used for side
effects:

```
if debug {
    println("debug mode");
}
```

### if as an Expression

`if` returns a value, so you can use it in assignments:

```
let max = if a > b { a } else { b };
```

When used as an expression, both branches must have the same type. If the
`else` is omitted, the type must be `()`.

### Chained Conditions

Use `else if` for multiple conditions:

```
let grade = if score >= 90 {
    "A"
} else if score >= 80 {
    "B"
} else if score >= 70 {
    "C"
} else {
    "F"
};
```

## Loops

### while

The `while` loop repeats while a condition is true:

```
var i = 0;
while i < 10 {
    println(i);
    i = i + 1;
}
```

### for over Ranges

The `for` loop iterates over a range. Ranges are half-open (include the start,
exclude the end):

```
for i in 0..10 {
    println(i);    // prints 0 through 9
}
```

Range bounds must be integer literals in the current syntax.

### for over Arrays

The `for` loop can iterate over array elements:

```
let items = [10, 20, 30];
for x in items {
    println(x);
}
```

### Loop with Destructuring

Loop variables can use patterns:

```
let pairs = [(1, 2), (3, 4), (5, 6)];
for (a, b) in pairs {
    println(a + b);
}
```

### break and continue

Use `break` to exit a loop early, and `continue` to skip to the next iteration:

```
for i in 0..10 {
    if i == 5 {
        break;
    } else {
        println(i);
    }
}
```

```
for i in 0..10 {
    if i % 2 == 1 {
        continue;
    } else {
        println(i);
    }
}
```

### return

The `return` statement exits the current function immediately:

```
fn clamp_to_even(n: u64) -> u64 {
    if n % 2 == 0 {
        return n;
    } else {
        return n - 1;
    }
}
```

For `()` functions, use `return;` or let the function end normally.

## Pattern Matching

### match

The `match` expression compares a value against patterns:

```
let x = 2;
let result = match x {
    0 => "zero",
    1 => "one",
    2 => "two",
    _ => "other",
};
```

The `_` pattern matches anything (wildcard).

### Matching Literals

Match against integer and boolean literals:

```
fn describe(n: u64) -> string {
    match n {
        0 => "zero",
        1 => "one",
        _ => "many",
    }
}

fn to_string(b: bool) -> string {
    match b {
        true => "yes",
        false => "no",
    }
}
```

### Matching Enums

Match on enum variants and bind payload values:

```
type Color = Red | Green | Blue

fn is_red(c: Color) -> bool {
    match c {
        Color::Red => true,
        _ => false,
    }
}
```

With payloads:

```
type Shape = Circle(u64) | Rect(u64, u64)

fn area(s: Shape) -> u64 {
    match s {
        Shape::Circle(r) => r * r * 3,
        Shape::Rect(w, h) => w * h,
    }
}
```

### Matching Tuples

Destructure tuples in match patterns:

```
fn describe_pair(p: (u64, u64)) -> string {
    match p {
        (0, 0) => "origin",
        (0, _) => "on y-axis",
        (_, 0) => "on x-axis",
        _ => "elsewhere",
    }
}
```

### Nested Patterns

Patterns can be nested:

```
type Option = Some(u64) | None
type Pair = (Option, Option)

fn both_some(p: Pair) -> bool {
    match p {
        (Option::Some(_), Option::Some(_)) => true,
        _ => false,
    }
}
```

### Exhaustiveness

Match expressions must cover all possible values. The compiler checks this:

```
type Color = Red | Green | Blue

fn to_num(c: Color) -> u64 {
    match c {
        Color::Red => 1,
        Color::Green => 2,
        // error: non-exhaustive match (Blue not covered)
    }
}
```

Add a wildcard or cover all variants:

```
fn to_num(c: Color) -> u64 {
    match c {
        Color::Red => 1,
        Color::Green => 2,
        Color::Blue => 3,
    }
}
```

## Blocks as Expressions

Blocks are expressions that return the value of their last expression:

```
let result = {
    let a = 10;
    let b = 20;
    a + b    // returned from block
};
// result is 30
```

This is useful for complex initializations or limiting variable scope.
