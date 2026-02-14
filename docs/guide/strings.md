# Strings

Machina provides string literals and formatted strings (f-strings).

> Note: snippets that call `print`/`println` assume:
>
> ```mc
> requires {
>     std::io::print
>     std::io::println
> }
> ```

## String Literals

String literals are enclosed in double quotes:

```
let greeting = "Hello, world!";
let empty = "";
```

Strings have the `string` type:

```
let message: string = "Welcome";
```

## Escape Sequences

Use backslash for special characters:

| Escape | Character |
|--------|-----------|
| `\n` | Newline |
| `\t` | Tab |
| `\\` | Backslash |
| `\"` | Double quote |

```
let multiline = "Line 1\nLine 2";
let quoted = "She said \"hello\"";
let path = "C:\\Users\\name";
```

## Formatted Strings (f-strings)

F-strings allow embedding expressions in strings:

```
let name = "Alice";
let age = 30;
let message = f"Name: {name}, Age: {age}";
// "Name: Alice, Age: 30"
```

### Syntax

Prefix the string with `f` and use `{}` to embed expressions:

```
let x = 10;
let y = 20;
let result = f"{x} + {y} = {x + y}";
// "10 + 20 = 30"
```

### Escaping Braces

Use double braces to include literal braces:

```
let code = f"Use {{braces}} for formatting";
// "Use {braces} for formatting"
```

### Expressions in f-strings

Expressions inside `{}` currently support `string`, integer types, and `bool`:

```
let arr = [1, 2, 3];
let msg = f"First element: {arr[0]}";

type Point = { x: u64, y: u64 }
let p = Point { x: 10, y: 20 };
let loc = f"Point at ({p.x}, {p.y})";
```

## Printing Strings

Use `print` and `println` to output strings:

```
print("no newline");
println("with newline");

let name = "World";
println(f"Hello, {name}!");
```

## Appending

Strings support in-place appending via methods:

```
var s = "hello";
s.append(" world");
println(s);
```

You can also append raw bytes with `append_bytes`, which expects UTF-8:

```
let bytes = u8[33];
var s = "hi";
s.append_bytes(bytes[..]);
```

Appending may promote a string view to an owned, growable buffer as needed.

## String Indexing

Strings are indexed as bytes (`u8`):

```
let s = "hello";
let first: u8 = s[0];
let last: u8 = s[4];
println(f"first={first}, last={last}");
```

String indexing is read-only; assignment to `s[i]` is not supported.

## Strings and Heap

Strings can be heap-allocated with `^`, which yields a `string^`:

```
let hs = ^"heap string";
println(f"first byte = {hs[0]}");
```

`string^` does not implicitly coerce to `string` in calls, so `print` and
`println` take plain `string` values.

## Example: Building Messages

```
type Status = Ok | Error(string)

fn status_message(s: Status) -> string {
    match s {
        Status::Ok => "Operation successful",
        Status::Error(msg) => f"Error: {msg}",
    }
}

fn main() {
    let s1 = Status::Ok;
    let s2 = Status::Error("file not found");

    println(status_message(s1));
    println(status_message(s2));
}
```

## Example: Formatting Output

```
fn main() {
    let items = [
        ("apple", 3),
        ("banana", 5),
        ("orange", 2),
    ];

    println("Inventory:");
    for (name, count) in items {
        println(f"  {name}: {count}");
    }
}
```

Output:
```
Inventory:
  apple: 3
  banana: 5
  orange: 2
```
