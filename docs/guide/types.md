# Types

Machina is statically typed with local inference. You can omit many annotations,
but function signatures should stay explicit.

## Scalar Types

### Integers

| Type                   | Size              |
|------------------------|-------------------|
| `u8` `u16` `u32` `u64` | Unsigned integers |
| `i8` `i16` `i32` `i64` | Signed integers   |

```mc
let small: u8 = 255;
let signed: i32 = -42;
let big: u64 = 1_000_000;
```

Unresolved integer literals default to `i32`.

```mc
let x = 42;      // inferred as i32
```

### Numeric Literals

```mc
let decimal = 42;
let grouped = 1_000_000;
let binary = 0b1010_0110;
let octal = 0o52;
let hex = 0x2a;
```

### Booleans, Characters, Unit

```mc
let ok: bool = true;
let ch: char = 'a';

fn log_done() {
    // returns ()
}
```

## Product and Sum Types

### Arrays

`T[N]` is a fixed-size array.

```mc
let xs: i32[3] = [1, 2, 3];
let zeros = [0; 8];
let bytes = u8[1, 2, 3];
```

### Slices

`T[]` is a borrowed view.

```mc
fn sum(xs: i32[]) -> i32 {
    var acc = 0;
    for x in xs { acc = acc + x; }
    acc
}
```

### Dynamic Arrays

`T[*]` is an owned growable array.

```mc
var xs: i32[*] = [1, 2, 3];
xs.append(4);
```

### Tuples

```mc
let pair: (i32, bool) = (42, true);
let a = pair.0;
let b = pair.1;
```

### Strings

```mc
let s: string = "hello";
let msg = f"value={42}";
```

### Structs and Enums

```mc
type Point = { x: i32, y: i32 }
type OptionI32 = Some(i32) | None
```

## Builtin Collection Types

### Sets

`set<T>` stores unique values.

```mc
var s = {1, 2, 3};            // inferred set<i32>
var empty = set<i32>{};
```

### Maps

`map<K, V>` stores key/value pairs.

```mc
var m = map<string, i32>{"one": 1, "two": 2};
```

## Heap-Owned Types

Postfix `^` in a type means heap-owned.

```mc
type Point = { x: i32, y: i32 }

let p: Point^ = ^Point { x: 1, y: 2 };
let a: i32[]^ = ^[1, 2, 3];
```

`A^` and `A` are different types.

## Function Types

```mc
type BinOp = fn(i32, i32) -> i32
```

## Union Types

Machina supports unions in type position.

```mc
type ParseError = { message: string }

fn parse(input: string) -> i32 | ParseError {
    // ...
}
```

Unions are used heavily for error handling.

## Refinement Types

```mc
type Index = range(10)      // 0..9
type Small = range(2, 8)    // 2..7
```

```mc
let i: range(10) = 5;      // ok
// let bad: range(10) = 20; // compile-time error for literal
```

## Inference

Type inference is local and constraint-based.

```mc
let x = 42;                        // i32
let arr = [1, 2, 3];               // i32[3]
let p = Point { x: 0, y: 0 };      // Point
```

Add annotations where intent is unclear or API boundaries need explicit types.
