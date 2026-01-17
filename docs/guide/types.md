# Types

Machina has a static type system with type inference. Types are checked at
compile time, and many type annotations can be omitted when they can be
inferred.

## Scalar Types

### Integers

Machina provides signed and unsigned integers in various sizes:

| Type | Size | Range |
|------|------|-------|
| `u8` | 8 bits | 0 to 255 |
| `u16` | 16 bits | 0 to 65,535 |
| `u32` | 32 bits | 0 to 4,294,967,295 |
| `u64` | 64 bits | 0 to 18,446,744,073,709,551,615 |
| `i8` | 8 bits | -128 to 127 |
| `i16` | 16 bits | -32,768 to 32,767 |
| `i32` | 32 bits | -2,147,483,648 to 2,147,483,647 |
| `i64` | 64 bits | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 |

```
let small: u8 = 255;
let big: u64 = 1_000_000;
let negative: i32 = -42;
```

### Numeric Literals

Integer literals can be written in multiple bases:

```
let decimal = 42;
let grouped = 1_000_000;     // underscores for readability
let binary = 0b1010_0110;    // binary (base 2)
let octal = 0o52;            // octal (base 8)
let hex = 0x2a;              // hexadecimal (base 16)
```

### Booleans

The `bool` type has two values: `true` and `false`.

```
let yes: bool = true;
let no = false;
```

### Characters

The `char` type represents a single character:

```
let letter: char = 'a';
let digit = '7';
```

### Unit

The unit type `()` represents the absence of a value. Functions that don't
return anything implicitly return `()`.

```
fn do_nothing() {
    // returns ()
}
```

You can also write `return;` to return `()` explicitly.

## Compound Types

### Arrays

Arrays are fixed-size sequences of elements of the same type. The type is
written as `T[N]` where `T` is the element type and `N` is the length.

```
let arr: u64[3] = [1, 2, 3];
let inferred = [10, 20, 30];      // type inferred as u64[3]
```

Repeat syntax creates an array with the same value:

```
let zeros = [0; 10];              // [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
let typed = u8[0; 32];            // 32 zeros of type u8
```

Multi-dimensional arrays:

```
let matrix: u64[2, 3] = [[1, 2, 3], [4, 5, 6]];
let elem = matrix[1, 2];          // 6
```

See [Arrays and Slices](arrays-slices.md) for indexing, slicing, and bounds
checking.

### Tuples

Tuples group values of potentially different types:

```
let pair: (u64, bool) = (42, true);
let triple = (1, "hello", false);
```

Access tuple elements by index:

```
let x = pair.0;    // 42
let y = pair.1;    // true
```

### Strings

String literals have the `string` type:

```
let greeting: string = "hello";
let name = "world";
```

See [Strings](strings.md) for formatted strings and string operations.

## User-Defined Types

### Type Aliases

Create a new name for an existing type:

```
type Size = u64
type Coordinate = i64
```

### Structs

Structs group named fields:

```
type Point = { x: u64, y: u64 }
type Person = { name: string, age: u64 }
```

### Enums

Enums define a type with multiple variants:

```
type Color = Red | Green | Blue
type Option = Some(u64) | None
type Shape = Circle(u64) | Rect(u64, u64)
```

See [Structs and Enums](structs-enums.md) for struct literals, destructuring,
and pattern matching.

## Heap Types

The `^T` type represents an owned heap allocation of type `T`:

```
type Point = { x: u64, y: u64 }

let p: ^Point = ^Point { x: 1, y: 2 };
```

Heap values are automatically dropped when they go out of scope. See
[Memory Safety](mem-safety.md) for ownership and move semantics.

## Slice Types

The `T[]` type represents a slice (non-owning view) into an array:

```
fn sum(xs: u64[]) -> u64 {
    var total = 0;
    for x in xs {
        total = total + x;
    }
    total
}
```

See [Arrays and Slices](arrays-slices.md) for slice creation and rules.

## Function Types

Function types describe the signature of a function:

```
type BinaryOp = fn(u64, u64) -> u64

fn apply(f: BinaryOp, a: u64, b: u64) -> u64 {
    f(a, b)
}
```

## Range Types

Range types represent bounded integers:

```
type Index = range(10)        // 0 to 9
type Small = range(2, 8)      // 2 to 7 (half-open)
```

Range bounds are non-negative. Assignments are checked at runtime, while
out-of-range literals are rejected at compile time:

```
let i: range(10) = 5;    // ok
let j: range(10) = 15;   // error: out of range
```

## Type Inference

Machina infers types when possible:

```
let x = 42;                // inferred as u64
let arr = [1, 2, 3];       // inferred as u64[3]
let p = Point { x: 0, y: 0 };  // inferred as Point
```

Type annotations are required for:
- Function parameters and return types (unless returning `()`)
- Closure parameters and return types (unless returning `()`)
- Ambiguous expressions
