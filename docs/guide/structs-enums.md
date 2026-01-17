# Structs and Enums

Machina supports user-defined types through structs (product types) and enums
(sum types).

## Structs

Structs group related data with named fields.

### Defining Structs

```
type Point = { x: u64, y: u64 }

type Person = {
    name: string,
    age: u64,
    active: bool,
}
```

### Creating Struct Values

Use struct literals to create values:

```
let origin = Point { x: 0, y: 0 };
let alice = Person { name: "Alice", age: 30, active: true };
```

### Accessing Fields

Use dot notation to access fields:

```
let px = origin.x;
let name = alice.name;
```

### Mutating Fields

For mutable variables, fields can be assigned:

```
var p = Point { x: 0, y: 0 };
p.x = 10;
p.y = 20;
```

### Struct Update Syntax

Create a new struct based on an existing one, overriding some fields:

```
let p1 = Point { x: 10, y: 20 };
let p2 = { p1 | x: 30 };    // p2 has x=30, y=20
```

This creates a new value; the original is unchanged.

### Destructuring

Extract fields into separate bindings:

```
let Point { x, y } = origin;
// x and y are now separate variables
```

Destructure inside function bodies:

```
fn distance(p: Point) -> u64 {
    let Point { x, y } = p;
    x + y
}
```

## Enums

Enums define types with multiple variants. Each value is exactly one variant.

### Simple Enums

Variants without data:

```
type Color = Red | Green | Blue
type Direction = North | South | East | West
```

Create values using the variant name with the type prefix:

```
let c = Color::Red;
let d = Direction::North;
```

### Enums with Payloads

Variants can carry data:

```
type Option = Some(u64) | None

type Shape = Circle(u64) | Rect(u64, u64)

type Result = Ok(u64) | Err(string)
```

Create values with the payload:

```
let x = Option::Some(42);
let y = Option::None;
let circle = Shape::Circle(10);
let rect = Shape::Rect(20, 30);
```

### Nested Types

Enums can have complex payloads:

```
type Point = { x: u64, y: u64 }
type Shade = Light | Dark

type Color = Red(Shade) | Green(u64) | Blue(u64, Shade, bool)

let c = Color::Blue(255, Shade::Dark, true);
```

## Pattern Matching

Use `match` to work with enum variants.

### Matching Simple Enums

```
type Color = Red | Green | Blue

fn to_string(c: Color) -> string {
    match c {
        Color::Red => "red",
        Color::Green => "green",
        Color::Blue => "blue",
    }
}
```

### Extracting Payloads

Bind payload values to variables:

```
type Option = Some(u64) | None

fn unwrap_or(opt: Option, default: u64) -> u64 {
    match opt {
        Option::Some(value) => value,
        Option::None => default,
    }
}
```

```
type Shape = Circle(u64) | Rect(u64, u64)

fn area(s: Shape) -> u64 {
    match s {
        Shape::Circle(r) => r * r * 3,
        Shape::Rect(w, h) => w * h,
    }
}
```

### Wildcard Pattern

Use `_` to match any value without binding it:

```
fn is_some(opt: Option) -> bool {
    match opt {
        Option::Some(_) => true,
        Option::None => false,
    }
}
```

### Exhaustiveness

The compiler ensures all variants are handled:

```
type Color = Red | Green | Blue

fn to_num(c: Color) -> u64 {
    match c {
        Color::Red => 1,
        Color::Green => 2,
        // error: Blue not covered
    }
}
```

## Type Aliases

Create new names for existing types:

```
type Coordinate = u64
type Size = u64
type Name = string
```

Aliases are interchangeable with their underlying type:

```
type Size = u64

fn area(w: Size, h: Size) -> Size {
    w * h
}

let s: Size = 10;
let a = area(s, 20);    // u64 and Size are interchangeable
```

## Combining Structs and Enums

Structs and enums work together for complex data modeling:

```
type Point = { x: u64, y: u64 }
type Color = Red | Green | Blue

type ColoredPoint = {
    point: Point,
    color: Color,
}

type Geometry = Point(Point) | Line(Point, Point) | Triangle(Point, Point, Point)
```

Matching nested structures:

```
fn get_start(g: Geometry) -> Point {
    match g {
        Geometry::Point(p) => p,
        Geometry::Line(start, _) => start,
        Geometry::Triangle(a, _, _) => a,
    }
}
```
