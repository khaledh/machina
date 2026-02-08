requires {
    std::io::println
    std::io as io
}

// Custom types (type aliases, enums, and structs)

// Type Alias
type Coord = u64

type Shade = Light | Dark

// Enum
type Color = Red(Shade) | Green(u64) | Blue(u64, Shade, bool)

// Struct
type Point = {
  x: Coord,
  y: Coord,
  color: Color,
}

fn main() {
    // Struct literals
    let a = Point { x: 10, y: 20, color: Color::Red(Shade::Light) };
    let b = Point { x: 5, y: 40, color: Color::Blue(20, Shade::Dark, true) };

    // Function calls (pass and return by value)
    // (Optimized to pass by reference and RVO/NRVO internally)
    let c = scale(a, 2, 3);
    let d = change_color(b, Color::Blue(20, Shade::Dark, false));

    // Tuple destructuring
    let (dx, dy) = delta(c, d);

    if is_blue(d) {
        println(42);
    } else {
        println(21);
    }
}

fn scale(p: Point, dx: u64, dy: u64) -> Point {
    // Struct destructuring
    let Point { x, y, color } = p;

    // Struct update (creates a new struct value)
    { p | x: x * dx, y: y * dy }
}

fn change_color(p: Point, color: Color) -> Point {
    { p | color: color }
}

fn is_blue(p: Point) -> bool {
   // pattern matching on enum
    match p.color {
        Color::Blue(val, shade, b) => true,
        _ => false,
    }
}

fn delta(p1: Point, p2: Point) -> (u64, u64) {
    (p2.x - p1.x, p2.y - p1.y)
}
