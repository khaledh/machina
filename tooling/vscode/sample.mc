// Sample Machina code to demonstrate syntax highlighting.

requires {
    std::io::println
}

// Attributes and fixed-layout types
@public
@layout(fixed, size: 24)
type Header = {
    magic: u64,
    flags: u64,
    next: vaddr?,
}

@public
type Point = {
    x: u64,
    y: u64,
}

@section("__DATA,__demo")
static var LAST_HEADER: view<Header>? = None;

type HeaderMissing = {}

@public
fn distance(p1: Point, p2: Point) -> u64 {
    let dx = if p1.x > p2.x { p1.x - p2.x } else { p2.x - p1.x };
    let dy = if p1.y > p2.y { p1.y - p2.y } else { p2.y - p1.y };
    dx + dy
}

@public
trait Drawable {
    fn draw(self);
}

// Enum types
type Color = Red | Green | Blue

// Linear type
@linear
type Connection = {
    addr: string,

    states { Disconnected, Connected }
    actions {
        connect: Disconnected -> Connected,
        disconnect: Connected -> Disconnected,
    }
}

Connection :: {
    fn connect(self) -> Connected {
        Connected { addr: self.addr }
    }

    fn disconnect(self) -> Disconnected {
        Disconnected { addr: self.addr }
    }
}

// Numbers and literals
fn test_literals() {
    let binary = 0b1010_1010;
    let octal = 0o755;
    let hex = 0xFF_00;
    let decimal = 42;
    let boolean = true;
    let nothing = None;
    let character = 'A';
    let text = "Hello, Machina!";
    let formatted = f"Value: {decimal}";
    nothing;
}

fn read_header(addr: vaddr) -> view<Header> | HeaderMissing {
    let header: view<Header> = unsafe {
        view_at(addr)
    };
    header
}

fn header_magic() -> u64 {
    let header = LAST_HEADER or {
        return 0;
    };

    header.magic
}

// Control flow
fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

// Loops
fn count_to_ten() {
    for i in 0..10 {
        println(f"Count: {i}");
    }

    var x = 0;
    while x < 10 {
        x += 1;
    }

    defer println("done counting");
}

// Main function
fn main() -> u64 {
    let p1 = Point { x: 10, y: 20 };
    let p2 = Point { x: 30, y: 40 };
    let dist = distance(p1, p2);
    println(f"Distance: {dist}");
    0
}
