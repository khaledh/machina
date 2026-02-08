requires {
    std::io as io
}

// Demonstrate function overload resolution.
type Small = u64: bounds(0, 10);

fn id(x: u64) -> u64 {
    x
}

fn id(x: Small) -> u64 {
    let y: u64 = x;
    y + 100
}

fn main() {
    let a: Small = 5;
    let b = 20;

    io::println(id(a));
    io::println(id(b));
}
