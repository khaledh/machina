// Demonstrate function overload resolution.
type Small = range(0, 10);

fn id(x: u64) -> u64 {
    x
}

fn id(x: Small) -> u64 {
    let y: u64 = x;
    y + 100
}

fn main() -> u64 {
    let a: Small = 5;
    let b = 20;

    println(id(a));
    println(id(b));
    0
}
