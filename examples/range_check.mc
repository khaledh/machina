// Range check runtime error

type MidRange = u64: bounds(50, 100);

fn main() -> u64 {
    let x = 42;
    let y: MidRange = x;
    y
}
