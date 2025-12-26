// Range check runtime error

type MidRange = range(50, 100);

fn main() -> u64 {
    let x = 42;
    let y: MidRange = x;
    y
}
