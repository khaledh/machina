// Index out of bounds runtime check

fn main() -> u64 {
    let a = [1, 2, 3];
    // a[5] // Runtime error: Index out of bounds: index=5, len=3
}
