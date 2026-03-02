// Indirect function calls

fn add(a: u64, b: u64) -> u64 {
    a + b
}

fn main() {
    let f_add: fn(u64, u64) -> u64 = add;

    let sum = f_add(2, 3);

    println(sum);
}
