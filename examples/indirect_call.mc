requires {
    std.io as io
}

fn add(a: u64, b: u64) -> u64 {
    a + b
}

fn main() {
    let f_add: fn(u64, u64) -> u64 = add;
    let f_inc: fn(u64) -> u64 = |x: u64| -> u64 x + 1;

    let sum = f_add(2, 3);
    let next = f_inc(41);

    io.println(sum);
    io.println(next);
}
