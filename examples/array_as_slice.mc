requires {
    std::io as io
}

fn sum(xs: u64[]) -> u64 {
    var acc = 0;
    for x in xs {
        acc = acc + x;
    }
    acc
}

fn main() {
    let arr = [1, 2, 3];
    let total = sum(arr);
    io::println(total);
}
