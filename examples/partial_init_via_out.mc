type Pair = { a: u64[2], b: u64[2] }

fn fill_a(out a: u64[2]) {
    a = u64[1, 2];
}

fn fill_b(out b: u64[2]) {
    b = u64[3, 4];
}

fn main() {
    var p: Pair;
    fill_a(out p.a);
    fill_b(out p.b);
    println(p.a[0] + p.b[0]);
}
