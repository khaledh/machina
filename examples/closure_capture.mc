fn main() {
    var base = 10;
    let add = |x: u64| -> u64 {
        base + x
    };

    let a = add(5);
    let b = add(7);
    println(f"a={a}, b={b}");
}
