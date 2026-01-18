// If/else blocks with optional else and else-if chains

fn classify(n: u64) -> u64 {
    if n == 0 {
        0
    } else if n < 10 {
        1
    } else {
        2
    }
}

fn max(a: u64, b: u64) -> u64 {
    let result = if a > b { a } else { b };
    result
}

fn main() {
    let a = classify(0);
    let b = classify(7);
    let c = classify(20);
    let m = max(10, 3);
    println(f"a={a}");
    println(f"b={b}");
    println(f"c={c}");
    println(f"m={m}");

    let debug = true;
    if debug {
        println("debug mode");
    }
}
