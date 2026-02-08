requires {
    std.io as io
}

// If/else blocks with optional else and else-if chains

fn classify(n: u64) -> string {
    if n == 0 {
        "zero"
    } else if n < 10 {
        "small"
    } else {
        "big"
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
    io.println(f"a={a}");
    io.println(f"b={b}");
    io.println(f"c={c}");
    io.println(f"m={m}");

    let debug = true;
    if debug {
        io.println("debug mode");
    }
}
