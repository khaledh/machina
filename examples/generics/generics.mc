requires {
    std::io::println
}

// Generic functions.

fn id<T>(x: T) -> T {
    x
}

type Pair<L, R> = {
    left: L,
    right: R,
}

fn pair<L, R>(left: L, right: R) -> Pair<L, R> {
    Pair { left, right }
}

fn main() {
    println(f"id type = {type_of(id)}");
    println(f"pair type = {type_of(pair)}");

    let n = id(42);
    println(f"n = {n}, type = {type_of(n)}");

    let n64: u64 = id(42);
    println(f"n64 = {n64}, type = {type_of(n64)}");

    let b = id(false);
    println(f"b = {b}, type = {type_of(b)}");

    let s = id("machina");
    println(f"s = {s}, type = {type_of(s)}");

    let p = pair("age", 7);
    println(f"p type = {type_of(p)}");
    println(f"p.left type = {type_of(p.left)}");
    println(f"p.right type = {type_of(p.right)}");

    let arr = id([1, 2, 3]);
    println(f"arr type = {type_of(arr)}");
}
