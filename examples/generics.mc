// Generic functions.

fn id<T>(x: T) -> T {
    x
}

fn main() -> bool {
    let value = id(42);
    println(value);

    let ok = id(false);
    return ok;
}
