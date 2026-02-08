requires {
    std.io as io
}

// Generic functions.

fn id<T>(x: T) -> T {
    x
}

fn main() -> bool {
    let value = id(42);
    io.println(value);

    let ok = id(false);
    return ok;
}
