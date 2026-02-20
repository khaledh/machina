requires {
    std::io::println
}

type Point = {
    x: u64,
    y: u64,
}

fn main() {
    let p = Point { x: 1, y: 2 };

    println(type_of(p));
    println(type_of(42));
    println(type_of("hello"));
    println(type_of([1, 2, 3]));
}
