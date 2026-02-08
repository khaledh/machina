requires {
    std::io as io
}

fn main() {
    let a: u64 = 42;
    let b: i64 = -7;
    io::println(f"a = {a}, b = {b}");
    io::println(f"next = {a + 1}");
    io::println(f"escaped = {{ and }}");
}
