requires {
    std::io::println
    std::io as io
}

fn main() {
    var s = "hello";
    s.append(" world");

    let bytes = u8[33];
    s.append_bytes(bytes);

    println(s);
    println(f"owned = {s}");
}
