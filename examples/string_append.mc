requires {
    std.io as io
}

fn main() {
    var s = "hello";
    s.append(" world");

    let bytes = u8[33];
    s.append_bytes(bytes);

    io.println(s);
    io.println(f"owned = {s}");
}
