requires {
    std.io as io
}

// Numeric literal bases and digit grouping

fn main() {
    let dec = 42;
    let bin = 0b1010_0110;
    let oct = 0o52;
    let hex = 0x2a;
    let grouped = 1_000_000;

    io.println(f"dec = {dec}");
    io.println(dec);
    io.println(f"bin = {bin}");
    io.println(f"oct = {oct}");
    io.println(f"hex = {hex}");
    io.println(f"grouped = {grouped}");
}
