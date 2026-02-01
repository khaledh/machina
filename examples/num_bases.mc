// Numeric literal bases and digit grouping

fn main() {
    let dec = 42;
    let bin = 0b1010_0110;
    let oct = 0o52;
    let hex = 0x2a;
    let grouped = 1_000_000;

    println(f"dec = {dec}");
    println(dec);
    println(f"bin = {bin}");
    println(f"oct = {oct}");
    println(f"hex = {hex}");
    println(f"grouped = {grouped}");
}
