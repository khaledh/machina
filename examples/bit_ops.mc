requires {
    std::io as io
}

// Bitwise operators

fn main() {
    let a = 0b1010;
    let b = 0b0110;

    let and_val = a & b;
    let or_val = a | b;
    let xor_val = a ^ b;
    let not_val = ~a;
    let shl_val = a << 2;
    let shr_val = a >> 1;

    io::println(f"and = {and_val}");
    io::println(f"or = {or_val}");
    io::println(f"xor = {xor_val}");
    io::println(f"not = {not_val}");
    io::println(f"shl = {shl_val}");
    io::println(f"shr = {shr_val}");
}
