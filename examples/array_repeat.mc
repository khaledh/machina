fn main() {
    // Repeat initializer at threshold (inlined stores).
    let small = u8[0; 16];
    println("initialized small array");

    // Repeat initializer above threshold (lowered to __mc_memset).
    let big = u8[0; 32];
    println("initialized big array");
}
