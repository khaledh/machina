// Bitwise operators

fn main() {
    let a = 10;
    let b = 6;

    let and_val = a & b;
    let or_val = a | b;
    let xor_val = a ^ b;
    let not_val = ~a;
    let shl_val = a << 2;
    let shr_val = a >> 1;

    print("and = ");
    println(and_val);
    print("or = ");
    println(or_val);
    print("xor = ");
    println(xor_val);
    print("not = ");
    println(not_val);
    print("shl = ");
    println(shl_val);
    print("shr = ");
    println(shr_val);
}
