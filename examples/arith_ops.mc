requires {
    std.io as io
}

// Arithmetic operators

fn main() {
    let a = 20;
    let b = 6;

    let add = a + b;
    let sub = a - b;
    let mul = a * b;
    let div = a / b;
    let rem = a % b;

    io.print("add = ");
    io.println(add);
    io.print("sub = ");
    io.println(sub);
    io.print("mul = ");
    io.println(mul);
    io.print("div = ");
    io.println(div);
    io.print("rem = ");
    io.println(rem);
}
