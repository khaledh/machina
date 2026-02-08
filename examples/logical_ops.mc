requires {
    std.io as io
}

// Logical operators (&&, ||, !)

fn side() -> bool {
    io.println("side");
    true
}

fn main() {
    let a = false && side(); // short-circuit, side not called
    let b = true || side();  // short-circuit, side not called
    let c = !false && true;
    if a || b || c {
        io.println("logical ops ok");
    } else {
        io.println("logical ops failed");
    }
}
