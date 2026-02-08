requires {
    std::io::println
    std::io as io
}

// Logical operators (&&, ||, !)

fn side() -> bool {
    println("side");
    true
}

fn main() {
    let a = false && side(); // short-circuit, side not called
    let b = true || side();  // short-circuit, side not called
    let c = !false && true;
    if a || b || c {
        println("logical ops ok");
    } else {
        println("logical ops failed");
    }
}
