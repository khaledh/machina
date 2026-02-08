requires {
    std::io as io
}

// Print integers

fn main() {
    let x = 42;
    io::print("x = ");
    io::println(x);
}
