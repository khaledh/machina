requires {
    std::io as io
}

// Print strings

fn main() {
    let s = greeting();
    io::print(s);
    output("Machina!");
}

fn greeting() -> string {
    "Hello, "
}

fn output(s: string) {
    io::println(s);
}
