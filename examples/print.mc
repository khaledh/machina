// Print strings (literal and via helper functions)

fn main() -> u64 {
    output(greeting());
    output("Machina!");
    println();
    0
}

fn greeting() -> string {
    "Hello, "
}

fn output(s: string) {
    print(s);
}
