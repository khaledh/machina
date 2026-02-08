requires {
    std::io::println
    std::io as io
}

// Demonstrate generic methods on non-generic types.
type Boxed = {
    value: u64
}

Boxed::{
    fn cast<T>(self, x: T) -> T {
        x
    }
}

fn main() {
    let b1 = Boxed { value: 1 };
    let b2 = Boxed { value: 2 };

    let a = b1.cast(42);
    println(f"a = {a}");

    let b = b2.cast(false);
    println(f"b = {b}");
}
