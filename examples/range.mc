// Range type

type MidRange = u64: bounds(50, 100);

fn take_bounded_int(x: MidRange) {
    println(x);
}

fn main() {
    let small: u64: bounds(10) = 3;
    let mid: MidRange = 75;
    let midhigh: MidRange = 99;

    // let bad1: MidRange = 42; // Compile error: Value out of range: 42 not in range [50,100)
    // take_bounded_int(42); // Compile error: Value out of range: 42 not in range [50,100)

    let x = 42;
    // let bad2: MidRange = x; // Compile error: Value out of range: 42 not in range [50,100)
    // take_bounded_int(x); // Compile error: Value out of range: 42 not in range [50,100)

    if mid < midhigh {
        println(mid);
    } else {
        println(midhigh);
    }
}
