// Range type

type MidRange = u64: bounds(50, 100);
type Delta = i8: bounds(-10, 10);

fn take_bounded_int(x: MidRange) {
    println(x);
}

fn make_int() -> i8 { 11 }

fn main() {
    let small: u64: bounds(10) = 3;
    let mid: MidRange = 75;
    let midhigh: MidRange = 99;

    // let bad1: MidRange = 42; // Compile error: Value out of range: 42 not in range [50,100)
    // take_bounded_int(42); // Compile error: Value out of range: 42 not in range [50,100)

    let x = 42;
    // let bad2: MidRange = x; // Compile error: Value out of range: 42 not in range [50,100)
    // take_bounded_int(x); // Compile error: Value out of range: 42 not in range [50,100)

    let delta_pos: Delta = 9;
    let delta_neg: Delta = -10;
    println(f"delta_pos: {delta_pos}, delta_neg: {delta_neg}");

    // let bad_delta_pos: Delta = 11; // Compile error: Value out of range: 11 not in range [-10,10)
    // let bad_delta_neg: Delta = -11; // Compile error: Value out of range: -11 not in range [-10,10)

    // let delta: Delta = make_int(); // Runtime error: Value out of range: value=11, min(incl)=-10, max(excl)=10

    if mid < midhigh {
        println(f"mid: {mid}");
    } else {
        println(f"midhigh: {midhigh}");
    }
}
