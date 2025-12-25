// Range type demo

type MidRange = range(50, 100);

fn main() -> u64 {
    let small: range(10) = 3;
    let mid: MidRange = 75;
    let midhigh: MidRange = 99;
    // let bad: MidRange = 42; // out of range

    if mid < midhigh {
        1
    } else {
        0
    }
}
