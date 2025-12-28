// Range type

type MidRange = range(50, 100);

fn main() {
    let small: range(10) = 3;
    let mid: MidRange = 75;
    let midhigh: MidRange = 99;
    // let bad: MidRange = 42; // out of range

    if mid < midhigh {
        // println(mid);
        println("mid < midhigh");
    } else {
        // println(midhigh);
        println("mid >= midhigh");
    }
}
