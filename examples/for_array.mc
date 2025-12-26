// For-loop over an array

fn main() -> u64 {
    let arr = [1, 2, 3, 4];
    var acc = 0;
    for x in arr {
        acc = acc + x;
    }
    acc
}
