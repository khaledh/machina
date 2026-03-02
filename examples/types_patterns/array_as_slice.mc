// Pass an array as a slice

fn sum(xs: u64[]) -> u64 {
    var acc = 0;
    for x in xs {
        acc += x;
    }
    acc
}

fn main() {
    let arr = [1, 2, 3];
    let total = sum(arr);
    println(total);
}
