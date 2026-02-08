requires {
    std::io::println
}

fn sum(xs: u64[]) -> u64 {
    var i = 0;
    var acc = 0;
    while i < xs.len {
        acc = acc + xs[i];
        i = i + 1;
    }
    acc
}

fn main() {
    var arr: u64[*] = [1, 2, 3];
    arr.append(4);

    let total = sum(arr);
    println(f"total = {total}");
    println(f"len = {arr.len}");
    println(f"cap = {arr.capacity}");
    println(f"arr[3] = {arr[3]}");

    let mid = arr[1..3];

    // Base array cannot be mutated while a borrowed slice is live.
    // (The borrow remains live until the slice's last use.)
    // arr.append(5); // Compile error: Slice borrow conflicts with mutation

    println(f"mid[0] = {mid[0]}");
    println(f"mid[1] = {mid[1]}");

    // Mutation is allowed after the slice's last use (flow-sensitive release).
    arr.append(5);
    println(f"arr[4] = {arr[4]}");
}
