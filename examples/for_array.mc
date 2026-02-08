requires {
    std::io::println
    std::io as io
}

// For-loop over an array

fn main() {
    let arr = [1, 2, 3, 4];
    var acc = 0;
    for x in arr {
        acc = acc + x;
    }
    println(acc);
}
