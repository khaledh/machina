requires {
    std::io::println
    std::io as io
}

// For-loop over a range

fn main() {
    var acc = 0;
    for i in 0..10 {
        acc = acc + i;
    }
    println(acc);
}
