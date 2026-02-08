requires {
    std.io as io
}

fn main() {
    let a = [1, 2, 3];
    let b = move a;

    io.println(b[0]);
    // io.println(a[0]); // ERROR: use after move
}
