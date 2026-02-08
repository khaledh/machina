requires {
    std.io as io
}

fn main() {
    let arr = [1, 2, 3, 4];
    let slice = arr[1..3];
    let s = "hello";

    io.println(f"arr len: {arr.len}");
    io.println(f"slice len: {slice.len}");
    io.println(f"string len: {s.len}");

    var i = 0;
    while i < arr.len {
        io.println(f"[while] arr[{i}]: {arr[i]}");
        i = i + 1;
    }

    for i in 0..arr.len {
        io.println(f"[for] arr[{i}]: {arr[i]}");
    }
}
