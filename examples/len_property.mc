fn main() {
    let arr = [1, 2, 3, 4];
    let slice = arr[1..3];
    let s = "hello";

    println(f"arr len: {arr.len}");
    println(f"slice len: {slice.len}");
    println(f"string len: {s.len}");

    var i = 0;
    while i < arr.len {
        println(f"[while] arr[{i}]: {arr[i]}");
        i = i + 1;
    }

    for i in 0..arr.len {
        println(f"[for] arr[{i}]: {arr[i]}");
    }
}
