fn main() {
    let a = [1, 2, 3];
    let b = move a;

    println(b[0]);
    // println(a[0]); // ERROR: use after move
}
