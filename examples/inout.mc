fn update(inout arr: u64[3]) {
    arr[0] = 10;
}

fn main() {
    var arr = u64[1, 2, 3];
    update(arr);
    println(arr[0]);
}
