requires {
    std::io as io
}

fn update(inout arr: u64[3]) {
    arr[0] = 10;
}

fn main() {
    var arr = u64[1, 2, 3];
    update(inout arr);
    io::println(arr[0]);
}
