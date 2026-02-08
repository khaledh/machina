requires {
    std::io::println
    std::io as io
}

// Slice expressions with optional bounds
fn consume_slice(_s: u64[]) -> u64 {
    0
}

fn main() {
    let arr = [10, 20, 30, 40, 50];

    // Slices with optional bounds
    let mid: u64[] = arr[1..4];
    let head = arr[..2];
    let tail = arr[3..];
    let all = arr[..];

    // Slices with function calls (no slice returns in v1)
    consume_slice(mid);
    consume_slice(head);
    consume_slice(tail);
    consume_slice(all);

    // Two-dimensional array slices
    let arr2d = [[1, 2, 3], [4, 5, 6]]; //       (array of u64[3] arrays)
    let s_rows = arr2d[0..2];  // type: u64[3][] (slice of u64[3] arrays)
    let row = s_rows[0];       // type: u64[3]   (array of u64 elements)
    let s_elems = row[0..3];   // type: u64[]    (slice of u64 elements)
    let s_elem = s_elems[0];   // type: u64      (element)
    println(f"s_elem={s_elem}");
}
