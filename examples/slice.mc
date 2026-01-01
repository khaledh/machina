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
}
