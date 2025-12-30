// Demonstrate copy-elision and the cases that prevent it.

fn use_arr(arr: u64[3]) -> u64 {
    arr[0]
}

fn use_slice(s: u64[]) -> u64 {
    s;
    0
}

// Copy should be elided due to last-use.
fn elide_simple() -> u64 {
    let a = u64[1, 2, 3];
    let b = a;
    b[0]
}

// Copy should be elided across blocks: b is only read after the branch.
fn elide_across_blocks(flag: bool) -> u64 {
    let a = u64[1, 2, 3];
    let b = a;
    if flag {
        b[0]
    } else {
        b[1]
    }
}

// Copy should NOT be elided: source is used after the copy.
fn no_elide_src_live() -> u64 {
    let a = u64[1, 2, 3];
    let b = a;
    a[0] + b[0]
}

// Copy should NOT be elided: aggregate call args are treated as addr-taken.
fn no_elide_addr_taken() -> u64 {
    let a = u64[1, 2, 3];
    let b = a;
    use_arr(b)
}

// Copy should NOT be elided: slice view keeps the source address alive.
fn no_elide_slice_view() -> u64 {
    let a = u64[1, 2, 3];
    let s = a[..];
    let b = a;
    use_slice(s)
}

fn main() {
    elide_simple();
    no_elide_src_live();
    elide_across_blocks(true);
    no_elide_addr_taken();
    no_elide_slice_view();
}
