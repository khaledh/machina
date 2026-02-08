fn maybe_alloc(flag: bool) {
    var p: u64^;
    if flag {
        p = ^1;
        p = ^2;
    } else {
    }
}

fn main() {
    // Run with `--trace-alloc` to see conditional drops.
    maybe_alloc(true);
    maybe_alloc(false);
}
