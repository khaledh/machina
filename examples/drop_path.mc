type Point = { x: u64, y: u64 }

fn consume(sink p: ^Point) {
  // dropped at function exit
}

fn maybe_consume(flag: bool, sink p: ^Point) {
    // p starts as initialized (due to sink param)
    if flag {
        consume(move p); // clears the init flag (avoids dropping at scope exit)
        0
    } else {
        p.x = 1;
        0
    };
    // dropped at scope exit if init flag is still set
}

fn main() {
    let p = ^Point { x: 1, y: 2 };
    maybe_consume(false, move p);
}
