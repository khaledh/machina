type Point = { x: u64, y: u64 }

fn make_point(x: u64, y: u64) -> ^Point {
    ^Point { x: x, y: y }
}

fn consume_point(p: ^Point) -> u64 {
    0
}

fn main() -> u64 {
    let p = make_point(1, 2);
    let q = move p;
    consume_point(move q);

    var r = ^Point { x: 3, y: 4 };
    r = ^Point { x: 5, y: 6 };

    0
}
