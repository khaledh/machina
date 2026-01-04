type Point = { x: u64, y: u64 }

fn sum_point(sink p: ^Point, q: ^Point) -> u64 {
    p.x + p.y + q.x + q.y
}

fn forward_sum(sink p: ^Point, q: ^Point) -> u64 {
    sum_point(move p, q)
}

fn main() {
    let p = ^Point { x: 1, y: 2 };
    let q = ^Point { x: 3, y: 4 };

    let total = forward_sum(move p, q);

    println(f"total: {total}");

    println(f"q.x: {q.x}"); // OK: non-sink param is a borrow
    // println(p.x); // ERROR: use after move into sink param
}
