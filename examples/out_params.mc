requires {
    std.io as io
}

type Pair = { x: u64, y: u64 }
type Point = { x: u64, y: u64 }
type Boxed = { p: ^Point }

fn make_pair(out p: Pair) {
    p = Pair { x: 1, y: 2 };
}

fn make_boxed_inner(out b: Boxed) {
    b = Boxed { p: ^Point { x: 3, y: 4 } };
}

fn make_boxed(out b: Boxed) {
    make_boxed_inner(out b);
    b = Boxed { p: ^Point { x: 5, y: 6 } };
}

fn main() {
    var p: Pair;
    make_pair(out p);
    let sum = p.x + p.y;
    io.println(f"sum = {sum}");

    var b: Boxed;
    make_boxed(out b);
    io.println(f"boxed: b.p.x = {b.p.x}, b.p.y = {b.p.y}");
}
