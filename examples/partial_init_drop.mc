requires {
    std.io as io
}

type Point = { x: u64, y: u64 }
type Boxed = { p: ^Point }
type Pair = { a: Boxed, b: Boxed }

fn fill_a(out a: Boxed) {
    a = Boxed { p: ^Point { x: 1, y: 2 } };
}

fn fill_b(out b: Boxed) {
    b = Boxed { p: ^Point { x: 3, y: 4 } };
}

fn main() {
    var p: Pair;
    fill_a(out p.a);
    fill_b(out p.b);
    io.println(p.a.p.x + p.b.p.x);
}
