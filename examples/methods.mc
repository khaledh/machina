type Point = { x: u64, y: u64 }
type Boxed = { p: ^Point }

Boxed :: {
    fn sum(self) -> u64 {
        self.p.x + self.p.y
    }

    fn shift(inout self, dx: u64, dy: u64) {
        self.p.x = self.p.x + dx;
        self.p.y = self.p.y + dy;
    }

    fn consume(sink self) -> u64 {
        self.p.x + self.p.y
    }
}

fn main() {
    var b = Boxed { p: ^Point { x: 1, y: 2 } };
    println(f"sum: {b.sum()}");

    b.shift(10, 20);
    println(f"shifted: {b.sum()}");

    let total = b.consume();
    println(f"consumed: {total}");

    // b.shift(10, 20);  // ERROR: use after move
}
