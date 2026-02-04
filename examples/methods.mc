type Point = { x: u64, y: u64 }
type Boxed = { p: ^Point }
type Msg = Ping(u64) | Pong(u64)

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

Msg :: {
    fn is_ping(self) -> bool {
        match self {
            Msg::Ping(_) => true,
            _ => false,
        }
    }
}

fn main() {
    var b = Boxed { p: ^Point { x: 1, y: 2 } };
    println(f"sum: {b.sum()}");

    b.shift(10, 20);
    println(f"shifted: {b.sum()}");

    let total = b.consume();
    println(f"consumed: {total}");

    let msg = Msg::Ping(1);
    if msg.is_ping() {
        println("ping");
    } else {
        println("pong");
    }

    // b.shift(10, 20);  // ERROR: use after move
}
