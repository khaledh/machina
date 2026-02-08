requires {
    std.io as io
}

type Point = { x: u64, y: u64 }
type Msg = Ping(u64) | Pong(u64)

Point :: {
    prop sum: u64 {
        get { self.x + self.y }
    }

    prop y_val: u64 {
        get { self.y }
        set(v) { self.y = v; }
    }
}

Msg :: {
    prop id: u64 {
        get {
            match self {
                Msg::Ping(v) => v,
                Msg::Pong(v) => v,
            }
        }
    }
}

fn main() {
    var p = Point { x: 1, y: 2 };

    p.y_val = 5;
    io.println(f"y: {p.y_val}");

    io.println(f"sum: {p.sum}");

    let msg = Msg::Pong(9);
    io.println(f"id: {msg.id}");
}
