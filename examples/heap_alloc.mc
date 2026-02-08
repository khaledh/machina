requires {
    std::io as io
}

type Point = { x: u64, y: u64 }
type Msg = Ping(u64) | Pong(u64)
type BoxedMsg = { msg: ^Msg }

fn make_point(x: u64, y: u64) -> ^Point {
    ^Point { x: x, y: y }
}

fn consume_point(p: ^Point) -> u64 {
    0
}

fn main() {
    let p = make_point(1, 2);
    let q = p;
    consume_point(q);

    var r = ^Point { x: 3, y: 4 };
    r = ^Point { x: 5, y: 6 };

    io::println(f"r.x={r.x}, r.y={r.y}");

    let boxed = BoxedMsg { msg: ^Msg::Pong(7) };
    match boxed.msg {
        Msg::Ping(n) => io::println(f"Ping({n})"),
        Msg::Pong(n) => io::println(f"Pong({n})"),
    };

    let t = ^(10, 20);
    io::println(f"t.0={t.0}, t.1={t.1}");

    let arr = ^[1, 2, 3];
    io::println(f"arr[1]={arr[1]}");

    let hs = ^"hi";
    io::println(f"hs[1]={hs[1]}");

    let msg = ^Msg::Ping(42);
    match msg {
        Msg::Ping(n) => io::println(f"Ping({n})"),
        Msg::Pong(n) => io::println(f"Pong({n})"),
    };
}
