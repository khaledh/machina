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

    println(f"r.x={r.x}, r.y={r.y}");

    let boxed = BoxedMsg { msg: ^Msg::Pong(7) };
    match boxed.msg {
        Msg::Ping(n) => println(f"Ping({n})"),
        Msg::Pong(n) => println(f"Pong({n})"),
    };

    let t = ^(10, 20);
    println(f"t.0={t.0}, t.1={t.1}");

    let arr = ^[1, 2, 3];
    println(f"arr[1]={arr[1]}");

    let hs = ^"hi";
    println(f"hs[1]={hs[1]}");

    let msg = ^Msg::Ping(42);
    match msg {
        Msg::Ping(n) => println(f"Ping({n})"),
        Msg::Pong(n) => println(f"Pong({n})"),
    };
}
