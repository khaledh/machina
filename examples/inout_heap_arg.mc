requires {
    std.io as io
}

type Point = { x: u64, y: u64 }
type Box = { p1: ^Point, p2: ^Point }

fn update(inout p1: ^Point, inout p2: ^Point) {
  p1.x = 10;
  p1.y = 20;

  p2.x = 30;
  p2.y = 40;
}

fn main() {
    var b = Box {
      p1: ^Point { x: 1, y: 2 },
      p2: ^Point { x: 3, y: 4 },
    };

    update(inout b.p1, inout b.p2);

    io.println(f"b.p1 = {b.p1.x}, {b.p1.y}");
    io.println(f"b.p2 = {b.p2.x}, {b.p2.y}");
}
