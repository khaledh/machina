requires {
    std.io as io
}

// Generic type definitions + literals.

type Pair<T> = {
  left: T,
  right: T,
}

type Option<T>
  = Some(T)
  | None

fn make_some<T>(x: T) -> Option<T> {
    Some(x)
}

fn main() -> () {
    let p = Pair { left: 1, right: 2 };
    io.println(f"sum = {p.left + p.right}");

    let opt_int = Option::Some(3);
    match opt_int {
        Some(x) => io.println(f"opt = Some({x})"),
        None => io.println("opt = None"),
    };

    let opt_int2 = make_some(4);
    match opt_int2 {
        Some(x) => io.println(f"opt2 = Some({x})"),
        None => io.println("opt2 = None"),
    };

    let opt_bool = Option::Some(true);
    match opt_bool {
        Some(b) => if b {
          io.println("opt = Some(true)")
        } else {
          io.println("opt = Some(false)")
        },
        None => io.println("opt = None"),
    };
}
