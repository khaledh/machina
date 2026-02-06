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
    let p = Pair<u64> { left: 1, right: 2 };
    println(f"sum = {p.left + p.right}");

    let opt_int: Option<u64> = Some(3);
    match opt_int {
        Some(x) => println(f"opt = Some({x})"),
        None => println("opt = None"),
    };

    let opt_int2 = make_some(4);
    match opt_int2 {
        Some(x) => println(f"opt2 = Some({x})"),
        None => println("opt2 = None"),
    };

    let opt_bool: Option<bool> = Some(true);
    match opt_bool {
        Some(b) => if b {
          println("opt = Some(true)")
        } else {
          println("opt = Some(false)")
        },
        None => println("opt = None"),
    };
}
