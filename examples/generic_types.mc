// Generic type definitions + literals.

type Pair<T> = { left: T, right: T }
type Option<T> = None | Some(T)

fn main() -> () {
    let p = Pair<u64> { left: 1, right: 2 };
    println(f"sum = {p.left + p.right}");

    let opt_int = Option<u64>::Some(3);
    match opt_int {
        Option<u64>::Some(x) => println(f"opt = Some({x})"),
        Option<u64>::None => println("opt = None"),
    };

    let opt_bool = Option<bool>::Some(true);
    match opt_bool {
        Option<bool>::Some(b) => if b {
          println("opt = Some(true)")
        } else {
          println("opt = Some(false)")
        },
        Option<bool>::None => println("opt = None"),
    };
}
