requires {
    std::io::println
    std::io as io
}

type Option<T>
  = Some(T)
  | None

type Pair<L, R> = {
    left: L,
    right: R,
}

fn make_some<T>(value: T) -> Option<T> {
    Some(value)
}

fn pair<L, R>(left: L, right: R) -> Pair<L, R> {
    Pair { left: left, right: right }
}

fn choose<T>(primary: Option<T>, fallback: T) -> T {
    match primary {
        Some(value) => value,
        None => fallback,
    }
}

fn id<T>(x: T) -> T {
    x
}

fn apply<T, U>(f: fn(T) -> U, x: T) -> U {
    f(x)
}

fn main() {
    // Generic call return type flows into an unannotated let.
    let inferred = make_some(41);

    // Pattern payload `n` is inferred from Option<T> without annotations.
    let from_match = match inferred {
        Some(n) => n + 1,
        None => 0,
    };

    // Generic struct instantiation is inferred from arguments.
    let summary = pair("score", from_match);
    println(f"summary.left = {summary.left}");
    println(f"summary.right = {summary.right}");

    // Unconstrained integer literals default locally.
    let defaulted = id(123);
    println(f"defaulted = {defaulted}");

    // Generic return + fallback unify through local constraints.
    let chosen = choose(make_some(defaulted), 0);
    println(f"chosen = {chosen}");

    // Higher-order function inference.
    let lifted_int = apply(|n| n + 1, 41);
    println(f"lifted_int = {lifted_int}");

    let lifted_bool_str = apply(|b| if b { "true" } else { "false" }, true);
    println(f"lifted_bool_str = {lifted_bool_str}");

    // Higher-order typeless closure inference:
    // both `f` and `x` are inferred from call-site usage.
    let apply_fn = |f, x| f(x);
    let lifted = apply_fn(|n| n + 1, 41);
    println(f"lifted = {lifted}");
}
