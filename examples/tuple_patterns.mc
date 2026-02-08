requires {
    std::io as io
}

type Flag = On | Off
type Wrap = Wrap(u64) | Empty

fn describe(t: (u64, Flag, bool)) -> u64 {
    match t {
        (0, Flag::Off, false) => 10,
        (1, Flag::On, true) => 20,
        (x, Flag::On, _) => x + 30,
        _ => 0,
    }
}

fn nested(t: (u64, (bool, Wrap))) -> u64 {
    match t {
        (x, (true, Wrap::Wrap(n))) => x + n,
        (_, (false, _)) => 0,
        (x, (_, Wrap::Empty)) => x,
        _ => 1,
    }
}

fn main() {
    let a = describe((1, Flag::On, true));
    let b = describe((5, Flag::On, false));
    let c = nested((3, (true, Wrap::Wrap(4))));
    let d = nested((7, (false, Wrap::Empty)));
    io::println(f"a={a}, b={b}, c={c}, d={d}");
}
