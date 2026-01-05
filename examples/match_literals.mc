type Flag = On | Off

fn describe_flag(f: Flag) -> u64 {
    match f {
        Flag::On => 1,
        Flag::Off => 0,
    }
}

fn pick(x: u64) -> u64 {
    match x {
        0 => 10,
        1 => 20,
        _ => 99,
    }
}

fn bool_to_u64(b: bool) -> u64 {
    match b {
        true => 1,
        false => 0,
    }
}

fn main() {
    let a = pick(0);
    let b = pick(3);
    let c = bool_to_u64(true);
    let d = describe_flag(Flag::Off);
    println(f"a={a}, b={b}, c={c}, d={d}");
}
