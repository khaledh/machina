requires {
    std::io::println
    std::io as io
}

type IoError = {
    code: u64,
}

fn ok(value: u64) -> u64 | IoError {
    value
}

fn fail() -> u64 | IoError {
    IoError { code: 7 }
}

fn choose(flag: bool, value: u64) -> u64 | IoError {
    if flag {
        ok(value)
    } else {
        fail()
    }
}

// Demonstrates if-join lifting: success arm is plain u64, error arm is IoError.
fn choose_if_lift(flag: bool, value: u64) -> u64 | IoError {
    if flag {
        value
    } else {
        IoError { code: 70 }
    }
}

// Demonstrates match-join lifting with the same plain-success + error pattern.
fn choose_match_lift(flag: bool, value: u64) -> u64 | IoError {
    match flag {
        true => value,
        false => IoError { code: 71 },
    }
}

fn add_one(flag: bool, value: u64) -> u64 | IoError {
    let base = choose(flag, value)?;
    base + 1
}

fn main() {
    let ok_case = add_one(true, 42);
    let err_case = add_one(false, 42);

    let left = match ok_case {
        value: u64 => value,
        err: IoError => err.code,
    };
    println(f"left: {left}");

    let right = match err_case {
        value: u64 => value,
        err: IoError => err.code,
    };
    println(f"right: {right}");

    let lifted_if_ok = choose_if_lift(true, 10);
    let lifted_if_err = choose_if_lift(false, 10);
    let if_left = match lifted_if_ok {
        value: u64 => value,
        err: IoError => err.code,
    };
    let if_right = match lifted_if_err {
        value: u64 => value,
        err: IoError => err.code,
    };
    println(f"if_lift_ok: {if_left}");
    println(f"if_lift_err: {if_right}");

    let lifted_match_ok = choose_match_lift(true, 11);
    let lifted_match_err = choose_match_lift(false, 11);
    let match_left = match lifted_match_ok {
        value: u64 => value,
        err: IoError => err.code,
    };
    let match_right = match lifted_match_err {
        value: u64 => value,
        err: IoError => err.code,
    };
    println(f"match_lift_ok: {match_left}");
    println(f"match_lift_err: {match_right}");
}
