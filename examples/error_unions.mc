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

fn add_one(flag: bool, value: u64) -> u64 | IoError {
    let base = choose(flag, value)?;
    base + 1
}

fn main() -> u64 {
    let ok_case = add_one(true, 42);
    let err_case = add_one(false, 42);

    let left = match ok_case {
        value: u64 => value,
        err: IoError => err.code,
    };

    let right = match err_case {
        value: u64 => value,
        err: IoError => err.code,
    };

    left + right
}
