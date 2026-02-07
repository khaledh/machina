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

fn main() -> u64 {
    let ok_case = choose(true, 42);
    let err_case = choose(false, 42);

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
