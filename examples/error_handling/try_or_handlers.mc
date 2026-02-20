requires {
    std::io::println
}

type IoError = {
    code: u64,
}

type ParseError = {
    line: u64,
}

fn read_io(flag: bool, value: u64) -> u64 | IoError | ParseError {
    flag ? value : IoError { code: 7 }
}

fn read_parse(flag: bool, value: u64) -> u64 | IoError | ParseError {
    flag ? value : ParseError { line: 11 }
}

fn read(flag: bool, from_io: bool, value: u64) -> u64 | IoError | ParseError {
    from_io ? read_io(flag, value) : read_parse(flag, value)
}

// Handler ignores the concrete error kind and returns a fallback value.
fn recover_any(flag: bool, from_io: bool) -> u64 {
    read(flag, from_io, 42) or |err| {
        err;
        0
    }
}

// Handler explicitly matches error variants and recovers differently per kind.
fn recover_by_kind(flag: bool, from_io: bool) -> u64 {
    read(flag, from_io, 42) or |err| {
        match err {
            value: u64 => value,
            io: IoError => io.code + 100,
            parse: ParseError => parse.line + 200,
        }
    }
}

fn main() {
    let ok = recover_any(true, true);
    let io_fallback = recover_by_kind(false, true);
    let parse_fallback = recover_by_kind(false, false);

    println(f"ok = {ok}");
    println(f"io_fallback = {io_fallback}");
    println(f"parse_fallback = {parse_fallback}");
}
