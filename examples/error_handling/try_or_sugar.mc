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
    from_io
        ? read_io(flag, value)
        : read_parse(flag, value)
}

// Sugar form: `or { ... }` -> `or |_| { ... }`
fn recover_with_block(flag: bool, from_io: bool) -> u64 {
    read(flag, from_io, 42) or {
        0
    }
}

// Sugar form: arm-style `or { ... => ... }`
// desugars to `or |err| { match err { ... } }`.
fn recover_with_arms(flag: bool, from_io: bool) -> u64 {
    read(flag, from_io, 42) or {
        value: u64 => value,
        io: IoError => io.code + 100,
        parse: ParseError => parse.line + 200,
    }
}

fn main() {
    println(f"block(ok) = {recover_with_block(true, true)}");
    println(f"block(err) = {recover_with_block(false, true)}");

    println(f"arms(ok) = {recover_with_arms(true, true)}");
    println(f"arms(io) = {recover_with_arms(false, true)}");
    println(f"arms(parse) = {recover_with_arms(false, false)}");
}
