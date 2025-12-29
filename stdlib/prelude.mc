// Runtime externs: implemented in runtime/*
fn __mc_print(s: string, newline: u64);
fn __mc_u64_to_dec(buf: u8[], value: u64) -> u64;
// NOTE: caller allocates the string value; the runtime mutates it in place.
// When `inout` lands this should use it.
fn __mc_string_from_bytes(dst: string, bytes: u8[]);

// String helpers
fn string_from_bytes(bytes: u8[]) -> string {
  var s = "";
  __mc_string_from_bytes(s, bytes);
  s
}

// Stdlib wrappers (string only for now)
fn print(s: string) {
  __mc_print(s, 0);
}

fn println(s: string) {
  __mc_print(s, 1);
}

fn println() {
  __mc_print("", 1);
}

// Stdlib wrappers for u64
fn print(value: u64) {
  var buf = u8[
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
  ];
  let len = __mc_u64_to_dec(buf[..], value);
  let s = string_from_bytes(buf[..len]);
  __mc_print(s, 0);
}

fn println(value: u64) {
  var buf = u8[
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
  ];
  let len = __mc_u64_to_dec(buf[..], value);
  let s = string_from_bytes(buf[..len]);
  __mc_print(s, 1);
}
