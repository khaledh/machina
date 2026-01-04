// Runtime externs: implemented in runtime/*
fn __mc_print(s: string, newline: u64);
fn __mc_u64_to_dec(inout buf: u8[], value: u64) -> u64;
fn __mc_memset(inout buf: u8[], value: u8);
fn __mc_string_from_bytes(inout dst: string, bytes: u8[]);
fn __mc_alloc(size: u64, align: u64) -> u64;
fn __mc_realloc(ptr: u64, size: u64, align: u64) -> u64;
fn __mc_free(ptr: u64);

// String helpers
fn string_from_bytes(bytes: u8[]) -> string {
  var s = "";
  __mc_string_from_bytes(inout s, bytes);
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
  var buf = u8[0; 32];
  let len = __mc_u64_to_dec(inout buf[..], value);
  let s = string_from_bytes(buf[..len]);
  __mc_print(s, 0);
}

fn println(value: u64) {
  var buf = u8[0; 32];
  let len = __mc_u64_to_dec(inout buf[..], value);
  let s = string_from_bytes(buf[..len]);
  __mc_print(s, 1);
}
