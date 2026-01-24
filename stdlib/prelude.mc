// Runtime externs: implemented in runtime/*
@[intrinsic, link_name("__rt_print")]
fn __rt_print(s: string, newline: u64);

@[intrinsic, link_name("__rt_u64_to_dec")]
fn __rt_u64_to_dec(inout buf: u8[], value: u64) -> u64;

@[intrinsic, link_name("__rt_memset")]
fn __rt_memset(inout buf: u8[], value: u8);

@[intrinsic, link_name("__rt_string_from_bytes")]
fn __rt_string_from_bytes(inout dst: string, bytes: u8[]);

fn __rt_alloc(size: u64, align: u64) -> u64;
fn __rt_realloc(ptr: u64, size: u64, align: u64) -> u64;
fn __rt_free(ptr: u64);

// String helpers
fn string_from_bytes(bytes: u8[]) -> string {
  var s = "";
  __rt_string_from_bytes(inout s, bytes);
  s
}

string :: {
  @[intrinsic, link_name("__rt_string_append_bytes")]
  fn append(inout self, other: string);

  @[intrinsic, link_name("__rt_string_append_bytes")]
  fn append_bytes(inout self, bytes: u8[]);
}

// Stdlib wrappers
fn print(s: string) {
  __rt_print(s, 0);
}

fn println(s: string) {
  __rt_print(s, 1);
}

fn println() {
  __rt_print("", 1);
}

// Stdlib wrappers for u64
fn print(value: u64) {
  var buf = u8[0; 32];
  let len = __rt_u64_to_dec(inout buf[..], value);
  let s = string_from_bytes(buf[..len]);
  __rt_print(s, 0);
}

fn println(value: u64) {
  var buf = u8[0; 32];
  let len = __rt_u64_to_dec(inout buf[..], value);
  let s = string_from_bytes(buf[..len]);
  __rt_print(s, 1);
}
