// Runtime externs: implemented in runtime/*
@[runtime]
fn __rt_print(s: string, newline: u64);

@[runtime]
fn __rt_u64_to_dec(inout buf: u8[], value: u64) -> u64;

@[runtime]
fn __rt_memset(inout buf: u8[], value: u8);

@[runtime]
fn __rt_string_from_bytes(inout dst: string, bytes: u8[]);

fn __rt_alloc(size: u64, align: u64) -> u64;
fn __rt_realloc(ptr: u64, size: u64, align: u64) -> u64;
fn __rt_free(ptr: u64);

// String helpers
fn string_from_bytes(bytes: u8[]) -> string;

string :: {
  @[intrinsic]
  fn len(self) -> u64;

  @[runtime]
  fn append(inout self, other: string);

  @[runtime]
  fn append_bytes(inout self, bytes: u8[]);
}

// Stdlib wrappers
fn print(s: string);
fn println(s: string);
fn println();

// Stdlib wrappers for u64
fn print(value: u64);
fn println(value: u64);
