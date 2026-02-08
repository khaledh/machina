// Runtime externs: implemented in runtime/*
@[runtime]
fn __rt_print(s: string, newline: u64);

@[runtime]
fn __rt_u64_to_dec(inout buf: u8[], value: u64) -> u64;

@[runtime]
fn __rt_memset(inout buf: u8[], value: u8);

@[runtime]
fn __rt_string_from_bytes(out dst: string, bytes: u8[]);

string :: {
  @[intrinsic]
  prop len: u64 {
    get;
  }

  @[runtime]
  fn append(inout self, other: string);

  @[runtime]
  fn append_bytes(inout self, bytes: u8[]);
}
