// String helpers
fn string_from_bytes(bytes: u8[]) -> string {
  var s: string;
  __rt_string_from_bytes(out s, bytes);
  s
}
