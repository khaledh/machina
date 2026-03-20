// String helpers
fn string_from_bytes(bytes: u8[]) -> string {
  var s: string;
  __rt_string_from_bytes(out s, bytes);
  s
}

@link_name("string_contains_impl")
fn string_contains_impl(haystack: string, needle: string) -> bool {
  if needle.len == 0 {
    return true;
  };
  if needle.len > haystack.len {
    return false;
  };

  var i: u64 = 0;
  while i + needle.len <= haystack.len {
    var matched: bool = true;
    var j: u64 = 0;
    while j < needle.len {
      if haystack[i + j] != needle[j] {
        matched = false;
        j = needle.len;
      } else {
        j += 1;
      }
    }
    if matched {
      return true;
    };
    i += 1;
  }

  false
}
