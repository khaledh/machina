// String helpers
fn string_from_bytes(bytes: u8[]) -> string {
  var s: string;
  __rt_string_from_bytes(out s, bytes);
  s
}

fn string_trim_is_whitespace(b: u8) -> bool {
  if b == 32 {
    true
  } else if b == 10 {
    true
  } else if b == 9 {
    true
  } else if b == 13 {
    true
  } else {
    false
  }
}

@link_name("string_trim_impl")
fn string_trim_impl(s: string) -> string {
  var start: u64 = 0;
  while start < s.len && string_trim_is_whitespace(s[start]) {
    start += 1;
  }

  var end: u64 = s.len;
  while end > start && string_trim_is_whitespace(s[end - 1]) {
    end -= 1;
  }

  var result: string = "";
  result.append_bytes(s[start..end]);
  result
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
