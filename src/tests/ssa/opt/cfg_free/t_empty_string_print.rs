use super::lower_and_optimize;

#[test]
fn test_empty_string_print_peephole() {
    let src = r#"
@[runtime]
fn __rt_print(s: string, newline: u64);

fn main() {
  var s = "";
  __rt_print(s, 1);
}
"#;
    let ir = lower_and_optimize(src);

    assert!(ir.contains("__rt_print"));
    assert!(!ir.contains("__rt_string_drop"));
}
