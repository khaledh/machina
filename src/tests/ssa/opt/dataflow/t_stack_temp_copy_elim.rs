use super::lower_and_optimize;
use indoc::indoc;

#[test]
fn test_stack_temp_copy_elim() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u8 {
            var a = u8[0; 8];
            var b = a;
            b[0]
        }
    "});

    assert!(!text.contains("__rt_memcpy"));
}
