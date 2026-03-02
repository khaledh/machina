use super::lower_and_optimize;
use indoc::indoc;

#[test]
fn test_local_memcpy_elim() {
    let text = lower_and_optimize(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main() -> u64 {
            var a = Pair { a: 1, b: 2 };
            var b = a;
            b.a
        }
    "});

    assert!(!text.contains("__rt_memcpy"));
}

#[test]
fn test_local_memcpy_elim_blocks_on_late_write() {
    let text = lower_and_optimize(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main() -> u64 {
            var a = Pair { a: 1, b: 2 };
            var b = a;
            a.a = 3;
            b.a
        }
    "});

    assert!(!text.contains("__rt_memcpy"));
}
