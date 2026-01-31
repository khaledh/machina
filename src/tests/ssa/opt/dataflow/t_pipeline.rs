use super::lower_and_optimize;
use super::lower_and_optimize_all;
use indoc::indoc;

#[test]
fn test_pipeline_elims_param_and_local_copies() {
    let text = lower_and_optimize(indoc! {"
        type Pair = { x: u64, y: u64 }

        fn main(p: Pair) -> u64 {
            var a = p;
            var b = a;
            b.x
        }
    "});

    assert!(!text.contains("__rt_memcpy"));
}

#[test]
fn test_pipeline_exact_ir_const_return() {
    let text = lower_and_optimize_all(indoc! {"
        fn main() -> u64 {
            var x: u64;
            x = 42;
            let y = x;
            y
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: u64
          bb0():
            %v0: ptr<u64> = addr_of %l0
            %v1: u64 = const 42:u64
            store %v0, %v1
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}
