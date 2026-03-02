use super::assert_ir_eq;
use super::lower_and_optimize;
use indoc::indoc;

#[test]
fn test_const_fold_binop() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u64 {
            let x = 1 + 2;
            x
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: u64 = const 3:u64
            ret %v2
        }
    "};
    assert_ir_eq(text, expected);
}

#[test]
fn test_const_fold_cond_br() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u64 {
            if true { 1 } else { 2 }
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: bool = const true
            br bb1

          bb1():
            %v2: u64 = const 1:u64
            br bb3(%v2)

          bb2():
            %v3: u64 = const 2:u64
            br bb3(%v3)

          bb3(%v1: u64):
            ret %v1
        }
    "};
    assert_ir_eq(text, expected);
}

#[test]
fn test_const_fold_cross_block_param() {
    let text = lower_and_optimize(indoc! {"
        fn pick(flag: bool) -> u64 {
            let x = if flag { 1 } else { 1 };
            x + 2
        }
    "});

    let expected = indoc! {"
        fn pick(bool) -> u64 {
          bb0(%v0: bool):
            cbr %v0, bb1, bb2

          bb1():
            %v3: u64 = const 1:u64
            br bb3(%v3, %v0)

          bb2():
            %v4: u64 = const 1:u64
            br bb3(%v4, %v0)

          bb3(%v1: u64, %v2: bool):
            %v5: u64 = const 2:u64
            %v6: u64 = const 3:u64
            ret %v6
        }
    "};
    assert_ir_eq(text, expected);
}
