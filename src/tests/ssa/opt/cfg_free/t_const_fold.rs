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
    assert_eq!(text, expected);
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
    assert_eq!(text, expected);
}
