use super::lower_and_optimize;
use indoc::indoc;

#[test]
fn test_byref_copy_elim_reads_only() {
    let text = lower_and_optimize(indoc! {"
        type Pair = { x: u64, y: u64 }

        fn sum(p: Pair) -> u64 {
            p.x + p.y
        }
    "});

    assert!(!text.contains("__rt_memcpy"));
}

#[test]
fn test_byref_copy_elim_blocks_on_mutation() {
    let text = lower_and_optimize(indoc! {"
        type Pair = { x: u64, y: u64 }

        fn bump(p: Pair) -> u64 {
            var q = p;
            q.x = q.x + 1;
            q.x
        }
    "});

    assert!(!text.contains("__rt_memcpy"));
}
