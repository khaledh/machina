#![cfg(target_arch = "aarch64")]

use crate::common::run_program;

#[test]
fn test_bounds_check_traps_with_message_and_exit_code() {
    let run = run_program(
        "bounds_check",
        r#"
            fn main() -> u64 {
                let arr = [1, 2, 3];
                arr[5]
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(101));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Index out of bounds: index=5, len=3\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_div_by_zero_traps_with_message_and_exit_code() {
    let run = run_program(
        "div_by_zero",
        r#"
            fn div_by_zero(z: u64) -> u64 {
                10 / z
            }

            fn main() -> u64 {
                div_by_zero(0)
            }
        "#,
    );
    assert_eq!(
        run.status.code(),
        Some(100),
        "unexpected exit code: {:?}",
        run.status.code()
    );

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Division by zero\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_range_check_traps_with_message_and_exit_code() {
    let run = run_program(
        "range_check",
        r#"
            type MidRange = range(50, 100);

            fn main() -> u64 {
                let x = 42;
                let y: MidRange = x;
                y
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(102));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Value out of range: value=42, min(incl)=50, max(excl)=100\n",
        "unexpected stderr: {stderr}"
    );
}
