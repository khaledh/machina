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
            type MidRange = u64: bounds(50, 100);

            fn make() -> u64 { 42 }

            fn main() -> u64 {
                let x = make();
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

#[test]
fn test_nonzero_check_traps_with_message_and_exit_code() {
    let run = run_program(
        "nonzero_check",
        r#"
            type NonZero = u64: nonzero;

            fn make() -> u64 { 0 }

            fn main() -> u64 {
                let x = make();
                let y: NonZero = x;
                y
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(104));

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Value must be nonzero: value=0\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_main_error_union_reports_unhandled_error_name() {
    let run = run_program(
        "main_error_union_unhandled_name",
        r#"
            type IoError = {
                code: u64,
            }

            fn fail() -> u64 | IoError {
                IoError { code: 7 }
            }

            fn main() -> u64 | IoError {
                let v = fail()?;
                v
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(106));
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Unhandled error in main: IoError\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_main_error_union_enum_error_reports_variant_name() {
    let run = run_program(
        "main_error_union_enum_variant_name",
        r#"
            type AppError
              = NotFound
              | InvalidCode(u64)

            fn fail() -> u64 | AppError {
                AppError::InvalidCode(7)
            }

            fn main() -> u64 | AppError {
                let v = fail()?;
                v
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(106));
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Unhandled error in main: AppError::InvalidCode\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_main_error_union_large_payload_is_reported_not_crashed() {
    let run = run_program(
        "main_error_union_large_payload",
        r#"
            type BigError = {
                a: u64,
                b: u64,
                c: u64,
                d: u64,
            }

            fn fail() -> u64 | BigError {
                BigError { a: 1, b: 2, c: 3, d: 4 }
            }

            fn main() -> u64 | BigError {
                let v = fail()?;
                v
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(106));
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(
        stderr, "Runtime error: Unhandled error in main: BigError\n",
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_main_error_union_ok_payload_maps_to_exit_code() {
    let run = run_program(
        "main_error_union_ok_exit_code",
        r#"
            type IoError = {
                code: u64,
            }

            fn ok(value: u64) -> u64 | IoError {
                value
            }

            fn main() -> u64 | IoError {
                ok(42)
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(42));
    assert_eq!(run.stderr, b"");
}
