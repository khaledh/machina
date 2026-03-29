use crate::common::run_program;

#[test]
fn test_print_outputs_string() {
    let run = run_program(
        "print_str",
        r#"
            requires {
                std::io as io
            }

            fn main() -> u64 {
                io::print("hello");
                io::println();
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "hello\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_print_outputs_u64() {
    let run = run_program(
        "print_u64",
        r#"
            requires {
                std::io as io
            }

            fn main() -> u64 {
                let x = 42;
                io::print("x=");
                io::print(x);
                io::println();
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "x=42\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_print_outputs_returned_fstring() {
    let run = run_program(
        "print_returned_fstring",
        r#"
            requires {
                std::io as io
            }

            fn label(index: u64) -> string {
                f"{index + 1}"
            }

            fn main() -> u64 {
                io::println(label(0));
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "1\n", "unexpected stdout: {stdout}");
}
