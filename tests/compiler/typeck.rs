use crate::common::run_program;

#[test]
fn test_overloaded_functions() {
    let run = run_program(
        "overloads",
        r#"
            type Small = u64: bounds(0, 10);

            fn id(x: u64) -> u64 {
                x
            }

            fn id(x: Small) -> u64 {
                let y: u64 = x;
                y + 100
            }

            fn main() -> u64 {
                let a: Small = 5;
                let b = 20;
                println(id(a));
                println(id(b));
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "105\n20\n", "unexpected stdout: {stdout}");
}
