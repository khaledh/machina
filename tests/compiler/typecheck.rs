use crate::common::{run_program, run_program_with_args};
use machina::core::capsule::CapsuleError;
use machina::core::diag::CompileError;
use machina::core::typecheck::TypeCheckErrorKind;
use machina::driver::compile::{CompileOptions, check_with_path, compile_with_path};
use std::fs;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[test]
fn test_overloaded_functions() {
    let run = run_program(
        "overloads",
        r#"
            requires {
                std::io as io
            }

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
                io::println(id(a));
                io::println(id(b));
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "105\n20\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_type_of_intrinsic_prints_static_type_names() {
    let run = run_program(
        "type_of_intrinsic",
        r#"
            requires {
                std::io::println
            }

            type Point = { x: u64, y: u64 }

            fn main() {
                let p = Point { x: 1, y: 2 };
                println(type_of(p));
                println(type_of(42));
                println(type_of("hello"));
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "Point\ni32\nstring\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_type_of_intrinsic_handles_generic_struct_values() {
    let run = run_program(
        "type_of_intrinsic_generic_struct",
        r#"
            requires {
                std::io::println
            }

            type Pair<L, R> = {
                left: L,
                right: R,
            }

            fn pair<L, R>(left: L, right: R) -> Pair<L, R> {
                Pair { left, right }
            }

            fn main() {
                println(type_of(pair));
                let p = pair("age", 7);
                println(type_of(p));
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "fn<L, R>(L, R) -> Pair<L, R>\nPair<string, i32>\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_statement_position_block_forms_do_not_require_semicolons() {
    let run = run_program(
        "stmt_position_block_forms_no_semicolons",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                if true {
                    println("if");
                }
                match 1 {
                    1 => println("match"),
                    _ => println("nope"),
                }
                {
                    println("block");
                }
                println("done");
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "if\nmatch\nblock\ndone\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_std_io_file_open_read_write_close_roundtrip() {
    let run = run_program(
        "std_io_file_roundtrip",
        r#"
            requires {
                std::io::open_read
                std::io::open_write
                std::io::IoError
                std::io::println
            }

            fn main() -> () | IoError {
                let path = "/tmp/machina_io_roundtrip_test.txt";

                let raw_writer = open_write(path)?;
                let writer = raw_writer.text();
                writer.write_all("abc\n")?;
                writer.close()?;

                let raw_reader = open_read(path)?;
                let reader = raw_reader.text();
                let text = reader.read_all()?;
                reader.close()?;

                println(text);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "abc\n\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_returned_dyn_array_keeps_elements_alive() {
    let run = run_program(
        "returned_dyn_array_keeps_elements_alive",
        r#"
            requires {
                std::io::println
            }

            fn make() -> u64[*] {
                var xs: u64[*] = [];
                xs.append(11);
                xs.append(22);
                xs
            }

            fn main() {
                let xs = make();
                println(xs.len);
                println(xs[0]);
                println(xs[1]);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "2\n11\n22\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_args_error_union_early_return_builds_and_runs() {
    let run = run_program(
        "args_error_union_early_return",
        r#"
            requires {
                std::env::args
                std::io::IoError
            }

            fn main() -> () | IoError {
                let argv = args();
                if argv.len < 3 {
                    println("usage");
                    return ();
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "usage\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_args_indexing_builds_and_runs_in_error_union_main() {
    let run = run_program_with_args(
        "args_indexing_error_union_main",
        r#"
            requires {
                std::env::args
                std::io::IoError
            }

            fn main() -> () | IoError {
                let argv = args();
                if argv.len < 3 {
                    println("usage");
                    return ();
                }

                let path = argv[1];
                let needle = argv[2];
                println(path);
                println(needle);
            }
        "#,
        &["alpha", "beta"],
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alpha\nbeta\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_args_array_destructure_with_rest_builds_and_runs() {
    let run = run_program_with_args(
        "args_array_destructure_with_rest",
        r#"
            requires {
                std::env::args
                std::io::IoError
            }

            fn main() -> () | IoError {
                let argv = args();
                let [program, path, needle, ...rest] = argv;
                println(path);
                println(needle);
                println(rest.len);
            }
        "#,
        &["alpha", "beta", "gamma", "delta"],
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alpha\nbeta\n2\n");
}

#[test]
fn test_args_array_destructure_with_wildcard_builds_and_runs() {
    let run = run_program_with_args(
        "args_array_destructure_with_wildcard",
        r#"
            requires {
                std::env::args
                std::io::IoError
            }

            fn main() -> () | IoError {
                let argv = args();
                let [_, path, needle, ...] = argv;
                println(path);
                println(needle);
            }
        "#,
        &["/tmp/file.txt", "bytes"],
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "/tmp/file.txt\nbytes\n");
}

#[test]
fn test_dyn_array_destructure_with_middle_rest_keeps_prefix_and_suffix() {
    let run = run_program(
        "dyn_array_destructure_middle_rest",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let nums: u64[*] = [1, 2, 3, 4, 5];
                let [first, ...middle, last] = nums;
                println(first);
                println(middle.len);
                println(last);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "1\n3\n5\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_dyn_array_destructure_with_leading_rest_keeps_suffix() {
    let run = run_program(
        "dyn_array_destructure_leading_rest",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let nums: u64[*] = [1, 2, 3, 4];
                let [...prefix, last] = nums;
                println(prefix.len);
                println(last);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "3\n4\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_dyn_array_destructure_with_bare_rest_discards_remainder() {
    let run = run_program(
        "dyn_array_destructure_bare_rest",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let nums: u64[*] = [7, 8, 9];
                let [first, ...] = nums;
                println(first);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "7\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_fixed_array_destructure_with_middle_rest_builds_and_runs() {
    let run = run_program(
        "fixed_array_destructure_middle_rest",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let nums = u64[10, 20, 30, 40];
                let [first, ...middle, last] = nums;
                println(first);
                println(middle.len);
                println(last);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "10\n2\n40\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_for_entry_in_map_builds_and_runs() {
    let run = run_program(
        "for_entry_in_map",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let counts = map<string, u64>{"alpha": 2, "beta": 3, "z": 5};
                var distinct: u64 = 0;
                var total: u64 = 0;
                var key_bytes: u64 = 0;

                for entry in counts {
                    distinct = distinct + 1;
                    total = total + entry.1;
                    key_bytes = key_bytes + entry.0.len;
                }

                println(distinct);
                println(total);
                println(key_bytes);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "3\n10\n10\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_for_tuple_destructure_in_map_builds_and_runs() {
    let run = run_program(
        "for_tuple_destructure_in_map",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let counts = map<string, u64>{"alpha": 2, "beta": 3, "z": 5};
                var distinct: u64 = 0;
                var total: u64 = 0;
                var key_bytes: u64 = 0;

                for (key, value) in counts {
                    distinct = distinct + 1;
                    total = total + value;
                    key_bytes = key_bytes + key.len;
                }

                println(distinct);
                println(total);
                println(key_bytes);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "3\n10\n10\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_tuple_destructure_with_unused_trailing_binding_builds_and_runs() {
    let run = run_program(
        "tuple_destructure_unused_trailing_binding",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let pair: (string, u64) = ("alpha", 5);
                let (key, value) = pair;
                println(key);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alpha\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_tuple_destructure_with_wildcard_builds_and_runs() {
    let run = run_program(
        "tuple_destructure_with_wildcard",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let pair: (string, u64) = ("alpha", 5);
                let (key, _) = pair;
                println(key);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alpha\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_string_lines_returns_owned_lines_without_newlines() {
    let run = run_program(
        "string_lines_returns_owned_lines_without_newlines",
        r#"
            requires {
                std::io::println
            }

            fn make_lines() -> string[*] {
                var text: string = "";
                text.append("alpha\n");
                text.append("beta\r\n");
                text.append("gamma");
                text.lines()
            }

            fn main() {
                let lines = make_lines();
                println(lines.len);
                println(lines[0]);
                println(lines[1]);
                println(lines[2]);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "3\nalpha\nbeta\ngamma\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_string_split_returns_owned_fields() {
    let run = run_program(
        "string_split_returns_owned_fields",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let parts = "alpha,beta,,gamma,".split(",");
                println(parts.len);
                println(parts[0]);
                println(parts[1]);
                println(parts[2].len);
                println(parts[3]);
                println(parts[4].len);

                let multi = "a--b----c".split("--");
                println(multi.len);
                println(multi[0]);
                println(multi[1]);
                println(multi[2].len);
                println(multi[3]);

                let empty = "alpha".split("");
                println(empty.len);
                println(empty[0]);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "5\nalpha\nbeta\n0\ngamma\n0\n4\na\nb\n0\nc\n1\nalpha\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_string_trim_strips_ascii_whitespace() {
    let run = run_program(
        "string_trim_strips_ascii_whitespace",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let a = "  alpha  ".trim();
                let b = "\n\t beta\r\n".trim();
                let c = "   ".trim();
                let d = "gamma".trim();
                println(a);
                println(b);
                println(c.len);
                println(d);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "alpha\nbeta\n0\ngamma\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_string_contains_finds_substrings() {
    let run = run_program(
        "string_contains_finds_substrings",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let text = "alpha beta";
                if text.contains("alpha") {
                    println("head");
                }
                if text.contains("beta") {
                    println("tail");
                }
                if text.contains("") {
                    println("empty");
                }
                if !text.contains("gamma") {
                    println("miss");
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "head\ntail\nempty\nmiss\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_parse_u64_parses_decimal_and_rejects_invalid_input() {
    let run = run_program(
        "parse_u64_decimal",
        r#"
            requires {
                std::io::println
                std::parse as parse
                std::parse::ParseError
            }

            fn main() {
                let ok = parse::parse_u64("42");
                println("ok");
                match ok {
                    value: u64 => println(value),
                    err: ParseError => println("err"),
                }

                let empty = parse::parse_u64("");
                println("empty");
                match empty {
                    value: u64 => println(value),
                    err: ParseError => println("err"),
                }

                let bad = parse::parse_u64("7x");
                println("bad");
                match bad {
                    value: u64 => println(value),
                    err: ParseError => println("err"),
                }

                let overflow = parse::parse_u64("18446744073709551616");
                println("overflow");
                match overflow {
                    value: u64 => println(value),
                    err: ParseError => println("err"),
                }

                let max = parse::parse_u64("18446744073709551615");
                println("max");
                match max {
                    value: u64 => println(value),
                    err: ParseError => println("err"),
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "ok\n42\nempty\nerr\nbad\nerr\noverflow\nerr\nmax\n18446744073709551615\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_string_method_on_indexed_inferred_split_field_builds() {
    let run = run_program(
        "string_method_on_indexed_inferred_split_field",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                for line in " a ,b\n".lines() {
                    let cols = line.split(",");
                    let name: string = cols[0].trim();
                    println(name);
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "a\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_string_indexed_split_field_copy_does_not_abort() {
    let run = run_program(
        "string_indexed_split_field_copy_does_not_abort",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                for line in "alice,10\n".lines() {
                    let cols: string[*] = line.split(",");
                    let name: string = cols[0];
                    println(name);
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alice\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_copied_dyn_array_of_strings_remains_usable() {
    let run = run_program(
        "copied_dyn_array_of_strings_remains_usable",
        r#"
            requires {
                std::io::println
            }

            fn main() {
                let original = "pear\napple\nbanana\n".lines();
                let copy = original;

                println("original:");
                for line in original {
                    println(line);
                }

                println("copy:");
                for line in copy {
                    println(line);
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "original:\npear\napple\nbanana\ncopy:\npear\napple\nbanana\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn test_returning_indexed_split_field_keeps_string_alive() {
    let run = run_program(
        "returning_indexed_split_field_keeps_string_alive",
        r#"
            requires {
                std::io::println
            }

            fn leak() -> string {
                let cols: string[*] = "alice,10".split(",");
                cols[0]
            }

            fn main() {
                let name: string = leak();
                println(name);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alice\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_std_io_using_auto_closes_text_handles() {
    let run = run_program(
        "std_io_using_roundtrip",
        r#"
            requires {
                std::io::IoError
                std::io::open_read
                std::io::open_write
                std::io::println
            }

            fn main() -> () | IoError {
                let path = "/tmp/machina_io_using_roundtrip_test.txt";

                // The writer is scoped to this block and closes automatically
                // when `using` exits.
                using writer = open_write(path)?.text() {
                    writer.write_all("abc\n")?;
                }

                // Reopen the same path to confirm the first handle really
                // closed and the contents are visible to the next user.
                using reader = open_read(path)?.text() {
                    let text = reader.read_all()?;
                    println(text);
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "abc\n\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_std_io_text_adapter_on_temporary_open_handle() {
    let run = run_program(
        "std_io_temporary_text_adapter",
        r#"
            requires {
                std::io::IoError
                std::io::open_read
                std::io::open_write
                std::io::println
            }

            fn main() -> () | IoError {
                let path = "/tmp/machina_io_temporary_adapter_test.txt";

                // Opening and immediately adapting the temporary read/write
                // handle should be allowed even though `text()` consumes the
                // intermediate file value.
                let writer = open_write(path)?.text();
                writer.write_all("temporary\n")?;
                writer.close()?;

                let reader = open_read(path)?.text();
                let text = reader.read_all()?;
                reader.close()?;

                println(text);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "temporary\n\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_try_then_method_then_try_chain_infers_text_result() {
    let run = run_program(
        "try_method_try_chain_infers_text",
        r#"
            requires {
                std::io::IoError
                std::io::open_read
                std::io::open_write
                std::io::println
            }

            fn main() -> () | IoError {
                let path = "/tmp/machina_try_method_try_chain.txt";

                let writer = open_write(path)?.text();
                writer.write_all("hello\n")?;
                writer.close()?;

                let reader = open_read(path)?.text();
                let text = reader.read_all()?;
                reader.close()?;

                println(text);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "hello\n\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_defer_runs_before_try_propagates_error() {
    let run = run_program(
        "defer_try_cleanup",
        r#"
            requires {
                std::io::println
            }

            type AppError = {
                code: u64,
            }

            fn cleanup() {
                println("cleanup");
            }

            fn fail() -> u64 | AppError {
                AppError { code: 9 }
            }

            fn main() -> () | AppError {
                defer cleanup();
                let _value = fail()?;
                ()
            }
        "#,
    );
    assert_ne!(
        run.status.code(),
        Some(0),
        "propagated error should not exit successfully"
    );

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "cleanup\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_same_scope_name_rebinding_for_let_var_and_params() {
    let run = run_program(
        "same_scope_name_rebinding",
        r#"
            fn bump(x: u64) {
                let x = x + 1;
                println(x);
            }

            fn main() {
                let x = 1;
                let x = x + 1;
                println(x);

                var y = 1;
                var y = y + 2;
                println(y);

                bump(5);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "2\n3\n6\n", "unexpected stdout: {stdout}");
}

fn with_temp_program(
    name: &str,
    entry_source: &str,
    extra_modules: &[(&str, &str)],
    test: impl FnOnce(&Path, &str),
) {
    let run_id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_compiler_test_{}_{}_{}",
        name,
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let entry_path = temp_dir.join("main.mc");
    fs::write(&entry_path, entry_source).expect("failed to write entry source");

    for (module_path, source) in extra_modules {
        let file_path = temp_dir.join(module_path);
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).expect("failed to create module parent");
        }
        fs::write(&file_path, source).expect("failed to write module source");
    }

    test(&entry_path, entry_source);
    let _ = fs::remove_dir_all(&temp_dir);
}

fn typecheck_with_modules(
    entry_path: &Path,
    entry_source: &str,
) -> Result<machina::driver::compile::CompileOutput, Vec<CompileError>> {
    compile_with_path(
        entry_source,
        Some(entry_path),
        &CompileOptions {
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: false,
        },
    )
}

fn check_with_modules(entry_path: &Path, entry_source: &str) -> Result<(), Vec<CompileError>> {
    check_with_path(entry_source, entry_path, true)
}

#[test]
fn test_match_arm_alternation_runs() {
    let run = run_program(
        "match_arm_alternation",
        r#"
            fn is_whitespace(b: u8) -> bool {
                match b {
                    32 | 10 | 9 | 13 => true,
                    _ => false,
                }
            }

            fn main() -> u64 {
                if is_whitespace(32) {
                } else {
                    return 1;
                }
                if is_whitespace(13) {
                } else {
                    return 2;
                }
                if is_whitespace(65) {
                    return 3;
                } else {
                }
                return 0;
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_match_arm_alternation_rejects_binding_patterns() {
    let entry_source = r#"
        fn main(t: (u64, u64)) -> u64 {
            match t {
                (x, _) | (1, 2) => x,
                _ => 0,
            }
        }
    "#;

    with_temp_program(
        "match_arm_alternation_rejects_bindings",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "expected parse failure for binding alternation"
            );
        },
    );
}

#[test]
fn test_modules_opaque_field_access_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn read_secret(v: Secret) -> u64 {
            v.x
        }

        fn main() -> u64 {
            0
        }
    "#;

    let secret_source = r#"
        @opaque
        type Secret = { x: u64 }
    "#;

    with_temp_program(
        "opaque_field_access",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for opaque field access"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::OpaqueFieldAccess(type_name, field, ..)
                                    if type_name == "Secret" && field == "x"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_opaque_construction_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            let s = Secret { x: 1 };
            secret.take(s)
        }
    "#;

    let secret_source = r#"
        @opaque
        type Secret = { x: u64 }

        @public
        fn take(v: Secret) -> u64 {
            0
        }
    "#;

    with_temp_program(
        "opaque_construction",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for opaque construction"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::OpaqueTypeConstruction(type_name, ..)
                                    if type_name == "Secret"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_private_method_call_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            var c = Counter { ticks: 0 };
            c.inc();
            0
        }
    "#;

    let secret_source = r#"
        @public
        type Counter = { ticks: u64 }

        Counter :: {
            fn inc(inout self) {
                self.ticks = self.ticks + 1;
            }
        }
    "#;

    with_temp_program(
        "private_method_call",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for private method call"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::CallableNotAccessible(name, ..)
                                    if name == "inc"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_private_property_access_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            let c = Counter { ticks: 3 };
            c.tick_count
        }
    "#;

    let secret_source = r#"
        @public
        type Counter = { ticks: u64 }

        Counter :: {
            prop tick_count: u64 {
                get { self.ticks }
            }
        }
    "#;

    with_temp_program(
        "private_property_access",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for private property access"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::PropertyNotAccessible(name, ..)
                                    if name == "tick_count"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_strict_compile_rejects_mixed_region_resolve_fixture() {
    let entry_source = r#"
        fn id(x: u64) -> u64 { x }

        fn bad_region() -> u64 {
            missing
        }

        fn main() -> u64 {
            id(1)
        }
    "#;

    with_temp_program(
        "strict_mixed_region_resolve",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "strict compile must still fail on unresolved symbols"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::Resolve(resolve_err)
                            if resolve_err.to_string().contains("Undefined variable")
                    )
                }));
            }
        },
    );
}

#[test]
fn test_strict_compile_rejects_mixed_region_type_fixture() {
    let entry_source = r#"
        fn id(x: u64) -> u64 { x }

        fn bad_region() -> u64 {
            let v: u64 = true;
            v
        }

        fn main() -> u64 {
            id(1)
        }
    "#;

    with_temp_program(
        "strict_mixed_region_type",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "strict compile must still fail on type errors"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(type_err.kind(), TypeCheckErrorKind::DeclTypeMismatch(_, _, ..))
                    )
                }));
            }
        },
    );
}

#[test]
fn test_check_module_graph_parity_std_io_import() {
    let entry_source = r#"
        requires {
            std::io as io
        }

        fn main() -> u64 {
            io::println("ok");
            0
        }
    "#;

    with_temp_program(
        "check_std_io_parity",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_ok(),
                "module-aware check should accept std::io usage without unresolved symbol false positives"
            );
        },
    );
}

#[test]
fn test_check_module_graph_parity_multi_module_requires() {
    let entry_source = r#"
        requires {
            app::util as util
        }

        fn main() -> u64 {
            util::answer()
        }
    "#;

    let util_source = r#"
        @public
        fn answer() -> u64 {
            42
        }
    "#;

    with_temp_program(
        "check_multi_module_parity",
        entry_source,
        &[("app/util.mc", util_source)],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_ok(),
                "module-aware check should follow requires/module flattening like compile"
            );
        },
    );
}

#[test]
fn test_check_single_file_behavior_unchanged() {
    let entry_source = r#"
        fn main() -> u64 {
            missing
        }
    "#;

    with_temp_program(
        "check_single_file_behavior",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "single-file unresolved symbol should still fail check"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(err, CompileError::Resolve(resolve_err) if resolve_err.to_string().contains("Undefined variable"))
                }));
            }
        },
    );
}

#[test]
fn test_modules_opaque_pattern_destructure_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            let Token { raw } = secret.make();
            raw
        }
    "#;

    let secret_source = r#"
        @opaque
        type Token = { raw: u64 }

        @public
        fn make() -> Token {
            Token { raw: 7 }
        }
    "#;

    with_temp_program(
        "opaque_pattern_destructure",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for opaque destructuring pattern"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::OpaquePatternDestructure(type_name, ..)
                                    if type_name == "Token"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_private_trait_bound_rejected() {
    let entry_source = r#"
        requires {
            app::secret as sec
        }

        fn run_it<T: sec::Internal>(value: T) -> u64 {
            0
        }

        fn main() -> u64 { 0 }
    "#;

    let secret_source = r#"
        trait Internal {
            fn run(self);
        }
    "#;

    with_temp_program(
        "private_trait_bound",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for private trait bound"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::Capsule(CapsuleError::RequireMemberPrivate {
                            alias,
                            member,
                            expected_kind,
                            ..
                        }) if alias == "sec" && member == "Internal" && *expected_kind == "trait"
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_duplicate_public_function_names_allowed_with_aliases() {
    let entry_source = r#"
        requires {
            app::util as util
            app::math as math
        }

        fn main() -> u64 {
            util::answer() + math::answer()
        }
    "#;

    let util_source = r#"
        @public
        fn answer() -> u64 { 7 }
    "#;

    let math_source = r#"
        @public
        fn answer() -> u64 { 11 }
    "#;

    with_temp_program(
        "duplicate_public_callables",
        entry_source,
        &[("app/util.mc", util_source), ("app/math.mc", math_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_ok(),
                "compile should succeed for aliased duplicate exports"
            );
        },
    );
}

#[test]
fn test_iter_done_prelude_type_builds_and_runs() {
    let run = run_program(
        "iter_done_prelude_type",
        r#"
            requires {
                std::io::println
            }

            fn make_done() -> IterDone {
                IterDone {}
            }

            fn main() {
                let done = make_done();
                println("ok");
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "ok\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_for_protocol_iterable_builds_and_runs() {
    let run = run_program(
        "for_protocol_iterable",
        r#"
            requires {
                std::io::println
            }

            type Counter = {
                cur: u64,
                end: u64,
            }

            type CounterIter = {
                cur: u64,
                end: u64,
            }

            Counter :: {
                fn iter(self) -> CounterIter {
                    CounterIter { cur: self.cur, end: self.end }
                }
            }

            CounterIter :: {
                fn next(inout self) -> u64 | IterDone {
                    if self.cur < self.end {
                        let value = self.cur;
                        self.cur = self.cur + 1;
                        value
                    } else {
                        IterDone {}
                    }
                }
            }

            fn main() {
                let counter = Counter { cur: 2, end: 5 };
                var sum: u64 = 0;
                for n in counter {
                    sum = sum + n;
                }
                println(sum);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "9\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_for_protocol_fallible_iterable_propagates_error() {
    let run = run_program(
        "for_protocol_fallible_iterable",
        r#"
            type ParseError = {}

            type Counter = {
                cur: u64,
                end: u64,
                fail_at: u64,
            }

            type CounterIter = {
                cur: u64,
                end: u64,
                fail_at: u64,
            }

            Counter :: {
                fn iter(self) -> CounterIter {
                    CounterIter {
                        cur: self.cur,
                        end: self.end,
                        fail_at: self.fail_at,
                    }
                }
            }

            CounterIter :: {
                fn next(inout self) -> u64 | ParseError | IterDone {
                    if self.cur == self.fail_at {
                        ParseError {}
                    } else if self.cur < self.end {
                        let value = self.cur;
                        self.cur = self.cur + 1;
                        value
                    } else {
                        IterDone {}
                    }
                }
            }

            fn main() -> u64 | ParseError {
                let counter = Counter { cur: 1, end: 5, fail_at: 3 };
                var sum: u64 = 0;
                for n in counter {
                    sum = sum + n;
                }
                sum
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(106));
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("Unhandled error in main: ParseError"),
        "unexpected stderr: {stderr}"
    );
}

#[test]
fn test_for_protocol_fallible_iterable_requires_error_in_return() {
    let entry_source = r#"
        type ParseError = {}

        type Counter = {
            cur: u64,
            end: u64,
        }

        type CounterIter = {
            cur: u64,
            end: u64,
        }

        Counter :: {
            fn iter(self) -> CounterIter {
                CounterIter { cur: self.cur, end: self.end }
            }
        }

        CounterIter :: {
            fn next(inout self) -> u64 | ParseError | IterDone {
                if self.cur < self.end {
                    let value = self.cur;
                    self.cur = self.cur + 1;
                    value
                } else if self.cur == self.end {
                    IterDone {}
                } else {
                    ParseError {}
                }
            }
        }

        fn main() {
            let counter = Counter { cur: 0, end: 3 };
            for n in counter {
                println(n);
            }
        }
    "#;

    with_temp_program(
        "for_protocol_fallible_missing_return_error",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail when for-loop propagation has no enclosing error union"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::TryReturnTypeNotErrorUnion(_)
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_method_if_join_coerces_local_tail_into_error_union() {
    let run = run_program(
        "method_if_join_error_union_coercion",
        r#"
            type EmptyErr = {}
            type CounterIter = {
                cur: u64,
                end: u64,
            }

            CounterIter :: {
                fn step(inout self) -> u64 | EmptyErr {
                    if self.cur < self.end {
                        let value = self.cur;
                        self.cur = self.cur + 1;
                        value
                    } else {
                        EmptyErr {}
                    }
                }
            }

            fn main() -> u64 | EmptyErr {
                var it = CounterIter { cur: 2, end: 5 };
                it.step()
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(2));
    assert_eq!(
        run.stderr,
        b"",
        "unexpected stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn test_for_protocol_adapter_iterable_builds_and_runs() {
    let run = run_program(
        "for_protocol_adapter_iterable",
        r#"
            requires {
                std::io::println
            }

            type Counter = {
                start: u64,
                end: u64,
            }

            type CounterIter = {
                cur: u64,
                end: u64,
            }

            Counter :: {
                fn iter(self) -> CounterIter {
                    CounterIter { cur: self.start, end: self.end }
                }
            }

            CounterIter :: {
                fn next(inout self) -> u64 | IterDone {
                    if self.cur < self.end {
                        let value = self.cur;
                        self.cur = self.cur + 1;
                        value
                    } else {
                        IterDone {}
                    }
                }
            }

            type SkipCounter = {
                inner: Counter,
                skip: u64,
            }

            type SkipCounterIter = {
                inner: CounterIter,
                remaining: u64,
            }

            SkipCounter :: {
                fn iter(self) -> SkipCounterIter {
                    SkipCounterIter {
                        inner: self.inner.iter(),
                        remaining: self.skip,
                    }
                }
            }

            SkipCounterIter :: {
                fn next(inout self) -> u64 | IterDone {
                    while self.remaining > 0 {
                        let step = self.inner.next();
                        match step {
                            value: u64 => {
                                self.remaining = self.remaining - 1;
                            }
                            done: IterDone => {
                                return IterDone {};
                            }
                        }
                    }

                    self.inner.next()
                }
            }

            fn main() {
                let skipped = SkipCounter {
                    inner: Counter { start: 2, end: 7 },
                    skip: 2,
                };

                var sum: u64 = 0;
                for n in skipped {
                    println(n);
                    sum = sum + n;
                }
                println(sum);
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "4\n5\n6\n15\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_generic_method_block_typechecks() {
    let source = r#"
        requires {
            std::io::println
        }

        type Box<T> = {
            value: T,
        }

        Box<T> :: {
            fn value_of(self) -> T {
                self.value
            }
        }

        fn main() {
            let boxed = Box<u64> { value: 7 };
            let value: u64 = boxed.value_of();
            println(value);
        }
    "#;
    let result = check_with_path(
        source,
        Path::new("/tmp/generic_method_block_typechecks.mc"),
        true,
    );
    assert!(result.is_ok(), "unexpected diagnostics: {:?}", result.err());
}

#[test]
fn test_generic_method_block_builds_and_runs() {
    let run = run_program(
        "generic_method_block_builds_and_runs",
        r#"
            requires {
                std::io::println
            }

            type Box<T> = {
                value: T,
            }

            Box<T> :: {
                fn value_of(self) -> T {
                    self.value
                }
            }

            fn main() {
                let boxed = Box<u64> { value: 7 };
                println(boxed.value_of());
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "7\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_generic_map_iter_adapter_builds_and_runs() {
    let run = run_program(
        "generic_map_iter_adapter_builds_and_runs",
        r#"
            requires {
                std::io::println
            }

            type Counter = {
                cur: u64,
                end: u64,
            }

            type CounterIter = {
                cur: u64,
                end: u64,
            }

            Counter :: {
                fn iter(self) -> CounterIter {
                    CounterIter { cur: self.cur, end: self.end }
                }
            }

            CounterIter :: {
                fn next(inout self) -> u64 | IterDone {
                    if self.cur < self.end {
                        let value = self.cur;
                        self.cur = self.cur + 1;
                        value
                    } else {
                        IterDone {}
                    }
                }
            }

            type MapIter<S, In, Out> = {
                source: S,
                f: fn(In) -> Out,
            }

            MapIter<S, In, Out> :: {
                fn iter(self) -> MapIter<S, In, Out> {
                    self
                }

                fn next(inout self) -> Out | IterDone {
                    match self.source.next() {
                        item: In => {
                            let f = self.f;
                            f(item)
                        },
                        done: IterDone => IterDone {},
                    }
                }
            }

            fn double(n: u64) -> u64 { n * 2 }

            fn map_values<S, In, Out>(
                source: S,
                f: fn(In) -> Out,
            ) -> MapIter<S, In, Out> {
                MapIter { source, f }
            }

            fn main() {
                let counter = Counter { cur: 2, end: 5 };
                let mapped: MapIter<CounterIter, u64, u64> =
                    map_values(counter.iter(), double);
                for n in mapped {
                    println(n);
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "4\n6\n8\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_string_iter_done_union_match_builds_and_runs() {
    let run = run_program(
        "string_iter_done_union_match",
        r#"
            fn make_done() -> string | IterDone {
                IterDone {}
            }

            fn main() {
                let step = make_done();
                match step {
                    line: string => println(line),
                    done: IterDone => println("done"),
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "done\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_direct_dyn_array_tail_coerces_into_error_union() {
    with_temp_program(
        "direct_dyn_array_tail_coerces_into_error_union",
        r#"
            fn make_fields() -> string[*] | IterDone {
                let line = "a,b";
                line.split(",")
            }
        "#,
        &[],
        |entry_path, entry_source| {
            let result = check_with_modules(entry_path, entry_source);
            assert!(result.is_ok(), "compile should succeed: {result:?}");
        },
    );
}

#[test]
fn test_direct_int_return_coerces_into_error_union() {
    let run = run_program(
        "direct_int_return_coerces_into_error_union",
        r#"
            fn one() -> u64 | IterDone {
                return 1;
            }

            fn two() -> u64 | IterDone {
                2
            }

            fn main() {
                match one() {
                    value: u64 => println(value),
                    done: IterDone => println("done"),
                }
                match two() {
                    value: u64 => println(value),
                    done: IterDone => println("done"),
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "1\n2\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_for_protocol_string_iterable_builds_and_runs() {
    let run = run_program(
        "for_protocol_string_iterable",
        r#"
            requires {
                std::io::IoError
                std::io::open_read
                std::io::open_write
            }

            type LineIter = {
                lines: string[*],
                index: u64,
            }

            LineIter :: {
                fn iter(self) -> LineIter {
                    self
                }

                fn next(inout self) -> string | IterDone {
                    if self.index < self.lines.len {
                        let line = self.lines[self.index];
                        self.index = self.index + 1;
                        line
                    } else {
                        IterDone {}
                    }
                }
            }

            fn main() -> () | IoError {
                let path = "/tmp/machina_for_protocol_string_iterable.txt";

                using writer = open_write(path)?.text() {
                    writer.write_all("a\n")?;
                    writer.write_all("b\n")?;
                }

                using reader = open_read(path)?.text() {
                    let text = reader.read_all()?;
                    let it = LineIter {
                        lines: text.lines(),
                        index: 0,
                    };

                    for line in it {
                        println(line);
                    }
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "a\nb\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_typed_csv_iterator_pipeline_builds_and_runs() {
    let run = run_program(
        "typed_csv_iterator_pipeline",
        r#"
            requires {
                std::io::IoError
                std::io::open_read
                std::io::open_write
                std::parse as parse
                std::parse::ParseError
            }

            type Row = {
                name: string,
                score: u64,
            }

            type CsvFields = {
                values: string[*],
            }

            type CsvFieldIter = {
                lines: string[*],
                index: u64,
            }

            CsvFieldIter :: {
                fn iter(self) -> CsvFieldIter {
                    self
                }

                fn next(inout self) -> CsvFields | IterDone {
                    if self.index < self.lines.len {
                        let line = self.lines[self.index];
                        self.index = self.index + 1;
                        CsvFields { values: line.split(",") }
                    } else {
                        IterDone {}
                    }
                }
            }

            type RowIter = {
                source: CsvFieldIter,
            }

            RowIter :: {
                fn iter(self) -> RowIter {
                    self
                }

                fn next(inout self) -> Row | ParseError | IterDone {
                    match self.source.next() {
                        fields: CsvFields => {
                            let [name_text, score_text, ...] = fields.values;
                            let score = parse::parse_u64(score_text.trim())?;
                            Row {
                                name: name_text.trim(),
                                score,
                            }
                        }
                        done: IterDone => {
                            IterDone {}
                        }
                    }
                }
            }

            fn main() -> () | IoError | ParseError {
                let path = "/tmp/machina_typed_csv_iterator_pipeline.txt";

                using writer = open_write(path)?.text() {
                    writer.write_all("name,score\n")?;
                    writer.write_all("alice,10\n")?;
                    writer.write_all("bob,42\n")?;
                    writer.write_all("cara,7\n")?;
                }

                using reader = open_read(path)?.text() {
                    let text = reader.read_all()?;
                    let rows = RowIter {
                        source: CsvFieldIter {
                            lines: text.lines(),
                            index: 1,
                        },
                    };

                    for row in rows {
                        if row.score > 9 {
                            println(row.name);
                        }
                    }
                }
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "alice\nbob\n", "unexpected stdout: {stdout}");
}

#[test]
fn test_for_protocol_reports_missing_iter_method() {
    let entry_source = r#"
        type Counter = {
            start: u64,
            end: u64,
        }

        fn main() {
            let counter = Counter { start: 0, end: 3 };
            for n in counter {
                println(n);
            }
        }
    "#;

    with_temp_program(
        "for_protocol_missing_iter",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(result.is_err(), "compile should fail without iter(self)");
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                matches!(
                    err,
                    CompileError::TypeCheck(type_err)
                        if matches!(
                            type_err.kind(),
                            TypeCheckErrorKind::ForIterProtocolMissingIter(ty, ..)
                                if matches!(ty, machina::core::types::Type::Struct { name, .. } if name == "Counter")
                        )
                )
            }));
            }
        },
    );
}

#[test]
fn test_for_protocol_reports_missing_next_method() {
    let entry_source = r#"
        type Counter = {
            start: u64,
            end: u64,
        }

        type CounterIter = {
            cur: u64,
            end: u64,
        }

        Counter :: {
            fn iter(self) -> CounterIter {
                CounterIter { cur: self.start, end: self.end }
            }
        }

        fn main() {
            let counter = Counter { start: 0, end: 3 };
            for n in counter {
                println(n);
            }
        }
    "#;

    with_temp_program(
        "for_protocol_missing_next",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail without next(inout self)"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                matches!(
                    err,
                    CompileError::TypeCheck(type_err)
                        if matches!(
                            type_err.kind(),
                            TypeCheckErrorKind::ForIterProtocolMissingNext(ty, ..)
                                if matches!(ty, machina::core::types::Type::Struct { name, .. } if name == "CounterIter")
                        )
                )
            }));
            }
        },
    );
}

#[test]
fn test_for_protocol_reports_invalid_next_return_shape() {
    let entry_source = r#"
        type Counter = {
            start: u64,
            end: u64,
        }

        type CounterIter = {
            cur: u64,
            end: u64,
        }

        Counter :: {
            fn iter(self) -> CounterIter {
                CounterIter { cur: self.start, end: self.end }
            }
        }

        CounterIter :: {
            fn next(inout self) -> u64 {
                self.cur
            }
        }

        fn main() {
            let counter = Counter { start: 0, end: 3 };
            for n in counter {
                println(n);
            }
        }
    "#;

    with_temp_program(
        "for_protocol_invalid_next_return",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail when next() does not return Item | IterDone"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::ForIterProtocolInvalidNextReturn(ty, ..)
                                    if matches!(ty, machina::core::types::Type::Int { .. })
                            )
                    )
                }));
            }
        },
    );
}
