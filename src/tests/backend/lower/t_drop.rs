use super::{analyze, format_func, indoc, lower_func};
use crate::core::backend::lower::{LowerOpts, lower_module_with_opts};
use crate::core::ir::format::format_func_with_comments;

fn count(text: &str, needle: &str) -> usize {
    text.matches(needle).count()
}

#[test]
fn test_drop_on_field_overwrite() {
    let ctx = analyze(indoc! {"
        type S = { a: string }

        fn main() -> u64 {
            var s = S { a: \"hi\" };
            s.a = \"bye\";
            0
        }
    "});

    let lowered = lower_module_with_opts(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
        &LowerOpts {
            trace_drops: true,
            ..Default::default()
        },
    )
    .expect("failed to lower");
    let func_names: Vec<_> = lowered.funcs.iter().map(|f| f.func.name.clone()).collect();
    let main = lowered
        .funcs
        .iter()
        .find(|func| func.func.name == "main")
        .unwrap_or_else(|| panic!("missing main function in {func_names:?}"));
    let text = format_func_with_comments(&main.func, &main.types);

    assert!(
        count(&text, "__rt_string_drop") >= 2,
        "expected field overwrite to drop the prior value"
    );
}

#[test]
fn test_drop_promotes_full_init() {
    let ctx = analyze(indoc! {"
        type S = { a: string }

        fn fill(out a: string) {
            a = \"hi\";
        }

        fn main() -> u64 {
            var s: S;
            fill(out s.a);
            0
        }
    "});

    let lowered = lower_module_with_opts(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
        &LowerOpts {
            trace_drops: true,
            ..Default::default()
        },
    )
    .expect("failed to lower");
    let func_names: Vec<_> = lowered.funcs.iter().map(|f| f.func.name.clone()).collect();
    let main = lowered
        .funcs
        .iter()
        .find(|func| func.func.name == "main")
        .unwrap_or_else(|| panic!("missing main function in {func_names:?}"));
    let text = format_func_with_comments(&main.func, &main.types);

    assert!(
        text.contains("drop s") || text.contains("drop-if-live s"),
        "expected drop for s when out init promotes full init\n{text}"
    );
}

#[test]
fn test_sink_call_clears_drop_flag() {
    let ctx = analyze(indoc! {"
        fn consume(sink s: string) -> u64 {
            0
        }

        fn main() -> u64 {
            let s = \"hi\";
            consume(move s);
            0
        }
    "});

    let main_def = ctx.module.func_defs()[1];
    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = format_func(&lowered.func, &lowered.types);

    assert!(
        !text.contains("__rt_string_drop"),
        "expected sink call to clear caller-side drop"
    );
}

#[test]
fn test_heap_drop_calls_free_and_payload_drop() {
    let ctx = analyze(indoc! {"
        type Inner = { s: string }

        fn main() -> u64 {
            let p = ^Inner { s: \"hi\" };
            0
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = format_func(&lowered.func, &lowered.types);

    assert!(
        text.contains("__rt_string_drop"),
        "expected heap element drop to drop string payload"
    );
    assert!(
        text.contains("__rt_free"),
        "expected heap drop to free the allocation"
    );
}
