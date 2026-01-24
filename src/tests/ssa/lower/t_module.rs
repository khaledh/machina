use super::{analyze, formact_func, indoc, lower_module};

#[test]
fn test_lower_module_globals() {
    let ctx = analyze(indoc! {r#"
        fn a() -> string {
            "a"
        }

        fn b() -> string {
            "b"
        }
    "#});

    let lowered = lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
    )
    .unwrap();

    assert_eq!(lowered.funcs.len(), 2);
    assert_eq!(lowered.globals.len(), 2);

    let a_text = formact_func(&lowered.funcs[0].func, &lowered.funcs[0].types);
    let b_text = formact_func(&lowered.funcs[1].func, &lowered.funcs[1].types);

    assert!(a_text.contains("@g0"));
    assert!(b_text.contains("@g1"));
}
