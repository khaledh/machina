use super::{analyze, formact_func, indoc, lower_func};

#[test]
fn test_lower_const_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            42
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_return_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            return 42;
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_binop_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            1 + 2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: u64 = add %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_param_binop_return() {
    let ctx = analyze(indoc! {"
        fn main(a: u64, b: u64) -> u64 {
            a + b
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64, u64) -> u64 {
          bb0(%v0: u64, %v1: u64):
            %v2: u64 = add %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_unop_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            ~1
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = bitnot %v0
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_unop_not_return() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            !true
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> bool {
          bb0():
            %v0: bool = const true
            %v1: bool = not %v0
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_cmp_return() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            1 < 2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> bool {
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: bool = cmp.lt %v0, %v1
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}
