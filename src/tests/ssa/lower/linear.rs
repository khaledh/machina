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

#[test]
fn test_lower_tuple_lit_return() {
    let ctx = analyze(indoc! {"
        fn main() -> (u64, bool) {
            (1, true)
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> (u64, bool) {
          locals:
            %l0: (u64, bool)
          bb0():
            %v0: ptr<(u64, bool)> = addr_of %l0
            %v1: u64 = const 1:u64
            %v2: ptr<u64> = field_addr %v0, 0
            store %v2, %v1
            %v3: bool = const true
            %v4: ptr<bool> = field_addr %v0, 1
            store %v4, %v3
            %v5: (u64, bool) = load %v0
            ret %v5
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_struct_lit_return() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main() -> Pair {
            Pair { b: 2, a: 1 }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Pair {
          locals:
            %l0: Pair
          bb0():
            %v0: ptr<Pair> = addr_of %l0
            %v1: u64 = const 2:u64
            %v2: ptr<u64> = field_addr %v0, 1
            store %v2, %v1
            %v3: u64 = const 1:u64
            %v4: ptr<u64> = field_addr %v0, 0
            store %v4, %v3
            %v5: Pair = load %v0
            ret %v5
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_struct_update_return() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main() -> Pair {
            let p = Pair { a: 1, b: 2 };
            { p | b: 5 }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Pair {
          locals:
            %l0: Pair
            %l1: Pair
          bb0():
            %v0: ptr<Pair> = addr_of %l0
            %v1: u64 = const 1:u64
            %v2: ptr<u64> = field_addr %v0, 0
            store %v2, %v1
            %v3: u64 = const 2:u64
            %v4: ptr<u64> = field_addr %v0, 1
            store %v4, %v3
            %v5: Pair = load %v0
            %v6: ptr<Pair> = addr_of %l1
            store %v6, %v5
            %v7: u64 = const 5:u64
            %v8: ptr<u64> = field_addr %v6, 1
            store %v8, %v7
            %v9: Pair = load %v6
            ret %v9
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_struct_update_multi_field_return() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main() -> Pair {
            let p = Pair { a: 1, b: 2 };
            { p | b: 5, a: 3 }
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Pair {
          locals:
            %l0: Pair
            %l1: Pair
          bb0():
            %v0: ptr<Pair> = addr_of %l0
            %v1: u64 = const 1:u64
            %v2: ptr<u64> = field_addr %v0, 0
            store %v2, %v1
            %v3: u64 = const 2:u64
            %v4: ptr<u64> = field_addr %v0, 1
            store %v4, %v3
            %v5: Pair = load %v0
            %v6: ptr<Pair> = addr_of %l1
            store %v6, %v5
            %v7: u64 = const 5:u64
            %v8: ptr<u64> = field_addr %v6, 1
            store %v8, %v7
            %v9: u64 = const 3:u64
            %v10: ptr<u64> = field_addr %v6, 0
            store %v10, %v9
            %v11: Pair = load %v6
            ret %v11
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_array_lit_elems_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64[3] {
            [1, 2, 3]
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64[3] {
          locals:
            %l0: u64[3]
          bb0():
            %v0: ptr<u64[3]> = addr_of %l0
            %v1: u64 = const 1:u64
            %v2: u64 = const 0:u64
            %v3: ptr<u64> = index_addr %v0, %v2
            store %v3, %v1
            %v4: u64 = const 2:u64
            %v5: u64 = const 1:u64
            %v6: ptr<u64> = index_addr %v0, %v5
            store %v6, %v4
            %v7: u64 = const 3:u64
            %v8: u64 = const 2:u64
            %v9: ptr<u64> = index_addr %v0, %v8
            store %v9, %v7
            %v10: u64[3] = load %v0
            ret %v10
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_array_lit_repeat_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64[3] {
            [0; 3]
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64[3] {
          locals:
            %l0: u64[3]
          bb0():
            %v0: ptr<u64[3]> = addr_of %l0
            %v1: u64 = const 0:u64
            %v2: u64 = const 0:u64
            %v3: ptr<u64> = index_addr %v0, %v2
            store %v3, %v1
            %v4: u64 = const 1:u64
            %v5: ptr<u64> = index_addr %v0, %v4
            store %v5, %v1
            %v6: u64 = const 2:u64
            %v7: ptr<u64> = index_addr %v0, %v6
            store %v7, %v1
            %v8: u64[3] = load %v0
            ret %v8
        }
    "};
    assert_eq!(text, expected);
}
