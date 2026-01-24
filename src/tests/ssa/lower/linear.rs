use super::{analyze, formact_func, indoc, lower_func};

#[test]
fn test_lower_const() {
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
fn test_lower_stmt() {
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
fn test_lower_binop() {
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
fn test_lower_param_binop() {
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
fn test_lower_unop() {
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
fn test_lower_unop_not() {
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
fn test_lower_cmp() {
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
fn test_lower_tuple_lit() {
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
fn test_lower_struct_lit() {
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
fn test_lower_struct_update() {
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
fn test_lower_struct_update_multi_field() {
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
fn test_lower_enum_variant_no_payload() {
    let ctx = analyze(indoc! {"
        type Flag = Off | On

        fn main() -> Flag {
            Flag::On
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Flag {
          locals:
            %l0: Flag
          bb0():
            %v0: ptr<Flag> = addr_of %l0
            %v1: ptr<u32> = field_addr %v0, 0
            %v2: u32 = const 1:u32
            store %v1, %v2
            %v3: ptr<blob<0, align=1>> = field_addr %v0, 1
            %v4: Flag = load %v0
            ret %v4
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_enum_variant_payload() {
    let ctx = analyze(indoc! {"
        type Option = None | Some(u64)

        fn main() -> Option {
            Option::Some(42)
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(func_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Option {
          locals:
            %l0: Option
            %l1: u64
          bb0():
            %v0: ptr<Option> = addr_of %l0
            %v1: ptr<u32> = field_addr %v0, 0
            %v2: u32 = const 1:u32
            store %v1, %v2
            %v3: ptr<blob<8, align=8>> = field_addr %v0, 1
            %v4: u64 = const 42:u64
            %v5: ptr<u64> = addr_of %l1
            store %v5, %v4
            %v6: u64 = const 0:u64
            %v7: ptr<u8> = index_addr %v3, %v6
            %v8: u64 = const 8:u64
            memcpy %v7, %v5, %v8
            %v9: Option = load %v0
            ret %v9
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_array_lit_elems() {
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
fn test_lower_array_lit_repeat() {
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
