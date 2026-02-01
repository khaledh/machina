use super::{analyze, assert_ir_eq, formact_func, indoc, lower_func};

#[test]
fn test_lower_const() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            42
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_range_type_value() {
    let ctx = analyze(indoc! {"
        fn main() -> range(0, 10) {
            3
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 3:u64
            ret %v0
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_range_lit_value() {
    let ctx = analyze(indoc! {"
        fn main() -> range(0, 3) {
            0..3
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 0:u64
            ret %v0
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            return 42;
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v0: u64 = const 42:u64
            ret %v0
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_var_decl() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var x: u64;
            x = 3;
            x
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: u64
          bb0():
            %v0: ptr<u64> = addr_of %l0
            %v1: u64 = const 3:u64
            store %v0, %v1
            %v2: u64 = load %v0
            ret %v2
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_binop() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            1 + 2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_param_binop() {
    let ctx = analyze(indoc! {"
        fn main(a: u64, b: u64) -> u64 {
            a + b
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64, u64) -> u64 {
          bb0(%v0: u64, %v1: u64):
            %v2: u64 = add %v0, %v1
            ret %v2
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_unop() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            ~1
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_unop_not() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            !true
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_char_lit() {
    let ctx = analyze(indoc! {"
        fn main() -> char {
            'a'
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u32 {
          bb0():
            %v0: u32 = const 97:u32
            ret %v0
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_string_lit() {
    let ctx = analyze(indoc! {"
        fn main() -> string {
            \"hi\"
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> string {
          locals:
            %l0: string
          bb0():
            %v0: ptr<string> = addr_of %l0
            %v1: ptr<u8> = const @g0
            %v2: u32 = const 2:u32
            %v3: u32 = const 0:u32
            %v4: ptr<ptr<u8>> = field_addr %v0, 0
            store %v4, %v1
            %v5: ptr<u32> = field_addr %v0, 1
            store %v5, %v2
            %v6: ptr<u32> = field_addr %v0, 2
            store %v6, %v3
            %v7: string = load %v0
            ret %v7
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_string_fmt_view() {
    let ctx = analyze(indoc! {r#"
        fn main(a: u64) -> string {
            f"{a}"
        }
    "#});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("__rt_fmt_init"));
    assert!(text.contains("__rt_fmt_append_u64"));
    assert!(text.contains("__rt_fmt_finish"));
}

#[test]
fn test_lower_string_fmt_owned() {
    let ctx = analyze(indoc! {r#"
        fn main(s: string) -> string {
            f"hi {s}"
        }
    "#});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("__rt_string_ensure"));
    assert!(text.contains("__rt_string_append_bytes"));
}

#[test]
fn test_lower_heap_alloc() {
    let ctx = analyze(indoc! {"
        fn main() -> ^u64 {
            ^42
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("__rt_alloc"));
    assert!(text.contains("store"));
}

#[test]
fn test_lower_drop_string_local() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var s = \"hi\";
            0
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("__rt_string_drop"));
}

#[test]
fn test_lower_cmp() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            1 < 2
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_tuple_lit() {
    let ctx = analyze(indoc! {"
        fn main() -> (u64, bool) {
            (1, true)
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_tuple_bind() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            let (a, b) = (1, 2);
            a + b
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: (u64, u64)
            %l1: (u64, u64)
            %l2: (u64, u64)
          bb0():
            %v0: ptr<(u64, u64)> = addr_of %l0
            %v1: u64 = const 1:u64
            %v2: ptr<u64> = field_addr %v0, 0
            store %v2, %v1
            %v3: u64 = const 2:u64
            %v4: ptr<u64> = field_addr %v0, 1
            store %v4, %v3
            %v5: (u64, u64) = load %v0
            %v6: ptr<(u64, u64)> = addr_of %l1
            %v7: ptr<(u64, u64)> = addr_of %l2
            store %v7, %v5
            %v8: u64 = const 16:u64
            memcpy %v6, %v7, %v8
            %v9: ptr<u64> = field_addr %v6, 0
            %v10: u64 = load %v9
            %v11: ptr<u64> = field_addr %v6, 1
            %v12: u64 = load %v11
            %v13: u64 = add %v10, %v12
            ret %v13
        }
    "};
    assert_ir_eq(&text, expected);
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
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_struct_bind() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main() -> u64 {
            let Pair { a, b } = Pair { a: 1, b: 2 };
            a + b
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: Pair
            %l1: Pair
            %l2: Pair
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
            %v7: ptr<Pair> = addr_of %l2
            store %v7, %v5
            %v8: u64 = const 16:u64
            memcpy %v6, %v7, %v8
            %v9: ptr<u64> = field_addr %v6, 0
            %v10: u64 = load %v9
            %v11: ptr<u64> = field_addr %v6, 1
            %v12: u64 = load %v11
            %v13: u64 = add %v10, %v12
            ret %v13
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_array_bind() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            let [a, b] = u64[1, 2];
            a + b
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: u64[2]
            %l1: u64[2]
            %l2: u64[2]
          bb0():
            %v0: ptr<u64[2]> = addr_of %l0
            %v1: u64 = const 1:u64
            %v2: u64 = const 0:u64
            %v3: ptr<u64> = index_addr %v0, %v2
            store %v3, %v1
            %v4: u64 = const 2:u64
            %v5: u64 = const 1:u64
            %v6: ptr<u64> = index_addr %v0, %v5
            store %v6, %v4
            %v7: u64[2] = load %v0
            %v8: ptr<u64[2]> = addr_of %l1
            %v9: ptr<u64[2]> = addr_of %l2
            store %v9, %v7
            %v10: u64 = const 16:u64
            memcpy %v8, %v9, %v10
            %v11: u64 = const 0:u64
            %v12: ptr<u64> = index_addr %v8, %v11
            %v13: u64 = load %v12
            %v14: u64 = const 1:u64
            %v15: ptr<u64> = index_addr %v8, %v14
            %v16: u64 = load %v15
            %v17: u64 = add %v13, %v16
            ret %v17
        }
    "};
    assert_ir_eq(&text, expected);
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
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Pair {
          locals:
            %l0: Pair
            %l1: Pair
            %l2: Pair
            %l3: Pair
            %l4: Pair
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
            %v7: ptr<Pair> = addr_of %l2
            store %v7, %v5
            %v8: u64 = const 16:u64
            memcpy %v6, %v7, %v8
            %v9: ptr<Pair> = addr_of %l3
            %v10: Pair = load %v6
            %v11: ptr<Pair> = addr_of %l4
            store %v11, %v10
            %v12: u64 = const 16:u64
            memcpy %v9, %v11, %v12
            %v13: u64 = const 5:u64
            %v14: ptr<u64> = field_addr %v9, 1
            store %v14, %v13
            %v15: Pair = load %v9
            ret %v15
        }
    "};
    assert_ir_eq(&text, expected);
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
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Pair {
          locals:
            %l0: Pair
            %l1: Pair
            %l2: Pair
            %l3: Pair
            %l4: Pair
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
            %v7: ptr<Pair> = addr_of %l2
            store %v7, %v5
            %v8: u64 = const 16:u64
            memcpy %v6, %v7, %v8
            %v9: ptr<Pair> = addr_of %l3
            %v10: Pair = load %v6
            %v11: ptr<Pair> = addr_of %l4
            store %v11, %v10
            %v12: u64 = const 16:u64
            memcpy %v9, %v11, %v12
            %v13: u64 = const 5:u64
            %v14: ptr<u64> = field_addr %v9, 1
            store %v14, %v13
            %v15: u64 = const 3:u64
            %v16: ptr<u64> = field_addr %v9, 0
            store %v16, %v15
            %v17: Pair = load %v9
            ret %v17
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_aggregate_assign_memcpy() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }
        type Outer = { inner: Pair }

        fn main() -> Pair {
            var o = Outer { inner: Pair { a: 1, b: 2 } };
            o.inner = Pair { a: 3, b: 4 };
            o.inner
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("memcpy"));
}

#[test]
fn test_lower_aggregate_assign_needs_drop_no_memcpy() {
    let ctx = analyze(indoc! {"
        type Wrap = { s: string }
        type Outer = { w: Wrap }

        fn main() -> Outer {
            var o = Outer { w: Wrap { s: \"hi\" } };
            o.w = Wrap { s: \"bye\" };
            o
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(!text.contains("memcpy"));
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
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Flag {
          locals:
            %l0: Flag
          bb0():
            %v0: ptr<Flag> = addr_of %l0
            %v1: u32 = const 1:u32
            %v2: ptr<u32> = field_addr %v0, 0
            store %v2, %v1
            %v3: ptr<blob<0, align=1>> = field_addr %v0, 1
            %v4: Flag = load %v0
            ret %v4
        }
    "};
    assert_ir_eq(&text, expected);
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
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> Option {
          locals:
            %l0: Option
            %l1: u64
          bb0():
            %v0: ptr<Option> = addr_of %l0
            %v1: u32 = const 1:u32
            %v2: ptr<u32> = field_addr %v0, 0
            store %v2, %v1
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_drop_enum_payload() {
    let ctx = analyze(indoc! {"
        type Option = None | Some(string)

        fn main() -> u64 {
            var opt = Option::Some(\"hi\");
            0
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("switch"));
    assert!(text.contains("__rt_string_drop"));
}

#[test]
fn test_lower_array_lit_elems() {
    let ctx = analyze(indoc! {"
        fn main() -> u64[3] {
            [1, 2, 3]
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_array_lit_repeat() {
    let ctx = analyze(indoc! {"
        fn main() -> u64[3] {
            [0; 3]
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
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
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_slice_expr_array() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            let arr = [1, 2, 3];
            let s: u64[] = arr[1..];
            0
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: u64[3]
            %l1: u64[3]
            %l2: u64[3]
            %l3: struct { ptr: ptr<u64>, len: u64 }
            %l4: struct { ptr: ptr<u64>, len: u64 }
            %l5: struct { ptr: ptr<u64>, len: u64 }
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
            %v11: ptr<u64[3]> = addr_of %l1
            %v12: ptr<u64[3]> = addr_of %l2
            store %v12, %v10
            %v13: u64 = const 24:u64
            memcpy %v11, %v12, %v13
            %v14: u64 = const 0:u64
            %v15: ptr<u64> = index_addr %v11, %v14
            %v16: u64 = const 3:u64
            %v17: u64 = const 1:u64
            %v18: u64 = const 0:u64
            %v19: u64 = const 1:u64
            %v20: u64 = add %v16, %v19
            %v21: bool = cmp.ge %v17, %v18
            %v22: bool = cmp.lt %v17, %v20
            %v23: bool = and %v21, %v22
            cbr %v23, bb1, bb2

          bb1():
            %v26: ptr<u64> = index_addr %v15, %v17
            %v27: u64 = sub %v16, %v17
            %v28: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l3
            %v29: ptr<ptr<u64>> = field_addr %v28, 0
            store %v29, %v26
            %v30: ptr<u64> = field_addr %v28, 1
            store %v30, %v27
            %v31: struct { ptr: ptr<u64>, len: u64 } = load %v28
            %v32: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l4
            %v33: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l5
            store %v33, %v31
            %v34: u64 = const 16:u64
            memcpy %v32, %v33, %v34
            %v35: u64 = const 0:u64
            ret %v35

          bb2():
            %v24: u64 = const 2:u64
            %v25: () = call @__rt_trap(%v24, %v17, %v18, %v20)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_slice_expr_string() {
    let ctx = analyze(indoc! {"
        fn main(s: string) -> u64 {
            let t: u8[] = s[1..];
            0
        }
    "});
    let func_def = ctx.module.func_defs()[0];
    let lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(ptr<string>) -> u64 {
          locals:
            %l0: string
            %l1: struct { ptr: ptr<u8>, len: u64 }
            %l2: struct { ptr: ptr<u8>, len: u64 }
            %l3: struct { ptr: ptr<u8>, len: u64 }
          bb0(%v0: ptr<string>):
            %v1: ptr<string> = addr_of %l0
            %v2: u64 = const 16:u64
            memcpy %v1, %v0, %v2
            %v3: ptr<ptr<u8>> = field_addr %v1, 0
            %v4: ptr<u8> = load %v3
            %v5: ptr<u32> = field_addr %v1, 1
            %v6: u32 = load %v5
            %v7: u64 = zext %v6 to u64
            %v8: u64 = const 1:u64
            %v9: u64 = const 0:u64
            %v10: u64 = const 1:u64
            %v11: u64 = add %v7, %v10
            %v12: bool = cmp.ge %v8, %v9
            %v13: bool = cmp.lt %v8, %v11
            %v14: bool = and %v12, %v13
            cbr %v14, bb1, bb2

          bb1():
            %v17: ptr<u8> = index_addr %v4, %v8
            %v18: u64 = sub %v7, %v8
            %v19: ptr<struct { ptr: ptr<u8>, len: u64 }> = addr_of %l1
            %v20: ptr<ptr<u8>> = field_addr %v19, 0
            store %v20, %v17
            %v21: ptr<u64> = field_addr %v19, 1
            store %v21, %v18
            %v22: struct { ptr: ptr<u8>, len: u64 } = load %v19
            %v23: ptr<struct { ptr: ptr<u8>, len: u64 }> = addr_of %l2
            %v24: ptr<struct { ptr: ptr<u8>, len: u64 }> = addr_of %l3
            store %v24, %v22
            %v25: u64 = const 16:u64
            memcpy %v23, %v24, %v25
            %v26: u64 = const 0:u64
            ret %v26

          bb2():
            %v15: u64 = const 2:u64
            %v16: () = call @__rt_trap(%v15, %v8, %v9, %v11)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}
