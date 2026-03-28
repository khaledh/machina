use super::{analyze, assert_ir_eq, format_func, indoc, lower_func};

#[test]
fn test_lower_struct_field_load() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main(p: Pair) -> u64 {
            p.a
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

    let expected = indoc! {"
        fn main(ptr<Pair>) -> u64 {
          locals:
            %l0: Pair
          bb0(%v0: ptr<Pair>):
            %v1: ptr<Pair> = addr_of %l0
            %v2: u64 = const 16:u64
            memcpy %v1, %v0, %v2
            %v3: ptr<u64> = field_addr %v1, 0
            %v4: u64 = load %v3
            ret %v4
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_struct_field_assign() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main(p: Pair) -> u64 {
            var q = p;
            q.a = 5;
            q.a
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

    let expected = indoc! {"
        fn main(ptr<Pair>) -> u64 {
          locals:
            %l0: Pair
            %l1: Pair
          bb0(%v0: ptr<Pair>):
            %v1: ptr<Pair> = addr_of %l0
            %v2: u64 = const 16:u64
            memcpy %v1, %v0, %v2
            %v3: Pair = load %v1
            %v4: ptr<Pair> = addr_of %l1
            store %v4, %v3
            %v5: u64 = const 5:u64
            %v6: ptr<u64> = field_addr %v4, 0
            store %v6, %v5
            %v7: ptr<u64> = field_addr %v4, 0
            %v8: u64 = load %v7
            ret %v8
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_array_index_load() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[3]) -> u64 {
            a[1]
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

    let expected = indoc! {"
        fn main(ptr<u64[3]>) -> u64 {
          locals:
            %l0: u64[3]
          bb0(%v0: ptr<u64[3]>):
            %v1: ptr<u64[3]> = addr_of %l0
            %v2: u64 = const 24:u64
            memcpy %v1, %v0, %v2
            %v3: u64 = const 1:u64
            %v4: u64 = const 0:u64
            %v5: ptr<u64> = index_addr %v1, %v4
            %v6: u64 = const 3:u64
            %v7: bool = cmp.lt %v3, %v6
            cbr %v7, bb1, bb2

          bb1():
            %v11: ptr<u64> = index_addr %v5, %v3
            %v12: u64 = load %v11
            ret %v12

          bb2():
            %v8: u64 = const 1:u64
            %v9: u64 = const 0:u64
            %v10: () = call @__rt_trap(%v8, %v3, %v6, %v9)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_array_index_assign() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[3]) -> u64 {
            var b = a;
            b[1] = 9;
            b[1]
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

    let expected = indoc! {"
        fn main(ptr<u64[3]>) -> u64 {
          locals:
            %l0: u64[3]
            %l1: u64[3]
          bb0(%v0: ptr<u64[3]>):
            %v1: ptr<u64[3]> = addr_of %l0
            %v2: u64 = const 24:u64
            memcpy %v1, %v0, %v2
            %v3: u64[3] = load %v1
            %v4: ptr<u64[3]> = addr_of %l1
            store %v4, %v3
            %v5: u64 = const 9:u64
            %v6: u64 = const 1:u64
            %v7: u64 = const 0:u64
            %v8: ptr<u64> = index_addr %v4, %v7
            %v9: u64 = const 3:u64
            %v10: bool = cmp.lt %v6, %v9
            cbr %v10, bb1, bb2

          bb1():
            %v14: ptr<u64> = index_addr %v8, %v6
            store %v14, %v5
            %v15: u64 = const 1:u64
            %v16: u64 = const 0:u64
            %v17: ptr<u64> = index_addr %v4, %v16
            %v18: u64 = const 3:u64
            %v19: bool = cmp.lt %v15, %v18
            cbr %v19, bb3, bb4

          bb2():
            %v11: u64 = const 1:u64
            %v12: u64 = const 0:u64
            %v13: () = call @__rt_trap(%v11, %v6, %v9, %v12)
            unreachable

          bb3():
            %v23: ptr<u64> = index_addr %v17, %v15
            %v24: u64 = load %v23
            ret %v24

          bb4():
            %v20: u64 = const 1:u64
            %v21: u64 = const 0:u64
            %v22: () = call @__rt_trap(%v20, %v15, %v18, %v21)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_array_index_multi_dim() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[2, 2]) -> u64 {
            a[1, 0]
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

    let expected = indoc! {"
        fn main(ptr<u64[2, 2]>) -> u64 {
          locals:
            %l0: u64[2, 2]
          bb0(%v0: ptr<u64[2, 2]>):
            %v1: ptr<u64[2, 2]> = addr_of %l0
            %v2: u64 = const 32:u64
            memcpy %v1, %v0, %v2
            %v3: u64 = const 1:u64
            %v4: u64 = const 0:u64
            %v5: ptr<u64[2]> = index_addr %v1, %v4
            %v6: u64 = const 2:u64
            %v7: bool = cmp.lt %v3, %v6
            cbr %v7, bb1, bb2

          bb1():
            %v11: ptr<u64[2]> = index_addr %v5, %v3
            %v12: u64 = const 0:u64
            %v13: u64 = const 0:u64
            %v14: ptr<u64> = index_addr %v11, %v13
            %v15: u64 = const 2:u64
            %v16: bool = cmp.lt %v12, %v15
            cbr %v16, bb3, bb4

          bb2():
            %v8: u64 = const 1:u64
            %v9: u64 = const 0:u64
            %v10: () = call @__rt_trap(%v8, %v3, %v6, %v9)
            unreachable

          bb3():
            %v20: ptr<u64> = index_addr %v14, %v12
            %v21: u64 = load %v20
            ret %v21

          bb4():
            %v17: u64 = const 1:u64
            %v18: u64 = const 0:u64
            %v19: () = call @__rt_trap(%v17, %v12, %v15, %v18)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_array_index_partial() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[2, 2]) -> u64[2] {
            a[1]
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

    let expected = indoc! {"
        fn main(ptr<u64[2, 2]>) -> u64[2] {
          locals:
            %l0: u64[2, 2]
          bb0(%v0: ptr<u64[2, 2]>):
            %v1: ptr<u64[2, 2]> = addr_of %l0
            %v2: u64 = const 32:u64
            memcpy %v1, %v0, %v2
            %v3: u64 = const 1:u64
            %v4: u64 = const 0:u64
            %v5: ptr<u64[2]> = index_addr %v1, %v4
            %v6: u64 = const 2:u64
            %v7: bool = cmp.lt %v3, %v6
            cbr %v7, bb1, bb2

          bb1():
            %v11: ptr<u64[2]> = index_addr %v5, %v3
            %v12: u64[2] = load %v11
            ret %v12

          bb2():
            %v8: u64 = const 1:u64
            %v9: u64 = const 0:u64
            %v10: () = call @__rt_trap(%v8, %v3, %v6, %v9)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_slice_index_load() {
    let ctx = analyze(indoc! {"
        fn main(s: u64[]) -> u64 {
            s[1]
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

    let expected = indoc! {"
        fn main(ptr<struct { ptr: ptr<u64>, len: u64 }>) -> u64 {
          locals:
            %l0: struct { ptr: ptr<u64>, len: u64 }
          bb0(%v0: ptr<struct { ptr: ptr<u64>, len: u64 }>):
            %v1: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l0
            %v2: u64 = const 16:u64
            memcpy %v1, %v0, %v2
            %v3: ptr<ptr<u64>> = field_addr %v1, 0
            %v4: ptr<u64> = load %v3
            %v5: ptr<u64> = field_addr %v1, 1
            %v6: u64 = load %v5
            %v7: u64 = const 1:u64
            %v8: bool = cmp.lt %v7, %v6
            cbr %v8, bb1, bb2

          bb1():
            %v12: ptr<u64> = index_addr %v4, %v7
            %v13: u64 = load %v12
            ret %v13

          bb2():
            %v9: u64 = const 1:u64
            %v10: u64 = const 0:u64
            %v11: () = call @__rt_trap(%v9, %v7, %v6, %v10)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_slice_index_assign() {
    let ctx = analyze(indoc! {"
        fn main(s: u64[]) -> u64 {
            var t = s;
            t[1] = 9;
            t[1]
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

    let expected = indoc! {"
        fn main(ptr<struct { ptr: ptr<u64>, len: u64 }>) -> u64 {
          locals:
            %l0: struct { ptr: ptr<u64>, len: u64 }
            %l1: struct { ptr: ptr<u64>, len: u64 }
          bb0(%v0: ptr<struct { ptr: ptr<u64>, len: u64 }>):
            %v1: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l0
            %v2: u64 = const 16:u64
            memcpy %v1, %v0, %v2
            %v3: struct { ptr: ptr<u64>, len: u64 } = load %v1
            %v4: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l1
            store %v4, %v3
            %v5: u64 = const 9:u64
            %v6: ptr<ptr<u64>> = field_addr %v4, 0
            %v7: ptr<u64> = load %v6
            %v8: ptr<u64> = field_addr %v4, 1
            %v9: u64 = load %v8
            %v10: u64 = const 1:u64
            %v11: bool = cmp.lt %v10, %v9
            cbr %v11, bb1, bb2

          bb1():
            %v15: ptr<u64> = index_addr %v7, %v10
            store %v15, %v5
            %v16: ptr<ptr<u64>> = field_addr %v4, 0
            %v17: ptr<u64> = load %v16
            %v18: ptr<u64> = field_addr %v4, 1
            %v19: u64 = load %v18
            %v20: u64 = const 1:u64
            %v21: bool = cmp.lt %v20, %v19
            cbr %v21, bb3, bb4

          bb2():
            %v12: u64 = const 1:u64
            %v13: u64 = const 0:u64
            %v14: () = call @__rt_trap(%v12, %v10, %v9, %v13)
            unreachable

          bb3():
            %v25: ptr<u64> = index_addr %v17, %v20
            %v26: u64 = load %v25
            ret %v26

          bb4():
            %v22: u64 = const 1:u64
            %v23: u64 = const 0:u64
            %v24: () = call @__rt_trap(%v22, %v20, %v19, %v23)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_string_index_load() {
    let ctx = analyze(indoc! {"
        fn main(s: string) -> u8 {
            s[1]
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

    let expected = indoc! {"
        fn main(ptr<string>) -> u8 {
          locals:
            %l0: string
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
            %v9: bool = cmp.lt %v8, %v7
            cbr %v9, bb1, bb2

          bb1():
            %v13: ptr<u8> = index_addr %v4, %v8
            %v14: u8 = load %v13
            ret %v14

          bb2():
            %v10: u64 = const 1:u64
            %v11: u64 = const 0:u64
            %v12: () = call @__rt_trap(%v10, %v8, %v7, %v11)
            unreachable
        }
    "};
    assert_ir_eq(&text, expected);
}
