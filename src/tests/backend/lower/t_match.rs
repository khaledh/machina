use super::{analyze, assert_ir_eq, format_func, indoc, lower_func};

#[test]
fn test_lower_match_enum_no_payload() {
    let ctx = analyze(indoc! {"
        type Flag = Off | On

        fn main() -> u64 {
            match Flag::On {
                Flag::Off => 0,
                Flag::On => 1,
            }
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
    let text = format_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: Flag
            %l1: Flag
            %l2: Flag
          bb0():
            %v0: ptr<Flag> = addr_of %l0
            %v1: u32 = const 1:u32
            %v2: ptr<u32> = field_addr %v0, 0
            store %v2, %v1
            %v3: ptr<blob<0, align=1>> = field_addr %v0, 1
            %v4: Flag = load %v0
            %v5: ptr<Flag> = addr_of %l1
            %v6: ptr<Flag> = addr_of %l2
            store %v6, %v4
            %v7: u64 = const 4:u64
            memcpy %v5, %v6, %v7
            %v8: ptr<u32> = field_addr %v5, 0
            %v9: u32 = load %v8
            switch %v9 {
              case 0:u32 -> bb1
              case 1:u32 -> bb2
              default -> bb3
            }

          bb1():
            %v11: u64 = const 0:u64
            br bb4(%v11)

          bb2():
            %v12: u64 = const 1:u64
            br bb4(%v12)

          bb3():
            unreachable

          bb4(%v10: u64):
            ret %v10
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_match_enum_payload_binding() {
    let ctx = analyze(indoc! {"
        type Option = None | Some(u64)

        fn main() -> u64 {
            match Option::Some(42) {
                Option::None => 0,
                Option::Some(value) => value,
            }
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
    let text = format_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: Option
            %l1: Option
            %l2: Option
          bb0():
            %v0: ptr<Option> = addr_of %l0
            %v1: u32 = const 1:u32
            %v2: ptr<u32> = field_addr %v0, 0
            store %v2, %v1
            %v3: ptr<blob<8, align=8>> = field_addr %v0, 1
            %v4: u64 = const 42:u64
            %v5: u64 = const 0:u64
            %v6: ptr<u8> = index_addr %v3, %v5
            %v7: ptr<u64> = cast.ptr %v6 to ptr<u64>
            store %v7, %v4
            %v8: Option = load %v0
            %v9: ptr<Option> = addr_of %l1
            %v10: ptr<Option> = addr_of %l2
            store %v10, %v8
            %v11: u64 = const 16:u64
            memcpy %v9, %v10, %v11
            %v12: ptr<u32> = field_addr %v9, 0
            %v13: ptr<u32> = cast.ptr %v12 to ptr<u32>
            %v14: u32 = load %v13
            switch %v14 {
              case 0:u32 -> bb1
              case 1:u32 -> bb2
              default -> bb3
            }

          bb1():
            %v16: u64 = const 0:u64
            br bb4(%v16)

          bb2():
            %v17: ptr<blob<8, align=8>> = field_addr %v9, 1
            %v18: u64 = const 0:u64
            %v19: ptr<u8> = index_addr %v17, %v18
            %v20: ptr<u64> = cast.ptr %v19 to ptr<u64>
            %v21: u64 = load %v20
            br bb4(%v21)

          bb3():
            unreachable

          bb4(%v15: u64):
            ret %v15
        }
    "};
    assert_ir_eq(&text, expected);
}

#[test]
fn test_lower_match_tuple_decision_tree() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            match (true, 2) {
                (true, 1) => 10,
                (true, 2) => 20,
                _ => 0,
            }
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
    let text = format_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: (bool, i32)
            %l1: (bool, i32)
            %l2: (bool, i32)
          bb0():
            %v0: ptr<(bool, i32)> = addr_of %l0
            %v1: bool = const true
            %v2: ptr<bool> = field_addr %v0, 0
            store %v2, %v1
            %v3: i32 = const 2
            %v4: ptr<i32> = field_addr %v0, 1
            store %v4, %v3
            %v5: (bool, i32) = load %v0
            %v6: ptr<(bool, i32)> = addr_of %l1
            %v7: ptr<(bool, i32)> = addr_of %l2
            store %v7, %v5
            %v8: u64 = const 8
            memcpy %v6, %v7, %v8
            %v19: ptr<bool> = field_addr %v6, 0
            %v20: ptr<bool> = cast.ptr %v19 to ptr<bool>
            %v21: bool = load %v20
            %v22: bool = const true
            %v23: bool = cmp.eq %v21, %v22
            cbr %v23, bb9, bb5

          bb1():
            %v30: u64 = const 10:u64
            br bb10(%v30)

          bb2():
            %v31: u64 = const 20:u64
            br bb10(%v31)

          bb3():
            %v32: u64 = const 0:u64
            br bb10(%v32)

          bb4():
            br bb1

          bb5():
            %v9: ptr<bool> = field_addr %v6, 0
            %v10: ptr<bool> = cast.ptr %v9 to ptr<bool>
            %v11: bool = load %v10
            %v12: bool = const true
            %v13: bool = cmp.eq %v11, %v12
            cbr %v13, bb8, bb7

          bb6():
            br bb2

          bb7():
            br bb3

          bb8():
            %v14: ptr<i32> = field_addr %v6, 1
            %v15: ptr<i32> = cast.ptr %v14 to ptr<i32>
            %v16: i32 = load %v15
            %v17: i32 = const 2
            %v18: bool = cmp.eq %v16, %v17
            cbr %v18, bb6, bb7

          bb9():
            %v24: ptr<i32> = field_addr %v6, 1
            %v25: ptr<i32> = cast.ptr %v24 to ptr<i32>
            %v26: i32 = load %v25
            %v27: i32 = const 1
            %v28: bool = cmp.eq %v26, %v27
            cbr %v28, bb4, bb5

          bb10(%v29: u64):
            ret %v29
        }
    "};
    assert_ir_eq(&text, expected);
}
