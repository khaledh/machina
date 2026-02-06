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
            %l1: u64
            %l2: Option
            %l3: Option
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
            %v10: ptr<Option> = addr_of %l2
            %v11: ptr<Option> = addr_of %l3
            store %v11, %v9
            %v12: u64 = const 16:u64
            memcpy %v10, %v11, %v12
            %v13: ptr<u32> = field_addr %v10, 0
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
            %v17: ptr<blob<8, align=8>> = field_addr %v10, 1
            %v18: u64 = const 0:u64
            %v19: ptr<u8> = index_addr %v17, %v18
            %v20: u64 = load %v19
            br bb4(%v20)

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
            %v17: ptr<bool> = field_addr %v6, 0
            %v18: bool = load %v17
            %v19: bool = const true
            %v20: bool = cmp.eq %v18, %v19
            cbr %v20, bb9, bb5

          bb1():
            %v26: u64 = const 10:u64
            br bb10(%v26)

          bb2():
            %v27: u64 = const 20:u64
            br bb10(%v27)

          bb3():
            %v28: u64 = const 0:u64
            br bb10(%v28)

          bb4():
            br bb1

          bb5():
            %v9: ptr<bool> = field_addr %v6, 0
            %v10: bool = load %v9
            %v11: bool = const true
            %v12: bool = cmp.eq %v10, %v11
            cbr %v12, bb8, bb7

          bb6():
            br bb2

          bb7():
            br bb3

          bb8():
            %v13: ptr<i32> = field_addr %v6, 1
            %v14: i32 = load %v13
            %v15: i32 = const 2
            %v16: bool = cmp.eq %v14, %v15
            cbr %v16, bb6, bb7

          bb9():
            %v21: ptr<i32> = field_addr %v6, 1
            %v22: i32 = load %v21
            %v23: i32 = const 1
            %v24: bool = cmp.eq %v22, %v23
            cbr %v24, bb4, bb5

          bb10(%v25: u64):
            ret %v25
        }
    "};
    assert_ir_eq(&text, expected);
}
