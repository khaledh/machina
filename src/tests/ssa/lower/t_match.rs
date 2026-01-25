use super::{analyze, formact_func, indoc, lower_func};

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
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: Flag
            %l1: Flag
          bb0():
            %v0: ptr<Flag> = addr_of %l0
            %v1: u32 = const 1:u32
            %v2: ptr<u32> = field_addr %v0, 0
            store %v2, %v1
            %v3: ptr<blob<0, align=1>> = field_addr %v0, 1
            %v4: Flag = load %v0
            %v5: ptr<Flag> = addr_of %l1
            store %v5, %v4
            %v6: ptr<u32> = field_addr %v5, 0
            %v7: u32 = load %v6
            switch %v7 {
              case 0:u32 -> bb1
              case 1:u32 -> bb2
              default -> bb3
            }

          bb1():
            %v9: u64 = const 0:u64
            br bb4(%v9)

          bb2():
            %v10: u64 = const 1:u64
            br bb4(%v10)

          bb3():
            unreachable

          bb4(%v8: u64):
            ret %v8
        }
    "};
    assert_eq!(text, expected);
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
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main() -> u64 {
          locals:
            %l0: Option
            %l1: u64
            %l2: Option
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
            store %v10, %v9
            %v11: ptr<u32> = field_addr %v10, 0
            %v12: u32 = load %v11
            switch %v12 {
              case 0:u32 -> bb1
              case 1:u32 -> bb2
              default -> bb3
            }

          bb1():
            %v14: u64 = const 0:u64
            br bb4(%v14)

          bb2():
            %v15: ptr<blob<8, align=8>> = field_addr %v10, 1
            %v16: u64 = const 0:u64
            %v17: ptr<u8> = index_addr %v15, %v16
            %v18: u64 = load %v17
            br bb4(%v18)

          bb3():
            unreachable

          bb4(%v13: u64):
            ret %v13
        }
    "};
    assert_eq!(text, expected);
}
