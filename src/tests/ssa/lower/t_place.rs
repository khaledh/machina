use super::{analyze, formact_func, indoc, lower_func};

#[test]
fn test_lower_struct_field_load() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        fn main(p: Pair) -> u64 {
            p.a
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(Pair) -> u64 {
          locals:
            %l0: Pair
          bb0(%v0: Pair):
            %v1: ptr<Pair> = addr_of %l0
            store %v1, %v0
            %v2: ptr<u64> = field_addr %v1, 0
            %v3: u64 = load %v2
            ret %v3
        }
    "};
    assert_eq!(text, expected);
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
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(Pair) -> u64 {
          locals:
            %l0: Pair
          bb0(%v0: Pair):
            %v1: u64 = const 5:u64
            %v2: ptr<Pair> = addr_of %l0
            store %v2, %v0
            %v3: ptr<u64> = field_addr %v2, 0
            store %v3, %v1
            %v4: ptr<u64> = field_addr %v2, 0
            %v5: u64 = load %v4
            ret %v5
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_array_index_load() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[3]) -> u64 {
            a[1]
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64[3]) -> u64 {
          locals:
            %l0: u64[3]
          bb0(%v0: u64[3]):
            %v1: ptr<u64[3]> = addr_of %l0
            store %v1, %v0
            %v2: u64 = const 1:u64
            %v3: ptr<u64> = index_addr %v1, %v2
            %v4: u64 = load %v3
            ret %v4
        }
    "};
    assert_eq!(text, expected);
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
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64[3]) -> u64 {
          locals:
            %l0: u64[3]
          bb0(%v0: u64[3]):
            %v1: u64 = const 9:u64
            %v2: ptr<u64[3]> = addr_of %l0
            store %v2, %v0
            %v3: u64 = const 1:u64
            %v4: ptr<u64> = index_addr %v2, %v3
            store %v4, %v1
            %v5: u64 = const 1:u64
            %v6: ptr<u64> = index_addr %v2, %v5
            %v7: u64 = load %v6
            ret %v7
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_array_index_multi_dim() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[2, 2]) -> u64 {
            a[1, 0]
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64[2, 2]) -> u64 {
          locals:
            %l0: u64[2, 2]
          bb0(%v0: u64[2, 2]):
            %v1: ptr<u64[2, 2]> = addr_of %l0
            store %v1, %v0
            %v2: u64 = const 1:u64
            %v3: ptr<u64[2]> = index_addr %v1, %v2
            %v4: u64 = const 0:u64
            %v5: ptr<u64> = index_addr %v3, %v4
            %v6: u64 = load %v5
            ret %v6
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_array_index_partial() {
    let ctx = analyze(indoc! {"
        fn main(a: u64[2, 2]) -> u64[2] {
            a[1]
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(u64[2, 2]) -> u64[2] {
          locals:
            %l0: u64[2, 2]
          bb0(%v0: u64[2, 2]):
            %v1: ptr<u64[2, 2]> = addr_of %l0
            store %v1, %v0
            %v2: u64 = const 1:u64
            %v3: ptr<u64[2]> = index_addr %v1, %v2
            %v4: u64[2] = load %v3
            ret %v4
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_slice_index_load() {
    let ctx = analyze(indoc! {"
        fn main(s: u64[]) -> u64 {
            s[1]
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
        .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn main(struct { ptr: ptr<u64>, len: u64 }) -> u64 {
          locals:
            %l0: struct { ptr: ptr<u64>, len: u64 }
          bb0(%v0: struct { ptr: ptr<u64>, len: u64 }):
            %v1: ptr<struct { ptr: ptr<u64>, len: u64 }> = addr_of %l0
            store %v1, %v0
            %v2: ptr<ptr<u64>> = field_addr %v1, 0
            %v3: ptr<u64> = load %v2
            %v4: u64 = const 1:u64
            %v5: ptr<u64> = index_addr %v3, %v4
            %v6: u64 = load %v5
            ret %v6
        }
    "};
    assert_eq!(text, expected);
}
