use super::{analyze, formact_func, indoc, lower_func};

#[test]
fn test_lower_if_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            if true {
                1
            } else {
                2
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
          bb0():
            %v0: bool = const true
            cbr %v0, bb1, bb2

          bb1():
            %v2: u64 = const 1:u64
            br bb3(%v2)

          bb2():
            %v3: u64 = const 2:u64
            br bb3(%v3)

          bb3(%v1: u64):
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_return_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> () {
            if true {
                return;
            } else {
                return;
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
        fn main() -> () {
          bb0():
            %v0: bool = const true
            cbr %v0, bb1, bb2

          bb1():
            ret

          bb2():
            ret

          bb3(%v1: ()):
            unreachable
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_stmt_side_effect() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            if true {
                return 1;
            } else {
                i = i + 1;
            };
            i
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
            %v1: bool = const true
            cbr %v1, bb1, bb2

          bb1():
            %v4: u64 = const 1:u64
            ret %v4

          bb2():
            %v5: u64 = const 1:u64
            %v6: u64 = add %v0, %v5
            %v7: () = const ()
            br bb3(%v7, %v6)

          bb3(%v2: (), %v3: u64):
            ret %v3
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_if_cmp_return() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            if 1 < 2 {
                3
            } else {
                4
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
          bb0():
            %v0: u64 = const 1:u64
            %v1: u64 = const 2:u64
            %v2: bool = cmp.lt %v0, %v1
            cbr %v2, bb1, bb2

          bb1():
            %v4: u64 = const 3:u64
            br bb3(%v4)

          bb2():
            %v5: u64 = const 4:u64
            br bb3(%v5)

          bb3(%v3: u64):
            ret %v3
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_logical_and() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            false && true
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
            %v0: bool = const false
            cbr %v0, bb1, bb2

          bb1():
            %v2: bool = const true
            br bb3(%v2)

          bb2():
            %v3: bool = const false
            br bb3(%v3)

          bb3(%v1: bool):
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_logical_or() {
    let ctx = analyze(indoc! {"
        fn main() -> bool {
            true || false
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
            cbr %v0, bb2, bb1

          bb1():
            %v2: bool = const false
            br bb3(%v2)

          bb2():
            %v3: bool = const true
            br bb3(%v3)

          bb3(%v1: bool):
            ret %v1
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_while_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                i = i + 1;
            }
            i
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
            br bb1(%v0)

          bb1(%v1: u64):
            %v3: u64 = const 2:u64
            %v4: bool = cmp.lt %v1, %v3
            cbr %v4, bb2, bb3(%v1)

          bb2():
            %v5: u64 = const 1:u64
            %v6: u64 = add %v1, %v5
            %v7: () = const ()
            br bb1(%v6)

          bb3(%v2: u64):
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_for_range_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var acc = 0;
            for i in 0..3 {
                acc = acc + i;
            }
            acc
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
            %v1: u64 = const 0:u64
            %v2: u64 = const 3:u64
            br bb1(%v0, %v1, %v2)

          bb1(%v3: u64, %v4: u64, %v5: u64):
            %v9: bool = cmp.lt %v4, %v5
            cbr %v9, bb2, bb3(%v3, %v4, %v5)

          bb2():
            %v10: u64 = const 1:u64
            %v11: u64 = add %v4, %v10
            %v12: u64 = add %v3, %v4
            %v13: () = const ()
            %v14: () = const ()
            br bb1(%v12, %v11, %v5)

          bb3(%v6: u64, %v7: u64, %v8: u64):
            %v15: () = const ()
            ret %v6
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_while_return_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                return 1;
            }
            2
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
            br bb1(%v0)

          bb1(%v1: u64):
            %v3: u64 = const 2:u64
            %v4: bool = cmp.lt %v1, %v3
            cbr %v4, bb2, bb3(%v1)

          bb2():
            %v5: u64 = const 1:u64
            ret %v5

          bb3(%v2: u64):
            %v6: u64 = const 2:u64
            ret %v6
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_while_break_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                break;
            }
            i
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
            br bb1(%v0)

          bb1(%v1: u64):
            %v3: u64 = const 2:u64
            %v4: bool = cmp.lt %v1, %v3
            cbr %v4, bb2, bb3(%v1)

          bb2():
            br bb3(%v1)

          bb3(%v2: u64):
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}

#[test]
fn test_lower_while_continue_stmt() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            var i = 0;
            while i < 2 {
                continue;
            }
            i
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
            br bb1(%v0)

          bb1(%v1: u64):
            %v3: u64 = const 2:u64
            %v4: bool = cmp.lt %v1, %v3
            cbr %v4, bb2, bb3(%v1)

          bb2():
            br bb1(%v1)

          bb3(%v2: u64):
            ret %v2
        }
    "};
    assert_eq!(text, expected);
}
