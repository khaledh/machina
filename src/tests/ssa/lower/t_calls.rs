use super::{analyze, formact_func, indoc, lower_func};
use crate::tree::semantic::MethodItem;

#[test]
fn test_lower_call_with_params() {
    let ctx = analyze(indoc! {"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }

        fn main() -> u64 {
            add(1, 2)
        }
    "});
    let add_def = ctx.module.func_defs()[0];
    let main_def = ctx.module.func_defs()[1];
    let add_id = add_def.def_id;

    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: u64 = const 1:u64
                %v1: u64 = const 2:u64
                %v2: u64 = call @{}(%v0, %v1)
                ret %v2
            }}
        "},
        add_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_call_stmt() {
    let ctx = analyze(indoc! {"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }

        fn main() -> u64 {
            add(1, 2);
            3
        }
    "});
    let add_def = ctx.module.func_defs()[0];
    let main_def = ctx.module.func_defs()[1];
    let add_id = add_def.def_id;

    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: u64 = const 1:u64
                %v1: u64 = const 2:u64
                %v2: u64 = call @{}(%v0, %v1)
                %v3: u64 = const 3:u64
                ret %v3
            }}
        "},
        add_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_method_call_param() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64, b: u64 }

        Pair :: {
            fn sum(self) -> u64 {
                self.a + self.b
            }
        }

        fn main(p: Pair) -> u64 {
            p.sum()
        }
    "});

    let method_def = ctx
        .module
        .method_blocks()
        .iter()
        .flat_map(|block| block.method_items.iter())
        .find_map(|item| match item {
            MethodItem::Def(def) => Some(def),
            MethodItem::Decl(_) => None,
        })
        .expect("missing method def");
    let method_def_id = method_def.def_id;

    let main_def = ctx.module.func_defs()[0];

    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main(Pair) -> u64 {{
              bb0(%v0: Pair):
                %v1: u64 = call @{}(%v0)
                ret %v1
            }}
        "},
        method_def_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_call_array_to_slice_arg() {
    let ctx = analyze(indoc! {"
        fn take(xs: u64[]) -> u64 {
            xs[1]
        }

        fn main() -> u64 {
            let arr = [1, 2, 3];
            take(arr)
        }
    "});
    let take_def = ctx.module.func_defs()[0];
    let main_def = ctx.module.func_defs()[1];
    let take_id = take_def.def_id;

    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              locals:
                %l0: u64[3]
                %l1: u64[3]
                %l2: struct {{ ptr: ptr<u64>, len: u64 }}
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
                store %v11, %v10
                %v12: u64 = const 0:u64
                %v13: ptr<u64> = index_addr %v11, %v12
                %v14: u64 = const 3:u64
                %v15: u64 = const 0:u64
                %v16: ptr<u64> = index_addr %v13, %v15
                %v17: u64 = sub %v14, %v15
                %v18: ptr<struct {{ ptr: ptr<u64>, len: u64 }}> = addr_of %l2
                %v19: ptr<ptr<u64>> = field_addr %v18, 0
                store %v19, %v16
                %v20: ptr<u64> = field_addr %v18, 1
                store %v20, %v17
                %v21: struct {{ ptr: ptr<u64>, len: u64 }} = load %v18
                %v22: u64 = call @{}(%v21)
                ret %v22
            }}
        "},
        take_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_indirect_call() {
    let ctx = analyze(indoc! {"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }

        fn main() -> u64 {
            let f: fn(u64, u64) -> u64 = add;
            f(1, 2)
        }
    "});
    let add_def = ctx.module.func_defs()[0];
    let main_def = ctx.module.func_defs()[1];
    let add_id = add_def.def_id;

    let lowered = lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: fn(u64, u64) -> u64 = const @{}
                %v1: u64 = const 1:u64
                %v2: u64 = const 2:u64
                %v3: u64 = call %v0(%v1, %v2)
                ret %v3
            }}
        "},
        add_id
    );
    assert_eq!(text, expected);
}

#[test]
fn test_lower_out_param_def() {
    let ctx = analyze(indoc! {"
        type Pair = { a: u64 }

        fn set(out p: Pair) {
            p.a = 3;
        }

        fn main() -> u64 {
            0
        }
    "});
    let set_def = ctx.module.func_defs()[0];

    let lowered = lower_func(
        set_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");
    let text = formact_func(&lowered.func, &lowered.types);

    let expected = indoc! {"
        fn set(ptr<Pair>) -> () {
          bb0(%v0: ptr<Pair>):
            %v1: u64 = const 3:u64
            %v2: ptr<u64> = field_addr %v0, 0
            store %v2, %v1
            %v3: () = const ()
            ret %v3
        }
    "};
    assert_eq!(text, expected);
}
