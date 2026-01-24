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

    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
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

    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
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

    let lowered = lower_func(main_def, &ctx.def_table, &ctx.type_map, &ctx.lowering_plans)
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
