use super::{analyze, formact_func, indoc, lower_module};
use std::collections::HashMap;

#[test]
fn test_lower_module_globals() {
    let ctx = analyze(indoc! {r#"
        fn a() -> string {
            "a"
        }

        fn b() -> string {
            "b"
        }
    "#});

    let lowered = lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .unwrap();

    assert_eq!(lowered.funcs.len(), 2);
    assert_eq!(lowered.globals.len(), 2);

    let a_text = formact_func(&lowered.funcs[0].func, &lowered.funcs[0].types);
    let b_text = formact_func(&lowered.funcs[1].func, &lowered.funcs[1].types);

    assert!(a_text.contains("@g0"));
    assert!(b_text.contains("@g1"));
}

#[test]
fn test_lower_module_method_defs() {
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

    let method_def_id = ctx
        .module
        .method_blocks()
        .iter()
        .flat_map(|block| block.method_items.iter())
        .find_map(|item| match item {
            crate::tree::semantic::MethodItem::Def(def) => Some(def.def_id),
            crate::tree::semantic::MethodItem::Decl(_) => None,
        })
        .expect("missing method def");

    let lowered = lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .unwrap();

    let mut func_texts = HashMap::new();
    for lowered_func in &lowered.funcs {
        let text = formact_func(&lowered_func.func, &lowered_func.types);
        func_texts.insert(lowered_func.func.name.clone(), text);
    }

    let main_text = func_texts
        .get("main")
        .unwrap_or_else(|| panic!("missing lowered function for main"));
    let sum_text = func_texts
        .get("Pair$sum")
        .unwrap_or_else(|| panic!("missing lowered function for Pair$sum"));

    let expected_main = format!(
        indoc! {"
            fn main(Pair) -> u64 {{
              bb0(%v0: Pair):
                %v1: u64 = call @{}(%v0)
                ret %v1
            }}
        "},
        method_def_id
    );
    assert_eq!(main_text, &expected_main);

    let expected_sum = indoc! {"
        fn Pair$sum(Pair) -> u64 {
          locals:
            %l0: Pair
            %l1: Pair
          bb0(%v0: Pair):
            %v1: ptr<Pair> = addr_of %l0
            %v2: ptr<Pair> = addr_of %l1
            store %v2, %v0
            %v3: u64 = const 16:u64
            memcpy %v1, %v2, %v3
            %v4: ptr<u64> = field_addr %v1, 0
            %v5: u64 = load %v4
            %v6: ptr<u64> = field_addr %v1, 1
            %v7: u64 = load %v6
            %v8: u64 = add %v5, %v7
            ret %v8
        }
    "};
    assert_eq!(sum_text, expected_sum);
}
