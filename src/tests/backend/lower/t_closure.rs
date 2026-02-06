use crate::tree::semantic as sem;

use super::{analyze, assert_ir_eq, format_func, indoc, lower_module};

#[test]
fn test_lower_closure_ref_captureless() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            let add = |x: u64| -> u64 {
                x + 1
            };
            add(3)
        }
    "});

    let closure_def = ctx
        .module
        .func_defs()
        .into_iter()
        .find(|def| def.sig.name.contains("$closure$"))
        .expect("missing closure function definition");
    let closure_name = closure_def.sig.name.clone();
    let closure_def_id = closure_def.def_id;

    assert!(
        ctx.module
            .method_blocks()
            .iter()
            .all(|block| !block.type_name.contains("$closure$"))
    );

    let lowered = lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .unwrap();

    let mut func_texts = std::collections::HashMap::new();
    for lowered_func in &lowered.funcs {
        let text = format_func(&lowered_func.func, &lowered_func.types);
        func_texts.insert(lowered_func.func.name.clone(), text);
    }

    let main_text = func_texts
        .get("main")
        .unwrap_or_else(|| panic!("missing lowered function for main"));
    let closure_text = func_texts
        .get(&closure_name)
        .unwrap_or_else(|| panic!("missing lowered function for {closure_name}"));

    let expected_main = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: fn(u64) -> u64 = const @{}
                %v1: u64 = const 3:u64
                %v2: u64 = call %v0(%v1)
                ret %v2
            }}
        "},
        closure_def_id
    );
    assert_ir_eq(main_text, &expected_main);

    let expected_closure = format!(
        indoc! {"
            fn {}(u64) -> u64 {{
              bb0(%v0: u64):
                %v1: u64 = const 1:u64
                %v2: u64 = add %v0, %v1
                ret %v2
            }}
        "},
        closure_name
    );
    assert_ir_eq(closure_text, &expected_closure);
}

#[test]
fn test_lower_closure_invoke() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            let base = 10;
            let add = [move base] |x: u64| -> u64 {
                base + x
            };
            add(5)
        }
    "});

    let method_def = ctx
        .module
        .method_blocks()
        .iter()
        .find(|block| block.type_name == "main$closure$1")
        .and_then(|block| {
            block.method_items.iter().find_map(|item| match item {
                sem::MethodItem::Def(def) if def.sig.name == "invoke" => Some(def),
                _ => None,
            })
        })
        .expect("missing closure invoke method");
    let invoke_def_id = method_def.def_id;

    let lowered = lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .unwrap();

    let mut func_texts = std::collections::HashMap::new();
    for lowered_func in &lowered.funcs {
        let text = format_func(&lowered_func.func, &lowered_func.types);
        func_texts.insert(lowered_func.func.name.clone(), text);
    }

    let main_text = func_texts
        .get("main")
        .unwrap_or_else(|| panic!("missing lowered function for main"));
    let invoke_text = func_texts
        .get("main$closure$1$invoke")
        .unwrap_or_else(|| panic!("missing lowered function for main$closure$1$invoke"));

    let expected_main = format!(
        indoc! {"
            fn main() -> u64 {{
              locals:
                %l0: main$closure$1
                %l1: main$closure$1
                %l2: main$closure$1
                %l3: main$closure$1
              bb0():
                %v0: u64 = const 10:u64
                %v1: ptr<main$closure$1> = addr_of %l0
                %v2: ptr<u64> = field_addr %v1, 0
                store %v2, %v0
                %v3: main$closure$1 = load %v1
                %v4: ptr<main$closure$1> = addr_of %l1
                store %v4, %v3
                %v5: main$closure$1 = load %v4
                %v6: u64 = const 5:u64
                %v7: ptr<main$closure$1> = addr_of %l2
                %v8: ptr<main$closure$1> = addr_of %l3
                store %v8, %v5
                %v9: u64 = const 8:u64
                memcpy %v7, %v8, %v9
                %v10: u64 = call @{}(%v7, %v6)
                ret %v10
            }}
        "},
        invoke_def_id
    );
    assert_ir_eq(main_text, &expected_main);

    let expected_invoke = indoc! {"
        fn main$closure$1$invoke(ptr<main$closure$1>, u64) -> u64 {
          locals:
            %l0: main$closure$1
          bb0(%v0: ptr<main$closure$1>, %v1: u64):
            %v2: ptr<main$closure$1> = addr_of %l0
            %v3: u64 = const 8:u64
            memcpy %v2, %v0, %v3
            %v4: ptr<u64> = field_addr %v2, 0
            %v5: u64 = load %v4
            %v6: u64 = add %v5, %v1
            ret %v6
        }
    "};
    assert_ir_eq(invoke_text, expected_invoke);
}

#[test]
fn test_lower_closure_borrow_capture() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            let n = 1;
            var m = 2;
            let add = |x: u64| -> u64 {
                n + x
            };
            let bump = |x: u64| -> u64 {
                m = m + x;
                m
            };
            add(5) + bump(3)
        }
    "});

    let invoke_def_id = |type_name: &str| {
        ctx.module
            .method_blocks()
            .iter()
            .find(|block| block.type_name == type_name)
            .and_then(|block| {
                block.method_items.iter().find_map(|item| match item {
                    sem::MethodItem::Def(def) if def.sig.name == "invoke" => Some(def.def_id),
                    _ => None,
                })
            })
            .unwrap_or_else(|| panic!("missing closure invoke method for {type_name}"))
    };
    let add_invoke_def_id = invoke_def_id("main$closure$1");
    let bump_invoke_def_id = invoke_def_id("main$closure$2");

    let lowered = lower_module(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .unwrap();

    let mut func_texts = std::collections::HashMap::new();
    for lowered_func in &lowered.funcs {
        let text = format_func(&lowered_func.func, &lowered_func.types);
        func_texts.insert(lowered_func.func.name.clone(), text);
    }

    let main_text = func_texts
        .get("main")
        .unwrap_or_else(|| panic!("missing lowered function for main"));
    let add_invoke_text = func_texts
        .get("main$closure$1$invoke")
        .unwrap_or_else(|| panic!("missing lowered function for main$closure$1$invoke"));
    let bump_invoke_text = func_texts
        .get("main$closure$2$invoke")
        .unwrap_or_else(|| panic!("missing lowered function for main$closure$2$invoke"));

    let expected_main = format!(
        indoc! {"
            fn main() -> u64 {{
              locals:
                %l0: main$closure$1
                %l1: u64
                %l2: main$closure$1
                %l3: main$closure$2
                %l4: u64
                %l5: main$closure$2
                %l6: main$closure$1
                %l7: main$closure$1
                %l8: main$closure$2
                %l9: main$closure$2
              bb0():
                %v0: u64 = const 1:u64
                %v1: u64 = const 2:u64
                %v2: ptr<main$closure$1> = addr_of %l0
                %v3: ptr<u64> = addr_of %l1
                store %v3, %v0
                %v4: ptr<ptr<u64>> = field_addr %v2, 0
                store %v4, %v3
                %v5: main$closure$1 = load %v2
                %v6: ptr<main$closure$1> = addr_of %l2
                store %v6, %v5
                %v7: ptr<main$closure$2> = addr_of %l3
                %v8: ptr<u64> = addr_of %l4
                store %v8, %v1
                %v9: ptr<ptr<u64>> = field_addr %v7, 0
                store %v9, %v8
                %v10: main$closure$2 = load %v7
                %v11: ptr<main$closure$2> = addr_of %l5
                store %v11, %v10
                %v12: main$closure$1 = load %v6
                %v13: u64 = const 5:u64
                %v14: ptr<main$closure$1> = addr_of %l6
                %v15: ptr<main$closure$1> = addr_of %l7
                store %v15, %v12
                %v16: u64 = const 8:u64
                memcpy %v14, %v15, %v16
                %v17: u64 = call @{}(%v14, %v13)
                %v18: main$closure$2 = load %v11
                %v19: u64 = const 3:u64
                %v20: ptr<main$closure$2> = addr_of %l8
                %v21: ptr<main$closure$2> = addr_of %l9
                store %v21, %v18
                %v22: u64 = const 8:u64
                memcpy %v20, %v21, %v22
                %v23: u64 = call @{}(%v20, %v19)
                %v24: u64 = add %v17, %v23
                ret %v24
            }}
        "},
        add_invoke_def_id, bump_invoke_def_id
    );
    assert_ir_eq(main_text, &expected_main);

    let expected_add_invoke = indoc! {"
        fn main$closure$1$invoke(ptr<main$closure$1>, u64) -> u64 {
          locals:
            %l0: main$closure$1
          bb0(%v0: ptr<main$closure$1>, %v1: u64):
            %v2: ptr<main$closure$1> = addr_of %l0
            %v3: u64 = const 8:u64
            memcpy %v2, %v0, %v3
            %v4: ptr<ptr<u64>> = field_addr %v2, 0
            %v5: ptr<u64> = load %v4
            %v6: u64 = load %v5
            %v7: u64 = add %v6, %v1
            ret %v7
        }
    "};
    assert_ir_eq(add_invoke_text, expected_add_invoke);

    let expected_bump_invoke = indoc! {"
        fn main$closure$2$invoke(ptr<main$closure$2>, u64) -> u64 {
          locals:
            %l0: main$closure$2
          bb0(%v0: ptr<main$closure$2>, %v1: u64):
            %v2: ptr<main$closure$2> = addr_of %l0
            %v3: u64 = const 8:u64
            memcpy %v2, %v0, %v3
            %v4: ptr<ptr<u64>> = field_addr %v2, 0
            %v5: ptr<u64> = load %v4
            %v6: u64 = load %v5
            %v7: u64 = add %v6, %v1
            %v8: ptr<ptr<u64>> = field_addr %v2, 0
            %v9: ptr<u64> = load %v8
            store %v9, %v7
            %v10: ptr<ptr<u64>> = field_addr %v2, 0
            %v11: ptr<u64> = load %v10
            %v12: u64 = load %v11
            ret %v12
        }
    "};
    assert_ir_eq(bump_invoke_text, expected_bump_invoke);
}
