use super::{analyze, formact_func, indoc, lower_module};

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
                crate::tree::semantic::MethodItem::Def(def) if def.sig.name == "invoke" => {
                    Some(def)
                }
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
    )
    .unwrap();

    let mut func_texts = std::collections::HashMap::new();
    for lowered_func in &lowered.funcs {
        let text = formact_func(&lowered_func.func, &lowered_func.types);
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
              bb0():
                %v0: u64 = const 10:u64
                %v1: ptr<main$closure$1> = addr_of %l0
                %v2: ptr<u64> = field_addr %v1, 0
                store %v2, %v0
                %v3: main$closure$1 = load %v1
                %v4: u64 = const 5:u64
                %v5: u64 = call @{}(%v3, %v4)
                ret %v5
            }}
        "},
        invoke_def_id
    );
    assert_eq!(main_text, &expected_main);

    let expected_invoke = indoc! {"
        fn main$closure$1$invoke(main$closure$1, u64) -> u64 {
          locals:
            %l0: main$closure$1
          bb0(%v0: main$closure$1, %v1: u64):
            %v2: ptr<main$closure$1> = addr_of %l0
            store %v2, %v0
            %v3: ptr<u64> = field_addr %v2, 0
            %v4: u64 = load %v3
            %v5: u64 = add %v4, %v1
            ret %v5
        }
    "};
    assert_eq!(invoke_text, expected_invoke);
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
                    crate::tree::semantic::MethodItem::Def(def) if def.sig.name == "invoke" => {
                        Some(def.def_id)
                    }
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
    )
    .unwrap();

    let mut func_texts = std::collections::HashMap::new();
    for lowered_func in &lowered.funcs {
        let text = formact_func(&lowered_func.func, &lowered_func.types);
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
                %l2: main$closure$2
                %l3: u64
              bb0():
                %v0: u64 = const 1:u64
                %v1: u64 = const 2:u64
                %v2: ptr<main$closure$1> = addr_of %l0
                %v3: ptr<u64> = addr_of %l1
                store %v3, %v0
                %v4: ptr<ptr<u64>> = field_addr %v2, 0
                store %v4, %v3
                %v5: main$closure$1 = load %v2
                %v6: ptr<main$closure$2> = addr_of %l2
                %v7: ptr<u64> = addr_of %l3
                store %v7, %v1
                %v8: ptr<ptr<u64>> = field_addr %v6, 0
                store %v8, %v7
                %v9: main$closure$2 = load %v6
                %v10: u64 = const 5:u64
                %v11: u64 = call @{}(%v5, %v10)
                %v12: u64 = const 3:u64
                %v13: u64 = call @{}(%v9, %v12)
                %v14: u64 = add %v11, %v13
                ret %v14
            }}
        "},
        add_invoke_def_id, bump_invoke_def_id
    );
    assert_eq!(main_text, &expected_main);

    let expected_add_invoke = indoc! {"
        fn main$closure$1$invoke(main$closure$1, u64) -> u64 {
          locals:
            %l0: main$closure$1
          bb0(%v0: main$closure$1, %v1: u64):
            %v2: ptr<main$closure$1> = addr_of %l0
            store %v2, %v0
            %v3: ptr<ptr<u64>> = field_addr %v2, 0
            %v4: ptr<u64> = load %v3
            %v5: u64 = load %v4
            %v6: u64 = add %v5, %v1
            ret %v6
        }
    "};
    assert_eq!(add_invoke_text, expected_add_invoke);

    let expected_bump_invoke = indoc! {"
        fn main$closure$2$invoke(main$closure$2, u64) -> u64 {
          locals:
            %l0: main$closure$2
          bb0(%v0: main$closure$2, %v1: u64):
            %v2: ptr<main$closure$2> = addr_of %l0
            store %v2, %v0
            %v3: ptr<ptr<u64>> = field_addr %v2, 0
            %v4: ptr<u64> = load %v3
            %v5: u64 = load %v4
            %v6: u64 = add %v5, %v1
            %v7: ptr<ptr<u64>> = field_addr %v2, 0
            %v8: ptr<u64> = load %v7
            store %v8, %v6
            %v9: ptr<ptr<u64>> = field_addr %v2, 0
            %v10: ptr<u64> = load %v9
            %v11: u64 = load %v10
            ret %v11
        }
    "};
    assert_eq!(bump_invoke_text, expected_bump_invoke);
}
