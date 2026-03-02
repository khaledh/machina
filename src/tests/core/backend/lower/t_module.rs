use super::{analyze, analyze_typestate, assert_ir_eq, format_func, indoc, lower_module};
use crate::core::backend::lower::{LowerOpts, lower_module_with_opts};
use crate::core::ir::IrTypeKind;
use crate::core::tree::semantic as sem;
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

    let a_text = format_func(&lowered.funcs[0].func, &lowered.funcs[0].types);
    let b_text = format_func(&lowered.funcs[1].func, &lowered.funcs[1].types);

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
            sem::MethodItem::Def(def) => Some(def.def_id),
            sem::MethodItem::Decl(_) => None,
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
        let text = format_func(&lowered_func.func, &lowered_func.types);
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
            fn main(ptr<Pair>) -> u64 {{
              locals:
                %l0: Pair
                %l1: Pair
                %l2: Pair
              bb0(%v0: ptr<Pair>):
                %v1: ptr<Pair> = addr_of %l0
                %v2: u64 = const 16:u64
                memcpy %v1, %v0, %v2
                %v3: Pair = load %v1
                %v4: ptr<Pair> = addr_of %l1
                %v5: ptr<Pair> = addr_of %l2
                store %v5, %v3
                %v6: u64 = const 16:u64
                memcpy %v4, %v5, %v6
                %v7: u64 = call @{}(%v4)
                ret %v7
            }}
        "},
        method_def_id
    );
    assert_ir_eq(main_text, &expected_main);

    let expected_sum = indoc! {"
        fn Pair$sum(ptr<Pair>) -> u64 {
          locals:
            %l0: Pair
          bb0(%v0: ptr<Pair>):
            %v1: ptr<Pair> = addr_of %l0
            %v2: u64 = const 16:u64
            memcpy %v1, %v0, %v2
            %v3: ptr<u64> = field_addr %v1, 0
            %v4: u64 = load %v3
            %v5: ptr<u64> = field_addr %v1, 1
            %v6: u64 = load %v5
            %v7: u64 = add %v4, %v6
            ret %v7
        }
    "};
    assert_ir_eq(sum_text, expected_sum);
}

#[test]
fn test_lower_module_typestate_machine_plans_materialize_artifacts() {
    let ctx = analyze_typestate(indoc! {"
        type Ping = {}

        typestate M {
            fn new() -> Ready { Ready {} }

            state Ready {
                on Ping(e: Ping) -> Ready {
                    e;
                    Ready {}
                }
            }
        }
    "});

    let lowered = lower_module_with_opts(
        &ctx.module,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
        &LowerOpts {
            machine_plans: Some(&ctx.machine_plans),
            ..Default::default()
        },
    )
    .expect("failed to lower typestate machine artifacts");

    // Machine plans should already be concretized enough for backend materialization.
    for plan in ctx.machine_plans.thunks.values() {
        let state_ty = ctx.type_map.type_table().get(plan.state_layout_ty);
        let payload_ty = ctx.type_map.type_table().get(plan.payload_layout_ty);
        let next_ty = ctx.type_map.type_table().get(plan.next_state_layout_ty);
        assert!(
            !state_ty.contains_unresolved()
                && !payload_ty.contains_unresolved()
                && !next_ty.contains_unresolved(),
            "expected concrete machine plan layout types, got state={state_ty:?} payload={payload_ty:?} next={next_ty:?}"
        );
    }

    let thunk = lowered
        .funcs
        .iter()
        .find(|func| func.func.name.starts_with("__mc_machine_dispatch_thunk_"))
        .expect("expected synthetic machine dispatch thunk in lowered module");
    assert_eq!(
        thunk.func.sig.params.len(),
        6,
        "expected dispatch thunk ABI arity of 6 params"
    );
    assert!(
        matches!(
            thunk.types.kind(thunk.func.sig.ret),
            IrTypeKind::Int {
                signed: false,
                bits: 8
            }
        ),
        "expected dispatch thunk return type to be u8 dispatch result"
    );
    let thunk_text = format_func(&thunk.func, &thunk.types);
    assert!(
        thunk_text
            .contains("fn(ptr<__ts_M_Ready>, ptr<Ping>, ptr<Ping>) -> __ts_M_Ready = const @"),
        "expected dispatch thunk to reference concrete handler signature, got: {thunk_text}"
    );
    assert!(
        thunk_text.lines().any(|line| {
            let trimmed = line.trim();
            trimmed.contains("call %v") && trimmed.contains("(")
        }),
        "expected dispatch thunk to contain an indirect call, got: {thunk_text}"
    );
    assert!(
        thunk_text.contains("__rt_alloc"),
        "expected dispatch thunk to allocate next-state token, got: {thunk_text}"
    );

    let has_descriptor_magic = lowered.globals.iter().any(|g| g.bytes.starts_with(b"MCHD"));
    assert!(
        has_descriptor_magic,
        "expected serialized machine descriptor blob in lowered globals"
    );
}
