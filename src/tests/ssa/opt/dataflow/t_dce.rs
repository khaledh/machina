use super::assert_ir_eq;
use super::lower_and_optimize;
use indoc::indoc;

#[test]
fn test_dce_removes_unused_binop() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u64 {
            let x = 1 + 2;
            4
        }
    "});

    let expected = indoc! {"
        fn main() -> u64 {
          bb0():
            %v3: u64 = const 4:u64
            ret %v3
        }
    "};
    assert_ir_eq(text, expected);
}

#[test]
fn test_dce_keeps_call() {
    let ctx = super::analyze(indoc! {"
        fn main() -> u64 {
            side();
            4
        }

        fn side() -> u64 {
            1
        }
    "});

    let main_def = ctx.module.func_defs()[0];
    let side_def = ctx.module.func_defs()[1];
    let side_id = side_def.def_id;

    let mut lowered = crate::ssa::lower::lower_func(
        main_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = crate::ssa::opt::dataflow::PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    let text = crate::ssa::model::format::formact_func(&lowered.func, &lowered.types);

    let expected = format!(
        indoc! {"
            fn main() -> u64 {{
              bb0():
                %v0: u64 = call @{}()
                %v1: u64 = const 4:u64
                ret %v1
            }}
        "},
        side_id
    );
    assert_ir_eq(text, expected);
}
