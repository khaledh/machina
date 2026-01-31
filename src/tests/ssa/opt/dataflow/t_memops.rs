use super::lower_and_optimize;
use indoc::indoc;

#[test]
fn test_memops_to_runtime_calls() {
    let ctx = super::analyze(indoc! {"
        type Option = None | Some(u64)

        fn main() -> Option {
            Option::Some(42)
        }
    "});

    let func_def = ctx.module.func_defs()[0];
    let mut lowered = crate::ssa::lower::lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = crate::ssa::opt::dataflow::PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    let text = crate::ssa::model::format::formact_func(&lowered.func, &lowered.types);

    assert!(text.contains("__rt_memcpy"));
}

#[test]
fn test_memops_to_runtime_calls_array() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u8 {
            var a: u8[8];
            var b = u8[0; 8];
            a = b;
            b[0] = 1;
            a[0]
        }
    "});

    assert!(text.contains("__rt_memcpy"));
}

#[test]
fn test_memops_lowers_memset() {
    let text = lower_and_optimize(indoc! {"
        fn main() -> u8 {
            var a = u8[0; 8];
            a[0]
        }
    "});

    assert!(text.contains("__rt_memset"));
}
