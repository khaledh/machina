use super::lower_and_optimize;
use crate::backend::{IrTypeCache, IrTypeKind};
use crate::ir::builder::FunctionBuilder;
use crate::ir::format::format_func;
use crate::ir::ir::{FunctionSig, Terminator};
use crate::resolve::DefId;
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
    let mut lowered = crate::backend::lower::lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = crate::backend::opt::dataflow::PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    let text = crate::ir::format::format_func(&lowered.func, &lowered.types);

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

#[test]
fn test_memops_elides_zero_len_memcpy() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let ptr_u8_ty = types.add(IrTypeKind::Ptr { elem: u8_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let a = builder.add_local(u8_ty, None);
    let b = builder.add_local(u8_ty, None);
    let ptr_a = builder.addr_of_local(a, ptr_u8_ty);
    let ptr_b = builder.addr_of_local(b, ptr_u8_ty);
    let len = builder.const_int(0, false, 64, u64_ty);
    builder.memcopy(ptr_a, ptr_b, len);
    builder.terminate(Terminator::Return { value: None });

    let mut func = builder.finish();
    let mut manager = crate::backend::opt::dataflow::PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = format_func(&func, &types);

    assert!(!text.contains("__rt_memcpy"));
}

#[test]
fn test_memops_elides_zero_len_memset() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let ptr_u8_ty = types.add(IrTypeKind::Ptr { elem: u8_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "main",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let a = builder.add_local(u8_ty, None);
    let ptr_a = builder.addr_of_local(a, ptr_u8_ty);
    let len = builder.const_int(0, false, 64, u64_ty);
    let byte = builder.const_int(0, false, 8, u8_ty);
    builder.memset(ptr_a, byte, len);
    builder.terminate(Terminator::Return { value: None });

    let mut func = builder.finish();
    let mut manager = crate::backend::opt::dataflow::PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = format_func(&func, &types);

    assert!(!text.contains("__rt_memset"));
}
