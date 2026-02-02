use crate::backend::opt::cfg_free::PassManager;
use crate::ir::builder::FunctionBuilder;
use crate::ir::{BinOp, FunctionSig, InstKind, IrTypeCache, IrTypeKind, Terminator};
use crate::resolve::DefId;

#[test]
fn test_load_cse() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "load_cse",
        FunctionSig {
            params: vec![u64_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let addr = builder.add_block_param(entry, u64_ptr);
    let first = builder.load(addr, u64_ty);
    let second = builder.load(addr, u64_ty);
    let sum = builder.binop(BinOp::Add, first, second, u64_ty);
    builder.terminate(Terminator::Return { value: Some(sum) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let load_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, InstKind::Load { .. }))
        .count();
    assert_eq!(load_count, 1);
}

#[test]
fn test_load_cse_blocked_by_store() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "load_cse_blocked",
        FunctionSig {
            params: vec![u64_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let addr = builder.add_block_param(entry, u64_ptr);
    let first = builder.load(addr, u64_ty);
    let value = builder.const_int(7, false, 64, u64_ty);
    builder.store(addr, value);
    let second = builder.load(addr, u64_ty);
    let sum = builder.binop(BinOp::Add, first, second, u64_ty);
    builder.terminate(Terminator::Return { value: Some(sum) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let load_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, InstKind::Load { .. }))
        .count();
    assert_eq!(load_count, 2);
}
