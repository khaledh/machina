use crate::core::backend::opt::cfg_free::PassManager;
use crate::core::ir::builder::FunctionBuilder;
use crate::core::ir::format::format_func;
use crate::core::ir::{FunctionSig, IrTypeCache, IrTypeKind, Terminator};
use crate::core::resolve::DefId;

#[test]
fn test_index_addr_zero_fold() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u8_ptr = types.add(IrTypeKind::Ptr { elem: u8_ty });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "idx_zero",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let local = builder.add_local(u8_ty, None);
    let base = builder.addr_of_local(local, u8_ptr);
    let zero = builder.const_int(0, false, 64, u64_ty);
    let idx = builder.index_addr(base, zero, u8_ptr);
    let _load = builder.load(idx, u8_ty);
    builder.terminate(Terminator::Return { value: None });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = format_func(&func, &types);

    assert!(!text.contains("index_addr"));
}

#[test]
fn test_index_addr_zero_param_fold() {
    let mut types = IrTypeCache::new();
    let bool_ty = types.add(IrTypeKind::Bool);
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u8_ptr = types.add(IrTypeKind::Ptr { elem: u8_ty });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "idx_zero_param",
        FunctionSig {
            params: vec![bool_ty],
            ret: u8_ty,
        },
    );

    let entry = builder.current_block();
    let cond = builder.add_block_param(entry, bool_ty);
    let then_bb = builder.add_block();
    let else_bb = builder.add_block();
    let join_bb = builder.add_block();
    let zero = builder.const_int(0, false, 64, u64_ty);

    builder.set_terminator(
        entry,
        Terminator::CondBr {
            cond,
            then_bb,
            then_args: Vec::new(),
            else_bb,
            else_args: Vec::new(),
        },
    );

    builder.select_block(then_bb);
    builder.terminate(Terminator::Br {
        target: join_bb,
        args: vec![zero],
    });

    builder.select_block(else_bb);
    builder.terminate(Terminator::Br {
        target: join_bb,
        args: vec![zero],
    });

    builder.select_block(join_bb);
    let idx = builder.add_block_param(join_bb, u64_ty);
    let local = builder.add_local(u8_ty, None);
    let base = builder.addr_of_local(local, u8_ptr);
    let addr = builder.index_addr(base, idx, u8_ptr);
    let loaded = builder.load(addr, u8_ty);
    builder.terminate(Terminator::Return {
        value: Some(loaded),
    });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = format_func(&func, &types);

    assert!(!text.contains("index_addr"));
}
