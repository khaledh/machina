use crate::backend::opt::cfg_free::PassManager;
use crate::backend::{IrTypeCache, IrTypeKind};
use crate::ir::builder::FunctionBuilder;
use crate::ir::format::format_func;
use crate::ir::ir::FunctionSig;
use crate::resolve::DefId;

#[test]
fn test_local_load_forward() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "local_load_forward",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let local = builder.add_local(u64_ty, None);
    let addr = builder.addr_of_local(local, u64_ptr);
    let value = builder.const_int(42, false, 64, u64_ty);
    builder.store(addr, value);
    let loaded = builder.load(addr, u64_ty);
    builder.terminate(crate::ir::ir::Terminator::Return {
        value: Some(loaded),
    });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = format_func(&func, &types);

    assert!(!text.contains("load "));
    assert!(text.contains("const 42"));
}

#[test]
fn test_local_load_forward_blocked_by_call() {
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
        "local_load_forward_blocked",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let local = builder.add_local(u8_ty, None);
    let addr = builder.addr_of_local(local, u8_ptr);
    let value = builder.const_int(42, false, 8, u8_ty);
    builder.store(addr, value);
    let zero_len = builder.const_int(0, false, 64, u64_ty);
    let zero_byte = builder.const_int(0, false, 8, u8_ty);
    let _call = builder.call(
        crate::ir::ir::Callee::Runtime(crate::ir::ir::RuntimeFn::MemSet),
        vec![addr, zero_len, zero_byte],
        unit_ty,
    );
    let _load = builder.load(addr, u8_ty);
    builder.terminate(crate::ir::ir::Terminator::Return { value: None });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let load_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, crate::ir::InstKind::Load { .. }))
        .count();
    assert_eq!(load_count, 1);
}
