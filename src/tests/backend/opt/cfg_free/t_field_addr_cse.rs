use crate::backend::opt::cfg_free::PassManager;
use crate::ir::FunctionSig;
use crate::ir::Terminator;
use crate::ir::builder::FunctionBuilder;
use crate::ir::{BinOp, InstKind, IrStructField, IrTypeCache, IrTypeKind};
use crate::resolve::DefId;

#[test]
fn test_field_addr_cse() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let pair_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "x".into(),
                ty: u64_ty,
            },
            IrStructField {
                name: "y".into(),
                ty: u64_ty,
            },
        ],
    });
    let pair_ptr = types.add(IrTypeKind::Ptr { elem: pair_ty });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "field_addr_cse",
        FunctionSig {
            params: vec![pair_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, pair_ptr);
    let first = builder.field_addr(param, 0, u64_ptr);
    let second = builder.field_addr(param, 0, u64_ptr);
    let a = builder.load(first, u64_ty);
    let b = builder.load(second, u64_ty);
    let sum = builder.binop(BinOp::Add, a, b, u64_ty);
    builder.set_terminator(entry, Terminator::Return { value: Some(sum) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let field_addr_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, InstKind::FieldAddr { .. }))
        .count();
    assert_eq!(field_addr_count, 1);
}

#[test]
fn test_field_addr_cse_distinct_indices() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let pair_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "x".into(),
                ty: u64_ty,
            },
            IrStructField {
                name: "y".into(),
                ty: u64_ty,
            },
        ],
    });
    let pair_ptr = types.add(IrTypeKind::Ptr { elem: pair_ty });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "field_addr_cse_distinct",
        FunctionSig {
            params: vec![pair_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, pair_ptr);
    let first = builder.field_addr(param, 0, u64_ptr);
    let second = builder.field_addr(param, 1, u64_ptr);
    let a = builder.load(first, u64_ty);
    let b = builder.load(second, u64_ty);
    let sum = builder.binop(BinOp::Add, a, b, u64_ty);
    builder.set_terminator(entry, Terminator::Return { value: Some(sum) });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let field_addr_count = func
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, InstKind::FieldAddr { .. }))
        .count();
    assert_eq!(field_addr_count, 2);
}
