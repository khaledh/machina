use crate::resolve::DefId;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::format::format_func;
use crate::ssa::model::ir::FunctionSig;
use crate::ssa::opt::cfg_free::PassManager;
use crate::ssa::{IrStructField, IrTypeCache, IrTypeKind};

#[test]
fn test_store_field_addr_elim() {
    let mut types = IrTypeCache::new();
    let _unit_ty = types.add(IrTypeKind::Unit);
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
        "store_field",
        FunctionSig {
            params: vec![pair_ptr],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, pair_ptr);
    let loaded = builder.load(param, pair_ty);
    let local = builder.add_local(pair_ty, None);
    let local_addr = builder.addr_of_local(local, pair_ptr);
    builder.store(local_addr, loaded);
    let field_ptr = builder.field_addr(local_addr, 0, u64_ptr);
    let field = builder.load(field_ptr, u64_ty);
    builder.set_terminator(
        entry,
        crate::ssa::model::ir::Terminator::Return { value: Some(field) },
    );

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));
    let text = format_func(&func, &types);

    assert!(!text.contains("store "));
    assert!(text.contains(&format!("field_addr %v{}, 0", param.0)));
}
