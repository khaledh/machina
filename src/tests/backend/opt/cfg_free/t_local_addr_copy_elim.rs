use crate::backend::opt::cfg_free::PassManager;
use crate::backend::{IrStructField, IrTypeCache, IrTypeKind};
use crate::ir::builder::FunctionBuilder;
use crate::ir::ir::{Callee, FunctionSig, InstKind};
use crate::resolve::DefId;

#[test]
fn test_local_addr_copy_elim() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let s_ty = types.add_named(
        IrTypeKind::Struct {
            fields: vec![IrStructField {
                name: "x".to_string(),
                ty: u64_ty,
            }],
        },
        "S".to_string(),
    );
    let s_ptr = types.add(IrTypeKind::Ptr { elem: s_ty });
    let unit = types.add(IrTypeKind::Unit);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "copy_elim",
        FunctionSig {
            params: Vec::new(),
            ret: unit,
        },
    );

    let l0 = builder.add_local(s_ty, Some("src".to_string()));
    let l1 = builder.add_local(s_ty, Some("tmp".to_string()));
    let l2 = builder.add_local(s_ty, Some("drop".to_string()));

    let src_ptr = builder.addr_of_local(l0, s_ptr);
    let tmp_ptr = builder.addr_of_local(l1, s_ptr);
    let drop_ptr = builder.addr_of_local(l2, s_ptr);
    let loaded = builder.load(src_ptr, s_ty);
    builder.store(tmp_ptr, loaded);
    builder.call(Callee::Direct(DefId(1)), vec![tmp_ptr], unit);
    builder.store(drop_ptr, loaded);
    builder.drop_ptr(drop_ptr);
    builder.terminate(crate::ir::ir::Terminator::Return { value: None });

    let mut func = builder.finish();
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut func));

    let mut saw_tmp_call = false;
    let mut saw_tmp_drop = false;
    let mut saw_tmp_store = false;
    for block in &func.blocks {
        for inst in &block.insts {
            match &inst.kind {
                InstKind::Store { ptr, .. } if *ptr == tmp_ptr || *ptr == drop_ptr => {
                    saw_tmp_store = true;
                }
                InstKind::Call { args, .. } if args.contains(&tmp_ptr) => {
                    saw_tmp_call = true;
                }
                InstKind::Drop { ptr } if *ptr == drop_ptr => {
                    saw_tmp_drop = true;
                }
                _ => {}
            }
        }
    }

    assert!(!saw_tmp_store);
    assert!(!saw_tmp_call);
    assert!(!saw_tmp_drop);
}
