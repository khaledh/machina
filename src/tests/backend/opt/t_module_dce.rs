use std::collections::HashSet;

use crate::core::backend::lower::{LoweredFunction, LoweredModule};
use crate::core::backend::opt::module_dce::{prune_globals, reachable_def_ids};
use crate::core::ir::builder::FunctionBuilder;
use crate::core::ir::{
    Callee, ConstValue, FunctionSig, GlobalData, GlobalId, InstKind, IrTypeCache, IrTypeId,
    IrTypeKind, RuntimeFn, Terminator,
};
use crate::core::resolve::DefId;

fn unit_sig(types: &mut IrTypeCache) -> (FunctionSig, IrTypeId) {
    let unit = types.add(IrTypeKind::Unit);
    (
        FunctionSig {
            params: Vec::new(),
            ret: unit,
        },
        unit,
    )
}

fn u8_ptr_ty(types: &mut IrTypeCache) -> IrTypeId {
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    types.add(IrTypeKind::Ptr { elem: u8_ty })
}

fn finalize_void(builder: &mut FunctionBuilder) {
    builder.terminate(Terminator::Return { value: None });
}

#[test]
fn test_module_dce_keeps_reachable() {
    let mut types = IrTypeCache::new();
    let (sig, unit) = unit_sig(&mut types);

    let mut main = FunctionBuilder::new(DefId(0), "main", sig.clone());
    let foo_id = DefId(1);
    main.call(Callee::Direct(foo_id), Vec::new(), unit);
    finalize_void(&mut main);

    let mut foo = FunctionBuilder::new(foo_id, "foo", sig.clone());
    finalize_void(&mut foo);

    let mut bar = FunctionBuilder::new(DefId(2), "bar", sig);
    finalize_void(&mut bar);

    let funcs = vec![main.finish(), foo.finish(), bar.finish()];
    let reachable = reachable_def_ids(&funcs);
    let expected: HashSet<_> = [DefId(0), DefId(1)].into_iter().collect();
    assert_eq!(reachable, expected);
}

#[test]
fn test_module_dce_keeps_address_taken() {
    let mut types = IrTypeCache::new();
    let (sig, unit) = unit_sig(&mut types);
    let fn_ty = types.add(IrTypeKind::Fn {
        params: Vec::new(),
        ret: unit,
    });

    let addr_taken_id = DefId(1);
    let mut main = FunctionBuilder::new(DefId(0), "main", sig.clone());
    main.const_func_addr(addr_taken_id, fn_ty);
    finalize_void(&mut main);

    let mut addr_taken = FunctionBuilder::new(addr_taken_id, "addr_taken", sig.clone());
    finalize_void(&mut addr_taken);

    let mut dead = FunctionBuilder::new(DefId(2), "dead", sig);
    finalize_void(&mut dead);

    let funcs = vec![main.finish(), addr_taken.finish(), dead.finish()];
    let reachable = reachable_def_ids(&funcs);
    let expected: HashSet<_> = [DefId(0), DefId(1)].into_iter().collect();
    assert_eq!(reachable, expected);
}

#[test]
fn test_module_dce_keeps_all_without_main() {
    let mut types = IrTypeCache::new();
    let (sig, _) = unit_sig(&mut types);

    let mut a = FunctionBuilder::new(DefId(0), "a", sig.clone());
    finalize_void(&mut a);

    let mut b = FunctionBuilder::new(DefId(1), "b", sig);
    finalize_void(&mut b);

    let funcs = vec![a.finish(), b.finish()];
    let reachable = reachable_def_ids(&funcs);
    let expected: HashSet<_> = [DefId(0), DefId(1)].into_iter().collect();
    assert_eq!(reachable, expected);
}

#[test]
fn test_module_dce_prunes_unused_globals() {
    let mut types = IrTypeCache::new();
    let (sig, unit) = unit_sig(&mut types);
    let ptr_ty = u8_ptr_ty(&mut types);

    let mut main = FunctionBuilder::new(DefId(0), "main", sig);
    main.const_global_addr(GlobalId(1), ptr_ty);
    main.call(Callee::Runtime(RuntimeFn::Trap), vec![], unit);
    finalize_void(&mut main);

    let func = main.finish();
    let lowered = LoweredFunction {
        func,
        types: types.clone(),
        globals: vec![
            GlobalData {
                id: GlobalId(0),
                bytes: vec![0],
                align: 1,
            },
            GlobalData {
                id: GlobalId(1),
                bytes: vec![1],
                align: 1,
            },
        ],
    };

    let mut module = LoweredModule {
        funcs: vec![lowered],
        globals: vec![
            GlobalData {
                id: GlobalId(0),
                bytes: vec![0],
                align: 1,
            },
            GlobalData {
                id: GlobalId(1),
                bytes: vec![1],
                align: 1,
            },
        ],
    };

    let changed = prune_globals(&mut module);
    assert!(changed);
    assert_eq!(module.globals.len(), 1);
    assert_eq!(module.globals[0].bytes, vec![1]);
    assert_eq!(module.globals[0].id, GlobalId(0));

    let inst = &module.funcs[0].func.blocks[0].insts[0];
    let InstKind::Const {
        value: ConstValue::GlobalAddr { id },
    } = &inst.kind
    else {
        panic!("expected global addr const");
    };
    assert_eq!(*id, GlobalId(0));
}
