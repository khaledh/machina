use std::collections::HashSet;

use crate::resolve::DefId;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{Callee, FunctionSig, Terminator};
use crate::ssa::model::types::{IrTypeCache, IrTypeKind};
use crate::ssa::opt::module_dce::reachable_def_ids;

fn unit_sig(types: &mut IrTypeCache) -> (FunctionSig, crate::ssa::IrTypeId) {
    let unit = types.add(IrTypeKind::Unit);
    (
        FunctionSig {
            params: Vec::new(),
            ret: unit,
        },
        unit,
    )
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
