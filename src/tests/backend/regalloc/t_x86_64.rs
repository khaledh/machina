use crate::backend::analysis::liveness;
use crate::backend::regalloc::target::PhysReg;
use crate::backend::regalloc::x86_64::{
    INDIRECT_CALL_REG, INDIRECT_RESULT_REG, X86_64Reg, X86_64Target, phys,
};
use crate::backend::regalloc::{Location, TargetSpec, regalloc};
use crate::core::resolve::DefId;
use crate::ir::builder::FunctionBuilder;
use crate::ir::{BinOp, Callee, FunctionSig, IrTypeCache, IrTypeKind, Terminator};

#[test]
fn test_x86_64_target_uses_sysv_param_and_result_registers() {
    let target = X86_64Target::new();

    assert_eq!(target.result_reg(), phys(X86_64Reg::Rax));
    assert_eq!(target.param_reg(0), Some(phys(X86_64Reg::Rdi)));
    assert_eq!(target.param_reg(1), Some(phys(X86_64Reg::Rsi)));
    assert_eq!(target.param_reg(2), Some(phys(X86_64Reg::Rdx)));
    assert_eq!(target.param_reg(3), Some(phys(X86_64Reg::Rcx)));
    assert_eq!(target.param_reg(4), Some(phys(X86_64Reg::R8)));
    assert_eq!(target.param_reg(5), Some(phys(X86_64Reg::R9)));
    assert_eq!(target.param_reg(6), None);
}

#[test]
fn test_x86_64_target_sets_special_abi_registers() {
    let target = X86_64Target::new();

    assert_eq!(target.indirect_call_reg(), INDIRECT_CALL_REG);
    assert_eq!(target.indirect_result_reg(), Some(INDIRECT_RESULT_REG));
    assert_eq!(
        target.scratch_regs(),
        &[phys(X86_64Reg::R10), phys(X86_64Reg::R11)]
    );
}

#[test]
fn test_x86_64_target_allocatable_pool_excludes_rsp_and_rbp() {
    let target = X86_64Target::new();
    let regs = target.allocatable_regs();

    assert!(!regs.contains(&phys(X86_64Reg::Rsp)));
    assert!(!regs.contains(&phys(X86_64Reg::Rbp)));
    assert!(regs.contains(&phys(X86_64Reg::Rax)));
    assert!(regs.contains(&phys(X86_64Reg::R15)));
}

#[test]
fn test_x86_64_regalloc_prefers_non_caller_saved_value_across_call() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "x86_call_safe",
        FunctionSig {
            params: vec![u64_ty],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, u64_ty);
    let call = builder.call(Callee::Direct(DefId(1)), vec![param], u64_ty);
    let sum = builder.binop(BinOp::Add, param, call, u64_ty);
    builder.terminate(Terminator::Return { value: Some(sum) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = X86_64Target::new();
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let location = alloc.alloc_map.get(&param).expect("missing alloc");
    assert!(matches!(
        location,
        Location::Reg(reg)
            if !target.caller_saved().contains(reg)
                || *reg == PhysReg(X86_64Reg::Rdi as u8)
    ));
}
