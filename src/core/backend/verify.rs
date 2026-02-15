use std::collections::HashMap;
use std::fmt;

use crate::core::backend::lower::{LoweredFunction, LoweredModule};
use crate::core::ir::{
    Block, BlockId, Callee, Function, FunctionSig, InstKind, IrTypeCache, IrTypeId, IrTypeKind,
    RuntimeFn, Terminator, ValueDef, ValueId, for_each_inst_use,
};
use crate::core::resolve::DefId;

#[derive(Debug, Clone)]
pub struct VerifyIrError {
    message: String,
}

impl VerifyIrError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for VerifyIrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for VerifyIrError {}

pub fn verify_module(module: &LoweredModule) -> Result<(), VerifyIrError> {
    let direct_sigs: HashMap<DefId, FunctionSig> = module
        .funcs
        .iter()
        .map(|func| (func.func.def_id, func.func.sig.clone()))
        .collect();
    for func in &module.funcs {
        verify_function(func, &direct_sigs)?;
    }
    Ok(())
}

#[cfg(test)]
#[path = "../../tests/backend/verify/t_verify.rs"]
mod tests;
fn verify_function(
    lowered: &LoweredFunction,
    direct_sigs: &HashMap<DefId, FunctionSig>,
) -> Result<(), VerifyIrError> {
    let func = &lowered.func;
    let types = &lowered.types;

    let mut blocks = HashMap::new();
    for block in &func.blocks {
        if blocks.insert(block.id, block).is_some() {
            return Err(err(
                func.name.as_str(),
                None,
                format!("duplicate block {:?}", block.id),
            ));
        }
    }

    let mut value_types: HashMap<ValueId, IrTypeId> = HashMap::new();
    for block in &func.blocks {
        for param in &block.params {
            if value_types.insert(param.value.id, param.value.ty).is_some() {
                return Err(err(
                    func.name.as_str(),
                    Some(block.id),
                    format!("duplicate value {:?}", param.value.id),
                ));
            }
        }
    }

    for block in &func.blocks {
        for inst in &block.insts {
            if let Some(result) = &inst.result
                && value_types.insert(result.id, result.ty).is_some()
            {
                return Err(err(
                    func.name.as_str(),
                    Some(block.id),
                    format!("duplicate value {:?}", result.id),
                ));
            }

            let mut use_error = None;
            for_each_inst_use(&inst.kind, |value| {
                if use_error.is_none() && !value_types.contains_key(&value) {
                    use_error = Some(err(
                        func.name.as_str(),
                        Some(block.id),
                        format!("use of undefined value {:?}", value),
                    ));
                }
            });
            if let Some(error) = use_error {
                return Err(error);
            }

            verify_inst_types(
                func.name.as_str(),
                block.id,
                &inst.kind,
                inst.result.as_ref(),
                &value_types,
                types,
                func,
                direct_sigs,
            )?;
        }

        verify_terminator(func.name.as_str(), block, &blocks, &value_types, types)?;
    }

    Ok(())
}

fn verify_inst_types(
    func_name: &str,
    block_id: BlockId,
    kind: &InstKind,
    result: Option<&ValueDef>,
    value_types: &HashMap<ValueId, IrTypeId>,
    types: &IrTypeCache,
    func: &Function,
    direct_sigs: &HashMap<DefId, FunctionSig>,
) -> Result<(), VerifyIrError> {
    match kind {
        InstKind::AddrOfLocal { local } => {
            let result = result
                .ok_or_else(|| err(func_name, Some(block_id), "addr_of should define a result"))?;
            let local = func.locals.get(local.index()).ok_or_else(|| {
                err(
                    func_name,
                    Some(block_id),
                    format!("addr_of references missing local {:?}", local),
                )
            })?;
            let IrTypeKind::Ptr { elem } = types.kind(result.ty) else {
                return Err(err(
                    func_name,
                    Some(block_id),
                    "addr_of result is not a pointer",
                ));
            };
            if *elem != local.ty {
                return Err(err(
                    func_name,
                    Some(block_id),
                    "addr_of result type does not match local",
                ));
            }
        }
        InstKind::FieldAddr { base, .. }
        | InstKind::IndexAddr { base, .. }
        | InstKind::Load { ptr: base }
        | InstKind::Store { ptr: base, .. }
        | InstKind::MemCopy { dst: base, .. }
        | InstKind::MemSet { dst: base, .. }
        | InstKind::Drop { ptr: base } => {
            require_ptr(func_name, block_id, *base, value_types, types)?;
        }
        InstKind::Call { callee, args } => {
            let result = result
                .ok_or_else(|| err(func_name, Some(block_id), "call should define a result"))?;
            verify_call_signature(
                func_name,
                block_id,
                callee,
                args,
                result.ty,
                value_types,
                types,
                direct_sigs,
            )?;
        }
        InstKind::Const { .. }
        | InstKind::BinOp { .. }
        | InstKind::UnOp { .. }
        | InstKind::IntTrunc { .. }
        | InstKind::IntExtend { .. }
        | InstKind::Cmp { .. }
        | InstKind::Cast { .. } => {}
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RuntimeArgKind {
    AnyReg,
    Ptr,
    Int,
    IntOrBool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RuntimeRetKind {
    Unit,
    Int,
    Bool,
    Ptr,
}

struct RuntimeCallSpec {
    args: &'static [RuntimeArgKind],
    ret: RuntimeRetKind,
}

fn runtime_call_spec(runtime: &RuntimeFn) -> RuntimeCallSpec {
    use RuntimeArgKind::{AnyReg, Int, IntOrBool, Ptr};
    use RuntimeRetKind::{Bool as RetBool, Int as RetInt, Ptr as RetPtr, Unit};

    match runtime {
        RuntimeFn::Trap => RuntimeCallSpec {
            args: &[AnyReg, AnyReg, AnyReg, AnyReg],
            ret: Unit,
        },
        RuntimeFn::Print => RuntimeCallSpec {
            args: &[Ptr, Int, Int],
            ret: Unit,
        },
        RuntimeFn::StringFromBytes => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::FmtInit => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::FmtAppendBytes => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::FmtAppendU64 => RuntimeCallSpec {
            args: &[Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::FmtAppendI64 => RuntimeCallSpec {
            args: &[Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::FmtAppendBool => RuntimeCallSpec {
            args: &[Ptr, IntOrBool],
            ret: Unit,
        },
        RuntimeFn::FmtFinish => RuntimeCallSpec {
            args: &[Ptr, Ptr],
            ret: Unit,
        },
        RuntimeFn::U64ToDec
        | RuntimeFn::I64ToDec
        | RuntimeFn::U64ToBin
        | RuntimeFn::U64ToOct
        | RuntimeFn::U64ToHex => RuntimeCallSpec {
            args: &[Ptr, Int, Int],
            ret: RetInt,
        },
        RuntimeFn::MemSet => RuntimeCallSpec {
            args: &[Ptr, Int, Int],
            ret: Unit,
        },
        RuntimeFn::MemCopy => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::StringEnsure => RuntimeCallSpec {
            args: &[Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::StringDrop => RuntimeCallSpec {
            args: &[Ptr],
            ret: Unit,
        },
        RuntimeFn::StringEq => RuntimeCallSpec {
            args: &[Ptr, Ptr],
            ret: RetBool,
        },
        RuntimeFn::StringAppendBytes => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int],
            ret: Unit,
        },
        RuntimeFn::StringAppendBool => RuntimeCallSpec {
            args: &[Ptr, IntOrBool],
            ret: Unit,
        },
        RuntimeFn::DynArrayEnsure => RuntimeCallSpec {
            args: &[Ptr, Int, Int, Int],
            ret: Unit,
        },
        RuntimeFn::DynArrayAppendElem => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int, Int],
            ret: Unit,
        },
        RuntimeFn::SetInsertElem => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int, Int],
            ret: RetBool,
        },
        RuntimeFn::SetContainsElem | RuntimeFn::SetRemoveElem => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int],
            ret: RetBool,
        },
        RuntimeFn::SetClear => RuntimeCallSpec {
            args: &[Ptr],
            ret: Unit,
        },
        RuntimeFn::MapInsertOrAssign => RuntimeCallSpec {
            args: &[Ptr, Ptr, Ptr, Int, Int],
            ret: RetBool,
        },
        RuntimeFn::MapContainsKey | RuntimeFn::MapRemoveKey => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int, Int],
            ret: RetBool,
        },
        RuntimeFn::MapGetValue => RuntimeCallSpec {
            args: &[Ptr, Ptr, Int, Int, Ptr],
            ret: RetBool,
        },
        RuntimeFn::MapClear | RuntimeFn::MapDrop => RuntimeCallSpec {
            args: &[Ptr],
            ret: Unit,
        },
        RuntimeFn::MachineEmitSend | RuntimeFn::MachineEmitReply => RuntimeCallSpec {
            args: &[Int, Int, Int],
            ret: RetBool,
        },
        RuntimeFn::MachineEmitRequest => RuntimeCallSpec {
            args: &[Int, Int, Int],
            ret: RetInt,
        },
        RuntimeFn::MachineRegisterThunkWithTag => RuntimeCallSpec {
            args: &[Int, Int, Int],
            ret: Unit,
        },
        RuntimeFn::MachineRegisterDescriptor => RuntimeCallSpec {
            args: &[Ptr, Int],
            ret: RetInt,
        },
        RuntimeFn::Alloc => RuntimeCallSpec {
            args: &[Int, Int],
            ret: RetPtr,
        },
        RuntimeFn::Realloc => RuntimeCallSpec {
            args: &[Ptr, Int, Int],
            ret: RetPtr,
        },
        RuntimeFn::Free => RuntimeCallSpec {
            args: &[Ptr],
            ret: Unit,
        },
        RuntimeFn::SetAllocTrace => RuntimeCallSpec {
            args: &[IntOrBool],
            ret: Unit,
        },
    }
}

fn verify_call_signature(
    func_name: &str,
    block_id: BlockId,
    callee: &Callee,
    args: &[ValueId],
    result_ty: IrTypeId,
    value_types: &HashMap<ValueId, IrTypeId>,
    types: &IrTypeCache,
    direct_sigs: &HashMap<DefId, FunctionSig>,
) -> Result<(), VerifyIrError> {
    match callee {
        Callee::Direct(def_id) => {
            if let Some(sig) = direct_sigs.get(def_id) {
                verify_typed_call_signature(
                    func_name,
                    block_id,
                    "direct call",
                    args,
                    &sig.params,
                    sig.ret,
                    result_ty,
                    value_types,
                )?;
            }
        }
        Callee::Value(callee_value) => {
            let callee_ty = value_ty(func_name, block_id, *callee_value, value_types)?;
            let IrTypeKind::Fn { params, ret } = types.kind(callee_ty) else {
                return Err(err(
                    func_name,
                    Some(block_id),
                    format!(
                        "indirect callee {:?} must be fn type, got {:?}",
                        callee_value,
                        types.kind(callee_ty)
                    ),
                ));
            };
            verify_typed_call_signature(
                func_name,
                block_id,
                "indirect call",
                args,
                params,
                *ret,
                result_ty,
                value_types,
            )?;
        }
        Callee::Runtime(runtime) => {
            let spec = runtime_call_spec(runtime);
            if args.len() != spec.args.len() {
                return Err(err(
                    func_name,
                    Some(block_id),
                    format!(
                        "runtime call {} expects {} args, got {}",
                        runtime.name(),
                        spec.args.len(),
                        args.len()
                    ),
                ));
            }
            for (idx, (arg, expected_kind)) in args.iter().zip(spec.args.iter()).enumerate() {
                let arg_ty = value_ty(func_name, block_id, *arg, value_types)?;
                if !matches_runtime_arg_kind(types, arg_ty, *expected_kind) {
                    return Err(err(
                        func_name,
                        Some(block_id),
                        format!(
                            "runtime call {} arg {} expects {:?}, got {:?}",
                            runtime.name(),
                            idx,
                            expected_kind,
                            types.kind(arg_ty)
                        ),
                    ));
                }
            }
            if !matches_runtime_ret_kind(types, result_ty, spec.ret) {
                return Err(err(
                    func_name,
                    Some(block_id),
                    format!(
                        "runtime call {} result expects {:?}, got {:?}",
                        runtime.name(),
                        spec.ret,
                        types.kind(result_ty)
                    ),
                ));
            }
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn verify_typed_call_signature(
    func_name: &str,
    block_id: BlockId,
    call_kind: &str,
    args: &[ValueId],
    params: &[IrTypeId],
    ret: IrTypeId,
    result_ty: IrTypeId,
    value_types: &HashMap<ValueId, IrTypeId>,
) -> Result<(), VerifyIrError> {
    if args.len() != params.len() {
        return Err(err(
            func_name,
            Some(block_id),
            format!(
                "{call_kind} expects {} args, got {}",
                params.len(),
                args.len()
            ),
        ));
    }

    for (idx, (arg, expected_ty)) in args.iter().zip(params.iter()).enumerate() {
        let arg_ty = value_ty(func_name, block_id, *arg, value_types)?;
        if arg_ty != *expected_ty {
            return Err(err(
                func_name,
                Some(block_id),
                format!(
                    "{call_kind} arg {} type mismatch: expected {:?}, got {:?}",
                    idx, expected_ty, arg_ty
                ),
            ));
        }
    }

    if result_ty != ret {
        return Err(err(
            func_name,
            Some(block_id),
            format!(
                "{call_kind} result type mismatch: expected {:?}, got {:?}",
                ret, result_ty
            ),
        ));
    }

    Ok(())
}

fn matches_runtime_arg_kind(types: &IrTypeCache, ty: IrTypeId, kind: RuntimeArgKind) -> bool {
    match kind {
        RuntimeArgKind::AnyReg => types.is_reg_type(ty),
        RuntimeArgKind::Ptr => matches!(types.kind(ty), IrTypeKind::Ptr { .. }),
        RuntimeArgKind::Int => matches!(types.kind(ty), IrTypeKind::Int { .. }),
        RuntimeArgKind::IntOrBool => {
            matches!(types.kind(ty), IrTypeKind::Int { .. } | IrTypeKind::Bool)
        }
    }
}

fn matches_runtime_ret_kind(types: &IrTypeCache, ty: IrTypeId, kind: RuntimeRetKind) -> bool {
    match kind {
        RuntimeRetKind::Unit => matches!(types.kind(ty), IrTypeKind::Unit),
        RuntimeRetKind::Int => matches!(types.kind(ty), IrTypeKind::Int { .. }),
        RuntimeRetKind::Bool => matches!(types.kind(ty), IrTypeKind::Bool),
        RuntimeRetKind::Ptr => matches!(types.kind(ty), IrTypeKind::Ptr { .. }),
    }
}

fn verify_terminator(
    func_name: &str,
    block: &Block,
    blocks: &HashMap<BlockId, &Block>,
    value_types: &HashMap<ValueId, IrTypeId>,
    types: &IrTypeCache,
) -> Result<(), VerifyIrError> {
    match &block.term {
        Terminator::Br { target, args } => {
            check_block_args(func_name, block.id, *target, args, blocks, value_types)?;
        }
        Terminator::CondBr {
            cond,
            then_bb,
            then_args,
            else_bb,
            else_args,
        } => {
            require_bool(func_name, block.id, *cond, value_types, types)?;
            check_block_args(
                func_name,
                block.id,
                *then_bb,
                then_args,
                blocks,
                value_types,
            )?;
            check_block_args(
                func_name,
                block.id,
                *else_bb,
                else_args,
                blocks,
                value_types,
            )?;
        }
        Terminator::Switch {
            value,
            cases,
            default,
            default_args,
        } => {
            let _ = value_ty(func_name, block.id, *value, value_types)?;
            for case in cases {
                check_block_args(
                    func_name,
                    block.id,
                    case.target,
                    &case.args,
                    blocks,
                    value_types,
                )?;
            }
            check_block_args(
                func_name,
                block.id,
                *default,
                default_args,
                blocks,
                value_types,
            )?;
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                let _ = value_ty(func_name, block.id, *value, value_types)?;
            }
        }
        Terminator::Unreachable => {}
    }

    Ok(())
}

fn check_block_args(
    func_name: &str,
    from_block: BlockId,
    target: BlockId,
    args: &[ValueId],
    blocks: &HashMap<BlockId, &Block>,
    value_types: &HashMap<ValueId, IrTypeId>,
) -> Result<(), VerifyIrError> {
    let target_block = blocks.get(&target).ok_or_else(|| {
        err(
            func_name,
            Some(from_block),
            format!("branch to missing block {:?}", target),
        )
    })?;

    if args.len() != target_block.params.len() {
        return Err(err(
            func_name,
            Some(from_block),
            format!(
                "block {:?} expects {} args, got {}",
                target,
                target_block.params.len(),
                args.len()
            ),
        ));
    }

    for (idx, (arg, param)) in args.iter().zip(&target_block.params).enumerate() {
        let arg_ty = value_ty(func_name, from_block, *arg, value_types)?;
        if arg_ty != param.value.ty {
            return Err(err(
                func_name,
                Some(from_block),
                format!("arg {} to block {:?} has wrong type", idx, target),
            ));
        }
    }

    Ok(())
}

fn value_ty(
    func_name: &str,
    block_id: BlockId,
    value: ValueId,
    value_types: &HashMap<ValueId, IrTypeId>,
) -> Result<IrTypeId, VerifyIrError> {
    value_types.get(&value).copied().ok_or_else(|| {
        err(
            func_name,
            Some(block_id),
            format!("use of undefined value {:?}", value),
        )
    })
}

fn require_ptr(
    func_name: &str,
    block_id: BlockId,
    value: ValueId,
    value_types: &HashMap<ValueId, IrTypeId>,
    types: &IrTypeCache,
) -> Result<(), VerifyIrError> {
    let ty = value_ty(func_name, block_id, value, value_types)?;
    if !matches!(types.kind(ty), IrTypeKind::Ptr { .. }) {
        return Err(err(
            func_name,
            Some(block_id),
            format!("expected pointer value, got {:?}", types.kind(ty)),
        ));
    }
    Ok(())
}

fn require_bool(
    func_name: &str,
    block_id: BlockId,
    value: ValueId,
    value_types: &HashMap<ValueId, IrTypeId>,
    types: &IrTypeCache,
) -> Result<(), VerifyIrError> {
    let ty = value_ty(func_name, block_id, value, value_types)?;
    if !matches!(types.kind(ty), IrTypeKind::Bool) {
        return Err(err(
            func_name,
            Some(block_id),
            "conditional branch expects bool",
        ));
    }
    Ok(())
}

fn err(func_name: &str, block_id: Option<BlockId>, message: impl Into<String>) -> VerifyIrError {
    let message = match block_id {
        Some(block_id) => format!(
            "backend verify: {func_name} {:?}: {}",
            block_id,
            message.into()
        ),
        None => format!("backend verify: {func_name}: {}", message.into()),
    };
    VerifyIrError::new(message)
}
