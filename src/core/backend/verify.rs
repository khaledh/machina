use std::collections::HashMap;
use std::fmt;

use crate::backend::lower::{LoweredFunction, LoweredModule};
use crate::ir::{
    Block, BlockId, Callee, Function, InstKind, IrTypeCache, IrTypeId, IrTypeKind, Terminator,
    ValueDef, ValueId, for_each_inst_use,
};

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
    for func in &module.funcs {
        verify_function(func)?;
    }
    Ok(())
}

#[cfg(test)]
#[path = "../../tests/backend/verify/t_verify.rs"]
mod tests;
fn verify_function(lowered: &LoweredFunction) -> Result<(), VerifyIrError> {
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
        InstKind::Call { callee, .. } => {
            if let Callee::Value(value) = callee {
                let _ = value_ty(func_name, block_id, *value, value_types)?;
            }
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
