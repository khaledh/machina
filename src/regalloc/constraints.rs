use std::collections::HashMap;

use crate::ir::pos::InstPos;
use crate::ir::types::{
    IrBlockId, IrFunction, IrInst, IrOperand, IrTempId, IrTempRole, IrTerminator, IrType,
};
use crate::regalloc::regs::{self, Arm64Reg};

#[derive(Debug)]
pub struct FnParamConstraint {
    pub temp: IrTempId,
    pub reg: Arm64Reg, // param arrives in this register on function entry
}

#[derive(Debug)]
pub struct FnReturnConstraint {
    pub operand: IrOperand,
    pub reg: Arm64Reg, // return value is returned in this register after the function returns
}

#[derive(Debug)]
pub struct CallArgConstraint {
    pub operand: IrOperand,
    pub reg: Arm64Reg, // call arg must be in this reg before the call is made
}

#[derive(Debug)]
pub struct CallResultConstraint {
    pub temp: IrTempId,
    pub reg: Arm64Reg, // call result is returned in this reg after the call returns
}

#[derive(Debug)]
pub struct CallConstraint {
    pub pos: InstPos,
    pub args: Vec<CallArgConstraint>,
    pub result: Option<CallResultConstraint>,
    pub clobbers: Vec<Arm64Reg>,
}

#[derive(Debug)]
pub struct ConstraintMap {
    pub call_constraints: Vec<CallConstraint>,
    pub fn_param_constraints: Vec<FnParamConstraint>,
    pub fn_return_constraints: HashMap<IrBlockId, FnReturnConstraint>,
}

impl ConstraintMap {
    pub fn new() -> Self {
        Self {
            call_constraints: Vec::new(),
            fn_param_constraints: Vec::new(),
            fn_return_constraints: HashMap::new(),
        }
    }
}

pub fn analyze_fn_params(func: &IrFunction) -> Vec<FnParamConstraint> {
    let mut constraints = Vec::new();
    for (i, temp) in func.temps.iter().enumerate() {
        if let IrTempRole::Param { index } = temp.role {
            let reg = regs::get_param_reg(index);
            constraints.push(FnParamConstraint {
                temp: IrTempId(i as u32),
                reg,
            });
        }
    }
    constraints
}

pub fn analyze_fn_return(func: &IrFunction) -> HashMap<IrBlockId, FnReturnConstraint> {
    if func.ret_ty == IrType::Unit {
        return HashMap::new();
    }

    let mut constraints = HashMap::new();
    for block in func.blocks.values() {
        if let IrTerminator::Ret {
            value: Some(operand),
        } = block.term
        {
            constraints.insert(
                block.id(),
                FnReturnConstraint {
                    operand,
                    reg: regs::get_result_reg(),
                },
            );
        }
    }
    constraints
}

pub fn analyze_call(func: &IrFunction) -> Vec<CallConstraint> {
    let mut constraints = Vec::new();
    for block in func.blocks.values() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            if let IrInst::Call { result, args, .. } = inst {
                let pos = InstPos::new(block.id(), inst_idx);
                let arg_constraints = args
                    .iter()
                    .enumerate()
                    .map(|(arg_idx, arg)| CallArgConstraint {
                        operand: *arg,
                        reg: regs::get_param_reg(arg_idx as u32),
                    })
                    .collect();
                let result_constraint = result.map(|temp| CallResultConstraint {
                    temp,
                    reg: regs::get_result_reg(),
                });
                constraints.push(CallConstraint {
                    pos,
                    args: arg_constraints,
                    result: result_constraint,
                    clobbers: regs::CALLER_SAVED_REGS.to_vec(),
                });
            }
        }
    }
    constraints
}

pub fn analyze_constraints(func: &IrFunction) -> ConstraintMap {
    let mut constraints = ConstraintMap::new();
    constraints.fn_param_constraints = analyze_fn_params(func);
    constraints.call_constraints = analyze_call(func);
    constraints.fn_return_constraints = analyze_fn_return(func);
    constraints
}

#[cfg(test)]
#[path = "../tests/t_regalloc_constraints.rs"]
mod tests;
