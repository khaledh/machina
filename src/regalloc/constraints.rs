use std::collections::HashMap;
use std::fmt;

use crate::ir::pos::InstPos;
use crate::ir::types::{
    IrBlockId, IrFunction, IrInst, IrOperand, IrTempId, IrTempKind, IrTerminator, IrType,
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
}

#[derive(Debug)]
pub struct ConstraintMap {
    pub call_constraints: Vec<CallConstraint>,
    pub fn_param_constraints: Vec<FnParamConstraint>,
    pub fn_return_constraints: HashMap<IrBlockId, FnReturnConstraint>,
}

pub fn analyze_fn_params(func: &IrFunction) -> Vec<FnParamConstraint> {
    let mut constraints = Vec::new();
    for (i, temp) in func.temps.iter().enumerate() {
        if let IrTempKind::Param { index } = temp.kind {
            let reg = regs::get_param_reg(index);
            constraints.push(FnParamConstraint {
                temp: IrTempId(i as u32),
                reg,
            });
        }
    }

    // add indirect result constraint if the return type is compound
    if let Some(ret_temp) = func.ret_temp {
        constraints.push(FnParamConstraint {
            temp: ret_temp,
            reg: regs::get_indirect_result_reg(),
        });
    }

    constraints
}

pub fn analyze_fn_return(func: &IrFunction) -> HashMap<IrBlockId, FnReturnConstraint> {
    if func.ret_ty == IrType::Unit || func.ret_ty.is_compound() {
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
            if let IrInst::Call {
                result,
                args,
                ret_ty,
                ..
            } = inst
            {
                let pos = InstPos::new(block.id(), inst_idx);

                // Create the argument constraints
                let mut arg_constraints = Vec::new();
                for (arg_idx, arg) in args.iter().enumerate() {
                    arg_constraints.push(CallArgConstraint {
                        operand: *arg,
                        reg: regs::get_param_reg(arg_idx as u32),
                    });
                }

                // Add indirect result constraint if the return type is compound
                if ret_ty.is_compound() {
                    arg_constraints.push(CallArgConstraint {
                        operand: IrOperand::Temp(result.unwrap()),
                        reg: regs::get_indirect_result_reg(),
                    });
                }

                // Create the result constraint (if there's a result and it's not a compound type)
                let result_constraint = if !ret_ty.is_compound() {
                    result.map(|temp| CallResultConstraint {
                        temp,
                        reg: regs::get_result_reg(),
                    })
                } else {
                    None
                };

                // Create the call constraint
                constraints.push(CallConstraint {
                    pos,
                    args: arg_constraints,
                    result: result_constraint,
                });
            }
        }
    }
    constraints
}

pub fn analyze_constraints(func: &IrFunction) -> ConstraintMap {
    ConstraintMap {
        fn_param_constraints: analyze_fn_params(func),
        call_constraints: analyze_call(func),
        fn_return_constraints: analyze_fn_return(func),
    }
}

impl fmt::Display for ConstraintMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ConstraintMap {{")?;
        writeln!(f, "  fn_param_constraints:")?;
        for constraint in &self.fn_param_constraints {
            writeln!(f, "{}", constraint)?;
        }
        writeln!(f, "  call_constraints:")?;
        for constraint in &self.call_constraints {
            writeln!(f, "{}", constraint)?;
        }
        writeln!(f, "  fn_return_constraints:")?;
        for (block_id, constraint) in &self.fn_return_constraints {
            writeln!(f, "    block.{}:", block_id.id())?;
            write!(f, "{}", constraint)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for FnParamConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "    temp: {} -> {}", self.temp, self.reg)?;
        Ok(())
    }
}

impl fmt::Display for FnReturnConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "      {} -> {}", self.operand, self.reg)?;
        Ok(())
    }
}

impl fmt::Display for CallArgConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "      {} -> {}", self.operand, self.reg)?;
        Ok(())
    }
}

impl fmt::Display for CallResultConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "      {} -> {}", self.reg, self.temp)?;
        Ok(())
    }
}

impl fmt::Display for CallConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "    pos: {}", self.pos)?;
        writeln!(f, "    args:")?;
        for constraint in &self.args {
            write!(f, "{}", constraint)?;
        }
        writeln!(f, "    result:")?;
        if let Some(constraint) = &self.result {
            write!(f, "{}", constraint)?;
        }
        Ok(())
    }
}

#[cfg(test)]
#[path = "../tests/t_regalloc_constraints.rs"]
mod tests;
