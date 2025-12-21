use std::collections::HashMap;

use crate::mcir::types::{BlockId, FuncBody, LocalId, LocalKind, PlaceAny, Statement, Terminator};
use crate::regalloc::pos::InstPos;
use crate::regalloc::regs::{self, Arm64Reg};

#[derive(Debug)]
pub struct FnParamConstraint {
    pub local: LocalId,
    pub reg: Arm64Reg,
}

#[derive(Debug)]
pub struct FnReturnConstraint {
    pub local: LocalId,
    pub reg: Arm64Reg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallArgKind {
    Value,
    Addr,
}

#[derive(Debug)]
pub struct CallArgConstraint {
    pub place: PlaceAny,
    pub reg: Arm64Reg,
    pub kind: CallArgKind,
}

#[derive(Debug)]
pub struct CallResultConstraint {
    pub local: LocalId,
    pub reg: Arm64Reg,
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
    pub fn_return_constraints: HashMap<BlockId, FnReturnConstraint>,
}

pub fn analyze_fn_params(body: &FuncBody) -> Vec<FnParamConstraint> {
    let mut out = Vec::new();
    for (i, local) in body.locals.iter().enumerate() {
        if let LocalKind::Param { index } = local.kind {
            // move local to param register
            out.push(FnParamConstraint {
                local: LocalId(i as u32),
                reg: regs::get_param_reg(index),
            });
        }
    }

    // Indirect result for aggregate returns.
    let ret_local = body.ret_local;
    let ret_ty = body.locals[ret_local.index()].ty;
    if body.types.get(ret_ty).is_aggregate() {
        out.push(FnParamConstraint {
            local: ret_local,
            reg: regs::get_indirect_result_reg(),
        });
    }
    out
}

pub fn analyze_fn_return(body: &FuncBody) -> HashMap<BlockId, FnReturnConstraint> {
    let ret_local = body.ret_local;
    let ret_ty = body.locals[ret_local.index()].ty;
    if body.types.get(ret_ty).is_aggregate() {
        // for aggregates, return constraints are empty (sret handled via call constraints)
        return HashMap::new();
    }

    let mut map = HashMap::new();
    for (i, block) in body.blocks.iter().enumerate() {
        if matches!(block.terminator, Terminator::Return) {
            // move ret_local to result register
            map.insert(
                BlockId(i as u32),
                FnReturnConstraint {
                    local: ret_local,
                    reg: regs::get_result_reg(),
                },
            );
        }
    }
    map
}

pub fn analyze_calls(body: &FuncBody) -> Vec<CallConstraint> {
    let mut constraints = Vec::new();
    for (b_idx, block) in body.blocks.iter().enumerate() {
        for (i_idx, stmt) in block.stmts.iter().enumerate() {
            let Statement::Call { dst, args, .. } = stmt else {
                continue;
            };

            let mut arg_constraints = Vec::new();
            for (arg_idx, arg) in args.iter().enumerate() {
                let arg_ty = arg.ty();
                let kind = if body.types.get(arg_ty).is_aggregate() {
                    // aggregates: pass by address
                    CallArgKind::Addr
                } else {
                    // scalar: pass by value
                    CallArgKind::Value
                };
                arg_constraints.push(CallArgConstraint {
                    place: arg.clone(),
                    reg: regs::get_param_reg(arg_idx as u32),
                    kind,
                });
            }

            let dst_ty = dst.ty();
            let result = if body.types.get(dst_ty).is_aggregate() {
                // aggregate result: add implicit indirect result arg constraint
                arg_constraints.push(CallArgConstraint {
                    place: dst.clone(),
                    reg: regs::get_indirect_result_reg(),
                    kind: CallArgKind::Addr,
                });
                None
            } else {
                // scalar result: add explicit result constraint
                let local = match dst {
                    PlaceAny::Scalar(p) => p.base(),
                    PlaceAny::Aggregate(p) => p.base(),
                };
                Some(CallResultConstraint {
                    local,
                    reg: regs::get_result_reg(),
                })
            };

            constraints.push(CallConstraint {
                pos: InstPos::new(BlockId(b_idx as u32), i_idx),
                args: arg_constraints,
                result,
            });
        }
    }
    constraints
}

pub fn analyze_constraints(body: &FuncBody) -> ConstraintMap {
    ConstraintMap {
        fn_param_constraints: analyze_fn_params(body),
        call_constraints: analyze_calls(body),
        fn_return_constraints: analyze_fn_return(body),
    }
}
