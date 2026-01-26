//! Constant folding and trivial branch simplification for SSA.

use crate::ssa::IrTypeId;
use crate::ssa::model::ir::{
    BinOp, ConstValue, Function, InstKind, Terminator, UnOp, ValueDef, ValueId,
};

use super::Pass;

pub struct ConstFold;

impl Pass for ConstFold {
    fn name(&self) -> &'static str {
        "ssa-const-fold"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let mut changed = false;
        let mut env = ConstEnv::new();

        for block in &mut func.blocks {
            // Keep this pass block-local; no cross-block constant propagation.
            env.clear();

            for inst in &mut block.insts {
                if let Some(result) = &inst.result {
                    env.remove(result.id);
                }

                match &mut inst.kind {
                    InstKind::Const { value } => {
                        if let Some(result) = &inst.result {
                            env.insert(result, value.clone());
                        }
                    }
                    InstKind::BinOp { op, lhs, rhs } => {
                        if let Some(result) = &inst.result {
                            if let Some(value) = fold_binop(*op, *lhs, *rhs, &env) {
                                inst.kind = InstKind::Const {
                                    value: value.clone(),
                                };
                                env.insert(result, value);
                                changed = true;
                            }
                        }
                    }
                    InstKind::Cmp { op, lhs, rhs } => {
                        if let Some(result) = &inst.result {
                            if let Some(value) = fold_cmp(*op, *lhs, *rhs, &env) {
                                inst.kind = InstKind::Const {
                                    value: value.clone(),
                                };
                                env.insert(result, value);
                                changed = true;
                            }
                        }
                    }
                    InstKind::UnOp { op, value } => {
                        if let Some(result) = &inst.result {
                            if let Some(value) = fold_unop(*op, *value, &env) {
                                inst.kind = InstKind::Const {
                                    value: value.clone(),
                                };
                                env.insert(result, value);
                                changed = true;
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Only fold terminators when the condition is known in this block.
            if simplify_terminator(&mut block.term, &env) {
                changed = true;
            }
        }

        changed
    }
}

/// Tracks block-local constant values keyed by SSA `ValueId`.
#[derive(Default)]
struct ConstEnv {
    // Indexable by ValueId.index(); only valid within the current block.
    values: Vec<Option<ConstValue>>,
    // Reserved for type-aware folds (kept in sync with values).
    tys: Vec<Option<IrTypeId>>,
}

impl ConstEnv {
    fn new() -> Self {
        Self::default()
    }

    fn clear(&mut self) {
        self.values.clear();
        self.tys.clear();
    }

    fn ensure_capacity(&mut self, id: ValueId) {
        let idx = id.index();
        if idx >= self.values.len() {
            self.values.resize(idx + 1, None);
            self.tys.resize(idx + 1, None);
        }
    }

    fn insert(&mut self, result: &ValueDef, value: ConstValue) {
        self.ensure_capacity(result.id);
        self.values[result.id.index()] = Some(value);
        self.tys[result.id.index()] = Some(result.ty);
    }

    fn remove(&mut self, id: ValueId) {
        if id.index() < self.values.len() {
            self.values[id.index()] = None;
            self.tys[id.index()] = None;
        }
    }

    fn get(&self, id: ValueId) -> Option<ConstValue> {
        self.values.get(id.index()).and_then(|v| v.clone())
    }

    fn get_int(&self, id: ValueId) -> Option<(i128, bool, u8)> {
        match self.get(id)? {
            ConstValue::Int {
                value,
                signed,
                bits,
            } => Some((value, signed, bits)),
            _ => None,
        }
    }

    fn get_bool(&self, id: ValueId) -> Option<bool> {
        match self.get(id)? {
            ConstValue::Bool(value) => Some(value),
            _ => None,
        }
    }
}

/// Fold integer binary operations when both operands are constant.
fn fold_binop(op: BinOp, lhs: ValueId, rhs: ValueId, env: &ConstEnv) -> Option<ConstValue> {
    let (lhs_val, signed, bits) = env.get_int(lhs)?;
    let (rhs_val, rhs_signed, rhs_bits) = env.get_int(rhs)?;
    if signed != rhs_signed || bits != rhs_bits {
        return None;
    }

    // Preserve the IR's integer semantics by using wrapping arithmetic.
    let value = match op {
        BinOp::Add => lhs_val.wrapping_add(rhs_val),
        BinOp::Sub => lhs_val.wrapping_sub(rhs_val),
        BinOp::Mul => lhs_val.wrapping_mul(rhs_val),
        BinOp::Div => lhs_val.wrapping_div(rhs_val),
        BinOp::Mod => lhs_val.wrapping_rem(rhs_val),
        BinOp::And => lhs_val & rhs_val,
        BinOp::Or => lhs_val | rhs_val,
        BinOp::Xor => lhs_val ^ rhs_val,
        BinOp::Shl => lhs_val.wrapping_shl(rhs_val as u32),
        BinOp::Shr => lhs_val.wrapping_shr(rhs_val as u32),
    };

    Some(ConstValue::Int {
        value,
        signed,
        bits,
    })
}

/// Fold integer comparisons into boolean constants when operands are constant.
fn fold_cmp(
    op: crate::ssa::model::ir::CmpOp,
    lhs: ValueId,
    rhs: ValueId,
    env: &ConstEnv,
) -> Option<ConstValue> {
    let (lhs_val, signed, bits) = env.get_int(lhs)?;
    let (rhs_val, rhs_signed, rhs_bits) = env.get_int(rhs)?;
    if signed != rhs_signed || bits != rhs_bits {
        return None;
    }

    let value = match op {
        crate::ssa::model::ir::CmpOp::Eq => lhs_val == rhs_val,
        crate::ssa::model::ir::CmpOp::Ne => lhs_val != rhs_val,
        crate::ssa::model::ir::CmpOp::Lt => lhs_val < rhs_val,
        crate::ssa::model::ir::CmpOp::Le => lhs_val <= rhs_val,
        crate::ssa::model::ir::CmpOp::Gt => lhs_val > rhs_val,
        crate::ssa::model::ir::CmpOp::Ge => lhs_val >= rhs_val,
    };

    Some(ConstValue::Bool(value))
}

/// Fold unary operations for constant operands.
fn fold_unop(op: UnOp, value: ValueId, env: &ConstEnv) -> Option<ConstValue> {
    match op {
        UnOp::Not => {
            // Logical not only folds booleans.
            let value = env.get_bool(value)?;
            Some(ConstValue::Bool(!value))
        }
        UnOp::Neg | UnOp::BitNot => {
            // Integer negation/bit-not only folds integer constants.
            let (value, signed, bits) = env.get_int(value)?;
            let value = match op {
                UnOp::Neg => value.wrapping_neg(),
                UnOp::BitNot => !value,
                UnOp::Not => unreachable!(),
            };
            Some(ConstValue::Int {
                value,
                signed,
                bits,
            })
        }
    }
}

/// Replace conditional terminators with direct branches when constant.
fn simplify_terminator(term: &mut Terminator, env: &ConstEnv) -> bool {
    match term {
        Terminator::CondBr {
            cond,
            then_bb,
            then_args,
            else_bb,
            else_args,
        } => {
            // Replace conditional branches when the condition is known.
            let Some(value) = env.get_bool(*cond) else {
                return false;
            };
            let (target, args) = if value {
                (*then_bb, then_args.clone())
            } else {
                (*else_bb, else_args.clone())
            };
            *term = Terminator::Br { target, args };
            true
        }
        Terminator::Switch {
            value,
            cases,
            default,
            default_args,
        } => {
            // Replace constant switches with a direct branch to the matching arm.
            let Some(const_value) = env.get(*value) else {
                return false;
            };
            if let Some(case) = cases.iter().find(|case| case.value == const_value) {
                *term = Terminator::Br {
                    target: case.target,
                    args: case.args.clone(),
                };
                true
            } else {
                *term = Terminator::Br {
                    target: *default,
                    args: default_args.clone(),
                };
                true
            }
        }
        _ => false,
    }
}
