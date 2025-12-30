use super::Pass;
use crate::mcir::{BinOp, Const, FuncBody, Operand, Rvalue, Statement, Terminator, UnOp};

/// Perform local statement/terminator simplifications without CFG or dataflow.
pub struct LocalSimplify;

impl Pass for LocalSimplify {
    fn name(&self) -> &'static str {
        "local-simplify"
    }

    fn run(&mut self, body: &mut FuncBody) -> bool {
        let mut changed = false;
        for block in &mut body.blocks {
            // Simplify rvalues in-place without changing statement structure.
            for stmt in &mut block.stmts {
                if let Statement::CopyScalar { src, .. } = stmt {
                    let new_src = simplify_rvalue(src);
                    if *src != new_src {
                        *src = new_src;
                        changed = true;
                    }
                }
            }

            // Fold trivial terminators (no CFG rewrite here).
            let new_term = simplify_terminator(&block.terminator, &block.stmts);
            if block.terminator != new_term {
                block.terminator = new_term;
                changed = true;
            }
        }
        changed
    }
}

fn simplify_rvalue(rvalue: &Rvalue) -> Rvalue {
    match rvalue {
        Rvalue::BinOp { op, lhs, rhs } => {
            // Constant fold and identity rules for binary ops.
            if let Some(const_val) = fold_binop(*op, lhs, rhs) {
                return Rvalue::Use(Operand::Const(const_val));
            }

            if matches!(op, BinOp::Eq | BinOp::Ne) && lhs == rhs {
                return Rvalue::Use(Operand::Const(Const::Bool(matches!(op, BinOp::Eq))));
            }

            if let Some(opnd) = simplify_binop_identity(*op, lhs, rhs) {
                return Rvalue::Use(opnd);
            }

            rvalue.clone()
        }
        Rvalue::UnOp { op, arg } => {
            if let Operand::Const(const_val) = arg {
                // Constant fold unary ops where possible.
                if let Some(const_val) = fold_unop(*op, const_val) {
                    return Rvalue::Use(Operand::Const(const_val));
                }
            }
            rvalue.clone()
        }
        Rvalue::Use(_) => rvalue.clone(),
        Rvalue::AddrOf(_) => rvalue.clone(),
    }
}

fn simplify_terminator(term: &Terminator, stmts: &[Statement]) -> Terminator {
    match term {
        Terminator::If {
            cond,
            then_bb,
            else_bb,
        } => {
            if let Some(value) = const_bool_from_last_assign(cond, stmts) {
                return Terminator::Goto(if value { *then_bb } else { *else_bb });
            }
            if then_bb == else_bb {
                return Terminator::Goto(*then_bb);
            }
            term.clone()
        }
        Terminator::Switch {
            discr: _,
            cases,
            default,
        } if cases.is_empty() => Terminator::Goto(*default),
        _ => term.clone(),
    }
}

fn fold_binop(op: BinOp, lhs: &Operand, rhs: &Operand) -> Option<Const> {
    let lhs = match lhs {
        Operand::Const(const_val) => const_val,
        _ => return None,
    };
    let rhs = match rhs {
        Operand::Const(const_val) => const_val,
        _ => return None,
    };

    match (lhs, rhs) {
        (
            Const::Int {
                value: l,
                signed: ls,
                bits: lb,
            },
            Const::Int {
                value: r,
                signed: rs,
                bits: rb,
            },
        ) if ls == rs && lb == rb => match op {
            BinOp::Add => Some(Const::Int {
                value: l + r,
                signed: *ls,
                bits: *lb,
            }),
            BinOp::Sub => Some(Const::Int {
                value: l - r,
                signed: *ls,
                bits: *lb,
            }),
            BinOp::Mul => Some(Const::Int {
                value: l * r,
                signed: *ls,
                bits: *lb,
            }),
            BinOp::Div => {
                if *r == 0 {
                    None
                } else {
                    Some(Const::Int {
                        value: l / r,
                        signed: *ls,
                        bits: *lb,
                    })
                }
            }
            BinOp::Eq => Some(Const::Bool(l == r)),
            BinOp::Ne => Some(Const::Bool(l != r)),
            BinOp::Lt => Some(Const::Bool(l < r)),
            BinOp::Gt => Some(Const::Bool(l > r)),
            BinOp::LtEq => Some(Const::Bool(l <= r)),
            BinOp::GtEq => Some(Const::Bool(l >= r)),
        },
        (Const::Bool(l), Const::Bool(r)) => match op {
            BinOp::Eq => Some(Const::Bool(l == r)),
            BinOp::Ne => Some(Const::Bool(l != r)),
            _ => None,
        },
        _ => None,
    }
}

fn fold_unop(op: UnOp, val: &Const) -> Option<Const> {
    match (op, val) {
        (
            UnOp::Neg,
            Const::Int {
                value,
                signed: true,
                bits,
            },
        ) => Some(Const::Int {
            value: -*value,
            signed: true,
            bits: *bits,
        }),
        _ => None,
    }
}

fn simplify_binop_identity(op: BinOp, lhs: &Operand, rhs: &Operand) -> Option<Operand> {
    match op {
        BinOp::Add => {
            if is_const_int_zero(rhs) {
                Some(lhs.clone())
            } else if is_const_int_zero(lhs) {
                Some(rhs.clone())
            } else {
                None
            }
        }
        BinOp::Sub => {
            if is_const_int_zero(rhs) {
                Some(lhs.clone())
            } else {
                None
            }
        }
        BinOp::Mul => {
            if is_const_int_one(rhs) {
                Some(lhs.clone())
            } else if is_const_int_one(lhs) {
                Some(rhs.clone())
            } else if let Some(int_zero) = const_int_zero_from(lhs) {
                Some(Operand::Const(int_zero))
            } else if let Some(int_zero) = const_int_zero_from(rhs) {
                Some(Operand::Const(int_zero))
            } else {
                None
            }
        }
        BinOp::Div => {
            if is_const_int_one(rhs) {
                Some(lhs.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_const_int_zero(op: &Operand) -> bool {
    matches!(op, Operand::Const(Const::Int { value: 0, .. }))
}

fn is_const_int_one(op: &Operand) -> bool {
    matches!(op, Operand::Const(Const::Int { value: 1, .. }))
}

fn const_int_zero_from(op: &Operand) -> Option<Const> {
    match op {
        Operand::Const(Const::Int { signed, bits, .. }) => Some(Const::Int {
            value: 0,
            signed: *signed,
            bits: *bits,
        }),
        _ => None,
    }
}

fn const_bool_from_last_assign(cond: &Operand, stmts: &[Statement]) -> Option<bool> {
    // Condition must be a copy or move
    let (Operand::Copy(place) | Operand::Move(place)) = cond else {
        return None;
    };

    // Last statement must be a copy scalar
    let Statement::CopyScalar { dst, src } = stmts.last()? else {
        return None;
    };

    // Destination must be the same as the condition operand
    if dst.base() != place.base() || dst.projections() != place.projections() {
        return None;
    }

    // Source must be a constant boolean
    match src {
        Rvalue::Use(Operand::Const(Const::Bool(value))) => Some(*value),
        _ => None,
    }
}
