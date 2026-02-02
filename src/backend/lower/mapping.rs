//! Operator mapping helpers for SSA lowering.

use crate::ir::{BinOp, CmpOp};
use crate::tree::BinaryOp;

pub(super) fn map_binop(op: BinaryOp) -> Option<BinOp> {
    match op {
        BinaryOp::Add => Some(BinOp::Add),
        BinaryOp::Sub => Some(BinOp::Sub),
        BinaryOp::Mul => Some(BinOp::Mul),
        BinaryOp::Div => Some(BinOp::Div),
        BinaryOp::Mod => Some(BinOp::Mod),
        BinaryOp::BitOr => Some(BinOp::Or),
        BinaryOp::BitXor => Some(BinOp::Xor),
        BinaryOp::BitAnd => Some(BinOp::And),
        BinaryOp::Shl => Some(BinOp::Shl),
        BinaryOp::Shr => Some(BinOp::Shr),
        _ => None,
    }
}

pub(super) fn map_cmp(op: BinaryOp) -> Option<CmpOp> {
    match op {
        BinaryOp::Eq => Some(CmpOp::Eq),
        BinaryOp::Ne => Some(CmpOp::Ne),
        BinaryOp::Lt => Some(CmpOp::Lt),
        BinaryOp::LtEq => Some(CmpOp::Le),
        BinaryOp::Gt => Some(CmpOp::Gt),
        BinaryOp::GtEq => Some(CmpOp::Ge),
        _ => None,
    }
}
