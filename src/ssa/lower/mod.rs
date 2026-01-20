//! Minimal SSA lowering for early smoke tests.
//!
//! Lowers a tiny subset of the semantic tree into SSA for validation of the
//! IR model and formatter.

use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{
    BinOp, BlockId, CmpOp, Function, FunctionSig, Terminator, TypeId, TypeKind, TypeTable, UnOp,
    ValueId,
};
use crate::tree::semantic as sem;
use crate::tree::{BinaryOp, UnaryOp};
use crate::typeck::type_map::TypeMap;
use crate::types::Type;

pub struct LoweredFunction {
    pub func: Function,
    pub types: TypeTable,
}

/// Lowers a trivial function body into SSA for smoke testing.
pub fn lower_const_return(func: &sem::FuncDef, type_map: &TypeMap) -> LoweredFunction {
    if !func.sig.params.is_empty() {
        panic!("ssa lower_const_return only supports functions without params");
    }

    let ret_ty = type_map.type_table().get(func.body.ty).clone();
    let mut types = TypeTable::new();
    let bool_id = types.add(TypeKind::Bool);
    let (ret_id, int_id, signed, bits) = match ret_ty {
        Type::Int { signed, bits } => {
            let int_id = types.add(TypeKind::Int { signed, bits });
            (int_id, int_id, signed, bits)
        }
        Type::Bool => {
            let signed = false;
            let bits = 64;
            let int_id = types.add(TypeKind::Int { signed, bits });
            (bool_id, int_id, signed, bits)
        }
        _ => panic!("ssa lower_const_return only supports integer or bool returns"),
    };
    let sig = FunctionSig {
        params: Vec::new(),
        ret: ret_id,
    };

    let mut builder = FunctionBuilder::new(func.def_id, func.sig.name.clone(), sig);
    let entry = builder.add_block();
    let result = lower_scalar_expr(
        &mut builder,
        entry,
        &func.body,
        int_id,
        bool_id,
        signed,
        bits,
    );
    builder.set_terminator(
        entry,
        Terminator::Return {
            value: Some(result),
        },
    );

    LoweredFunction {
        func: builder.finish(),
        types,
    }
}

/// Lowers a scalar-only expression subtree into SSA values.
fn lower_scalar_expr(
    builder: &mut FunctionBuilder,
    block: BlockId,
    expr: &sem::ValueExpr,
    int_ty: TypeId,
    bool_ty: TypeId,
    signed: bool,
    bits: u8,
) -> ValueId {
    match &expr.kind {
        sem::ValueExprKind::IntLit(value) => {
            builder.const_int(block, *value as i128, signed, bits, int_ty)
        }
        sem::ValueExprKind::BoolLit(value) => builder.const_bool(block, *value, bool_ty),
        sem::ValueExprKind::Block { items, tail } if items.is_empty() => {
            let tail = tail
                .as_ref()
                .unwrap_or_else(|| panic!("ssa lower_const_return missing block tail"));
            lower_scalar_expr(builder, block, tail, int_ty, bool_ty, signed, bits)
        }
        sem::ValueExprKind::BinOp { left, op, right } => {
            if let Some(binop) = map_binop(*op) {
                let lhs = lower_scalar_expr(builder, block, left, int_ty, bool_ty, signed, bits);
                let rhs = lower_scalar_expr(builder, block, right, int_ty, bool_ty, signed, bits);
                return builder.binop(block, binop, lhs, rhs, int_ty);
            }
            if let Some(cmp) = map_cmp(*op) {
                let lhs = lower_scalar_expr(builder, block, left, int_ty, bool_ty, signed, bits);
                let rhs = lower_scalar_expr(builder, block, right, int_ty, bool_ty, signed, bits);
                return builder.cmp(block, cmp, lhs, rhs, bool_ty);
            }
            panic!("ssa lower_const_return unsupported binary op");
        }
        sem::ValueExprKind::UnaryOp { op, expr } => {
            let value = lower_scalar_expr(builder, block, expr, int_ty, bool_ty, signed, bits);
            match op {
                UnaryOp::Neg => builder.unop(block, UnOp::Neg, value, int_ty),
                UnaryOp::LogicalNot => builder.unop(block, UnOp::Not, value, bool_ty),
                UnaryOp::BitNot => builder.unop(block, UnOp::BitNot, value, int_ty),
            }
        }
        _ => panic!("ssa lower_const_return requires a scalar expression body"),
    }
}

/// Maps Machina binary ops to SSA arithmetic and bitwise ops.
fn map_binop(op: BinaryOp) -> Option<BinOp> {
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

/// Maps Machina comparison ops to SSA comparison ops.
fn map_cmp(op: BinaryOp) -> Option<CmpOp> {
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

#[cfg(test)]
#[path = "../../tests/ssa/t_lower.rs"]
mod tests;
