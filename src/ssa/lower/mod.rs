//! Minimal SSA lowering for early smoke tests.
//!
//! Lowers a tiny subset of the semantic tree into SSA for validation of the
//! IR model and formatter.

use std::collections::HashMap;

use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{
    BinOp, BlockId, CmpOp, Function, FunctionSig, Terminator, TypeId, TypeKind, TypeTable, UnOp,
    ValueId,
};
use crate::tree::semantic as sem;
use crate::tree::{BinaryOp, UnaryOp};
use crate::typeck::type_map::TypeMap;
use crate::types::{Type, TypeId as TcTypeId};

pub struct LoweredFunction {
    pub func: Function,
    pub types: TypeTable,
}

struct LoweredValue {
    value: ValueId,
    block: BlockId,
}

struct LowerCtx<'a> {
    type_map: &'a TypeMap,
    types: TypeTable,
    type_cache: HashMap<TcTypeId, TypeId>,
}

impl<'a> LowerCtx<'a> {
    fn new(type_map: &'a TypeMap) -> Self {
        Self {
            type_map,
            types: TypeTable::new(),
            type_cache: HashMap::new(),
        }
    }

    fn ssa_type_for_expr(&mut self, expr: &sem::ValueExpr) -> TypeId {
        self.ssa_type_for_type_id(expr.ty)
    }

    fn ssa_type_for_type_id(&mut self, ty_id: TcTypeId) -> TypeId {
        if let Some(id) = self.type_cache.get(&ty_id) {
            return *id;
        }

        let ty = self.type_map.type_table().get(ty_id);
        let id = match ty {
            Type::Unit => self.types.add(TypeKind::Unit),
            Type::Bool => self.types.add(TypeKind::Bool),
            Type::Int { signed, bits } => self.types.add(TypeKind::Int {
                signed: *signed,
                bits: *bits,
            }),
            other => panic!("ssa lower_func unsupported type {:?}", other),
        };

        self.type_cache.insert(ty_id, id);
        id
    }

    fn int_info_for_expr(&self, expr: &sem::ValueExpr) -> (bool, u8) {
        match self.type_map.type_table().get(expr.ty) {
            Type::Int { signed, bits } => (*signed, *bits),
            other => panic!("ssa lower_func expected int type, found {:?}", other),
        }
    }
}

struct FuncLowerer<'a> {
    ctx: LowerCtx<'a>,
    builder: FunctionBuilder,
}

impl<'a> FuncLowerer<'a> {
    fn new(func: &sem::FuncDef, type_map: &'a TypeMap) -> Self {
        let mut ctx = LowerCtx::new(type_map);
        let ret_id = ctx.ssa_type_for_type_id(func.body.ty);
        let sig = FunctionSig {
            params: Vec::new(),
            ret: ret_id,
        };
        let builder = FunctionBuilder::new(func.def_id, func.sig.name.clone(), sig);
        Self { ctx, builder }
    }

    fn finish(self) -> LoweredFunction {
        LoweredFunction {
            func: self.builder.finish(),
            types: self.ctx.types,
        }
    }

    fn lower_expr(&mut self, block: BlockId, expr: &sem::ValueExpr) -> LoweredValue {
        match &expr.kind {
            sem::ValueExprKind::IntLit(value) => {
                let ty = self.ctx.ssa_type_for_expr(expr);
                let (signed, bits) = self.ctx.int_info_for_expr(expr);
                let value = self
                    .builder
                    .const_int(block, *value as i128, signed, bits, ty);
                LoweredValue { value, block }
            }
            sem::ValueExprKind::BoolLit(value) => {
                let ty = self.ctx.ssa_type_for_expr(expr);
                let value = self.builder.const_bool(block, *value, ty);
                LoweredValue { value, block }
            }
            sem::ValueExprKind::Block { items, tail } if items.is_empty() => {
                let tail = tail
                    .as_ref()
                    .unwrap_or_else(|| panic!("ssa lower_func missing block tail"));
                self.lower_expr(block, tail)
            }
            sem::ValueExprKind::BinOp { left, op, right } => {
                if let Some(binop) = map_binop(*op) {
                    let lhs = self.lower_pure_expr(block, left, "binary ops");
                    let rhs = self.lower_pure_expr(block, right, "binary ops");
                    let ty = self.ctx.ssa_type_for_expr(expr);
                    let value = self.builder.binop(block, binop, lhs, rhs, ty);
                    return LoweredValue { value, block };
                }
                if let Some(cmp) = map_cmp(*op) {
                    let lhs = self.lower_pure_expr(block, left, "comparisons");
                    let rhs = self.lower_pure_expr(block, right, "comparisons");
                    let ty = self.ctx.ssa_type_for_expr(expr);
                    let value = self.builder.cmp(block, cmp, lhs, rhs, ty);
                    return LoweredValue { value, block };
                }
                panic!("ssa lower_func unsupported binary op");
            }
            sem::ValueExprKind::UnaryOp { op, expr } => {
                let value = self.lower_pure_expr(block, expr, "unary ops");
                match op {
                    UnaryOp::Neg => {
                        let ty = self.ctx.ssa_type_for_expr(expr);
                        let value = self.builder.unop(block, UnOp::Neg, value, ty);
                        LoweredValue { value, block }
                    }
                    UnaryOp::LogicalNot => {
                        let ty = self.ctx.ssa_type_for_expr(expr);
                        let value = self.builder.unop(block, UnOp::Not, value, ty);
                        LoweredValue { value, block }
                    }
                    UnaryOp::BitNot => {
                        let ty = self.ctx.ssa_type_for_expr(expr);
                        let value = self.builder.unop(block, UnOp::BitNot, value, ty);
                        LoweredValue { value, block }
                    }
                }
            }
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_value = self.lower_pure_expr(block, cond, "if conditions");

                let then_bb = self.builder.add_block();
                let else_bb = self.builder.add_block();
                let join_bb = self.builder.add_block();
                let join_ty = self.ctx.ssa_type_for_expr(expr);
                let join_value = self.builder.add_block_param(join_bb, join_ty);

                self.builder.set_terminator(
                    block,
                    Terminator::CondBr {
                        cond: cond_value,
                        then_bb,
                        then_args: Vec::new(),
                        else_bb,
                        else_args: Vec::new(),
                    },
                );

                let then_value = self.lower_expr(then_bb, then_body);
                self.builder.set_terminator(
                    then_bb,
                    Terminator::Br {
                        target: join_bb,
                        args: vec![then_value.value],
                    },
                );

                let else_value = self.lower_expr(else_bb, else_body);
                self.builder.set_terminator(
                    else_bb,
                    Terminator::Br {
                        target: join_bb,
                        args: vec![else_value.value],
                    },
                );

                LoweredValue {
                    value: join_value,
                    block: join_bb,
                }
            }
            _ => panic!("ssa lower_func requires a scalar expression body"),
        }
    }

    fn lower_pure_expr(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
        context: &'static str,
    ) -> ValueId {
        let lowered = self.lower_expr(block, expr);
        if lowered.block != block {
            panic!("ssa lower_func does not support control flow in {context}");
        }
        lowered.value
    }
}

/// Lowers a trivial function body into SSA for smoke testing.
pub fn lower_func(func: &sem::FuncDef, type_map: &TypeMap) -> LoweredFunction {
    if !func.sig.params.is_empty() {
        panic!("ssa lower_func only supports functions without params");
    }

    let mut lowerer = FuncLowerer::new(func, type_map);
    let entry = lowerer.builder.add_block();
    let result = lowerer.lower_expr(entry, &func.body);
    lowerer.builder.set_terminator(
        result.block,
        Terminator::Return {
            value: Some(result.value),
        },
    );

    lowerer.finish()
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
