//! Minimal SSA lowering for early smoke tests.
//!
//! Lowers a tiny subset of the semantic tree into SSA for validation of the
//! IR model and formatter.

use std::collections::HashMap;

use crate::diag::Span;
mod linearize;

use crate::resolve::DefId;
use crate::ssa::lower::linearize::{linearize_block_item, linearize_expr};
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

type LinearValue = ValueId;

#[derive(Clone, Copy, PartialEq, Eq)]
struct LocalValue {
    value: ValueId,
    ty: TypeId,
}

struct BranchingValue {
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

    fn int_info_for_type_id(&self, ty_id: TcTypeId) -> (bool, u8) {
        match self.type_map.type_table().get(ty_id) {
            Type::Int { signed, bits } => (*signed, *bits),
            other => panic!("ssa lower_func expected int type, found {:?}", other),
        }
    }
}

struct FuncLowerer<'a> {
    ctx: LowerCtx<'a>,
    builder: FunctionBuilder,
    locals: HashMap<DefId, LocalValue>,
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
        Self {
            ctx,
            builder,
            locals: HashMap::new(),
        }
    }

    fn finish(self) -> LoweredFunction {
        LoweredFunction {
            func: self.builder.finish(),
            types: self.ctx.types,
        }
    }

    fn lower_branching_expr(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
    ) -> Result<BranchingValue, sem::LinearizeError> {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                let mut cur_block = block;
                for item in items {
                    if let sem::BlockItem::Stmt(stmt) = item {
                        if let sem::StmtExprKind::While { cond, body } = &stmt.kind {
                            cur_block = self.lower_while_stmt(cur_block, cond, body)?;
                            continue;
                        }
                    }
                    // Linearize block items so we can guarantee straight-line lowering.
                    let item = linearize_block_item(item)?;
                    match item {
                        sem::LinearBlockItem::Stmt(stmt) => {
                            cur_block = self.lower_linear_stmt(cur_block, &stmt)?;
                        }
                        sem::LinearBlockItem::Expr(expr) => {
                            // Drop the value; we only care about side effects in statements.
                            let _ = self.lower_linear_expr(cur_block, &expr)?;
                        }
                    }
                }
                if let Some(tail) = tail {
                    return self.lower_branching_expr(cur_block, tail);
                }
                // Empty blocks produce unit.
                let ty = self.ctx.ssa_type_for_expr(expr);
                let value = self.builder.const_unit(cur_block, ty);
                return Ok(BranchingValue {
                    value,
                    block: cur_block,
                });
            }
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_value = self.lower_linear_expr_value(block, cond)?;

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

                // For now, require that locals don't change across branches.
                let saved_locals = self.locals.clone();
                let then_value = self.lower_branching_expr(then_bb, then_body)?;
                if self.locals != saved_locals {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }
                self.locals = saved_locals.clone();
                self.builder.set_terminator(
                    then_bb,
                    Terminator::Br {
                        target: join_bb,
                        args: vec![then_value.value],
                    },
                );

                let else_value = self.lower_branching_expr(else_bb, else_body)?;
                if self.locals != saved_locals {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }
                self.locals = saved_locals;
                self.builder.set_terminator(
                    else_bb,
                    Terminator::Br {
                        target: join_bb,
                        args: vec![else_value.value],
                    },
                );

                Ok(BranchingValue {
                    value: join_value,
                    block: join_bb,
                })
            }
            _ => {
                // Fall back to the linear subset for everything else.
                let linear = linearize_expr(expr)?;
                let value = self.lower_linear_expr(block, &linear)?;
                Ok(BranchingValue { value, block })
            }
        }
    }

    fn lower_linear_expr(
        &mut self,
        block: BlockId,
        expr: &sem::LinearExpr,
    ) -> Result<LinearValue, sem::LinearizeError> {
        match &expr.kind {
            sem::LinearExprKind::Block { items, tail } => {
                let mut cur_block = block;
                for item in items {
                    match item {
                        sem::LinearBlockItem::Stmt(stmt) => {
                            cur_block = self.lower_linear_stmt(cur_block, stmt)?;
                        }
                        sem::LinearBlockItem::Expr(expr) => {
                            let _ = self.lower_linear_expr(cur_block, expr)?;
                        }
                    }
                }
                if let Some(tail) = tail {
                    return self.lower_linear_expr(cur_block, tail);
                }
                // Linear blocks without a tail evaluate to unit.
                let value = self
                    .builder
                    .const_unit(cur_block, self.ctx.ssa_type_for_type_id(expr.ty));
                Ok(value)
            }

            sem::LinearExprKind::UnitLit => Ok(self
                .builder
                .const_unit(block, self.ctx.ssa_type_for_type_id(expr.ty))),

            sem::LinearExprKind::IntLit(value) => {
                let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                let (signed, bits) = self.ctx.int_info_for_type_id(expr.ty);
                Ok(self
                    .builder
                    .const_int(block, *value as i128, signed, bits, ty))
            }

            sem::LinearExprKind::BoolLit(value) => {
                Ok(self
                    .builder
                    .const_bool(block, *value, self.ctx.ssa_type_for_type_id(expr.ty)))
            }

            sem::LinearExprKind::UnaryOp { op, expr } => {
                let value = self.lower_linear_expr(block, expr)?;
                let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                let value = match op {
                    UnaryOp::Neg => self.builder.unop(block, UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(block, UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(block, UnOp::BitNot, value, ty),
                };
                Ok(value)
            }

            sem::LinearExprKind::BinOp { left, op, right } => {
                if let Some(binop) = map_binop(*op) {
                    let lhs = self.lower_linear_expr(block, left)?;
                    let rhs = self.lower_linear_expr(block, right)?;
                    let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                    return Ok(self.builder.binop(block, binop, lhs, rhs, ty));
                }
                if let Some(cmp) = map_cmp(*op) {
                    let lhs = self.lower_linear_expr(block, left)?;
                    let rhs = self.lower_linear_expr(block, right)?;
                    let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                    return Ok(self.builder.cmp(block, cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }

            sem::LinearExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => Ok(self.lookup_local(*def_id).value),
                _ => Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)),
            },

            sem::LinearExprKind::CharLit(_)
            | sem::LinearExprKind::Call { .. }
            | sem::LinearExprKind::ClosureRef { .. } => {
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }
        }
    }

    fn lower_linear_expr_value(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, sem::LinearizeError> {
        let linear = linearize_expr(expr)?;
        self.lower_linear_expr(block, &linear)
    }

    fn lower_linear_stmt(
        &mut self,
        block: BlockId,
        stmt: &sem::LinearStmt,
    ) -> Result<BlockId, sem::LinearizeError> {
        match &stmt.kind {
            sem::LinearStmtKind::LetBind { pattern, value, .. }
            | sem::LinearStmtKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = self.lower_linear_expr(block, value_expr)?;
                let ty = self.ctx.ssa_type_for_type_id(value_expr.ty);
                self.bind_pattern(pattern, LocalValue { value, ty })?;
                Ok(block)
            }

            sem::LinearStmtKind::Assign { assignee, value } => {
                let value_expr = value;
                let value = self.lower_linear_expr(block, value_expr)?;
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        // Assignments currently model locals as SSA values.
                        let ty = self.ctx.ssa_type_for_type_id(value_expr.ty);
                        self.locals.insert(*def_id, LocalValue { value, ty });
                        Ok(block)
                    }
                    _ => Err(self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt)),
                }
            }

            sem::LinearStmtKind::VarDecl { .. } | sem::LinearStmtKind::Return { .. } => {
                Err(self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt))
            }
        }
    }

    fn lower_while_stmt(
        &mut self,
        block: BlockId,
        cond: &sem::ValueExpr,
        body: &sem::ValueExpr,
    ) -> Result<BlockId, sem::LinearizeError> {
        let locals_snapshot = self.ordered_locals();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let tys: Vec<TypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();
        let args: Vec<ValueId> = locals_snapshot
            .iter()
            .map(|(_, local)| local.value)
            .collect();

        let header_bb = self.builder.add_block();
        let body_bb = self.builder.add_block();
        let exit_bb = self.builder.add_block();
        let header_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(header_bb, *ty))
            .collect();
        let exit_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(exit_bb, *ty))
            .collect();

        self.builder.set_terminator(
            block,
            Terminator::Br {
                target: header_bb,
                args,
            },
        );

        self.set_locals_from_params(&defs, &tys, &header_params);
        let cond_value = self.lower_linear_expr_value(header_bb, cond)?;
        let exit_args = self.locals_args(&defs, cond.span)?;

        self.builder.set_terminator(
            header_bb,
            Terminator::CondBr {
                cond: cond_value,
                then_bb: body_bb,
                then_args: Vec::new(),
                else_bb: exit_bb,
                else_args: exit_args,
            },
        );

        let _body_value = self.lower_branching_expr(body_bb, body)?;
        let loop_args = self.locals_args(&defs, body.span)?;
        self.builder.set_terminator(
            body_bb,
            Terminator::Br {
                target: header_bb,
                args: loop_args,
            },
        );

        self.set_locals_from_params(&defs, &tys, &exit_params);
        Ok(exit_bb)
    }

    fn bind_pattern(
        &mut self,
        pattern: &sem::BindPattern,
        value: LocalValue,
    ) -> Result<(), sem::LinearizeError> {
        match &pattern.kind {
            sem::BindPatternKind::Name { def_id, .. } => {
                self.locals.insert(*def_id, value);
                Ok(())
            }
            _ => Err(sem::LinearizeError {
                kind: sem::LinearizeErrorKind::UnsupportedStmt,
                span: pattern.span,
            }),
        }
    }

    fn err_span(&self, span: Span, kind: sem::LinearizeErrorKind) -> sem::LinearizeError {
        sem::LinearizeError { kind, span }
    }

    fn err_stmt(
        &self,
        stmt: &sem::LinearStmt,
        kind: sem::LinearizeErrorKind,
    ) -> sem::LinearizeError {
        sem::LinearizeError {
            kind,
            span: stmt.span,
        }
    }

    fn ordered_locals(&self) -> Vec<(DefId, LocalValue)> {
        let mut locals: Vec<_> = self
            .locals
            .iter()
            .map(|(def, local)| (*def, *local))
            .collect();
        locals.sort_by_key(|(def, _)| def.0);
        locals
    }

    fn locals_args(&self, defs: &[DefId], span: Span) -> Result<Vec<ValueId>, sem::LinearizeError> {
        if self.locals.len() != defs.len() {
            return Err(self.err_span(span, sem::LinearizeErrorKind::UnsupportedExpr));
        }
        let mut args = Vec::with_capacity(defs.len());
        for def in defs {
            let Some(local) = self.locals.get(def) else {
                return Err(self.err_span(span, sem::LinearizeErrorKind::UnsupportedExpr));
            };
            args.push(local.value);
        }
        Ok(args)
    }

    fn set_locals_from_params(&mut self, defs: &[DefId], tys: &[TypeId], params: &[ValueId]) {
        self.locals.clear();
        for ((def, ty), value) in defs.iter().zip(tys.iter()).zip(params.iter()) {
            self.locals.insert(
                *def,
                LocalValue {
                    value: *value,
                    ty: *ty,
                },
            );
        }
    }

    fn lookup_local(&self, def_id: DefId) -> LocalValue {
        *self
            .locals
            .get(&def_id)
            .unwrap_or_else(|| panic!("ssa lower_func missing local {:?}", def_id))
    }
}

pub fn lower_func(
    func: &sem::FuncDef,
    type_map: &TypeMap,
) -> Result<LoweredFunction, sem::LinearizeError> {
    if !func.sig.params.is_empty() {
        panic!("ssa lower_func only supports functions without params");
    }

    let mut lowerer = FuncLowerer::new(func, type_map);
    let entry = lowerer.builder.add_block();
    let result = lowerer.lower_branching_expr(entry, &func.body)?;
    lowerer.builder.set_terminator(
        result.block,
        Terminator::Return {
            value: Some(result.value),
        },
    );

    Ok(lowerer.finish())
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
