use crate::ast::{self, NodeId};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::mcir::types::*;
use crate::resolve::def_map::Def;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    /// Map AST binary op to MCIR binary op.
    pub(super) fn map_binop(op: ast::BinaryOp) -> BinOp {
        match op {
            ast::BinaryOp::Add => BinOp::Add,
            ast::BinaryOp::Sub => BinOp::Sub,
            ast::BinaryOp::Mul => BinOp::Mul,
            ast::BinaryOp::Div => BinOp::Div,
            ast::BinaryOp::Eq => BinOp::Eq,
            ast::BinaryOp::Ne => BinOp::Ne,
            ast::BinaryOp::Lt => BinOp::Lt,
            ast::BinaryOp::LtEq => BinOp::LtEq,
            ast::BinaryOp::Gt => BinOp::Gt,
            ast::BinaryOp::GtEq => BinOp::GtEq,
        }
    }

    /// Map AST unary op to MCIR unary op.
    pub(super) fn map_unop(op: ast::UnaryOp) -> UnOp {
        match op {
            ast::UnaryOp::Neg => UnOp::Neg,
        }
    }

    /// Lookup a definition for an AST node id.
    pub(super) fn def_for_node(&self, node_id: NodeId) -> Result<&Def, LowerError> {
        self.ctx
            .def_map
            .lookup_def(node_id)
            .ok_or(LowerError::ExprDefNotFound(node_id))
    }

    /// Lookup the AST type for a node id.
    pub(super) fn ty_for_node(&self, node_id: NodeId) -> Result<Type, LowerError> {
        self.ctx
            .type_map
            .lookup_node_type(node_id)
            .ok_or(LowerError::ExprTypeNotFound(node_id))
    }

    /// Check whether a lowered type is scalar.
    pub(super) fn is_scalar(&self, ty_id: TyId) -> bool {
        self.ty_lowerer.table.get(ty_id).is_scalar()
    }

    /// Check whether a lowered type is aggregate.
    pub(super) fn is_aggregate(&self, ty_id: TyId) -> bool {
        self.ty_lowerer.table.get(ty_id).is_aggregate()
    }

    // Emit a copy statement for a scalar.
    pub(super) fn emit_copy_scalar(&mut self, dst: Place<Scalar>, src: Rvalue) {
        self.fb
            .push_stmt(self.curr_block, Statement::CopyScalar { dst, src });
    }

    // Emit a copy statement for an aggregate.
    pub(super) fn emit_copy_aggregate(&mut self, dst: Place<Aggregate>, src: Place<Aggregate>) {
        self.fb
            .push_stmt(self.curr_block, Statement::CopyAggregate { dst, src });
    }

    /// Create a scalar temp place.
    pub(super) fn new_temp_scalar(&mut self, ty_id: TyId) -> Place<Scalar> {
        let temp_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
        Place::new(temp_id, ty_id, vec![])
    }

    /// Create an aggregate temp place.
    pub(super) fn new_temp_aggregate(&mut self, ty_id: TyId) -> Place<Aggregate> {
        let temp_id = self.fb.new_local(ty_id, LocalKind::Temp, None);
        Place::new(temp_id, ty_id, vec![])
    }
}
