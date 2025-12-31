use crate::ast::{self, NodeId};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::resolve::def_map::Def;
use crate::types::{Type, TypeAssignability, type_assignable};

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
            ast::BinaryOp::LogicalAnd | ast::BinaryOp::LogicalOr => {
                unreachable!("compiler bug: logical ops lowered via short-circuiting lowering")
            }
        }
    }

    /// Map AST unary op to MCIR unary op.
    pub(super) fn map_unop(op: ast::UnaryOp) -> UnOp {
        match op {
            ast::UnaryOp::Neg => UnOp::Neg,
            ast::UnaryOp::LogicalNot => {
                unreachable!("compiler bug: logical not lowered via comparison")
            }
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

    /// Lookup the AST type for a definition bound to a node id.
    pub(super) fn def_ty_for_node(&self, node_id: NodeId) -> Result<Type, LowerError> {
        let def = self.def_for_node(node_id)?;
        self.ctx
            .type_map
            .lookup_def_type(def)
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

    pub(super) fn emit_conversion_check(
        &mut self,
        from_ty: &Type,
        to_ty: &Type,
        value_op: &Operand,
    ) {
        if let TypeAssignability::UInt64ToRange { min, max } = type_assignable(from_ty, to_ty) {
            self.emit_range_conversion_check(value_op, min, max);
        }
    }

    pub(super) fn emit_range_conversion_check(&mut self, value_op: &Operand, min: u64, max: u64) {
        let min_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: min as i128,
        });
        let max_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: max as i128,
        });
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

        let ge_min = self.emit_scalar_rvalue(
            bool_ty_id,
            Rvalue::BinOp {
                op: BinOp::GtEq,
                lhs: value_op.clone(),
                rhs: min_op.clone(),
            },
        );
        self.emit_runtime_check(
            ge_min,
            CheckKind::Range {
                value: value_op.clone(),
                min: min_op.clone(),
                max: max_op.clone(),
            },
        );

        let lt_max = self.emit_scalar_rvalue(
            bool_ty_id,
            Rvalue::BinOp {
                op: BinOp::Lt,
                lhs: value_op.clone(),
                rhs: max_op.clone(),
            },
        );
        self.emit_runtime_check(
            lt_max,
            CheckKind::Range {
                value: value_op.clone(),
                min: min_op,
                max: max_op,
            },
        );
    }

    pub(super) fn emit_runtime_check(&mut self, cond: Operand, kind: CheckKind) {
        // Create blocks
        let ok_bb = self.fb.new_block();
        let fail_bb = self.fb.new_block();

        // Add a conditional branch to the ok or fail block
        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond,
                then_bb: ok_bb,
                else_bb: fail_bb,
            },
        );

        // Emit the runtime trap call and terminate the fail block.
        self.curr_block = fail_bb;
        self.emit_runtime_trap_call(kind);
        self.fb
            .set_terminator(self.curr_block, Terminator::Unreachable);

        // Continue with the ok block
        self.curr_block = ok_bb;
    }

    // --- Runtime Calls ---

    /// Convert a runtime argument operand to a u64-sized place.
    pub(super) fn runtime_arg_place(&mut self, op: Operand) -> PlaceAny {
        match op {
            Operand::Copy(place) | Operand::Move(place) => PlaceAny::Scalar(place),
            Operand::Const(_) => {
                let ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
                let temp = self.new_temp_scalar(ty_id);
                self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
                PlaceAny::Scalar(temp)
            }
        }
    }

    fn emit_runtime_trap_call(&mut self, kind: CheckKind) {
        let zero_op = u64_const(0);

        self.fb
            .push_comment(self.curr_block, format!("runtime check: {}", kind));

        let (kind_op, arg0, arg1, arg2) = match kind.clone() {
            CheckKind::DivByZero => (u64_const(0), zero_op.clone(), zero_op.clone(), zero_op),
            CheckKind::Bounds { index, len } => (u64_const(1), index, len, zero_op),
            CheckKind::Range { value, min, max } => (u64_const(2), value, min, max),
        };

        let args = vec![
            self.runtime_arg_place(kind_op),
            self.runtime_arg_place(arg0),
            self.runtime_arg_place(arg1),
            self.runtime_arg_place(arg2),
        ];

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::Trap),
                args,
            },
        );
    }
}

#[inline]
pub(super) fn u64_const(value: u64) -> Operand {
    Operand::Const(Const::Int {
        signed: false,
        bits: 64,
        value: value as i128,
    })
}
