use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::lower::lower_util::u64_const;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::sir::model::Expr;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    pub(super) fn build_u8_slice(
        &mut self,
        ptr: Operand,
        len: Operand,
    ) -> Result<Place<Aggregate>, LowerError> {
        let slice_ty = Type::Slice {
            elem_ty: Box::new(Type::uint(8)),
        };
        let slice_ty_id = self.ty_lowerer.lower_ty(&slice_ty);
        let slice = self.new_temp_aggregate(slice_ty_id);

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let ptr_field = Place::new(
            slice.base(),
            u64_ty_id,
            vec![Projection::Field { index: 0 }],
        );
        let len_field = Place::new(
            slice.base(),
            u64_ty_id,
            vec![Projection::Field { index: 1 }],
        );

        self.emit_copy_scalar(ptr_field, Rvalue::Use(ptr));
        self.emit_copy_scalar(len_field, Rvalue::Use(len));
        Ok(slice)
    }

    pub(super) fn lower_string_index(
        &mut self,
        expr: &Expr,
        target: &Expr,
        indices: &[Expr],
    ) -> Result<Operand, LowerError> {
        if indices.len() != 1 {
            return Err(LowerError::UnsupportedOperandExpr(expr.id));
        }

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let u32_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));
        let u8_ty_id = self.ty_lowerer.lower_ty(&Type::uint(8));
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

        let target_ty = self.ty_for_node(target.id)?;
        let mut peeled_ty = target_ty.clone();
        let mut deref_count = 0usize;
        while let Type::Heap { elem_ty } = peeled_ty {
            deref_count += 1;
            peeled_ty = *elem_ty;
        }

        let target_place = self
            .lower_place(target)
            .or_else(|_| self.lower_agg_expr_to_temp(target).map(PlaceAny::Aggregate))
            .or_else(|_| {
                let ty_id = self.ty_lowerer.lower_ty(&target_ty);
                let temp = self.new_temp_scalar(ty_id);
                let op = self.lower_scalar_expr(target)?;
                self.emit_copy_scalar(temp.clone(), Rvalue::Use(op));
                Ok(PlaceAny::Scalar(temp))
            })?;

        let (base, mut base_projs) = match target_place {
            PlaceAny::Scalar(p) => (p.base(), p.projections().to_vec()),
            PlaceAny::Aggregate(p) => (p.base(), p.projections().to_vec()),
        };
        for _ in 0..deref_count {
            base_projs.push(Projection::Deref);
        }

        let mut ptr_proj = base_projs.clone();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_place = Place::new(base, u64_ty_id, ptr_proj);
        let ptr_op = Operand::Copy(ptr_place);

        let mut len_proj = base_projs;
        len_proj.push(Projection::Field { index: 1 });
        let len_place = Place::new(base, u32_ty_id, len_proj);
        let len_op = Operand::Copy(len_place);

        let index_expr = &indices[0];
        let index_op = self.lower_scalar_expr(index_expr)?;

        let cond_op = self.emit_scalar_rvalue(
            bool_ty_id,
            Rvalue::BinOp {
                op: BinOp::Lt,
                lhs: index_op.clone(),
                rhs: len_op.clone(),
            },
        );
        self.emit_runtime_check(
            cond_op,
            CheckKind::Bounds {
                index: index_op.clone(),
                len: len_op.clone(),
            },
        );

        let ptr_at = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Add,
                lhs: ptr_op,
                rhs: index_op,
            },
        );

        // Copy the byte into a stack buffer since MCIR cannot load from raw pointers.
        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::uint(8)),
            dims: vec![1],
        };
        let buf_ty_id = self.ty_lowerer.lower_ty(&buf_ty);
        let buf = self.new_temp_aggregate(buf_ty_id);

        let buf_ptr_place = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(
            buf_ptr_place.clone(),
            Rvalue::AddrOf(PlaceAny::Aggregate(buf.clone())),
        );
        let buf_ptr_op = Operand::Copy(buf_ptr_place);

        let dst_slice = self.build_u8_slice(buf_ptr_op, u64_const(1))?;
        let src_slice = self.build_u8_slice(ptr_at, u64_const(1))?;

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::MemCopy),
                args: vec![
                    PlaceAny::Aggregate(dst_slice),
                    PlaceAny::Aggregate(src_slice),
                ],
            },
        );

        let elem_place = Place::new(
            buf.base(),
            u8_ty_id,
            vec![Projection::Index {
                index: u64_const(0),
            }],
        );
        Ok(Operand::Copy(elem_place))
    }

    pub(super) fn lower_string_slice_into(
        &mut self,
        dst: &Place<Aggregate>,
        target: &Expr,
        start: &Option<Box<Expr>>,
        end: &Option<Box<Expr>>,
    ) -> Result<(), LowerError> {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let u32_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));

        let target_place = self
            .lower_place_agg(target)
            .or_else(|_| self.lower_agg_expr_to_temp(target))?;

        let mut ptr_proj = target_place.projections().to_vec();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_place = Place::new(target_place.base(), u64_ty_id, ptr_proj);
        let ptr_op = Operand::Copy(ptr_place);

        let mut len_proj = target_place.projections().to_vec();
        len_proj.push(Projection::Field { index: 1 });
        let len_place = Place::new(target_place.base(), u32_ty_id, len_proj);
        let len_u32_op = Operand::Copy(len_place);

        let start_op = match start {
            Some(expr) => self.lower_scalar_expr(expr)?,
            None => u64_const(0),
        };

        let end_op = match end {
            Some(expr) => self.lower_scalar_expr(expr)?,
            None => self.emit_scalar_rvalue(u64_ty_id, Rvalue::Use(len_u32_op)),
        };

        let ptr_op = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Add,
                lhs: ptr_op,
                rhs: start_op.clone(),
            },
        );

        let len_op = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Sub,
                lhs: end_op,
                rhs: start_op,
            },
        );

        let mut ptr_proj = dst.projections().to_vec();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_field = Place::new(dst.base(), u64_ty_id, ptr_proj);

        let mut len_proj = dst.projections().to_vec();
        len_proj.push(Projection::Field { index: 1 });
        let len_field = Place::new(dst.base(), u64_ty_id, len_proj);

        self.emit_copy_scalar(ptr_field, Rvalue::Use(ptr_op));
        self.emit_copy_scalar(len_field, Rvalue::Use(len_op));

        Ok(())
    }
}
