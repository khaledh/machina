use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::lower::lower_util::u64_const;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::tree::semantic::{PlaceExpr, ValueExpr};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_string_index(
        &mut self,
        place: &PlaceExpr,
        target: &PlaceExpr,
        indices: &[ValueExpr],
        deref_count: usize,
    ) -> Result<Operand, LowerError> {
        if indices.len() != 1 {
            return Err(LowerError::UnsupportedOperandExpr(place.id));
        }

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let u32_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));
        let u8_ty_id = self.ty_lowerer.lower_ty(&Type::uint(8));
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

        let target_place = self.lower_place(target)?;

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

        let dst_ptr_arg = self.runtime_arg_place(buf_ptr_op);
        let src_ptr_arg = self.runtime_arg_place(ptr_at);
        let len_arg = self.runtime_arg_place(u64_const(1));

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::MemCopy),
                args: vec![dst_ptr_arg, src_ptr_arg, len_arg],
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
}
