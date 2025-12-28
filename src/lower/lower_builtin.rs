use crate::ast::{Expr, ExprKind};
use crate::lower::{errors::LowerError, lower_ast::FuncLowerer};
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::type_rel::{PrintArgKind, print_arg_kind};
use crate::types::Type;

const BUF_SIZE: usize = 20;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_builtin_call(
        &mut self,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<bool, LowerError> {
        let ExprKind::Var(name) = &callee.kind else {
            return Ok(false);
        };
        match name.as_str() {
            "print" => {
                let arg = args
                    .get(0)
                    .unwrap_or_else(|| unreachable!("compiler bug: print takes 1 argument"));
                self.lower_print_arg(arg, false /* newline */)?;
                Ok(true)
            }
            "println" => {
                match args.len() {
                    0 => {
                        let empty = self.emit_empty_string();
                        self.emit_print_call(PlaceAny::Aggregate(empty), true /* newline */)?;
                    }
                    1 => {
                        self.lower_print_arg(&args[0], true /* newline */)?;
                    }
                    _ => unreachable!("compiler bug: println takes 0 or 1 argument"),
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn lower_print_arg(&mut self, arg: &Expr, newline: bool) -> Result<(), LowerError> {
        let arg_ty = self.ty_for_node(arg.id)?;
        match print_arg_kind(&arg_ty) {
            Some(PrintArgKind::UInt64Like) => {
                self.lower_print_u64(arg, newline)?;
            }
            Some(PrintArgKind::String) => {
                let string_place = self.lower_call_arg_place(arg)?;
                self.emit_print_call(string_place, newline)?;
            }
            None => {
                unreachable!("compiler bug: invalid print arg type {:?}", arg_ty);
            }
        }
        Ok(())
    }

    fn emit_empty_string(&mut self) -> Place<Aggregate> {
        let string_ty_id = self.ty_lowerer.lower_ty(&Type::String);
        let string_place = self.new_temp_aggregate(string_ty_id);

        let ptr_ty_id = self.ty_lowerer.lower_ty(&Type::UInt64);
        let len_ty_id = self.ty_lowerer.lower_ty(&Type::UInt32);
        let tag_ty_id = self.ty_lowerer.lower_ty(&Type::UInt8);

        self.emit_operand_into_agg_projection(
            &string_place,
            Projection::Field { index: 0 },
            Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value: 0,
            }),
            ptr_ty_id,
        )
        .expect("compiler bug: failed to write empty string ptr");
        self.emit_operand_into_agg_projection(
            &string_place,
            Projection::Field { index: 1 },
            Operand::Const(Const::Int {
                signed: false,
                bits: 32,
                value: 0,
            }),
            len_ty_id,
        )
        .expect("compiler bug: failed to write empty string len");
        self.emit_operand_into_agg_projection(
            &string_place,
            Projection::Field { index: 2 },
            Operand::Const(Const::Int {
                signed: false,
                bits: 8,
                value: 0,
            }),
            tag_ty_id,
        )
        .expect("compiler bug: failed to write empty string tag");

        string_place
    }

    fn emit_print_call(&mut self, string_place: PlaceAny, newline: bool) -> Result<(), LowerError> {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::UInt64);
        let newline_place = self.new_temp_scalar(u64_ty_id);
        let newline_value = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: if newline { 1 } else { 0 },
        });
        self.emit_copy_scalar(newline_place.clone(), Rvalue::Use(newline_value));
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::Print),
                args: vec![string_place, PlaceAny::Scalar(newline_place)],
            },
        );
        Ok(())
    }

    fn lower_print_u64(&mut self, arg: &Expr, newline: bool) -> Result<(), LowerError> {
        // 1) buffer: u8[20]
        let buf_ty = Type::Array {
            elem_ty: Box::new(Type::UInt8),
            dims: vec![BUF_SIZE],
        };
        let buf_ty_id = self.ty_lowerer.lower_ty(&buf_ty);
        let buf_place = self.new_temp_aggregate(buf_ty_id);

        // 2) slice: u8[] { ptr, len }
        let slice_ty = Type::Slice {
            elem_ty: Box::new(Type::UInt8),
        };
        let slice_ty_id = self.ty_lowerer.lower_ty(&slice_ty);
        let slice_place = self.new_temp_aggregate(slice_ty_id);
        let len_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: BUF_SIZE as i128,
        });
        self.emit_slice_value(
            slice_place.clone(),
            PlaceAny::Aggregate(buf_place.clone()),
            len_op,
        );

        // 3) call __mc_u64_to_dec(&slice, value) -> len
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::UInt64);
        let len_place = self.new_temp_scalar(u64_ty_id);
        let value_place = self.lower_call_arg_place(arg)?;
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: Some(PlaceAny::Scalar(len_place.clone())),
                callee: Callee::Runtime(RuntimeFn::U64ToDec),
                args: vec![PlaceAny::Aggregate(slice_place.clone()), value_place],
            },
        );

        // 4) build string { ptr, len, tag=0 }
        let string_ty_id = self.ty_lowerer.lower_ty(&Type::String);
        let string_place = self.new_temp_aggregate(string_ty_id);

        // ptr field
        let ptr_field = Place::new(
            string_place.base(),
            u64_ty_id,
            vec![Projection::Field { index: 0 }],
        );
        self.emit_copy_scalar(
            ptr_field,
            Rvalue::AddrOf(PlaceAny::Aggregate(buf_place.clone())),
        );

        // len field
        let u32_ty_id = self.ty_lowerer.lower_ty(&Type::UInt32);
        let len_field = Place::new(
            string_place.base(),
            u32_ty_id,
            vec![Projection::Field { index: 1 }],
        );
        self.emit_copy_scalar(len_field, Rvalue::Use(Operand::Copy(len_place)));

        // tag field = 0 (ascii)
        let u8_ty_id = self.ty_lowerer.lower_ty(&Type::UInt8);
        let tag_field = Place::new(
            string_place.base(),
            u8_ty_id,
            vec![Projection::Field { index: 2 }],
        );
        self.emit_copy_scalar(
            tag_field,
            Rvalue::Use(Operand::Const(Const::Int {
                signed: false,
                bits: 8,
                value: 0,
            })),
        );

        // 5) call __mc_print(&string, newline)
        self.emit_print_call(PlaceAny::Aggregate(string_place.clone()), newline)?;

        Ok(())
    }
}
