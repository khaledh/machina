use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

use crate::ast::{
    BinaryOp, Expr, ExprKind, Function, Pattern, PatternKind, StructLitField, UnaryOp,
};
use crate::context::{AnalyzedContext, LoweredContext};
use crate::ids::{DefId, NodeId};
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrAddr, IrFunction, IrOperand, IrTempId, IrTerminator, IrType};
use crate::layout::{
    lower_type, struct_field_byte_offset, try_const_fold_array_linear_index,
    tuple_field_byte_offset,
};
use crate::types::Type;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("Parameter definition not found: Node {0}")]
    ParamDefNotFound(NodeId),

    #[error("Node type not found: Node {0}")]
    NodeTypeNotFound(NodeId),

    #[error("Variable definition not found: Node {0}")]
    VarDefNotFound(NodeId),

    #[error("Destination is not a temp: Node {0}, Operand {1:?}")]
    DestIsNotTemp(NodeId, IrOperand),

    #[error("Operand not found: Node {0}, Def {1}")]
    OperandNotFound(NodeId, DefId),

    #[error("Mismatched branch types: Node {0} type {1} != Node {2} type {3}")]
    MismatchedBranchTypes(NodeId, Type, NodeId, Type),

    #[error("Array is not a temp: Node {0}, Operand {1:?}")]
    ArrayIsNotTemp(NodeId, IrOperand),

    #[error("Index on non-array type: Node {0}, Type {1:?}")]
    IndexOnNonArray(NodeId, IrType),

    #[error("Unsupported assignee: Node {0}, Kind {1:?}")]
    UnsupportedAssignee(NodeId, ExprKind),

    #[error("Compound literal requires a destination temp: Node {0}")]
    CompoundLitRequiresDestTemp(NodeId),

    #[error("Tuple is not a temp: Node {0}, Operand {1:?}")]
    TupleIsNotTemp(NodeId, IrOperand),
}

//---------------- Place ----------------

#[derive(Debug)]
struct Place {
    base: IrTempId,
    byte_offset: usize,
    ty: IrType,
}

impl Place {
    fn root(base: IrTempId, ty: IrType) -> Place {
        Place {
            base,
            byte_offset: 0,
            ty,
        }
    }

    fn with_offset(p: &Place, offset: usize, ty: IrType) -> Place {
        Place {
            base: p.base,
            byte_offset: p.byte_offset + offset,
            ty,
        }
    }

    fn tuple_field(p: &Place, field_index: usize, field_ty: IrType) -> Place {
        let field_offset = tuple_field_byte_offset(&p.ty, field_index).0;
        Place::with_offset(p, field_offset, p.ty.tuple_field_type(field_index))
    }

    fn struct_field(p: &Place, field_name: &str, field_ty: IrType) -> Place {
        let field_offset = struct_field_byte_offset(&p.ty, field_name).0;
        Place::with_offset(p, field_offset, field_ty)
    }
}

impl fmt::Display for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Place {{ base: {}, byte_offset: {}, ty: {} }}",
            self.base, self.byte_offset, self.ty
        )
    }
}

// ---------------------------------------

pub struct FuncLowerer<'a> {
    ctx: &'a AnalyzedContext,
    func: &'a Function,
    fb: IrFunctionBuilder,
    def_op: HashMap<DefId, IrOperand>,
}

impl<'a> FuncLowerer<'a> {
    pub fn new(ctx: &'a AnalyzedContext, func: &'a Function) -> Self {
        let ret_ty = ctx
            .type_map
            .lookup_node_type(func.id)
            .unwrap_or_else(|| panic!("Function {} not found in type_map", func.name));

        let fb = IrFunctionBuilder::new(func.name.clone(), lower_type(&ret_ty));

        Self {
            ctx,
            func,
            fb,
            def_op: HashMap::new(),
        }
    }

    pub fn lower(mut self) -> Result<IrFunction, LowerError> {
        // lower params and store them in the def_temp map
        for (i, param) in self.func.params.iter().enumerate() {
            match self.ctx.def_map.lookup_def(param.id) {
                Some(def) => {
                    let param_ty =
                        self.ctx
                            .type_map
                            .lookup_node_type(param.id)
                            .unwrap_or_else(|| {
                                panic!("Parameter {} not found in type_map", param.name)
                            });

                    let param_temp =
                        self.fb
                            .new_param(i as u32, param.name.clone(), lower_type(&param_ty));

                    self.def_op.insert(def.id, IrOperand::Temp(param_temp));
                }
                None => return Err(LowerError::ParamDefNotFound(param.id)),
            }
        }

        // lower the body
        let ret_temp = self.fb.ret_temp();
        let ret_op = self.lower_expr(&self.func.body, ret_temp)?;

        match (ret_temp, ret_op) {
            (Some(ret_temp), IrOperand::Temp(src_temp)) if ret_temp == src_temp => {
                // Result already in the return temp, so no need to copy
                self.fb.terminate(IrTerminator::Ret { value: None });
            }
            (Some(ret_temp), IrOperand::Temp(src_temp)) => {
                // Result is in a different temp, so we need to copy it
                self.fb
                    .mem_copy(ret_temp, src_temp, self.fb.ret_type().size_of());
                self.fb.terminate(IrTerminator::Ret { value: None });
            }
            _ => self.fb.terminate(IrTerminator::Ret {
                value: Some(ret_op),
            }),
        }

        let ir_func = self.fb.finish();
        Ok(ir_func)
    }

    fn lower_expr(
        &mut self,
        expr: &Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        match &expr.kind {
            // Block
            ExprKind::Block(body) => self.lower_block(body, dest_temp),

            // Literals (scalars)
            ExprKind::UInt64Lit(value) => {
                let const_op = self.fb.new_const_int(*value as i64, 64, false);
                if let Some(dest_temp) = dest_temp {
                    self.fb.copy(dest_temp, const_op);
                }
                Ok(const_op)
            }
            ExprKind::BoolLit(value) => {
                let const_op = self.fb.new_const_bool(*value);
                if let Some(dest_temp) = dest_temp {
                    self.fb.copy(dest_temp, const_op);
                }
                Ok(const_op)
            }
            ExprKind::UnitLit => {
                let const_op = self.fb.new_const_unit();
                if let Some(dest_temp) = dest_temp {
                    self.fb.copy(dest_temp, const_op);
                }
                Ok(const_op)
            }

            // Literals (compound)
            ExprKind::ArrayLit(elems) => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_array_lit(expr, elems, dest_temp)
                } else {
                    Err(LowerError::CompoundLitRequiresDestTemp(expr.id))
                }
            }
            ExprKind::TupleLit(fields) => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_tuple_lit(expr, fields, dest_temp, 0)
                } else {
                    Err(LowerError::CompoundLitRequiresDestTemp(expr.id))
                }
            }
            ExprKind::StructLit { fields, .. } => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_struct_lit(expr, fields, dest_temp, 0)
                } else {
                    Err(LowerError::CompoundLitRequiresDestTemp(expr.id))
                }
            }

            // Operators
            ExprKind::BinOp { left, op, right } => self.lower_binary_op(op, left, right, dest_temp),
            ExprKind::UnaryOp { op, expr } => self.lower_unary_op(op, expr, dest_temp),

            // Bindings
            ExprKind::LetBind { pattern, value, .. } => {
                debug_assert!(dest_temp.is_none()); // let bindings should not have a dest_temp
                self.lower_binding(pattern, value, false)
            }
            ExprKind::VarBind { pattern, value, .. } => {
                debug_assert!(dest_temp.is_none()); // var bindings should not have a dest_temp
                self.lower_binding(pattern, value, true)
            }

            // Assignment
            ExprKind::Assign {
                value, assignee, ..
            } => {
                debug_assert!(dest_temp.is_none()); // assignments should not have a dest_temp
                self.lower_assign(expr, assignee, value)
            }

            // Variable, array index, tuple field, struct field
            ExprKind::Var(_) => self.lower_var(expr, dest_temp),
            ExprKind::ArrayIndex { target, indices } => {
                self.lower_array_index(expr, target, indices)
            }
            ExprKind::TupleField { target, index } => self.lower_tuple_field(target, *index),
            ExprKind::StructField { target, field } => self.lower_struct_field(target, field),

            // Control flow
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if(cond, then_body, else_body, dest_temp),
            ExprKind::While { cond, body } => {
                debug_assert!(dest_temp.is_none()); // while should not have a dest_temp
                self.lower_while(cond, body)
            }

            // Function call
            ExprKind::Call { callee, args } => match &callee.kind {
                ExprKind::Var(name) => self.lower_call(expr, name.clone(), args, dest_temp),
                _ => panic!("Unsupported callee: {:?}", callee.kind),
            },
        }
    }

    fn lower_block(
        &mut self,
        body: &[Expr],
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        // Note: dest_temp is used only for the last expression.

        // lower all but the last expression
        for expr in body.iter().take(body.len().saturating_sub(1)) {
            self.lower_expr(expr, None)?;
        }

        // lower the last expression
        match body.last() {
            Some(expr) => Ok(self.lower_expr(expr, dest_temp)?),
            None => Ok(self.fb.new_const_unit()),
        }
    }

    fn lower_binding(
        &mut self,
        pattern: &Pattern,
        value: &Expr,
        is_mutable: bool,
    ) -> Result<IrOperand, LowerError> {
        let value_ast_ty = self.get_node_type(value)?;
        let value_ir_ty = lower_type(&value_ast_ty);

        // For compound patterns (Tuple/Array) with a simple Var value,
        // we can use the existing variable's temp directly without copying.
        let can_use_value_directly = matches!(
            (&pattern.kind, &value.kind),
            (
                PatternKind::Tuple { .. } | PatternKind::Array { .. },
                ExprKind::Var(_)
            )
        ) && value_ir_ty.is_compound();

        // Check if this binding is NRVO-eligible
        let dest_temp = if can_use_value_directly {
            // Don't create a dest_temp - we'll use the value's temp directly
            None
        } else if value_ir_ty.is_compound() {
            // Check if the pattern is an Ident and if it's NRVO-eligible
            let is_nrvo_eligible = match &pattern.kind {
                PatternKind::Ident { .. } => {
                    if let Some(def) = self.ctx.def_map.lookup_def(pattern.id) {
                        def.nrvo_eligible
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if is_nrvo_eligible {
                self.fb.ret_temp() // Use the return temp for NRVO-eligible variables
            } else {
                Some(self.fb.new_temp(value_ir_ty))
            }
        } else {
            None
        };
        let value_op = self.lower_expr(value, dest_temp)?;

        // Lower the pattern
        self.lower_pattern(pattern, value_op, &value_ast_ty, is_mutable)?;

        Ok(self.fb.new_const_unit())
    }

    fn lower_pattern(
        &mut self,
        pattern: &Pattern,
        value_op: IrOperand,
        value_ty: &Type,
        is_mutable: bool,
    ) -> Result<(), LowerError> {
        match &pattern.kind {
            PatternKind::Ident { name } => {
                self.lower_ident_pattern(&pattern.id, name.clone(), value_op, is_mutable)
            }
            PatternKind::Array { .. } => {
                let base_temp = match value_op {
                    IrOperand::Temp(temp) => temp,
                    _ => return Err(LowerError::ArrayIsNotTemp(pattern.id, value_op)),
                };
                let base_place = Place::root(base_temp, lower_type(value_ty));
                self.lower_pattern_place(pattern, &base_place, is_mutable)
            }
            PatternKind::Tuple { .. } => {
                let base_temp = match value_op {
                    IrOperand::Temp(temp) => temp,
                    _ => return Err(LowerError::TupleIsNotTemp(pattern.id, value_op)),
                };
                let base_place = Place::root(base_temp, lower_type(value_ty));
                self.lower_pattern_place(pattern, &base_place, is_mutable)
            }
        }
    }

    fn lower_pattern_place(
        &mut self,
        pattern: &Pattern,
        place: &Place,
        is_mutable: bool,
    ) -> Result<(), LowerError> {
        match &pattern.kind {
            PatternKind::Ident { name } => {
                let value_op = self.read_place(place);
                self.lower_ident_pattern(&pattern.id, name.clone(), value_op, is_mutable)
            }

            PatternKind::Tuple { patterns } => {
                let IrType::Tuple { fields } = &place.ty else {
                    unreachable!()
                };

                for (i, subpat) in patterns.iter().enumerate() {
                    let field_ty = &fields[i];
                    let field_place = Place::tuple_field(place, i, field_ty.clone());
                    self.lower_pattern_place(subpat, &field_place, is_mutable)?;
                }
                Ok(())
            }

            PatternKind::Array { patterns } => {
                let IrType::Array { elem_ty, dims } = &place.ty else {
                    unreachable!()
                };

                // subtype for each element at this dimension
                let sub_ty = if dims.len() == 1 {
                    (**elem_ty).clone()
                } else {
                    IrType::Array {
                        elem_ty: elem_ty.clone(),
                        dims: dims[1..].to_vec(),
                    }
                };

                // size in bytes of one element at  (scalar or compound)
                let sub_size = sub_ty.size_of();

                for (i, subpat) in patterns.iter().enumerate() {
                    let elem_offset = i * sub_size;
                    let elem_place = Place::with_offset(place, elem_offset, sub_ty.clone());
                    self.lower_pattern_place(subpat, &elem_place, is_mutable)?;
                }
                Ok(())
            }
        }
    }

    fn lower_ident_pattern(
        &mut self,
        id: &NodeId,
        name: String,
        value_op: IrOperand,
        is_mutable: bool,
    ) -> Result<(), LowerError> {
        let def = self
            .ctx
            .def_map
            .lookup_def(*id)
            .ok_or(LowerError::VarDefNotFound(*id))?;
        let op = if is_mutable {
            // must use a temp - materialize constants if needed
            let temp = match value_op {
                IrOperand::Temp(temp) => temp,
                IrOperand::Const(c) => {
                    // Materialize constant into a temp
                    let value_ty = c.type_of();
                    let temp = self.fb.new_temp(value_ty);
                    self.fb.copy(temp, value_op);
                    temp
                }
            };
            self.fb.make_local(temp, name);
            IrOperand::Temp(temp)
        } else {
            // any operand
            if let IrOperand::Temp(temp) = value_op {
                self.fb.make_local(temp, name);
            }
            value_op
        };
        self.def_op.insert(def.id, op);
        Ok(())
    }

    fn lower_assign(
        &mut self,
        expr: &Expr,
        assignee: &Expr,
        value: &Expr,
    ) -> Result<IrOperand, LowerError> {
        match &assignee.kind {
            ExprKind::Var(_) => {
                // Variable assignment
                let assignee_op = self.get_var_op(assignee)?;
                let IrOperand::Temp(dest_temp) = assignee_op else {
                    return Err(LowerError::DestIsNotTemp(assignee.id, assignee_op));
                };
                self.lower_expr(value, Some(dest_temp))?;
            }
            ExprKind::ArrayIndex { target, indices } => {
                // Array element assignment
                let array_place = self.place_of_expr(target)?;

                let (elem_ty, dims) = match &array_place.ty {
                    IrType::Array { elem_ty, dims } => (elem_ty.as_ref().clone(), dims.clone()),
                    _ => {
                        return Err(LowerError::IndexOnNonArray(expr.id, array_place.ty.clone()));
                    }
                };

                // If indices are all const, we can form a Place and use lower_expr_into_place
                if let Some(linear_index) = try_const_fold_array_linear_index(&dims, indices) {
                    let base_elem_size = elem_ty.size_of();
                    let byte_offset = linear_index.0 * base_elem_size;

                    // Type of the assigned location (supports partial indexing)
                    let result_ty = self.get_node_type(expr)?;
                    let elem_place =
                        Place::with_offset(&array_place, byte_offset, lower_type(&result_ty));

                    self.lower_expr_into_place(value, &elem_place)?;
                    return Ok(self.fb.new_const_unit());
                }

                // Runtime index: scalar store via StoreAtByteOffset only
                let linear_op = self.calc_array_offset(&dims, indices)?;
                let elem_size = elem_ty.size_of();
                let byte_offset_op =
                    self.byte_offset_from_index(array_place.byte_offset, linear_op, elem_size);

                let value_ty = self.get_node_type(value)?;
                let value_ir_ty = lower_type(&value_ty);

                if value_ir_ty.is_compound() {
                    let value_temp = self.fb.new_temp(value_ir_ty.clone());
                    self.lower_expr(value, Some(value_temp))?;

                    let zero = self.fb.new_const_int(0, 64, false);
                    self.fb.mem_copy_at(
                        array_place.base,
                        value_temp,
                        byte_offset_op,
                        zero,
                        value_ir_ty.size_of(),
                    );
                    return Ok(self.fb.new_const_unit());
                }

                let value_op = self.lower_expr(value, None)?;
                self.fb.store(array_place.base, byte_offset_op, value_op);
            }
            ExprKind::TupleField { .. } => {
                // Compute (base_temp, byte_offset) from the assignee lvalue
                let Some((base_temp, offset)) = self.try_get_base_and_offset(assignee)? else {
                    return Err(LowerError::UnsupportedAssignee(
                        expr.id,
                        assignee.kind.clone(),
                    ));
                };

                // Type of the assigned location
                let dest_ty = self.get_node_type(expr)?;
                let dest_ir_ty = lower_type(&dest_ty);

                if dest_ir_ty.is_compound() {
                    let dest_place = Place {
                        base: base_temp,
                        byte_offset: offset,
                        ty: lower_type(&dest_ty),
                    };
                    self.lower_expr_into_place(value, &dest_place)?;
                    return Ok(self.fb.new_const_unit());
                }

                // Scalar store
                let value_op = self.lower_expr(value, None)?;
                let offset_op = self.fb.new_const_int(offset as i64, 64, false);
                self.fb.store(base_temp, offset_op, value_op);
            }
            ExprKind::StructField { .. } => {
                // Compute (base_temp, byte_offset) from the assignee lvalue
                let Some((base_temp, offset)) = self.try_get_base_and_offset(assignee)? else {
                    return Err(LowerError::UnsupportedAssignee(
                        expr.id,
                        assignee.kind.clone(),
                    ));
                };

                // Type of the assigned location
                let dest_ty = self.get_node_type(expr)?;
                let dest_ir_ty = lower_type(&dest_ty);

                if dest_ir_ty.is_compound() {
                    // Compound type assignment: lower value into place
                    let dest_place = Place::with_offset(
                        &Place::root(base_temp, dest_ir_ty.clone()),
                        offset,
                        dest_ir_ty,
                    );
                    self.lower_expr_into_place(value, &dest_place)?;
                    return Ok(self.fb.new_const_unit());
                }

                // Scalar store
                let value_op = self.lower_expr(value, None)?;
                let offset_op = self.fb.new_const_int(offset as i64, 64, false);
                self.fb.store(base_temp, offset_op, value_op);
            }
            _ => {
                return Err(LowerError::UnsupportedAssignee(
                    expr.id,
                    assignee.kind.clone(),
                ));
            }
        }
        Ok(self.fb.new_const_unit())
    }

    fn lower_var(
        &mut self,
        expr: &Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        let var_op = self.get_var_op(expr)?;

        // if dest_temp provided, copy the var to it
        if let Some(dest_temp) = dest_temp {
            match var_op {
                IrOperand::Temp(var_temp) if dest_temp == var_temp => {}
                IrOperand::Temp(var_temp) => {
                    let var_ty = lower_type(&self.get_node_type(expr)?);
                    if var_ty.is_compound() {
                        self.fb.mem_copy(dest_temp, var_temp, var_ty.size_of());
                    } else {
                        self.fb.copy(dest_temp, var_op);
                    }
                }
                IrOperand::Const(_) => {
                    self.fb.copy(dest_temp, var_op);
                }
            }
        }

        Ok(var_op)
    }

    fn calc_array_offset(
        &mut self,
        dims: &[usize],
        indices: &[Expr],
    ) -> Result<IrOperand, LowerError> {
        // Try constant folding first
        if let Some(const_offset) = try_const_fold_array_linear_index(dims, indices) {
            return Ok(self.fb.new_const_int(const_offset.0 as i64, 64, false));
        }

        // Calculate linear offset from indices at runtime
        // Formula: offset = i0 * (d1 * d2 * ... * dn) + i1 * (d2 * d3 * ... * dn) + ... + in

        // Handle first index specially to avoid starting with const 0
        let first_index_op = self.lower_expr(&indices[0], None)?;
        let first_stride: usize = dims[1..].iter().product();

        let mut offset_op = if first_stride > 1 {
            let stride_op = self.fb.new_const_int(first_stride as i64, 64, false);
            let scaled_temp = self.fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            self.fb
                .binary_op(scaled_temp, BinaryOp::Mul, first_index_op, stride_op);
            IrOperand::Temp(scaled_temp)
        } else {
            first_index_op
        };

        // Add remaining indices
        for i in 1..indices.len() {
            let index_op = self.lower_expr(&indices[i], None)?;

            // Calculate stride: product of all remaining dimensions
            let stride: usize = dims[i + 1..].iter().product();

            // Scale the index by stride if needed
            let scaled_op = if stride > 1 {
                let stride_op = self.fb.new_const_int(stride as i64, 64, false);
                let scaled_temp = self.fb.new_temp(IrType::Int {
                    bits: 64,
                    signed: false,
                });
                self.fb
                    .binary_op(scaled_temp, BinaryOp::Mul, index_op, stride_op);
                IrOperand::Temp(scaled_temp)
            } else {
                index_op
            };

            // Add to accumulated offset
            let new_offset_temp = self.fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            self.fb
                .binary_op(new_offset_temp, BinaryOp::Add, offset_op, scaled_op);
            offset_op = IrOperand::Temp(new_offset_temp);
        }

        Ok(offset_op)
    }

    fn byte_offset_from_index(
        &mut self,
        base_byte_offset: usize,
        index: IrOperand,
        elem_size: usize,
    ) -> IrOperand {
        // scale the linear index by the element size if it's not 1
        let mut offset = if elem_size == 1 {
            index
        } else {
            let k = self.fb.new_const_int(elem_size as i64, 64, false);
            let tmp = self.fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            self.fb.binary_op(tmp, BinaryOp::Mul, index, k);
            IrOperand::Temp(tmp)
        };

        // add the base byte offset if it's not 0
        if base_byte_offset != 0 {
            let k = self.fb.new_const_int(base_byte_offset as i64, 64, false);
            let tmp = self.fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            self.fb.binary_op(tmp, BinaryOp::Add, offset, k);
            offset = IrOperand::Temp(tmp);
        }

        offset
    }

    fn lower_array_index(
        &mut self,
        expr: &Expr,
        target: &Expr,
        indices: &[Expr],
    ) -> Result<IrOperand, LowerError> {
        let array_place = self.place_of_expr(target)?;
        let (elem_ty, dims) = match &array_place.ty {
            IrType::Array { elem_ty, dims } => (elem_ty.as_ref().clone(), dims.clone()),
            _ => {
                return Err(LowerError::IndexOnNonArray(expr.id, array_place.ty.clone()));
            }
        };

        let result_ty = self.get_node_type(expr)?;

        if let Some(linear_index) = try_const_fold_array_linear_index(&dims, indices) {
            let base_elem_size = elem_ty.size_of();
            let byte_offset = linear_index.0 * base_elem_size;
            let elem_place = Place::with_offset(&array_place, byte_offset, lower_type(&result_ty));
            return Ok(self.read_place(&elem_place));
        }

        // Normal path: lower the target, calculate array offset, and mem copy
        // if the result is compound.
        let linear_op = self.calc_array_offset(&dims, indices)?;
        let elem_size = elem_ty.size_of();
        let byte_offset_op =
            self.byte_offset_from_index(array_place.byte_offset, linear_op, elem_size);

        let result_ir_ty = lower_type(&result_ty);
        let dest_temp = self.fb.new_temp(result_ir_ty.clone());

        if result_ir_ty.is_compound() {
            // Copy from array_place.base[byte_offset_op] to dest[0]
            let zero = self.fb.new_const_int(0, 64, false);
            self.fb.mem_copy_at(
                dest_temp,
                array_place.base,
                zero,
                byte_offset_op,
                result_ir_ty.size_of(),
            );
        } else {
            self.fb.load(dest_temp, array_place.base, byte_offset_op);
        }

        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_array_lit(
        &mut self,
        expr: &Expr,
        elems: &[Expr],
        dest_temp: IrTempId,
    ) -> Result<IrOperand, LowerError> {
        // Get element type from the array type
        let array_ty = self.get_node_type(expr)?;
        let dest_place = Place::root(dest_temp, lower_type(&array_ty));
        self.lower_array_lit_into_place(elems, &dest_place)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_tuple_lit(
        &mut self,
        expr: &Expr,
        fields: &[Expr],
        dest_temp: IrTempId,
        dest_offset: usize,
    ) -> Result<IrOperand, LowerError> {
        let tuple_ir_ty = lower_type(&self.get_node_type(expr)?);
        let base_place = Place::root(dest_temp, tuple_ir_ty.clone());
        let dest_place = Place::with_offset(&base_place, dest_offset, tuple_ir_ty);
        self.lower_tuple_lit_into_place(fields, &dest_place)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_struct_lit(
        &mut self,
        expr: &Expr,
        fields: &[StructLitField],
        dest_temp: IrTempId,
        dest_offset: usize,
    ) -> Result<IrOperand, LowerError> {
        let struct_ir_ty = lower_type(&self.get_node_type(expr)?);
        let base_place = Place::root(dest_temp, struct_ir_ty.clone());
        let dest_place = Place::with_offset(&base_place, dest_offset, struct_ir_ty);
        self.lower_struct_lit_into_place(fields, &dest_place)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    // Get the place of an expression, either from a base and offset or by materializing into a temp
    fn place_of_expr(&mut self, expr: &Expr) -> Result<Place, LowerError> {
        let ir_ty = lower_type(&self.get_node_type(expr)?);
        if let Some((base, offset)) = self.try_get_base_and_offset(expr)? {
            return Ok(Place::with_offset(
                &Place::root(base, ir_ty.clone()),
                offset,
                ir_ty.clone(),
            ));
        }

        // materialize into a temp if needed
        let dest = ir_ty.is_compound().then(|| self.fb.new_temp(ir_ty.clone()));
        let op = self.lower_expr(expr, dest)?;
        let temp = match op {
            IrOperand::Temp(temp) => temp,
            _ => return Err(LowerError::DestIsNotTemp(expr.id, op)),
        };
        Ok(Place::root(temp, ir_ty))
    }

    fn try_place_of_expr(&mut self, expr: &Expr) -> Result<Option<Place>, LowerError> {
        let Some((base, offset)) = self.try_get_base_and_offset(expr)? else {
            return Ok(None);
        };

        let ir_ty = lower_type(&self.get_node_type(expr)?);
        Ok(Some(Place {
            base,
            byte_offset: offset,
            ty: ir_ty,
        }))
    }

    // Read the value from a place, either by copying from a compound temp or by
    // loading from a scalar temp.
    fn read_place(&mut self, place: &Place) -> IrOperand {
        let ir_ty = &place.ty;
        if ir_ty.is_compound() {
            let temp = self.fb.new_temp(ir_ty.clone());
            self.fb
                .mem_copy_with_offset(temp, place.base, 0, place.byte_offset, ir_ty.size_of());
            IrOperand::Temp(temp)
        } else {
            self.emit_load_scalar(place, &ir_ty)
        }
    }

    fn lower_tuple_field(&mut self, target: &Expr, index: usize) -> Result<IrOperand, LowerError> {
        let target_place = self.place_of_expr(target)?;

        let target_ir_ty = &target_place.ty;
        let field_ty = match target_ir_ty {
            IrType::Tuple { fields } => fields[index].clone(),
            _ => unreachable!(),
        };

        let field_place = Place::tuple_field(&target_place, index, field_ty);
        Ok(self.read_place(&field_place))
    }

    fn lower_struct_field(&mut self, target: &Expr, field: &str) -> Result<IrOperand, LowerError> {
        let target_place = self.place_of_expr(target)?;

        let target_ty = &target_place.ty;
        let field_ty = match target_ty {
            IrType::Struct { fields, .. } => fields
                .iter()
                .find(|f| f.name == field)
                .map(|f| f.ty.clone())
                .expect("Field not found in struct"),
            _ => unreachable!(),
        };

        let field_place = Place::struct_field(&target_place, field, field_ty);
        Ok(self.read_place(&field_place))
    }

    // Extract the base temp and offset from a chain of tuple field accesses.
    // Returns (base_temp, accumulated_offset) if the expr is a simple chain,
    // otherwise None (i.e. fallback to memcpy).
    fn try_get_base_and_offset(
        &mut self,
        expr: &Expr,
    ) -> Result<Option<(IrTempId, usize)>, LowerError> {
        match &expr.kind {
            // Base case: Var
            ExprKind::Var(_) => {
                let var_op = self.lower_expr(expr, None)?;
                match var_op {
                    IrOperand::Temp(temp) => Ok(Some((temp, 0))),
                    _ => Ok(None), // Constants don't have offsets
                }
            }

            // Recursive case: TupleField
            ExprKind::TupleField { target, index } => {
                // Try to get the base temp and offset from the target
                if let Some((base_temp, offset)) = self.try_get_base_and_offset(target)? {
                    // Get the target's type to compute this field's offset
                    let target_ir_ty = lower_type(&self.get_node_type(target)?);
                    let field_offset = tuple_field_byte_offset(&target_ir_ty, *index).0;

                    // Combine the offsets
                    Ok(Some((base_temp, offset + field_offset)))
                } else {
                    Ok(None)
                }
            }

            // Recursive case: ArrayIndex
            ExprKind::ArrayIndex { target, indices } => {
                // Try to get the base temp and offset from the target
                if let Some((base_temp, offset)) = self.try_get_base_and_offset(target)? {
                    // Get the target's type (should be an array)
                    let target_ir_ty = lower_type(&self.get_node_type(target)?);

                    if let IrType::Array { elem_ty, dims } = &target_ir_ty {
                        // Try to constant-fold the array offset
                        if let Some(array_elem_offset) =
                            try_const_fold_array_linear_index(dims, indices)
                        {
                            // Get element size
                            let elem_size = elem_ty.size_of();

                            // Combine offsets: tuple_offset + array_elem_offset * elem_size
                            let combined_offset = offset + array_elem_offset.0 * elem_size;

                            return Ok(Some((base_temp, combined_offset)));
                        }
                    }
                }
                Ok(None)
            }

            // Recursive case: StructField
            ExprKind::StructField { target, field } => {
                // Try to get the base temp and offset from the target
                if let Some((base_temp, offset)) = self.try_get_base_and_offset(target)? {
                    // Get the target's type (should be a struct)
                    let target_ir_ty = lower_type(&self.get_node_type(target)?);
                    let field_offset = struct_field_byte_offset(&target_ir_ty, field).0;

                    // Combine offsets
                    Ok(Some((base_temp, offset + field_offset)))
                } else {
                    Ok(None)
                }
            }

            // Any other expression: fall back to memcpy
            _ => Ok(None),
        }
    }

    fn lower_expr_into_place(&mut self, expr: &Expr, dest_place: &Place) -> Result<(), LowerError> {
        let dest_ir_ty = &dest_place.ty;

        match &expr.kind {
            ExprKind::ArrayLit(elems) => self.lower_array_lit_into_place(elems, dest_place),
            ExprKind::TupleLit(fields) => self.lower_tuple_lit_into_place(fields, dest_place),
            ExprKind::StructLit { fields, .. } => {
                self.lower_struct_lit_into_place(fields, dest_place)
            }
            _ => {
                if dest_ir_ty.is_compound() {
                    // Best case: expression is already a const-addressable sub-place
                    if let Some(src_place) = self.try_place_of_expr(expr)? {
                        debug_assert!(src_place.ty.is_compound());

                        if src_place.base != dest_place.base
                            || src_place.byte_offset != dest_place.byte_offset
                        {
                            self.fb.mem_copy_with_offset(
                                dest_place.base,
                                src_place.base,
                                dest_place.byte_offset,
                                src_place.byte_offset,
                                dest_ir_ty.size_of(),
                            );
                        }
                        return Ok(());
                    }

                    // Fallback: materialize into a temp and copy
                    let src_op = self.lower_expr(expr, None)?;
                    let IrOperand::Temp(src_temp) = src_op else {
                        return Err(LowerError::DestIsNotTemp(expr.id, src_op));
                    };
                    self.fb.mem_copy_with_offset(
                        dest_place.base,
                        src_temp,
                        dest_place.byte_offset,
                        0,
                        dest_ir_ty.size_of(),
                    );
                    Ok(())
                } else {
                    // Scalar: compute value and store at dest byte offset
                    let value_op = self.lower_expr(expr, None)?;
                    let offset_op = self
                        .fb
                        .new_const_int(dest_place.byte_offset as i64, 64, false);
                    self.fb.store(dest_place.base, offset_op, value_op);
                    Ok(())
                }
            }
        }
    }

    fn lower_array_lit_into_place(
        &mut self,
        elems: &[Expr],
        dest_place: &Place,
    ) -> Result<(), LowerError> {
        let IrType::Array { elem_ty, dims } = &dest_place.ty else {
            unreachable!()
        };

        // Type checker ensures shapes match, but debug assert to be safe
        debug_assert!(!dims.is_empty());
        debug_assert_eq!(elems.len(), dims[0]);

        // Type of one element at this dimension:
        // 1D: element is elem_ty (scalar/tuple/...)
        // nD: element is a sub-array with dims[1..]
        let sub_ir_ty = if dims.len() == 1 {
            (**elem_ty).clone()
        } else {
            IrType::Array {
                elem_ty: elem_ty.clone(),
                dims: dims[1..].to_vec(),
            }
        };

        // Byte size of one element at this dimension (scalar or compound)
        let sub_size = sub_ir_ty.size_of();

        for (i, elem_expr) in elems.iter().enumerate() {
            let elem_offset = i * sub_size;
            let elem_place = Place::with_offset(dest_place, elem_offset, sub_ir_ty.clone());
            self.lower_expr_into_place(elem_expr, &elem_place)?;
        }
        Ok(())
    }

    fn lower_tuple_lit_into_place(
        &mut self,
        fields: &[Expr],
        dest_place: &Place,
    ) -> Result<(), LowerError> {
        for (i, field) in fields.iter().enumerate() {
            let field_ir_ty = lower_type(&self.get_node_type(field)?);
            let field_place = Place::tuple_field(dest_place, i, field_ir_ty);
            self.lower_expr_into_place(field, &field_place)?;
        }
        Ok(())
    }

    fn lower_struct_lit_into_place(
        &mut self,
        fields: &[StructLitField],
        dest_place: &Place,
    ) -> Result<(), LowerError> {
        for field in fields {
            let field_ir_ty = lower_type(&self.get_node_type(&field.value)?);
            let field_place = Place::struct_field(dest_place, &field.name, field_ir_ty);
            self.lower_expr_into_place(&field.value, &field_place)?;
        }
        Ok(())
    }

    fn lower_tuple_lit_at_addr(
        &mut self,
        fields: &[Expr],
        addr: &IrAddr,
    ) -> Result<(), LowerError> {
        for (i, field) in fields.iter().enumerate() {
            let field_addr = IrAddr::tuple_field(addr, i);
            self.lower_expr_at_addr(field, &field_addr)?;
        }
        Ok(())
    }

    fn lower_expr_at_addr(&mut self, expr: &Expr, addr: &IrAddr) -> Result<(), LowerError> {
        todo!()
    }

    fn lower_binary_op(
        &mut self,
        op: &BinaryOp,
        left: &Expr,
        right: &Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        // create the result temp (if not provided)
        let result_ty = self.get_node_type(left)?;
        let result = dest_temp.unwrap_or_else(|| self.fb.new_temp(lower_type(&result_ty)));

        // lower the left and right operands
        let left_op = self.lower_expr(left, None)?;
        let right_op = self.lower_expr(right, None)?;

        self.fb.binary_op(result, *op, left_op, right_op);

        Ok(IrOperand::Temp(result))
    }

    fn lower_unary_op(
        &mut self,
        op: &UnaryOp,
        expr: &Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        // create the result temp (if not provided)
        let result_ty = self.get_node_type(expr)?;
        let result = dest_temp.unwrap_or_else(|| self.fb.new_temp(lower_type(&result_ty)));

        // lower the operand
        let expr_op = self.lower_expr(expr, None)?;

        self.fb.unary_op(result, *op, expr_op);

        Ok(IrOperand::Temp(result))
    }

    fn lower_if(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        // Validate that the then and else bodies have the same type
        let then_ty = self.get_node_type(then_body)?;
        let else_ty = self.get_node_type(else_body)?;
        debug_assert_eq!(
            then_ty, else_ty,
            "then and else bodies have different types"
        );

        // Create the required blocks
        let then_b = self.fb.new_block("then".to_string());
        let else_b = self.fb.new_block("else".to_string());
        let merge_b = self.fb.new_block("merge".to_string());

        // Build the conditional branch
        let cond = self.lower_expr(cond, None)?;
        self.fb.terminate(IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        });

        // Then block
        self.fb.select_block(then_b);
        let then_op = match self.lower_expr(then_body, dest_temp)? {
            IrOperand::Temp(temp) => temp,
            other => {
                // Materialize the constant to a temp (so that phi can use it)
                let temp = self.fb.new_temp(lower_type(&then_ty));
                self.fb.copy(temp, other);
                temp
            }
        };
        self.fb.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        self.fb.select_block(else_b);
        let else_op = match self.lower_expr(else_body, dest_temp)? {
            IrOperand::Temp(temp) => temp,
            other => {
                // Materialize the constant to a temp (so that phi can use it)
                let temp = self.fb.new_temp(lower_type(&else_ty));
                self.fb.copy(temp, other);
                temp
            }
        };
        self.fb.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        self.fb.select_block(merge_b);

        // dest_temp present -> branches already lowered into dest_temp
        // dest_temp not present -> create a phi node to merge the branches
        let dest_temp = dest_temp.unwrap_or_else(|| {
            let merge_temp = self.fb.new_temp(lower_type(&then_ty));
            self.fb
                .phi(merge_temp, vec![(then_b, then_op), (else_b, else_op)]);
            merge_temp
        });

        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_while(&mut self, cond: &Expr, body: &Expr) -> Result<IrOperand, LowerError> {
        let header_b = self.fb.new_block("loop_header".to_string());
        let body_b = self.fb.new_block("loop_body".to_string());
        let after_b = self.fb.new_block("loop_after".to_string());

        // Terminate current block
        self.fb.terminate(IrTerminator::Br { target: header_b });

        // Build the header block
        self.fb.select_block(header_b);
        let cond = self.lower_expr(cond, None)?;
        self.fb.terminate(IrTerminator::CondBr {
            cond,
            then_b: body_b,
            else_b: after_b,
        });

        // Body block
        self.fb.select_block(body_b);
        self.lower_expr(body, None)?;
        self.fb.terminate(IrTerminator::Br { target: header_b });

        // After block
        self.fb.select_block(after_b);

        Ok(self.fb.new_const_unit())
    }

    fn lower_call(
        &mut self,
        expr: &Expr,
        name: String,
        args: &[Expr],
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        let ret_ty = lower_type(&self.get_node_type(expr)?);

        // Lower the arguments
        let args = args
            .iter()
            .map(|arg| self.lower_expr(arg, None))
            .collect::<Result<Vec<IrOperand>, LowerError>>()?;

        // Create the result temp
        let result = if let Some(dest_temp) = dest_temp {
            dest_temp
        } else {
            self.fb.new_temp(ret_ty.clone())
        };

        // Call the function
        self.fb.call(Some(result), name, args, ret_ty);

        Ok(IrOperand::Temp(result))
    }

    fn get_node_type(&self, expr: &Expr) -> Result<Type, LowerError> {
        self.ctx
            .type_map
            .lookup_node_type(expr.id)
            .ok_or(LowerError::NodeTypeNotFound(expr.id))
    }

    fn get_var_op(&self, expr: &Expr) -> Result<IrOperand, LowerError> {
        debug_assert!(matches!(expr.kind, ExprKind::Var(_)));

        let var_def_id = self
            .ctx
            .def_map
            .lookup_def(expr.id)
            .ok_or(LowerError::VarDefNotFound(expr.id))?
            .id;

        let var_op = *self
            .def_op
            .get(&var_def_id)
            .ok_or(LowerError::OperandNotFound(expr.id, var_def_id))?;

        Ok(var_op)
    }

    fn emit_load_scalar(&mut self, place: &Place, ir_ty: &IrType) -> IrOperand {
        let offset_op = self.fb.new_const_int(place.byte_offset as i64, 64, false);
        let result_temp = self.fb.new_temp(ir_ty.clone());
        self.fb.load(result_temp, place.base, offset_op);
        IrOperand::Temp(result_temp)
    }
}

pub fn lower(context: AnalyzedContext) -> Result<LoweredContext, LowerError> {
    let mut ir_funcs = Vec::new();
    for func in context.module.funcs() {
        let ir_func = FuncLowerer::new(&context, func).lower()?;
        ir_funcs.push(ir_func);
    }
    Ok(context.with_ir_funcs(ir_funcs))
}

#[cfg(test)]
#[path = "tests/t_lower.rs"]
mod tests;
