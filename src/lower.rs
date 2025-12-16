use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

use crate::ast;
use crate::context::{AnalyzedContext, LoweredContext};
use crate::ids::{DefId, NodeId};
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrFunction, IrOperand, IrTempId, IrTerminator, IrType};
use crate::layout::{
    struct_field_byte_offset, try_const_fold_array_linear_index, tuple_field_byte_offset,
};
use crate::types::Type;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("Parameter definition not found: Node {0}")]
    ParamDefNotFound(NodeId),

    #[error("Node type not found: Node {0}")]
    NodeTypeNotFound(NodeId),

    #[error("Block is empty: Node {0}")]
    BlockEmpty(NodeId),

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
    UnsupportedAssignee(NodeId, ast::ExprKind),

    #[error("Array literal requires a destination temp: Node {0}")]
    ArrayLitRequiresDestTemp(NodeId),

    #[error("Tuple literal requires a destination temp: Node {0}")]
    TupleLitRequiresDestTemp(NodeId),

    #[error("Tuple is not a temp: Node {0}, Operand {1:?}")]
    TupleIsNotTemp(NodeId, IrOperand),

    #[error("Struct literal requires a destination temp: Node {0}")]
    StructLitRequiresDestTemp(NodeId),
}

fn lower_type(ty: &Type) -> IrType {
    match ty {
        Type::UInt64 => IrType::Int {
            bits: 64,
            signed: false,
        },
        Type::Bool => IrType::Bool,
        Type::Unit => IrType::Int {
            bits: 1,
            signed: false,
        },
        Type::Unknown => panic!("Unknown type"),
        Type::Array { elem_ty, dims } => IrType::Array {
            elem_ty: Box::new(lower_type(elem_ty)),
            dims: dims.clone(),
        },
        Type::Tuple { fields } => IrType::Tuple {
            fields: fields.iter().map(lower_type).collect(),
        },
        Type::Struct { fields, .. } => IrType::Tuple {
            // structs are lowered as tuples of their fields
            fields: fields.iter().map(|f| lower_type(&f.ty)).collect(),
        },
    }
}

//---------------- Place ----------------

#[derive(Debug)]
struct Place {
    base: IrTempId,
    byte_offset: usize,
    ty: Type,
}

impl Place {
    fn root(base: IrTempId, ty: Type) -> Place {
        Place {
            base,
            byte_offset: 0,
            ty,
        }
    }

    fn with_offset(p: &Place, add: usize, ty: Type) -> Place {
        Place {
            base: p.base,
            byte_offset: p.byte_offset + add,
            ty,
        }
    }

    fn tuple_field(p: &Place, field_index: usize, field_ty: Type) -> Place {
        let field_offset = tuple_field_byte_offset(&p.ty, field_index).0;
        Place {
            base: p.base,
            byte_offset: p.byte_offset + field_offset,
            ty: field_ty,
        }
    }

    fn struct_field(p: &Place, field_name: &str, field_ty: Type) -> Place {
        let field_offset = struct_field_byte_offset(&p.ty, field_name).0;
        Place {
            base: p.base,
            byte_offset: p.byte_offset + field_offset,
            ty: field_ty,
        }
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

pub struct Lowerer<'a> {
    ctx: &'a AnalyzedContext,
    def_op: HashMap<DefId, IrOperand>,
}

impl<'a> Lowerer<'a> {
    pub fn new(ctx: &'a AnalyzedContext) -> Self {
        Self {
            ctx,
            def_op: HashMap::new(),
        }
    }

    pub fn lower_func(&mut self, func: &ast::Function) -> Result<IrFunction, LowerError> {
        // clear the def_addr map for each function
        self.def_op.clear();

        let ret_ty = self
            .ctx
            .type_map
            .lookup_node_type(func.id)
            .unwrap_or_else(|| panic!("Function {} not found in type_map", func.name));

        let mut fb = IrFunctionBuilder::new(func.name.clone(), lower_type(&ret_ty));

        // lower params and store them in the def_temp map
        for (i, param) in func.params.iter().enumerate() {
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
                        fb.new_param(i as u32, param.name.clone(), lower_type(&param_ty));

                    self.def_op.insert(def.id, IrOperand::Temp(param_temp));
                }
                None => return Err(LowerError::ParamDefNotFound(param.id)),
            }
        }

        // lower the body
        let ret_temp = fb.ret_temp();
        let ret_op = self.lower_expr(&mut fb, &func.body, ret_temp)?;

        match (ret_temp, ret_op) {
            (Some(ret_temp), IrOperand::Temp(src_temp)) if ret_temp == src_temp => {
                // Result already in the return temp, so no need to copy
                fb.terminate(IrTerminator::Ret { value: None });
            }
            (Some(ret_temp), IrOperand::Temp(src_temp)) => {
                // Result is in a different temp, so we need to copy it
                fb.mem_copy(ret_temp, src_temp, ret_ty.size_of());
                fb.terminate(IrTerminator::Ret { value: None });
            }
            _ => fb.terminate(IrTerminator::Ret {
                value: Some(ret_op),
            }),
        }

        Ok(fb.finish())
    }

    fn lower_pattern(
        &mut self,
        fb: &mut IrFunctionBuilder,
        pattern: &ast::Pattern,
        value_op: IrOperand,
        value_ty: &Type,
        is_mutable: bool,
    ) -> Result<(), LowerError> {
        match pattern {
            ast::Pattern::Ident { id, name, .. } => {
                self.lower_ident_pattern(fb, id, name.clone(), value_op, is_mutable)
            }
            ast::Pattern::Array { id, .. } => {
                let base_temp = match value_op {
                    IrOperand::Temp(temp) => temp,
                    _ => return Err(LowerError::ArrayIsNotTemp(*id, value_op)),
                };
                let base_place = Place::root(base_temp, value_ty.clone());
                self.lower_pattern_place(fb, pattern, &base_place, is_mutable)
            }
            ast::Pattern::Tuple { id, .. } => {
                let base_temp = match value_op {
                    IrOperand::Temp(temp) => temp,
                    _ => return Err(LowerError::TupleIsNotTemp(*id, value_op)),
                };
                let base_place = Place::root(base_temp, value_ty.clone());
                self.lower_pattern_place(fb, pattern, &base_place, is_mutable)
            }
        }
    }

    fn lower_pattern_place(
        &mut self,
        fb: &mut IrFunctionBuilder,
        pattern: &ast::Pattern,
        place: &Place,
        is_mutable: bool,
    ) -> Result<(), LowerError> {
        match pattern {
            ast::Pattern::Ident { id, name, .. } => {
                let value_op = Self::read_place(fb, place);
                self.lower_ident_pattern(fb, id, name.clone(), value_op, is_mutable)
            }

            ast::Pattern::Tuple { patterns, .. } => {
                let Type::Tuple { fields } = &place.ty else {
                    unreachable!()
                };

                for (i, subpat) in patterns.iter().enumerate() {
                    let field_ty = &fields[i];
                    let field_place = Place::tuple_field(place, i, field_ty.clone());
                    self.lower_pattern_place(fb, subpat, &field_place, is_mutable)?;
                }
                Ok(())
            }

            ast::Pattern::Array { patterns, .. } => {
                let Type::Array { elem_ty, dims } = &place.ty else {
                    unreachable!()
                };

                // subtype for each element at this dimension
                let sub_ty = if dims.len() == 1 {
                    (**elem_ty).clone()
                } else {
                    Type::Array {
                        elem_ty: elem_ty.clone(),
                        dims: dims[1..].to_vec(),
                    }
                };

                // size in bytes of one element at  (scalar or compound)
                let sub_size = sub_ty.size_of();

                for (i, subpat) in patterns.iter().enumerate() {
                    let elem_offset = i * sub_size;
                    let elem_place = Place::with_offset(place, elem_offset, sub_ty.clone());
                    self.lower_pattern_place(fb, subpat, &elem_place, is_mutable)?;
                }
                Ok(())
            }
        }
    }

    fn lower_ident_pattern(
        &mut self,
        fb: &mut IrFunctionBuilder,
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
                    let temp = fb.new_temp(value_ty);
                    fb.move_to(temp, value_op);
                    temp
                }
            };
            fb.make_local(temp, name);
            IrOperand::Temp(temp)
        } else {
            // any operand
            if let IrOperand::Temp(temp) = value_op {
                fb.make_local(temp, name);
            }
            value_op
        };
        self.def_op.insert(def.id, op);
        Ok(())
    }

    fn lower_binding(
        &mut self,
        fb: &mut IrFunctionBuilder,
        pattern: &ast::Pattern,
        value: &ast::Expr,
        is_mutable: bool,
    ) -> Result<IrOperand, LowerError> {
        let value_ast_ty = self.get_node_type(value)?;
        let value_ir_ty = lower_type(&value_ast_ty);

        // For compound patterns (Tuple/Array) with a simple VarRef value,
        // we can use the existing variable's temp directly without copying.
        let can_use_value_directly = matches!(
            (pattern, &value.kind),
            (
                ast::Pattern::Tuple { .. } | ast::Pattern::Array { .. },
                ast::ExprKind::VarRef(_)
            )
        ) && value_ir_ty.is_compound();

        // Check if this binding is NRVO-eligible
        let dest_temp = if can_use_value_directly {
            // Don't create a dest_temp - we'll use the value's temp directly
            None
        } else if value_ir_ty.is_compound() {
            // Check if the pattern is an Ident and if it's NRVO-eligible
            let is_nrvo_eligible = match pattern {
                ast::Pattern::Ident { id, .. } => {
                    if let Some(def) = self.ctx.def_map.lookup_def(*id) {
                        def.nrvo_eligible
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if is_nrvo_eligible {
                fb.ret_temp() // Use the return temp for NRVO-eligible variables
            } else {
                Some(fb.new_temp(value_ir_ty))
            }
        } else {
            None
        };
        let value_op = self.lower_expr(fb, value, dest_temp)?;

        // Lower the pattern
        self.lower_pattern(fb, pattern, value_op, &value_ast_ty, is_mutable)?;

        Ok(fb.new_const_unit())
    }

    fn lower_assign(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        assignee: &ast::Expr,
        value: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        match &assignee.kind {
            ast::ExprKind::VarRef(_) => {
                // Variable assignment
                match self.ctx.def_map.lookup_def(assignee.id) {
                    Some(def) => {
                        let temp = match self.def_op.get(&def.id) {
                            Some(IrOperand::Temp(temp)) => *temp,
                            Some(op) => return Err(LowerError::DestIsNotTemp(expr.id, *op)),
                            None => return Err(LowerError::VarDefNotFound(expr.id)),
                        };
                        let value_ty = lower_type(&self.get_node_type(value)?);
                        let dest_temp = if value_ty.is_compound() {
                            Some(temp)
                        } else {
                            None
                        };
                        let value_op = self.lower_expr(fb, value, dest_temp)?;
                        // Only move if the value is scalar (compound already constructed in place)
                        if !value_ty.is_compound() {
                            fb.move_to(temp, value_op);
                        }
                    }
                    None => return Err(LowerError::VarDefNotFound(expr.id)),
                }
            }
            ast::ExprKind::Index { target, indices } => {
                // Array element assignment
                let array_place = self.place_of_expr(fb, target)?;

                let (elem_ty, dims) = match &array_place.ty {
                    Type::Array { elem_ty, dims } => (elem_ty.as_ref().clone(), dims.clone()),
                    _ => {
                        return Err(LowerError::IndexOnNonArray(
                            expr.id,
                            lower_type(&array_place.ty),
                        ));
                    }
                };

                // If indices are all const, we can form a Place and use
                // lower_expr_into_place
                if let Some(linear_index) = try_const_fold_array_linear_index(&dims, indices) {
                    let base_elem_size = elem_ty.size_of();
                    let byte_offset = linear_index.0 * base_elem_size;

                    // Type of the assigned location (supports partial indexing)
                    let result_ty = self.get_node_type(expr)?;
                    let elem_place = Place::with_offset(&array_place, byte_offset, result_ty);

                    self.lower_expr_into_place(fb, value, &elem_place)?;
                    return Ok(fb.new_const_unit());
                }

                // Runtime index: scalar store via StoreAtByteOffset only
                let linear_op = self.calc_array_offset(fb, &dims, indices)?;
                let elem_size = elem_ty.size_of();
                let byte_offset_op = Self::byte_offset_from_linear_index(
                    fb,
                    array_place.byte_offset,
                    linear_op,
                    elem_size,
                );

                let value_ty = self.get_node_type(value)?;
                let value_ir_ty = lower_type(&value_ty);

                if value_ir_ty.is_compound() {
                    let value_temp = fb.new_temp(value_ir_ty.clone());
                    self.lower_expr(fb, value, Some(value_temp))?;

                    let zero = fb.new_const_int(0, 64, false);
                    fb.mem_copy_at(
                        array_place.base,
                        value_temp,
                        byte_offset_op,
                        zero,
                        value_ir_ty.size_of(),
                    );
                    return Ok(fb.new_const_unit());
                }

                let value_op = self.lower_expr(fb, value, None)?;
                fb.store(array_place.base, byte_offset_op, value_op);
            }
            ast::ExprKind::TupleFieldAccess { .. } => {
                // Compute (base_temp, byte_offset) from the assignee lvalue
                let Some((base_temp, offset)) = self.try_get_base_and_offset(fb, assignee)? else {
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
                        ty: dest_ty,
                    };
                    self.lower_expr_into_place(fb, value, &dest_place)?;
                    return Ok(fb.new_const_unit());
                }

                // Scalar store
                let value_op = self.lower_expr(fb, value, None)?;
                let offset_op = fb.new_const_int(offset as i64, 64, false);
                fb.store(base_temp, offset_op, value_op);
            }
            ast::ExprKind::FieldAccess { .. } => {
                // Compute (base_temp, byte_offset) from the assignee lvalue
                let Some((base_temp, offset)) = self.try_get_base_and_offset(fb, assignee)? else {
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
                        &Place::root(base_temp, dest_ty.clone()),
                        offset,
                        dest_ty,
                    );
                    self.lower_expr_into_place(fb, value, &dest_place)?;
                    return Ok(fb.new_const_unit());
                }

                // Scalar store
                let value_op = self.lower_expr(fb, value, None)?;
                let offset_op = fb.new_const_int(offset as i64, 64, false);
                fb.store(base_temp, offset_op, value_op);
            }
            _ => {
                return Err(LowerError::UnsupportedAssignee(
                    expr.id,
                    assignee.kind.clone(),
                ));
            }
        }
        Ok(fb.new_const_unit())
    }

    fn lower_var_ref(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        let var_def = self
            .ctx
            .def_map
            .lookup_def(expr.id)
            .ok_or(LowerError::VarDefNotFound(expr.id))?;

        let var_op = *self
            .def_op
            .get(&var_def.id)
            .ok_or(LowerError::OperandNotFound(expr.id, var_def.id))?;

        match (dest_temp, var_op) {
            (Some(dest_temp), IrOperand::Temp(var_temp)) if dest_temp != var_temp => {
                let var_ty = lower_type(&self.get_node_type(expr)?);
                if var_ty.is_compound() {
                    fb.mem_copy(dest_temp, var_temp, var_ty.size_of());
                } else {
                    fb.move_to(dest_temp, var_op);
                }
                Ok(IrOperand::Temp(dest_temp))
            }
            _ => Ok(var_op),
        }
    }

    fn lower_array_lit(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        elems: &[ast::Expr],
        dest_temp: IrTempId,
    ) -> Result<IrOperand, LowerError> {
        // Get element type from the array type
        let array_ty = self.get_node_type(expr)?;
        let dest_place = Place::root(dest_temp, array_ty);
        self.lower_array_lit_into_place(fb, elems, &dest_place)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    fn calc_array_offset(
        &mut self,
        fb: &mut IrFunctionBuilder,
        dims: &[usize],
        indices: &[ast::Expr],
    ) -> Result<IrOperand, LowerError> {
        // Try constant folding first
        if let Some(const_offset) = try_const_fold_array_linear_index(dims, indices) {
            return Ok(fb.new_const_int(const_offset.0 as i64, 64, false));
        }

        // Calculate linear offset from indices at runtime
        // Formula: offset = i0 * (d1 * d2 * ... * dn) + i1 * (d2 * d3 * ... * dn) + ... + in

        // Handle first index specially to avoid starting with const 0
        let first_index_op = self.lower_expr(fb, &indices[0], None)?;
        let first_stride: usize = dims[1..].iter().product();

        let mut offset_op = if first_stride > 1 {
            let stride_op = fb.new_const_int(first_stride as i64, 64, false);
            let scaled_temp = fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            fb.binary_op(scaled_temp, ast::BinaryOp::Mul, first_index_op, stride_op);
            IrOperand::Temp(scaled_temp)
        } else {
            first_index_op
        };

        // Add remaining indices
        for i in 1..indices.len() {
            let index_op = self.lower_expr(fb, &indices[i], None)?;

            // Calculate stride: product of all remaining dimensions
            let stride: usize = dims[i + 1..].iter().product();

            // Scale the index by stride if needed
            let scaled_op = if stride > 1 {
                let stride_op = fb.new_const_int(stride as i64, 64, false);
                let scaled_temp = fb.new_temp(IrType::Int {
                    bits: 64,
                    signed: false,
                });
                fb.binary_op(scaled_temp, ast::BinaryOp::Mul, index_op, stride_op);
                IrOperand::Temp(scaled_temp)
            } else {
                index_op
            };

            // Add to accumulated offset
            let new_offset_temp = fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            fb.binary_op(new_offset_temp, ast::BinaryOp::Add, offset_op, scaled_op);
            offset_op = IrOperand::Temp(new_offset_temp);
        }

        Ok(offset_op)
    }

    fn byte_offset_from_linear_index(
        fb: &mut IrFunctionBuilder,
        base_byte_offset: usize,
        linear_index: IrOperand,
        elem_size: usize,
    ) -> IrOperand {
        // scale the linear index by the element size if it's not 1
        let mut offset = if elem_size == 1 {
            linear_index
        } else {
            let k = fb.new_const_int(elem_size as i64, 64, false);
            let tmp = fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            fb.binary_op(tmp, ast::BinaryOp::Mul, linear_index, k);
            IrOperand::Temp(tmp)
        };

        // add the base byte offset if it's not 0
        if base_byte_offset != 0 {
            let k = fb.new_const_int(base_byte_offset as i64, 64, false);
            let tmp = fb.new_temp(IrType::Int {
                bits: 64,
                signed: false,
            });
            fb.binary_op(tmp, ast::BinaryOp::Add, offset, k);
            offset = IrOperand::Temp(tmp);
        }

        offset
    }

    fn lower_index(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        target: &ast::Expr,
        indices: &[ast::Expr],
    ) -> Result<IrOperand, LowerError> {
        let array_place = self.place_of_expr(fb, target)?;
        let (elem_ty, dims) = match &array_place.ty {
            Type::Array { elem_ty, dims } => (elem_ty.as_ref().clone(), dims.clone()),
            _ => {
                return Err(LowerError::IndexOnNonArray(
                    expr.id,
                    lower_type(&array_place.ty),
                ));
            }
        };

        if let Some(linear_index) = try_const_fold_array_linear_index(&dims, indices) {
            let result_ty = self.get_node_type(expr)?;
            let base_elem_size = elem_ty.size_of();
            let byte_offset = linear_index.0 * base_elem_size;
            let elem_place = Place::with_offset(&array_place, byte_offset, result_ty);
            return Ok(Self::read_place(fb, &elem_place));
        }

        // Normal path: lower the target, calculate array offset, and mem copy
        // if the result is compound.
        let linear_op = self.calc_array_offset(fb, &dims, indices)?;
        let elem_size = elem_ty.size_of();
        let byte_offset_op =
            Self::byte_offset_from_linear_index(fb, array_place.byte_offset, linear_op, elem_size);

        let result_ir_ty = lower_type(&self.get_node_type(expr)?);
        if result_ir_ty.is_compound() {
            let dest = fb.new_temp(result_ir_ty.clone());

            // Copy from array_place.base[byte_offset_op] to dest[0]
            let zero = fb.new_const_int(0, 64, false);
            fb.mem_copy_at(
                dest,
                array_place.base,
                zero,
                byte_offset_op,
                result_ir_ty.size_of(),
            );
            return Ok(IrOperand::Temp(dest));
        }

        let result = fb.new_temp(result_ir_ty);
        fb.load(result, array_place.base, byte_offset_op);

        Ok(IrOperand::Temp(result))
    }

    fn lower_tuple_lit(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        fields: &[ast::Expr],
        dest_temp: IrTempId,
        base_offset: usize,
    ) -> Result<IrOperand, LowerError> {
        let tuple_ty = &self.get_node_type(expr)?;
        let base_place = Place::root(dest_temp, tuple_ty.clone());
        let dest_place = Place::with_offset(&base_place, base_offset, tuple_ty.clone());
        self.lower_tuple_lit_into_place(fb, fields, &dest_place)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_struct_lit(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        fields: &[ast::StructLitField],
        dest_temp: IrTempId,
        base_offset: usize,
    ) -> Result<IrOperand, LowerError> {
        let struct_ty = &self.get_node_type(expr)?;
        let base_place = Place::root(dest_temp, struct_ty.clone());
        let dest_place = Place::with_offset(&base_place, base_offset, struct_ty.clone());
        self.lower_struct_lit_into_place(fb, fields, &dest_place)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    // Get the place of an expression, either from a base and offset or by materializing into a temp
    fn place_of_expr(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
    ) -> Result<Place, LowerError> {
        let ty = self.get_node_type(expr)?;
        if let Some((base, offset)) = self.try_get_base_and_offset(fb, expr)? {
            return Ok(Place::with_offset(
                &Place::root(base, ty.clone()),
                offset,
                ty,
            ));
        }

        // materialize into a temp if needed
        let ir_ty = lower_type(&ty);
        let dest = ir_ty.is_compound().then(|| fb.new_temp(ir_ty));
        let op = self.lower_expr(fb, expr, dest)?;
        let temp = match op {
            IrOperand::Temp(temp) => temp,
            _ => return Err(LowerError::DestIsNotTemp(expr.id, op)),
        };
        Ok(Place::root(temp, ty))
    }

    fn try_place_of_expr(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
    ) -> Result<Option<Place>, LowerError> {
        let Some((base, offset)) = self.try_get_base_and_offset(fb, expr)? else {
            return Ok(None);
        };

        let ty = self.get_node_type(expr)?;
        Ok(Some(Place {
            base,
            byte_offset: offset,
            ty,
        }))
    }

    // Read the value from a place, either by copying from a compound temp or by
    // loading from a scalar temp.
    fn read_place(fb: &mut IrFunctionBuilder, place: &Place) -> IrOperand {
        let ir_ty = lower_type(&place.ty);
        if ir_ty.is_compound() {
            let temp = fb.new_temp(ir_ty.clone());
            Self::emit_copy_compound_from(fb, place, temp, ir_ty.size_of());
            IrOperand::Temp(temp)
        } else {
            Self::emit_load_scalar(fb, place, &ir_ty)
        }
    }

    fn lower_tuple_field_access(
        &mut self,
        fb: &mut IrFunctionBuilder,
        target: &ast::Expr,
        index: u32,
    ) -> Result<IrOperand, LowerError> {
        let target_place = self.place_of_expr(fb, target)?;

        let target_ty = &target_place.ty;
        let field_ty = match target_ty {
            Type::Tuple { fields } => fields[index as usize].clone(),
            _ => unreachable!(),
        };

        let field_place = Place::tuple_field(&target_place, index as usize, field_ty);
        Ok(Self::read_place(fb, &field_place))
    }

    fn lower_struct_field_access(
        &mut self,
        fb: &mut IrFunctionBuilder,
        target: &ast::Expr,
        field: &str,
    ) -> Result<IrOperand, LowerError> {
        let target_place = self.place_of_expr(fb, target)?;

        let target_ty = &target_place.ty;
        let field_ty = match target_ty {
            Type::Struct { fields, .. } => fields
                .iter()
                .find(|f| f.name == field)
                .map(|f| f.ty.clone())
                .expect("Field not found in struct"),
            _ => unreachable!(),
        };

        let field_place = Place::struct_field(&target_place, field, field_ty);
        Ok(Self::read_place(fb, &field_place))
    }

    // Extract the base temp and offset from a chain of tuple field accesses.
    // Returns (base_temp, accumulated_offset) if the expr is a simple chain,
    // otherwise None (i.e. fallback to memcpy).
    fn try_get_base_and_offset(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
    ) -> Result<Option<(IrTempId, usize)>, LowerError> {
        match &expr.kind {
            // Base case: VarRef
            ast::ExprKind::VarRef(_) => {
                let var_op = self.lower_expr(fb, expr, None)?;
                match var_op {
                    IrOperand::Temp(temp) => Ok(Some((temp, 0))),
                    _ => Ok(None), // Constants don't have offsets
                }
            }

            // Recursive case: TupleFieldAccess
            ast::ExprKind::TupleFieldAccess { target, index } => {
                // Try to get the base temp and offset from the target
                if let Some((base_temp, offset)) = self.try_get_base_and_offset(fb, target)? {
                    // Get the target's type to compute this field's offset
                    let target_ty = &self.get_node_type(target)?;
                    let field_offset = tuple_field_byte_offset(target_ty, *index as usize).0;

                    // Combine the offsets
                    Ok(Some((base_temp, offset + field_offset)))
                } else {
                    Ok(None)
                }
            }

            // Recursive case: Index
            ast::ExprKind::Index { target, indices } => {
                // Try to get the base temp and offset from the target
                if let Some((base_temp, offset)) = self.try_get_base_and_offset(fb, target)? {
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

            // Recursive case: FieldAccess
            ast::ExprKind::FieldAccess { target, field } => {
                // Try to get the base temp and offset from the target
                if let Some((base_temp, offset)) = self.try_get_base_and_offset(fb, target)? {
                    // Get the target's type (should be a struct)
                    let target_ty = &self.get_node_type(target)?;
                    let field_offset = struct_field_byte_offset(target_ty, field).0;

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

    fn lower_expr_into_place(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        dest: &Place,
    ) -> Result<(), LowerError> {
        let dest_ir_ty = lower_type(&dest.ty);

        match &expr.kind {
            ast::ExprKind::TupleLit(fields) => self.lower_tuple_lit_into_place(fb, fields, dest),
            ast::ExprKind::ArrayLit(elems) => self.lower_array_lit_into_place(fb, elems, dest),
            ast::ExprKind::StructLit { fields, .. } => {
                self.lower_struct_lit_into_place(fb, fields, dest)
            }
            _ => {
                if dest_ir_ty.is_compound() {
                    // Best case: expression is already a const-addressable sub-place
                    if let Some(src_place) = self.try_place_of_expr(fb, expr)? {
                        debug_assert!(lower_type(&src_place.ty).is_compound());

                        if src_place.base == dest.base && src_place.byte_offset == dest.byte_offset
                        {
                            // no-op
                            return Ok(());
                        }

                        fb.mem_copy_with_offset(
                            dest.base,
                            src_place.base,
                            dest.byte_offset,
                            src_place.byte_offset,
                            dest_ir_ty.size_of(),
                        );
                        return Ok(());
                    }

                    // Fallback: materialize into a temp and copy
                    let src_op = self.lower_expr(fb, expr, None)?;
                    let IrOperand::Temp(src_temp) = src_op else {
                        return Err(LowerError::DestIsNotTemp(expr.id, src_op));
                    };
                    Self::emit_copy_compound_to(fb, dest, src_temp, dest_ir_ty.size_of());
                    Ok(())
                } else {
                    // Scalar: compute value and store at dest byte offset
                    let value_op = self.lower_expr(fb, expr, None)?;
                    let offset_op = fb.new_const_int(dest.byte_offset as i64, 64, false);
                    fb.store(dest.base, offset_op, value_op);
                    Ok(())
                }
            }
        }
    }

    fn lower_tuple_lit_into_place(
        &mut self,
        fb: &mut IrFunctionBuilder,
        fields: &[ast::Expr],
        dest: &Place,
    ) -> Result<(), LowerError> {
        for (i, field) in fields.iter().enumerate() {
            let field_ty = &self.get_node_type(field)?;
            let field_place = Place::tuple_field(dest, i, field_ty.clone());
            self.lower_expr_into_place(fb, field, &field_place)?;
        }
        Ok(())
    }

    fn lower_struct_lit_into_place(
        &mut self,
        fb: &mut IrFunctionBuilder,
        fields: &[ast::StructLitField],
        dest: &Place,
    ) -> Result<(), LowerError> {
        for field in fields {
            let field_ty = &self.get_node_type(&field.value)?;
            let field_place = Place::struct_field(dest, &field.name, field_ty.clone());
            self.lower_expr_into_place(fb, &field.value, &field_place)?;
        }
        Ok(())
    }

    fn lower_array_lit_into_place(
        &mut self,
        fb: &mut IrFunctionBuilder,
        elems: &[ast::Expr],
        dest: &Place,
    ) -> Result<(), LowerError> {
        let Type::Array { elem_ty, dims } = &dest.ty else {
            unreachable!()
        };

        // Type checker ensures shapes match, but debug assert to be safe
        debug_assert!(!dims.is_empty());
        debug_assert_eq!(elems.len(), dims[0]);

        // Type of one element at this dimension:
        // 1D: element is elem_ty (scalar/tuple/...)
        // nD: element is a sub-array with dims[1..]
        let sub_ty = if dims.len() == 1 {
            (**elem_ty).clone()
        } else {
            Type::Array {
                elem_ty: elem_ty.clone(),
                dims: dims[1..].to_vec(),
            }
        };

        // Byte size of one element at this dimension (scalar or compound)
        let sub_size = sub_ty.size_of();

        for (i, elem_expr) in elems.iter().enumerate() {
            let elem_offset = i * sub_size;
            let elem_place = Place::with_offset(dest, elem_offset, sub_ty.clone());
            self.lower_expr_into_place(fb, elem_expr, &elem_place)?;
        }
        Ok(())
    }

    fn lower_expr(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        match &expr.kind {
            ast::ExprKind::UInt64Lit(value) => Ok(fb.new_const_int(*value as i64, 64, false)),
            ast::ExprKind::BoolLit(value) => Ok(fb.new_const_bool(*value)),
            ast::ExprKind::UnitLit => Ok(fb.new_const_unit()),
            ast::ExprKind::BinOp { left, op, right } => self.lower_binary_op(fb, op, left, right),
            ast::ExprKind::UnaryOp { op, expr } => self.lower_unary_op(fb, op, expr),
            ast::ExprKind::Block(body) => self.lower_block_into(fb, expr.id, body, dest_temp),
            ast::ExprKind::Let { pattern, value, .. } => {
                self.lower_binding(fb, pattern, value, false)
            }
            ast::ExprKind::Var { pattern, value, .. } => {
                self.lower_binding(fb, pattern, value, true)
            }
            ast::ExprKind::Assign {
                value, assignee, ..
            } => self.lower_assign(fb, expr, assignee, value),
            ast::ExprKind::VarRef(_) => self.lower_var_ref(fb, expr, dest_temp),
            ast::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if(fb, cond, then_body, else_body, dest_temp),
            ast::ExprKind::While { cond, body } => self.lower_while(fb, cond, body),
            ast::ExprKind::Call { callee, args } => match &callee.kind {
                ast::ExprKind::VarRef(name) => {
                    self.lower_call(fb, expr, name.clone(), args, dest_temp)
                }
                _ => panic!("Unsupported callee: {:?}", callee.kind),
            },
            ast::ExprKind::ArrayLit(elems) => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_array_lit(fb, expr, elems, dest_temp)
                } else {
                    Err(LowerError::ArrayLitRequiresDestTemp(expr.id))
                }
            }
            ast::ExprKind::Index { target, indices } => self.lower_index(fb, expr, target, indices),
            ast::ExprKind::TupleLit(fields) => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_tuple_lit(fb, expr, fields, dest_temp, 0)
                } else {
                    Err(LowerError::TupleLitRequiresDestTemp(expr.id))
                }
            }
            ast::ExprKind::TupleFieldAccess { target, index } => {
                self.lower_tuple_field_access(fb, target, *index)
            }
            ast::ExprKind::StructLit { fields, .. } => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_struct_lit(fb, expr, fields, dest_temp, 0)
                } else {
                    Err(LowerError::StructLitRequiresDestTemp(expr.id))
                }
            }
            ast::ExprKind::FieldAccess { target, field } => {
                self.lower_struct_field_access(fb, target, field)
            }
        }
    }

    fn lower_binary_op(
        &mut self,
        fb: &mut IrFunctionBuilder,
        op: &ast::BinaryOp,
        left: &ast::Expr,
        right: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let result = fb.new_temp(lower_type(&self.get_node_type(left)?));
        let left_op = self.lower_expr(fb, left, None)?;
        let right_op = self.lower_expr(fb, right, None)?;
        fb.binary_op(result, *op, left_op, right_op);
        Ok(IrOperand::Temp(result))
    }

    fn lower_unary_op(
        &mut self,
        fb: &mut IrFunctionBuilder,
        op: &ast::UnaryOp,
        expr: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let result = fb.new_temp(lower_type(&self.get_node_type(expr)?));
        let expr_op = self.lower_expr(fb, expr, None)?;
        fb.unary_op(result, *op, expr_op);
        Ok(IrOperand::Temp(result))
    }

    fn lower_if(
        &mut self,
        fb: &mut IrFunctionBuilder,
        cond: &ast::Expr,
        then_body: &ast::Expr,
        else_body: &ast::Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        // Validate that the then and else bodies have the same type
        let then_type = self.get_node_type(then_body)?;
        let else_type = self.get_node_type(else_body)?;
        if then_type != else_type {
            return Err(LowerError::MismatchedBranchTypes(
                then_body.id,
                then_type,
                else_body.id,
                else_type,
            ));
        }

        // Create the required blocks
        let then_b = fb.new_block("then".to_string());
        let else_b = fb.new_block("else".to_string());
        let merge_b = fb.new_block("merge".to_string());

        // Build the conditional branch
        let cond = self.lower_expr(fb, cond, None)?;
        fb.terminate(IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        });

        // Then block
        fb.select_block(then_b);
        let then_op = match self.lower_expr(fb, then_body, dest_temp)? {
            IrOperand::Temp(temp) => temp,
            other => {
                // Materialize the constant to a temp (so that phi can use it)
                let temp = fb.new_temp(lower_type(&self.get_node_type(then_body)?));
                fb.move_to(temp, other);
                temp
            }
        };
        fb.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        fb.select_block(else_b);
        let else_op = match self.lower_expr(fb, else_body, dest_temp)? {
            IrOperand::Temp(temp) => temp,
            other => {
                // Materialize the constant to a temp (so that phi can use it)
                let temp = fb.new_temp(lower_type(&self.get_node_type(else_body)?));
                fb.move_to(temp, other);
                temp
            }
        };
        fb.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        fb.select_block(merge_b);
        if let Some(dest_temp) = dest_temp {
            Ok(IrOperand::Temp(dest_temp))
        } else {
            let merge_type = self.get_node_type(then_body)?;
            let merge_op = fb.new_temp(lower_type(&merge_type));
            fb.phi(merge_op, vec![(then_b, then_op), (else_b, else_op)]);
            Ok(IrOperand::Temp(merge_op))
        }
    }

    fn lower_while(
        &mut self,
        fb: &mut IrFunctionBuilder,
        cond: &ast::Expr,
        body: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let header_b = fb.new_block("loop_header".to_string());
        let body_b = fb.new_block("loop_body".to_string());
        let after_b = fb.new_block("loop_after".to_string());

        // Terminate current block
        fb.terminate(IrTerminator::Br { target: header_b });

        // Build the header block
        fb.select_block(header_b);
        let cond = self.lower_expr(fb, cond, None)?;
        fb.terminate(IrTerminator::CondBr {
            cond,
            then_b: body_b,
            else_b: after_b,
        });

        // Body block
        fb.select_block(body_b);
        self.lower_expr(fb, body, None)?;
        fb.terminate(IrTerminator::Br { target: header_b });

        // After block
        fb.select_block(after_b);

        Ok(fb.new_const_unit())
    }

    fn lower_call(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        name: String,
        args: &[ast::Expr],
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        let ret_ty = lower_type(&self.get_node_type(expr)?);

        // Lower the arguments
        let args = args
            .iter()
            .map(|arg| self.lower_expr(fb, arg, None))
            .collect::<Result<Vec<IrOperand>, LowerError>>()?;

        // Create the result temp
        let result = if let Some(dest_temp) = dest_temp {
            dest_temp
        } else {
            fb.new_temp(ret_ty.clone())
        };

        // Call the function
        fb.call(Some(result), name, args, ret_ty);

        Ok(IrOperand::Temp(result))
    }

    fn lower_block_into(
        &mut self,
        fb: &mut IrFunctionBuilder,
        id: NodeId,
        body: &[ast::Expr],
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        for expr in body.iter().take(body.len().saturating_sub(1)) {
            self.lower_expr(fb, expr, dest_temp)?;
        }
        match body.last() {
            Some(expr) => Ok(self.lower_expr(fb, expr, dest_temp)?),
            None => Err(LowerError::BlockEmpty(id)),
        }
    }

    fn get_node_type(&self, expr: &ast::Expr) -> Result<Type, LowerError> {
        self.ctx
            .type_map
            .lookup_node_type(expr.id)
            .ok_or(LowerError::NodeTypeNotFound(expr.id))
    }

    fn emit_load_scalar(fb: &mut IrFunctionBuilder, place: &Place, ir_ty: &IrType) -> IrOperand {
        let offset_op = fb.new_const_int(place.byte_offset as i64, 64, false);
        let result_temp = fb.new_temp(ir_ty.clone());
        fb.load(result_temp, place.base, offset_op);
        IrOperand::Temp(result_temp)
    }

    fn emit_copy_compound_from(
        fb: &mut IrFunctionBuilder,
        place: &Place,
        dest: IrTempId,
        bytes: usize,
    ) {
        fb.mem_copy_with_offset(dest, place.base, 0, place.byte_offset, bytes);
    }

    fn emit_copy_compound_to(
        fb: &mut IrFunctionBuilder,
        place: &Place,
        src: IrTempId,
        bytes: usize,
    ) {
        fb.mem_copy_with_offset(place.base, src, place.byte_offset, 0, bytes);
    }
}

pub fn lower(context: AnalyzedContext) -> Result<LoweredContext, LowerError> {
    let mut lowerer = Lowerer::new(&context);
    let mut ir_funcs = Vec::new();
    for func in context.module.funcs() {
        let ir_func = lowerer.lower_func(func)?;
        ir_funcs.push(ir_func);
    }
    Ok(context.with_ir_funcs(ir_funcs))
}

#[cfg(test)]
#[path = "tests/t_lower.rs"]
mod tests;
