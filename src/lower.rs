use std::collections::HashMap;
use thiserror::Error;

use crate::ast;
use crate::context::{AnalyzedContext, LoweredContext};
use crate::ids::{DefId, NodeId};
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrFunction, IrOperand, IrTempId, IrTerminator, IrType};
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
    }
}

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

        let ret_ty = lower_type(&func.return_type);
        let mut fb = IrFunctionBuilder::new(func.name.clone(), ret_ty.clone());

        // lower params and store them in the def_temp map
        for (i, param) in func.params.iter().enumerate() {
            match self.ctx.def_map.lookup_def(param.id) {
                Some(def) => {
                    let param_temp =
                        fb.new_param(i as u32, param.name.clone(), lower_type(&param.typ));
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
            ast::Pattern::Array { id, patterns, .. } => {
                let base_array = self.get_array_temp(value_op, *id)?;
                self.lower_array_pattern(fb, id, patterns, base_array, 0, value_ty, is_mutable)
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
            // must be a temp
            let IrOperand::Temp(temp) = value_op else {
                return Err(LowerError::DestIsNotTemp(*id, value_op));
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

    fn lower_array_pattern(
        &mut self,
        fb: &mut IrFunctionBuilder,
        id: &NodeId,
        patterns: &[ast::Pattern],
        base_array: IrTempId,
        base_offset: usize,
        value_ty: &Type,
        is_mutable: bool,
    ) -> Result<(), LowerError> {
        // Get array type info
        let (elem_ty, dims) = match value_ty {
            Type::Array { elem_ty, dims } => (elem_ty, dims),
            _ => return Err(LowerError::ArrayIsNotTemp(*id, IrOperand::Temp(base_array))),
        };

        // Determine the sub-type for each pattern element
        let sub_ty = if dims.len() == 1 {
            // 1D array: elements are scalars
            (**elem_ty).clone()
        } else {
            // Multi-dim array: elements are sub-arrays
            Type::Array {
                elem_ty: elem_ty.clone(),
                dims: dims[1..].to_vec(),
            }
        };

        let ir_sub_ty = lower_type(&sub_ty);
        let is_compound = ir_sub_ty.is_compound();
        let elem_size = lower_type(&elem_ty).size_of();
        let stride: usize = dims[1..].iter().product();

        // Extract each element and recursively lower the sub-pattern
        for (i, pattern) in patterns.iter().enumerate() {
            let src_offset = base_offset + i * stride * elem_size;

            match pattern {
                ast::Pattern::Ident { id, name, .. } => {
                    // Binding to a variable, need to create a temp
                    let elem_op = if is_compound {
                        // mem copy from base array at offset
                        let sub_array_temp = fb.new_temp(ir_sub_ty.clone());
                        let copy_length = stride * elem_size;
                        fb.mem_copy_with_offset(
                            sub_array_temp,
                            base_array,
                            0,
                            src_offset,
                            copy_length,
                        );
                        IrOperand::Temp(sub_array_temp)
                    } else {
                        // scalar element, just load it
                        let elem_index = src_offset / elem_size;
                        let offset_op = fb.new_const_int(elem_index as i64, 64, false);
                        let elem_temp = fb.new_temp(ir_sub_ty.clone());
                        fb.load_element(elem_temp, base_array, offset_op);
                        IrOperand::Temp(elem_temp)
                    };

                    self.lower_ident_pattern(fb, id, name.clone(), elem_op, is_mutable)?;
                }
                ast::Pattern::Array { id, patterns, .. } => {
                    // Nested array pattern, recurse with adjusted offset
                    self.lower_array_pattern(
                        fb, id, patterns, base_array, src_offset, &sub_ty, is_mutable,
                    )?;
                }
            }
        }
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

        // Lower the value expression
        let dest_temp = if value_ir_ty.is_compound() {
            Some(fb.new_temp(value_ir_ty))
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
                let array_op = self.lower_expr(fb, target, None)?;
                let array_temp = self.get_array_temp(array_op, expr.id)?;

                // Get array dimensions
                let array_ty = lower_type(&self.get_node_type(target)?);
                let dims = self.get_array_dims(array_ty, expr.id)?;

                // Calculate linear offset
                let offset_op = self.calc_array_offset(fb, &dims, indices)?;

                // Store the value at the calculated offset
                // TODO: Handle compound value as array element
                let value_op = self.lower_expr(fb, value, None)?;
                fb.store_element(array_temp, offset_op, value_op);
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
        elems: &[ast::Expr],
        dest_temp: IrTempId,
    ) -> Result<IrOperand, LowerError> {
        // Flatten and store all elements recursively
        self.lower_array_lit_recursive(fb, elems, dest_temp, 0)?;
        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_array_lit_recursive(
        &mut self,
        fb: &mut IrFunctionBuilder,
        elems: &[ast::Expr],
        dest_temp: IrTempId,
        base_offset: usize,
    ) -> Result<usize, LowerError> {
        let mut current_offset = base_offset;

        for elem in elems {
            match &elem.kind {
                ast::ExprKind::ArrayLit(inner_elems) => {
                    // Recursively flatten nested array
                    current_offset =
                        self.lower_array_lit_recursive(fb, inner_elems, dest_temp, current_offset)?;
                }
                _ => {
                    // Store the scalar element at the current offset
                    let elem_op = self.lower_expr(fb, elem, None)?;
                    let index_op = fb.new_const_int(current_offset as i64, 64, false);
                    fb.store_element(dest_temp, index_op, elem_op);
                    current_offset += 1;
                }
            }
        }

        Ok(current_offset)
    }

    // Helper to extract array temp from an operand
    fn get_array_temp(&self, op: IrOperand, expr_id: NodeId) -> Result<IrTempId, LowerError> {
        match op {
            IrOperand::Temp(temp) => Ok(temp),
            _ => Err(LowerError::ArrayIsNotTemp(expr_id, op)),
        }
    }

    // Helper to extract array dimensions from a type
    fn get_array_dims(&self, ty: IrType, expr_id: NodeId) -> Result<Vec<usize>, LowerError> {
        match ty {
            IrType::Array { dims, .. } => Ok(dims),
            _ => Err(LowerError::IndexOnNonArray(expr_id, ty)),
        }
    }

    // Try to constant fold the offset if all indices are constants
    fn try_const_fold_offset(&self, dims: &[usize], indices: &[ast::Expr]) -> Option<usize> {
        // Extract constant values from all indices
        let const_indices: Vec<usize> = indices
            .iter()
            .map(|idx| {
                if let ast::ExprKind::UInt64Lit(val) = idx.kind {
                    Some(val as usize)
                } else {
                    None
                }
            })
            .collect::<Option<Vec<_>>>()?;

        // Compute offset: i0 * (d1 * d2 * ... * dn) + i1 * (d2 * d3 * ... * dn) + ... + in
        let mut offset = 0;
        for (i, &idx) in const_indices.iter().enumerate() {
            let stride: usize = dims[i + 1..].iter().product();
            offset += idx * stride;
        }

        Some(offset)
    }

    fn calc_array_offset(
        &mut self,
        fb: &mut IrFunctionBuilder,
        dims: &[usize],
        indices: &[ast::Expr],
    ) -> Result<IrOperand, LowerError> {
        // Try constant folding first
        if let Some(const_offset) = self.try_const_fold_offset(dims, indices) {
            return Ok(fb.new_const_int(const_offset as i64, 64, false));
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

    fn lower_index(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        target: &ast::Expr,
        indices: &[ast::Expr],
    ) -> Result<IrOperand, LowerError> {
        let array_op = self.lower_expr(fb, target, None)?;
        let array_temp = self.get_array_temp(array_op, expr.id)?;

        // Get array dimensions from the target type
        let array_ty = lower_type(&self.get_node_type(target)?);
        let dims = self.get_array_dims(array_ty, expr.id)?;

        // Calculate linear offset
        let offset_op = self.calc_array_offset(fb, &dims, indices)?;

        // Load the element at the calculated offset
        let result_ty = lower_type(&self.get_node_type(expr)?);
        let result = fb.new_temp(result_ty);
        fb.load_element(result, array_temp, offset_op);

        Ok(IrOperand::Temp(result))
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
            ast::ExprKind::Let { pattern, value } => self.lower_binding(fb, pattern, value, false),
            ast::ExprKind::Var { pattern, value } => self.lower_binding(fb, pattern, value, true),
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
                    self.lower_array_lit(fb, elems, dest_temp)
                } else {
                    Err(LowerError::ArrayLitRequiresDestTemp(expr.id))
                }
            }
            ast::ExprKind::Index { target, indices } => self.lower_index(fb, expr, target, indices),
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
}

pub fn lower(context: AnalyzedContext) -> Result<LoweredContext, LowerError> {
    let mut lowerer = Lowerer::new(&context);
    let mut ir_funcs = Vec::new();
    for func in &context.module.funcs {
        let ir_func = lowerer.lower_func(func)?;
        ir_funcs.push(ir_func);
    }
    Ok(context.with_ir_funcs(ir_funcs))
}

#[cfg(test)]
#[path = "tests/t_lower.rs"]
mod tests;
