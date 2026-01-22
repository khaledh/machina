//! Call lowering.

use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue};
use crate::ssa::model::ir::{Callee, ValueId};
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_call_expr(
        &mut self,
        expr: &sem::LinearExpr,
        args: &[sem::LinearExpr],
    ) -> Result<LinearValue, sem::LinearizeError> {
        let call_plan = self
            .type_map
            .lookup_call_plan(expr.id)
            .ok_or_else(|| self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))?;

        // Direct calls (no receiver) only for now.
        if call_plan.has_receiver {
            return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
        }

        // Resolve the callee (only direct calls supported).
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect | sem::CallTarget::Intrinsic(_) => {
                return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
            }
        };

        // Lower argument expressions.
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            arg_values.push(self.lower_linear_expr(arg)?);
        }

        // Apply the call plan to reorder/transform arguments.
        let call_args = self.lower_call_args_from_plan(expr, &call_plan, None, &arg_values)?;
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self.builder.call(callee, call_args, ty))
    }

    pub(super) fn lower_method_call_expr(
        &mut self,
        expr: &sem::LinearExpr,
        receiver: &sem::LinearMethodReceiver,
        args: &[sem::LinearExpr],
    ) -> Result<LinearValue, sem::LinearizeError> {
        let call_plan = self
            .type_map
            .lookup_call_plan(expr.id)
            .ok_or_else(|| self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))?;

        // Method calls must have a receiver.
        if !call_plan.has_receiver {
            return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
        }

        // Resolve the callee (only direct calls supported).
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect | sem::CallTarget::Intrinsic(_) => {
                return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
            }
        };

        // Lower the receiver (value receivers only).
        let receiver_value = match receiver {
            sem::LinearMethodReceiver::Value(expr) => self.lower_linear_expr(expr)?,
            sem::LinearMethodReceiver::Place(_) => {
                return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
            }
        };

        // Lower argument expressions.
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            arg_values.push(self.lower_linear_expr(arg)?);
        }

        // Apply the call plan to build the final argument list.
        let call_args =
            self.lower_call_args_from_plan(expr, &call_plan, Some(receiver_value), &arg_values)?;
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self.builder.call(callee, call_args, ty))
    }

    fn lower_call_args_from_plan(
        &self,
        expr: &sem::LinearExpr,
        call_plan: &sem::CallPlan,
        receiver_value: Option<ValueId>,
        arg_values: &[ValueId],
    ) -> Result<Vec<ValueId>, sem::LinearizeError> {
        if call_plan.has_receiver != receiver_value.is_some() {
            return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
        }

        let mut call_args = Vec::with_capacity(call_plan.args.len());
        for lowering in &call_plan.args {
            match lowering {
                sem::ArgLowering::Direct(input) => {
                    let value = match input {
                        sem::CallInput::Receiver => receiver_value.unwrap_or_else(|| {
                            panic!("ssa call plan missing receiver value for {:?}", expr.id)
                        }),
                        sem::CallInput::Arg(index) => *arg_values
                            .get(*index)
                            .unwrap_or_else(|| panic!("ssa call arg index out of range: {index}")),
                    };
                    call_args.push(value);
                }
                sem::ArgLowering::PtrLen { .. } => {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }
            }
        }
        Ok(call_args)
    }
}
