//! Call lowering.

use crate::diag::Span;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue};
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::{Callee, ValueId};
use crate::tree::{NodeId, semantic as sem};

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_call_expr(
        &mut self,
        expr: &sem::ValueExpr,
        args: &[sem::CallArg],
    ) -> Result<LinearValue, LoweringError> {
        let call_plan = self
            .type_map
            .lookup_call_plan(expr.id)
            .ok_or_else(|| self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))?;

        // Direct calls (no receiver) only for now.
        if call_plan.has_receiver {
            return Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr));
        }

        // Resolve the callee (only direct calls supported).
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect | sem::CallTarget::Intrinsic(_) => {
                return Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr));
            }
        };

        // Lower argument expressions.
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            match arg {
                sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                    arg_values.push(self.lower_value_expr_linear(expr)?);
                }
                sem::CallArg::InOut { span, .. } | sem::CallArg::Out { span, .. } => {
                    return Err(self.err_span(*span, LoweringErrorKind::UnsupportedExpr));
                }
            }
        }

        // Apply the call plan to reorder/transform arguments.
        let call_args =
            self.lower_call_args_from_plan(expr.id, expr.span, &call_plan, None, &arg_values)?;
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self.builder.call(callee, call_args, ty))
    }

    pub(super) fn lower_method_call_expr(
        &mut self,
        expr: &sem::ValueExpr,
        receiver: &sem::MethodReceiver,
        args: &[sem::CallArg],
    ) -> Result<LinearValue, LoweringError> {
        let call_plan = self
            .type_map
            .lookup_call_plan(expr.id)
            .ok_or_else(|| self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))?;

        // Method calls must have a receiver.
        if !call_plan.has_receiver {
            return Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr));
        }

        // Resolve the callee (only direct calls supported).
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect | sem::CallTarget::Intrinsic(_) => {
                return Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr));
            }
        };

        // Lower the receiver (value receivers only).
        let receiver_value = match receiver {
            sem::MethodReceiver::ValueExpr(expr) => self.lower_value_expr_linear(expr)?,
            sem::MethodReceiver::PlaceExpr(place) => {
                return Err(self.err_span(place.span, LoweringErrorKind::UnsupportedExpr));
            }
        };

        // Lower argument expressions.
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            match arg {
                sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                    arg_values.push(self.lower_value_expr_linear(expr)?);
                }
                sem::CallArg::InOut { span, .. } | sem::CallArg::Out { span, .. } => {
                    return Err(self.err_span(*span, LoweringErrorKind::UnsupportedExpr));
                }
            }
        }

        // Apply the call plan to build the final argument list.
        let call_args = self.lower_call_args_from_plan(
            expr.id,
            expr.span,
            &call_plan,
            Some(receiver_value),
            &arg_values,
        )?;
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        Ok(self.builder.call(callee, call_args, ty))
    }

    fn lower_call_args_from_plan(
        &self,
        expr_id: NodeId,
        span: Span,
        call_plan: &sem::CallPlan,
        receiver_value: Option<ValueId>,
        arg_values: &[ValueId],
    ) -> Result<Vec<ValueId>, LoweringError> {
        if call_plan.has_receiver != receiver_value.is_some() {
            return Err(self.err_span(span, LoweringErrorKind::UnsupportedExpr));
        }

        let mut call_args = Vec::with_capacity(call_plan.args.len());
        for lowering in &call_plan.args {
            match lowering {
                sem::ArgLowering::Direct(input) => {
                    let value = match input {
                        sem::CallInput::Receiver => receiver_value.unwrap_or_else(|| {
                            panic!("ssa call plan missing receiver value for {:?}", expr_id)
                        }),
                        sem::CallInput::Arg(index) => *arg_values
                            .get(*index)
                            .unwrap_or_else(|| panic!("ssa call arg index out of range: {index}")),
                    };
                    call_args.push(value);
                }
                sem::ArgLowering::PtrLen { .. } => {
                    return Err(self.err_span(span, LoweringErrorKind::UnsupportedExpr));
                }
            }
        }
        Ok(call_args)
    }
}
