//! Call lowering.

use crate::diag::Span;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue};
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::{Callee, RuntimeFn, ValueId};
use crate::tree::{NodeId, semantic as sem};
use crate::types::Type;

struct CallInputValue {
    value: ValueId,
    ty: Type,
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    pub(super) fn lower_call_expr(
        &mut self,
        expr: &sem::ValueExpr,
        args: &[sem::CallArg],
    ) -> Result<LinearValue, LoweringError> {
        let call_plan = self
            .type_map
            .lookup_call_plan(expr.id)
            .unwrap_or_else(|| panic!("ssa lower_call_expr missing call plan {:?}", expr.id));

        // Direct calls (no receiver) only for now.
        if call_plan.has_receiver {
            panic!("ssa lower_call_expr expected no receiver for {:?}", expr.id);
        }

        // Resolve the callee (only direct calls supported).
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect => {
                return Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr));
            }
            sem::CallTarget::Intrinsic(intrinsic) => {
                Callee::Runtime(self.runtime_for_intrinsic(intrinsic)?)
            }
        };

        // Lower argument expressions.
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            match arg {
                sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                    let value = self.lower_value_expr_linear(expr)?;
                    let ty = self.type_map.type_table().get(expr.ty).clone();
                    arg_values.push(CallInputValue { value, ty });
                }
                sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                    let addr = self.lower_place_addr(place)?;
                    let ty = self.type_map.type_table().get(place.ty).clone();
                    arg_values.push(CallInputValue {
                        value: addr.addr,
                        ty,
                    });
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
        let call_plan = self.type_map.lookup_call_plan(expr.id).unwrap_or_else(|| {
            panic!("ssa lower_method_call_expr missing call plan {:?}", expr.id)
        });

        // Method calls must have a receiver.
        if !call_plan.has_receiver {
            panic!(
                "ssa lower_method_call_expr missing receiver for {:?}",
                expr.id
            );
        }

        // Resolve the callee (only direct calls supported).
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect => {
                return Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr));
            }
            sem::CallTarget::Intrinsic(intrinsic) => {
                Callee::Runtime(self.runtime_for_intrinsic(intrinsic)?)
            }
        };

        // Lower the receiver (value receivers only).
        let receiver_value = match receiver {
            sem::MethodReceiver::ValueExpr(expr) => {
                let value = self.lower_value_expr_linear(expr)?;
                let ty = self.type_map.type_table().get(expr.ty).clone();
                CallInputValue { value, ty }
            }
            sem::MethodReceiver::PlaceExpr(place) => {
                let addr = self.lower_place_addr(place)?;
                let ty = self.type_map.type_table().get(place.ty).clone();
                CallInputValue {
                    value: addr.addr,
                    ty,
                }
            }
        };

        // Lower argument expressions.
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            match arg {
                sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                    let value = self.lower_value_expr_linear(expr)?;
                    let ty = self.type_map.type_table().get(expr.ty).clone();
                    arg_values.push(CallInputValue { value, ty });
                }
                sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                    let addr = self.lower_place_addr(place)?;
                    let ty = self.type_map.type_table().get(place.ty).clone();
                    arg_values.push(CallInputValue {
                        value: addr.addr,
                        ty,
                    });
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
        &mut self,
        expr_id: NodeId,
        span: Span,
        call_plan: &sem::CallPlan,
        receiver_value: Option<CallInputValue>,
        arg_values: &[CallInputValue],
    ) -> Result<Vec<ValueId>, LoweringError> {
        if call_plan.has_receiver != receiver_value.is_some() {
            panic!(
                "ssa lower_call_args_from_plan receiver mismatch for {:?}",
                expr_id
            );
        }

        let mut call_args = Vec::with_capacity(call_plan.args.len());
        for lowering in &call_plan.args {
            match lowering {
                sem::ArgLowering::Direct(input) => {
                    let value = match input {
                        sem::CallInput::Receiver => {
                            receiver_value
                                .as_ref()
                                .unwrap_or_else(|| {
                                    panic!("ssa call plan missing receiver value for {:?}", expr_id)
                                })
                                .value
                        }
                        sem::CallInput::Arg(index) => {
                            arg_values
                                .get(*index)
                                .unwrap_or_else(|| {
                                    panic!("ssa call arg index out of range: {index}")
                                })
                                .value
                        }
                    };
                    call_args.push(value);
                }
                sem::ArgLowering::PtrLen { input, len_bits } => {
                    let input_value = match input {
                        sem::CallInput::Receiver => receiver_value.as_ref().unwrap_or_else(|| {
                            panic!("ssa call plan missing receiver value for {:?}", expr_id)
                        }),
                        sem::CallInput::Arg(index) => arg_values
                            .get(*index)
                            .unwrap_or_else(|| panic!("ssa call arg index out of range: {index}")),
                    };
                    let (ptr, len) = self.lower_ptr_len_from_value(
                        span,
                        input_value.value,
                        &input_value.ty,
                        *len_bits,
                    )?;
                    call_args.push(ptr);
                    call_args.push(len);
                }
            }
        }
        Ok(call_args)
    }

    fn runtime_for_intrinsic(
        &self,
        intrinsic: &sem::IntrinsicCall,
    ) -> Result<RuntimeFn, LoweringError> {
        match intrinsic {
            sem::IntrinsicCall::Print => Ok(RuntimeFn::Print),
            sem::IntrinsicCall::U64ToDec => Ok(RuntimeFn::U64ToDec),
            sem::IntrinsicCall::MemSet => Ok(RuntimeFn::MemSet),
            sem::IntrinsicCall::StringFromBytes => Ok(RuntimeFn::StringFromBytes),
            sem::IntrinsicCall::StringAppendBytes => Ok(RuntimeFn::StringAppendBytes),
        }
    }
}
