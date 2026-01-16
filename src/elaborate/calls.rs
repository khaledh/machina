use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{CallArgMode, NodeId, ParamMode};
use crate::typeck::type_map::{CallParam, CallSig};

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    pub(super) fn call_sig(&self, call_id: NodeId) -> CallSig {
        // Type checker should have recorded a signature for every call.
        self.type_map
            .lookup_call_sig(call_id)
            .unwrap_or_else(|| panic!("compiler bug: missing call signature for {call_id:?}"))
    }

    pub(super) fn elab_call_arg(&mut self, param: &CallParam, arg: &norm::CallArg) -> sem::CallArg {
        self.elab_call_arg_mode(param.mode.clone(), arg)
    }

    pub(super) fn elab_call_arg_mode(
        &mut self,
        mode: ParamMode,
        arg: &norm::CallArg,
    ) -> sem::CallArg {
        // Convert argument passing mode into the explicit semantic form.
        match mode {
            ParamMode::In => sem::CallArg::In {
                expr: self.elab_value(&arg.expr),
                span: arg.span,
            },
            ParamMode::InOut => {
                if matches!(arg.expr.kind, norm::ExprKind::Slice { .. }) {
                    return sem::CallArg::In {
                        expr: self.elab_value(&arg.expr),
                        span: arg.span,
                    };
                }
                sem::CallArg::InOut {
                    place: self.elab_place(&arg.expr),
                    span: arg.span,
                }
            }
            ParamMode::Out => {
                let place = self.elab_place(&arg.expr);
                sem::CallArg::Out {
                    init: self.init_info_for_id(place.id),
                    place,
                    span: arg.span,
                }
            }
            ParamMode::Sink => {
                let expr = if arg.mode == CallArgMode::Move {
                    let place = self.elab_place(&arg.expr);
                    self.new_value(
                        sem::ValueExprKind::Move {
                            place: Box::new(place),
                        },
                        arg.expr.ty,
                        arg.expr.span,
                    )
                } else {
                    self.elab_value(&arg.expr)
                };
                sem::CallArg::Sink {
                    expr,
                    span: arg.span,
                }
            }
        }
    }

    pub(super) fn elab_method_receiver(
        &mut self,
        receiver: &CallParam,
        callee: &norm::Expr,
    ) -> sem::MethodReceiver {
        match receiver.mode {
            ParamMode::InOut => sem::MethodReceiver::PlaceExpr(self.elab_place(callee)),
            _ => sem::MethodReceiver::ValueExpr(Box::new(self.elab_value(callee))),
        }
    }
}
