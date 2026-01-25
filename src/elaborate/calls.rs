//! Call elaboration and planning.
//!
//! This module pre-computes how each function/method call should be lowered.
//! The `CallPlan` captures:
//!
//! - **Target**: Direct call, indirect call, or intrinsic dispatch
//! - **Argument lowering**: How each argument maps to call inputs (direct pass,
//!   pointer+length split for slices, etc.)
//! - **Drop mask**: Which inputs need drop calls after the call returns
//! - **Receiver handling**: Whether there's a `self` receiver and its mode
//!
//! By computing this plan during elaboration, lowering can emit call code
//! without re-examining signatures or type information.

use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{CallArgMode, NodeId, ParamMode};
use crate::typeck::type_map::{CallParam, CallSig};
use crate::types::Type;

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    /// Build a `CallPlan` that describes how to lower a call site.
    ///
    /// The plan includes target dispatch (direct/indirect/intrinsic),
    /// argument lowering strategy, and post-call drop requirements.
    pub(super) fn build_call_plan(&mut self, call_id: NodeId, call_sig: &CallSig) -> sem::CallPlan {
        let def_id = call_sig.def_id;
        let mut target = def_id
            .map(sem::CallTarget::Direct)
            .unwrap_or(sem::CallTarget::Indirect);

        if let Some(def_id) = def_id {
            let def = self
                .def_table
                .lookup_def(def_id)
                .unwrap_or_else(|| panic!("compiler bug: missing def for call {call_id:?}"));
            // Intrinsics override the normal direct-call target with a lowering intent.
            if def.is_intrinsic() {
                let intrinsic_name = def.link_name().unwrap_or(def.name.as_str());
                match intrinsic_name {
                    "len" => {
                        if matches!(
                            call_sig.receiver.as_ref().map(|recv| &recv.ty),
                            Some(Type::String)
                        ) {
                            target = sem::CallTarget::Intrinsic(sem::IntrinsicCall::StringLen);
                        }
                    }
                    _ => {}
                }
            } else if def.is_runtime() {
                let runtime_name = def.link_name().unwrap_or(def.name.as_str());
                target = match runtime_name {
                    "__rt_print" => sem::CallTarget::Runtime(sem::RuntimeCall::Print),
                    "__rt_u64_to_dec" => sem::CallTarget::Runtime(sem::RuntimeCall::U64ToDec),
                    "__rt_memset" => sem::CallTarget::Runtime(sem::RuntimeCall::MemSet),
                    "__rt_string_from_bytes" => {
                        sem::CallTarget::Runtime(sem::RuntimeCall::StringFromBytes)
                    }
                    "__rt_string_append_bytes" => {
                        sem::CallTarget::Runtime(sem::RuntimeCall::StringAppendBytes)
                    }
                    "append" | "append_bytes"
                        if matches!(
                            call_sig.receiver.as_ref().map(|recv| &recv.ty),
                            Some(Type::String)
                        ) =>
                    {
                        sem::CallTarget::Runtime(sem::RuntimeCall::StringAppendBytes)
                    }
                    _ => target,
                };
            }
        }

        let has_receiver = call_sig.receiver.is_some();
        let mut drop_mask = Vec::new();
        if let Some(receiver) = &call_sig.receiver {
            // Receiver sits at input index 0 when present.
            drop_mask.push(receiver.mode == ParamMode::In && receiver.ty.needs_drop());
        }
        for param in &call_sig.params {
            // Non-receiver inputs follow in order.
            drop_mask.push(param.mode == ParamMode::In && param.ty.needs_drop());
        }

        let args = match target {
            sem::CallTarget::Runtime(sem::RuntimeCall::Print) => {
                if has_receiver {
                    panic!("compiler bug: intrinsic print has receiver");
                }
                if call_sig.params.len() != 2 {
                    panic!(
                        "compiler bug: intrinsic print expects 2 args, got {}",
                        call_sig.params.len()
                    );
                }
                vec![
                    sem::ArgLowering::PtrLen {
                        input: sem::CallInput::Arg(0),
                        len_bits: 32,
                    },
                    sem::ArgLowering::Direct(sem::CallInput::Arg(1)),
                ]
            }
            sem::CallTarget::Runtime(sem::RuntimeCall::U64ToDec) => {
                if has_receiver {
                    panic!("compiler bug: intrinsic u64_to_dec has receiver");
                }
                if call_sig.params.len() != 2 {
                    panic!(
                        "compiler bug: intrinsic u64_to_dec expects 2 args, got {}",
                        call_sig.params.len()
                    );
                }
                vec![
                    sem::ArgLowering::PtrLen {
                        input: sem::CallInput::Arg(0),
                        len_bits: 64,
                    },
                    sem::ArgLowering::Direct(sem::CallInput::Arg(1)),
                ]
            }
            sem::CallTarget::Runtime(sem::RuntimeCall::MemSet) => {
                if has_receiver {
                    panic!("compiler bug: intrinsic memset has receiver");
                }
                if call_sig.params.len() != 2 {
                    panic!(
                        "compiler bug: intrinsic memset expects 2 args, got {}",
                        call_sig.params.len()
                    );
                }
                vec![
                    sem::ArgLowering::PtrLen {
                        input: sem::CallInput::Arg(0),
                        len_bits: 64,
                    },
                    sem::ArgLowering::Direct(sem::CallInput::Arg(1)),
                ]
            }
            sem::CallTarget::Runtime(sem::RuntimeCall::StringFromBytes) => {
                if has_receiver {
                    panic!("compiler bug: intrinsic string_from_bytes has receiver");
                }
                if call_sig.params.len() != 2 {
                    panic!(
                        "compiler bug: intrinsic string_from_bytes expects 2 args, got {}",
                        call_sig.params.len()
                    );
                }
                vec![
                    sem::ArgLowering::Direct(sem::CallInput::Arg(0)),
                    sem::ArgLowering::PtrLen {
                        input: sem::CallInput::Arg(1),
                        len_bits: 64,
                    },
                ]
            }
            sem::CallTarget::Runtime(sem::RuntimeCall::StringAppendBytes) => {
                if !has_receiver {
                    panic!("compiler bug: intrinsic string append missing receiver");
                }
                if call_sig.params.len() != 1 {
                    panic!(
                        "compiler bug: intrinsic string append expects 1 arg, got {}",
                        call_sig.params.len()
                    );
                }
                // Pre-decide whether the length comes from a u32 (string) or u64 (slice).
                let len_bits = match &call_sig.params[0].ty {
                    Type::String => 32,
                    Type::Slice { elem_ty }
                        if matches!(
                            **elem_ty,
                            Type::Int {
                                signed: false,
                                bits: 8
                            }
                        ) =>
                    {
                        64
                    }
                    _ => panic!("compiler bug: invalid intrinsic param type"),
                };
                vec![
                    sem::ArgLowering::Direct(sem::CallInput::Receiver),
                    sem::ArgLowering::PtrLen {
                        input: sem::CallInput::Arg(0),
                        len_bits,
                    },
                ]
            }
            sem::CallTarget::Intrinsic(sem::IntrinsicCall::StringLen) => {
                if !has_receiver {
                    panic!("compiler bug: intrinsic string len missing receiver");
                }
                if !call_sig.params.is_empty() {
                    panic!(
                        "compiler bug: intrinsic string len expects 0 args, got {}",
                        call_sig.params.len()
                    );
                }
                vec![sem::ArgLowering::Direct(sem::CallInput::Receiver)]
            }
            _ => {
                // Default lowering passes inputs straight through in ABI order.
                let mut args = Vec::new();
                if has_receiver {
                    args.push(sem::ArgLowering::Direct(sem::CallInput::Receiver));
                }
                for index in 0..call_sig.params.len() {
                    args.push(sem::ArgLowering::Direct(sem::CallInput::Arg(index)));
                }
                args
            }
        };

        sem::CallPlan {
            target,
            args,
            drop_mask,
            has_receiver,
        }
    }

    pub(super) fn get_call_sig(&self, call_id: NodeId) -> CallSig {
        // Type checker should have recorded a signature for every call.
        self.call_sigs
            .get(&call_id)
            .cloned()
            .unwrap_or_else(|| panic!("compiler bug: missing call signature for {call_id:?}"))
    }

    /// Elaborate a call argument using the parameter's passing mode.
    pub(super) fn elab_call_arg(&mut self, param: &CallParam, arg: &norm::CallArg) -> sem::CallArg {
        self.elab_call_arg_mode(param.mode.clone(), arg)
    }

    /// Convert a call argument into its semantic form based on passing mode.
    ///
    /// - `In`: pass by value (elaborated as a value expression)
    /// - `InOut`: pass by mutable reference (elaborated as a place)
    /// - `Out`: uninitialized output (elaborated as a place with init info)
    /// - `Sink`: transfer ownership (may wrap in explicit move)
    pub(super) fn elab_call_arg_mode(
        &mut self,
        mode: ParamMode,
        arg: &norm::CallArg,
    ) -> sem::CallArg {
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
