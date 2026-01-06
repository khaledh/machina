use crate::ast::{CallArg, CallArgMode, Expr, ExprKind, FunctionParamMode};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;
use crate::types::{Type, array_to_slice_assignable};

impl<'a> FuncLowerer<'a> {
    // --- Calls ---

    /// Lower a call expression and return the produced value.
    pub(super) fn lower_call_expr(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[CallArg],
    ) -> Result<ExprValue, LowerError> {
        let result_ty = self.ty_for_node(call.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty == Type::Unit {
            self.lower_call_into(None, call, callee, args)?;
            return Ok(ExprValue::Scalar(Operand::Const(Const::Unit)));
        }

        if result_ty.is_scalar() {
            // Scalar call: capture result into a temp.
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.lower_call_into(
                Some(PlaceAny::Scalar(temp_place.clone())),
                call,
                callee,
                args,
            )?;
            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate call: capture result into a temp place.
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.lower_call_into(
                Some(PlaceAny::Aggregate(temp_place.clone())),
                call,
                callee,
                args,
            )?;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    pub(super) fn lower_method_call_expr(
        &mut self,
        call: &Expr,
        target: &Expr,
        args: &[CallArg],
    ) -> Result<ExprValue, LowerError> {
        let self_mode = self
            .ctx
            .type_map
            .lookup_call_sig(call.id)
            .and_then(|sig| sig.param_modes.first().cloned());
        let self_arg_mode = match self_mode {
            Some(FunctionParamMode::Sink) => CallArgMode::Move,
            _ => CallArgMode::Default,
        };

        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(CallArg {
            mode: self_arg_mode,
            expr: target.clone(),
            span: target.span,
        });
        call_args.extend(args.iter().cloned());

        self.lower_call_expr(call, target, &call_args)
    }

    /// Lower a call into the given destination place.
    pub(super) fn lower_call_into(
        &mut self,
        dst: Option<PlaceAny>,
        call: &Expr,
        callee: &Expr,
        args: &[CallArg],
    ) -> Result<(), LowerError> {
        let callee = match self.ctx.type_map.lookup_call_def(call.id) {
            Some(def_id) => Callee::Def(def_id),
            None => {
                let callee_def = self.def_for_node(callee.id)?;
                Callee::Def(callee_def.id)
            }
        };

        if let Callee::Runtime(runtime_fn) = &callee
            && runtime_fn.sig().arg_count != args.len() as u8
        {
            panic!(
                concat!(
                    "compiler bug: runtime func {} takes {} arguments, but {} were provided. ",
                    "This should have been caught by the type checker."
                ),
                runtime_fn.sig().name,
                runtime_fn.sig().arg_count,
                args.len()
            );
        }

        let mut out_args = Vec::new();
        let call_sig = self.ctx.type_map.lookup_call_sig(call.id);
        let arg_vals = if let Some(call_sig) = &call_sig {
            let mut vals = Vec::with_capacity(args.len());
            for (idx, (mode, arg)) in call_sig.param_modes.iter().zip(args).enumerate() {
                let arg_expr = &arg.expr;
                if *mode == FunctionParamMode::Out {
                    // Out args are write-only; skip drop only when the call is the first init.
                    let place = self.lower_place(arg_expr)?;
                    let arg_ty = self.ty_for_node(arg_expr.id)?;
                    if !self.ctx.init_assigns.contains(&arg_expr.id) {
                        self.emit_overwrite_drop(arg_expr, place.clone(), &arg_ty, true);
                    }
                    out_args.push(arg_expr);
                    vals.push(place);
                } else {
                    if let Some(param_ty) = call_sig.param_types.get(idx)
                        && matches!(arg.mode, CallArgMode::Default | CallArgMode::Inout)
                        && self.coerce_array_to_slice(arg_expr, param_ty)?
                    {
                        vals.push(self.lower_call_array_as_slice(arg_expr, param_ty)?);
                    } else {
                        vals.push(self.lower_call_arg_place(arg_expr)?);
                    }
                }
                if *mode == FunctionParamMode::Sink && arg.mode == CallArgMode::Move {
                    self.record_move(arg_expr);
                }
            }
            vals
        } else {
            args.iter()
                .map(|a| self.lower_call_arg_place(&a.expr))
                .collect::<Result<Vec<_>, _>>()?
        };

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst,
                callee,
                args: arg_vals,
            },
        );
        for arg in out_args {
            // Mark out args as initialized after the call.
            self.mark_initialized_if_needed(arg);
            self.mark_full_init_if_needed(arg);
        }
        Ok(())
    }

    /// Lower a call argument into a place (or temp if needed).
    pub(super) fn lower_call_arg_place(&mut self, arg: &Expr) -> Result<PlaceAny, LowerError> {
        if matches!(arg.kind, ExprKind::Var(_)) && self.ctx.implicit_moves.contains(&arg.id) {
            // Implicitly moved heap args should skip caller drops.
            self.record_move(arg);
        }
        let ty = self.ty_for_node(arg.id)?;
        let ty_id = self.ty_lowerer.lower_ty(&ty);

        if ty.is_scalar() {
            // Scalar arg: prefer a place, otherwise spill to temp.
            if let Ok(place) = self.lower_place_scalar(arg) {
                return Ok(PlaceAny::Scalar(place));
            }

            // Otherwise, evaluate to operand and spill into a temp.
            let op = self.lower_scalar_expr(arg)?;
            let temp_place = self.new_temp_scalar(ty_id);
            self.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
            Ok(PlaceAny::Scalar(temp_place))
        } else {
            // Aggregate arg: prefer a place, otherwise lower into a temp.
            if let Ok(place) = self.lower_place_agg(arg) {
                Ok(PlaceAny::Aggregate(place))
            } else {
                Ok(PlaceAny::Aggregate(self.lower_agg_expr_to_temp(arg)?))
            }
        }
    }

    fn coerce_array_to_slice(&mut self, arg: &Expr, param_ty: &Type) -> Result<bool, LowerError> {
        let arg_ty = self.ty_for_node(arg.id)?;
        Ok(array_to_slice_assignable(&arg_ty, param_ty))
    }

    fn lower_call_array_as_slice(
        &mut self,
        arg: &Expr,
        param_ty: &Type,
    ) -> Result<PlaceAny, LowerError> {
        let ty_id = self.ty_lowerer.lower_ty(param_ty);
        let temp_place = self.new_temp_aggregate(ty_id);
        let start: Option<Box<Expr>> = None;
        let end: Option<Box<Expr>> = None;
        self.lower_slice_into(&temp_place, arg, &start, &end)?;
        Ok(PlaceAny::Aggregate(temp_place))
    }
}
