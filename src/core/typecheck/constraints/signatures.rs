//! Function/method/closure signature collection helpers.

use super::*;
use crate::core::tree::{FunctionSig, Param, TypeExprKind};

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_closure_signature(
        &mut self,
        params: &[Param],
        return_ty: &TypeExpr,
    ) -> Option<ClosureSigInfo> {
        let mut fn_params = Vec::with_capacity(params.len());
        let mut param_tys = Vec::with_capacity(params.len());
        for param in params {
            let param_ty = if is_infer_type_expr(&param.typ) {
                self.fresh_var_term()
            } else {
                self.resolve_type_in_scope(&param.typ).ok()?
            };
            fn_params.push(crate::core::types::FnParam {
                mode: fn_param_mode(param.mode.clone()),
                ty: param_ty.clone(),
            });
            param_tys.push(param_ty);
        }

        let ret_ty = if is_infer_type_expr(return_ty) {
            self.fresh_var_term()
        } else {
            self.resolve_return_type_in_scope(return_ty).ok()?
        };
        let fn_ty = Type::Fn {
            params: fn_params,
            ret_ty: Box::new(ret_ty.clone()),
        };
        Some(ClosureSigInfo {
            fn_ty,
            param_tys,
            ret_ty,
        })
    }

    pub(super) fn collect_function_signature(&self, sig: &FunctionSig) -> Option<Type> {
        let params = self.resolve_fn_params(&sig.params)?;
        self.resolve_fn_type(params, &sig.ret_ty_expr)
    }

    pub(super) fn collect_method_signature(
        &self,
        type_name: &str,
        sig: &MethodSig,
    ) -> Option<Type> {
        let self_ty = self.type_defs.get(type_name).cloned()?;
        let tail_params = self.resolve_fn_params(&sig.params)?;
        let mut params = Vec::with_capacity(sig.params.len() + 1);
        params.push(crate::core::types::FnParam {
            mode: fn_param_mode(sig.self_param.mode.clone()),
            ty: self_ty,
        });
        params.extend(tail_params);
        self.resolve_fn_type(params, &sig.ret_ty_expr)
    }

    fn resolve_fn_params(&self, params: &[Param]) -> Option<Vec<crate::core::types::FnParam>> {
        params
            .iter()
            .map(|param| {
                self.resolve_type_in_scope(&param.typ)
                    .ok()
                    .map(|ty| crate::core::types::FnParam {
                        mode: fn_param_mode(param.mode.clone()),
                        ty,
                    })
            })
            .collect::<Option<Vec<_>>>()
    }

    fn resolve_fn_type(
        &self,
        params: Vec<crate::core::types::FnParam>,
        return_ty: &TypeExpr,
    ) -> Option<Type> {
        let ret_ty = self.resolve_return_type_in_scope(return_ty).ok()?;
        Some(Type::Fn {
            params,
            ret_ty: Box::new(ret_ty),
        })
    }
}

fn is_infer_type_expr(type_expr: &TypeExpr) -> bool {
    matches!(type_expr.kind, TypeExprKind::Infer)
}
