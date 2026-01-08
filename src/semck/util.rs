use crate::ast::{CallableRef, Expr, FunctionSig, MethodSig, ParamMode};
use crate::context::TypeCheckedContext;

pub(super) enum CallSig<'a> {
    Function(&'a FunctionSig),
    Method(&'a MethodSig),
}

impl<'a> CallSig<'a> {
    pub(super) fn params(&self) -> &'a [crate::ast::Param] {
        match self {
            CallSig::Function(sig) => &sig.params,
            CallSig::Method(sig) => &sig.params,
        }
    }

    pub(super) fn self_mode(&self) -> Option<ParamMode> {
        match self {
            CallSig::Function(_) => None,
            CallSig::Method(sig) => Some(sig.self_param.mode.clone()),
        }
    }
}

pub(super) fn lookup_call_sig<'a>(
    call_expr: &'a Expr,
    ctx: &'a TypeCheckedContext,
) -> Option<CallSig<'a>> {
    // Get the target function def_id.
    let def_id = ctx.type_map.lookup_call_def(call_expr.id)?;

    // Try module functions/methods.
    for callable in ctx.module.callables() {
        let def = ctx.def_map.lookup_def(callable.id())?;
        if def.id != def_id {
            continue;
        }
        return match callable {
            CallableRef::Function(func) => Some(CallSig::Function(&func.sig)),
            CallableRef::Method { method, .. } => Some(CallSig::Method(&method.sig)),
        };
    }

    // Try module function declarations.
    for decl in ctx.module.func_decls() {
        let def = ctx.def_map.lookup_def(decl.id)?;
        if def.id == def_id {
            return Some(CallSig::Function(&decl.sig));
        }
    }

    None
}
