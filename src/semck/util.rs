use crate::ast::{Expr, FunctionSig};
use crate::context::TypeCheckedContext;

pub(super) fn lookup_call_sig<'a>(
    call_expr: &'a Expr,
    ctx: &'a TypeCheckedContext,
) -> Option<&'a FunctionSig> {
    // Get the target function def_id.
    let def_id = ctx.type_map.lookup_call_def(call_expr.id)?;

    // Try module functions.
    for func in ctx.module.funcs() {
        let def = ctx.def_map.lookup_def(func.id)?;
        if def.id == def_id {
            return Some(&func.sig);
        }
    }

    // Try module function declarations.
    for decl in ctx.module.func_decls() {
        let def = ctx.def_map.lookup_def(decl.id)?;
        if def.id == def_id {
            return Some(&decl.sig);
        }
    }

    None
}
