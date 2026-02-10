//! Generic type-instance resolution helpers used by expression collection.

use super::*;

impl<'a> ConstraintCollector<'a> {
    pub(super) fn resolve_type_instance_with_args(
        &mut self,
        def_id: DefId,
        type_args: &[TypeExpr],
        expected: Option<&Type>,
    ) -> Option<Type> {
        let type_def = self.ctx.module.type_def_by_id(def_id)?;
        if type_args.is_empty()
            && let Some(expected_ty) = expected
            && nominal_base_name(expected_ty).is_some_and(|name| name == type_def.name.as_str())
        {
            return Some(expected_ty.clone());
        }

        let args = if type_args.is_empty() {
            if type_def.type_params.is_empty() {
                Vec::new()
            } else {
                type_def
                    .type_params
                    .iter()
                    .map(|_| Type::Var(self.vars.fresh_infer_local()))
                    .collect::<Vec<_>>()
            }
        } else {
            let mut out = Vec::with_capacity(type_args.len());
            for arg in type_args {
                out.push(self.resolve_type_in_scope(arg).ok()?);
            }
            out
        };
        resolve_type_def_with_args(&self.ctx.def_table, &self.ctx.module, def_id, &args).ok()
    }

    pub(super) fn resolve_type_instance_for_expr(
        &mut self,
        expr: &Expr,
        type_args: &[TypeExpr],
        expected: Option<&Type>,
    ) -> Option<Type> {
        let def_id = self.ctx.def_table.lookup_node_def_id(expr.id)?;
        self.resolve_type_instance_with_args(def_id, type_args, expected)
    }

    pub(super) fn resolve_type_instance_for_name(
        &mut self,
        type_name: &str,
        type_args: &[TypeExpr],
        expected: Option<&Type>,
    ) -> Option<Type> {
        let def_id = self
            .ctx
            .module
            .type_defs()
            .into_iter()
            .find(|type_def| type_def.name == type_name)
            .map(|type_def| type_def.def_id)?;
        self.resolve_type_instance_with_args(def_id, type_args, expected)
    }
}
