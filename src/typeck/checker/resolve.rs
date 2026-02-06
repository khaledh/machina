use super::*;

impl TypeChecker {
    pub(super) fn build_param_sigs(
        &self,
        params: &[Param],
        type_params: Option<&HashMap<DefId, TyVarId>>,
    ) -> Result<Vec<ParamSig>, Vec<TypeCheckError>> {
        params
            .iter()
            .map(|param| {
                let ty = resolve_type_expr_with_params(
                    &self.ctx.def_table,
                    &self.ctx.module,
                    &param.typ,
                    type_params,
                )?;
                Ok(ParamSig {
                    name: self.def_name(param.def_id).to_string(),
                    ty,
                    mode: param.mode.clone(),
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| vec![e])
    }

    pub(super) fn resolve_ret_type(
        &self,
        ret_type: &TypeExpr,
        type_params: Option<&HashMap<DefId, TyVarId>>,
    ) -> Result<Type, Vec<TypeCheckError>> {
        resolve_type_expr_with_params(&self.ctx.def_table, &self.ctx.module, ret_type, type_params)
            .map_err(|e| vec![e])
    }

    pub(super) fn resolve_type_expr_in_scope(
        &self,
        ty_expr: &TypeExpr,
    ) -> Result<Type, TypeCheckError> {
        resolve_type_expr_with_params(
            &self.ctx.def_table,
            &self.ctx.module,
            ty_expr,
            self.current_type_params(),
        )
    }

    pub(super) fn resolve_named_type_expr(
        &self,
        name: &str,
        type_args: &[TypeExpr],
        node_id: NodeId,
        span: Span,
    ) -> Result<Option<Type>, TypeCheckError> {
        if type_args.is_empty()
            && let Some(ty) = self.type_defs.get(name)
        {
            return Ok(Some(ty.clone()));
        }

        let Some(def_id) = self.ctx.def_table.lookup_type_def_id(name) else {
            return Ok(None);
        };

        let type_expr = TypeExpr {
            id: node_id,
            kind: TypeExprKind::Named {
                ident: name.to_string(),
                def_id,
                type_args: type_args.to_vec(),
            },
            span,
        };

        resolve_type_expr_with_params(
            &self.ctx.def_table,
            &self.ctx.module,
            &type_expr,
            self.current_type_params(),
        )
        .map(Some)
    }
}
