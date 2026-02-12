//! Pattern and match-arm constraint collection.

use super::*;

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_match_arm(&mut self, arm: &MatchArm, expected: Option<Type>) -> Type {
        self.collect_match_pattern_bindings(&arm.pattern);
        self.collect_expr(&arm.body, expected)
    }

    pub(super) fn collect_match_pattern_bindings(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding {
                def_id, id, span, ..
            } => {
                let bind_term = self.def_term(*def_id);
                let node_term = self.node_term(*id);
                self.push_eq(node_term, bind_term, ConstraintReason::Pattern(*id, *span));
            }
            MatchPattern::TypedBinding {
                def_id, id, span, ..
            } => {
                let bind_term = self.def_term(*def_id);
                let node_term = self.node_term(*id);
                self.push_eq(node_term, bind_term, ConstraintReason::Pattern(*id, *span));
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.collect_match_pattern_bindings(pattern);
                }
            }
            MatchPattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    if let crate::core::tree::resolved::MatchPatternBinding::Named {
                        def_id,
                        id,
                        span,
                        ..
                    } = binding
                    {
                        let bind_term = self.def_term(*def_id);
                        let node_term = self.node_term(*id);
                        self.push_eq(node_term, bind_term, ConstraintReason::Pattern(*id, *span));
                    }
                }
            }
            _ => {}
        }
    }

    pub(super) fn collect_bind_pattern(&mut self, pattern: &BindPattern, value_ty: Type) {
        // Record a generic bind obligation for structural pattern diagnostics.
        self.out.pattern_obligations.push(PatternObligation::Bind {
            pattern_id: pattern.id,
            pattern: pattern.clone(),
            value_ty: value_ty.clone(),
            caller_def_id: self.current_callable_def_id(),
            span: pattern.span,
        });

        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                let bind_ty = self.def_term(*def_id);
                let node_ty = self.node_term(pattern.id);
                self.push_eq(
                    value_ty,
                    bind_ty.clone(),
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
                self.push_eq(
                    node_ty,
                    bind_ty,
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
            }
            BindPatternKind::Tuple { patterns } => {
                let field_terms = patterns
                    .iter()
                    .map(|_| self.fresh_var_term())
                    .collect::<Vec<_>>();
                self.push_eq(
                    value_ty.clone(),
                    Type::Tuple {
                        field_tys: field_terms.clone(),
                    },
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
                for (child, child_term) in patterns.iter().zip(field_terms.into_iter()) {
                    self.collect_bind_pattern(child, child_term);
                }
            }
            BindPatternKind::Array { patterns } => {
                let elem_term = self.fresh_var_term();
                self.push_eq(
                    value_ty.clone(),
                    Type::Array {
                        elem_ty: Box::new(elem_term.clone()),
                        dims: vec![patterns.len()],
                    },
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
                for child in patterns {
                    self.collect_bind_pattern(child, elem_term.clone());
                }
            }
            BindPatternKind::Struct {
                name: type_name,
                fields,
            } => {
                if let Some(Type::Struct {
                    fields: struct_fields,
                    ..
                }) = self.type_defs.get(type_name)
                {
                    self.push_assignable(
                        value_ty.clone(),
                        Type::Struct {
                            name: type_name.clone(),
                            fields: struct_fields.clone(),
                        },
                        ConstraintReason::Pattern(pattern.id, pattern.span),
                    );
                    for StructFieldBindPattern {
                        name,
                        pattern: child,
                        ..
                    } in fields
                    {
                        if let Some(field_ty) = struct_fields
                            .iter()
                            .find(|field| field.name == *name)
                            .map(|field| field.ty.clone())
                        {
                            self.collect_bind_pattern(child, field_ty);
                        }
                    }
                } else {
                    for StructFieldBindPattern { pattern: child, .. } in fields {
                        let child_ty = self.fresh_var_term();
                        self.collect_bind_pattern(child, child_ty);
                    }
                }
            }
        }
    }
}
