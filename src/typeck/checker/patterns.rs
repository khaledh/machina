use super::*;

impl TypeChecker {
    pub(super) fn check_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
    ) -> Result<Type, TypeCheckError> {
        let scrutinee_ty = self.visit_expr(scrutinee, None)?;
        let view = self.view_type(&scrutinee_ty);
        let mut arm_ty: Option<Type> = None;

        match view.as_enum() {
            Some((name, variants)) => {
                let enum_name = name.to_string();
                self.visit_match_arms(arms, |this, arm| {
                    this.check_enum_match_pattern(&enum_name, variants, &arm.pattern)?;
                    this.check_match_arm_body(arm, &mut arm_ty)
                })?;
            }
            None => match view.as_tuple() {
                Some(field_tys) => {
                    self.visit_match_arms(arms, |this, arm| {
                        this.check_tuple_match_pattern(field_tys, &arm.pattern);
                        this.check_match_arm_body(arm, &mut arm_ty)
                    })?;
                }
                None => {
                    self.visit_match_arms(arms, |this, arm| {
                        this.check_match_arm_body(arm, &mut arm_ty)
                    })?;
                }
            },
        }

        Ok(arm_ty.unwrap_or(Type::Unit))
    }

    fn check_match_arm_body(
        &mut self,
        arm: &MatchArm,
        arm_ty: &mut Option<Type>,
    ) -> Result<(), TypeCheckError> {
        let body_ty = self.visit_match_arm(arm)?;
        let body_ty = self.apply_infer(&body_ty);
        if let Some(expected_ty) = arm_ty {
            let (body_ty, expected_ty, _unified) = self.unify_infer_types(&body_ty, expected_ty);
            if body_ty != expected_ty {
                return Err(TypeCheckErrorKind::MatchArmTypeMismatch(
                    expected_ty,
                    body_ty,
                    arm.span,
                )
                .into());
            }
        } else {
            *arm_ty = Some(body_ty);
        }

        Ok(())
    }

    fn check_enum_match_pattern(
        &mut self,
        enum_name: &String,
        variants: &[EnumVariant],
        pattern: &MatchPattern,
    ) -> Result<(), TypeCheckError> {
        match pattern {
            MatchPattern::EnumVariant {
                enum_name: pat_enum_name,
                variant_name,
                bindings,
                ..
            } => {
                if let Some(pat_enum_name) = pat_enum_name {
                    let matches = pat_enum_name == enum_name
                        || enum_name
                            .split_once('<')
                            .map_or(false, |(base, _)| base == pat_enum_name);
                    if !matches {
                        return Ok(());
                    }
                }

                if let Some(variant) = variants.iter().find(|v| v.name == *variant_name) {
                    if bindings.len() == variant.payload.len() {
                        for (binding, ty) in bindings.iter().zip(variant.payload.iter()) {
                            self.record_match_binding(binding, ty);
                        }
                    } else {
                        for binding in bindings {
                            self.record_match_binding(binding, &Type::Unknown);
                        }
                    }
                } else {
                    for binding in bindings {
                        self.record_match_binding(binding, &Type::Unknown);
                    }
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn record_match_binding(&mut self, binding: &MatchPatternBinding, ty: &Type) {
        let MatchPatternBinding::Named { def_id, .. } = binding else {
            return;
        };
        self.record_binding_def(*def_id, ty);
    }

    fn record_binding_def(&mut self, def_id: DefId, ty: &Type) {
        match self.ctx.def_table.lookup_def(def_id) {
            Some(def) => {
                self.type_map_builder
                    .record_def_type(def.clone(), ty.clone());
            }
            None => panic!(
                "compiler bug: binding def [{}] not found in def_table",
                def_id
            ),
        }
    }

    fn check_tuple_match_pattern(&mut self, fields: &[Type], pattern: &MatchPattern) {
        let MatchPattern::Tuple { patterns, .. } = pattern else {
            return;
        };

        self.record_tuple_pattern_bindings(fields, patterns);
    }

    fn record_tuple_pattern_bindings(&mut self, fields: &[Type], patterns: &[MatchPattern]) {
        if patterns.len() == fields.len() {
            for (pattern, ty) in patterns.iter().zip(fields.iter()) {
                self.record_pattern_bindings(pattern, Some(ty));
            }
        } else {
            for pattern in patterns {
                self.record_pattern_bindings(pattern, None);
            }
        }
    }

    fn record_pattern_bindings(&mut self, pattern: &MatchPattern, ty: Option<&Type>) {
        match pattern {
            MatchPattern::Binding { def_id, .. } => {
                self.record_binding_def(*def_id, ty.unwrap_or(&Type::Unknown));
            }
            MatchPattern::EnumVariant { bindings, .. } => match ty {
                Some(Type::Enum { name, variants }) => {
                    let _ = self.check_enum_match_pattern(name, variants, pattern);
                }
                _ => {
                    for binding in bindings {
                        self.record_match_binding(binding, &Type::Unknown);
                    }
                }
            },
            MatchPattern::Tuple { patterns, .. } => {
                let fields = match ty {
                    Some(Type::Tuple { field_tys }) => Some(field_tys.as_slice()),
                    _ => None,
                };
                if let Some(fields) = fields {
                    self.record_tuple_pattern_bindings(fields, patterns);
                } else {
                    for pattern in patterns {
                        self.record_pattern_bindings(pattern, None);
                    }
                }
            }
            _ => {}
        }
    }
}
