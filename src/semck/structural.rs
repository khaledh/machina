use crate::ast::{
    Expr, ExprKind, MatchArm, MatchPattern, Pattern, PatternKind, StructLitField,
    StructUpdateField, Visitor, walk_expr, walk_stmt_expr,
};
use crate::context::TypeCheckedContext;
use crate::semck::SemCheckError;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

pub(super) fn check(ctx: &TypeCheckedContext) -> Vec<SemCheckError> {
    // Structural checks depend on type map + AST shape, not value flow.
    let mut checker = StructuralChecker::new(ctx);
    checker.check_module();
    checker.errors
}

struct EnumVariantInfo {
    name: String,
    payload_len: usize,
}

struct StructuralChecker<'a> {
    ctx: &'a TypeCheckedContext,
    errors: Vec<SemCheckError>,
    // Cached field/variant shapes from type declarations.
    struct_fields: HashMap<String, Vec<String>>,
    enum_variants: HashMap<String, Vec<EnumVariantInfo>>,
}

impl<'a> StructuralChecker<'a> {
    fn new(ctx: &'a TypeCheckedContext) -> Self {
        let mut struct_fields = HashMap::new();
        let mut enum_variants = HashMap::new();

        for decl in ctx.module.type_decls() {
            match &decl.kind {
                crate::ast::TypeDeclKind::Struct { fields } => {
                    // Collect field names for fast membership checks.
                    struct_fields.insert(
                        decl.name.clone(),
                        fields.iter().map(|f| f.name.clone()).collect(),
                    );
                }
                crate::ast::TypeDeclKind::Enum { variants } => {
                    // Collect variant payload arity for enum literals and match patterns.
                    enum_variants.insert(
                        decl.name.clone(),
                        variants
                            .iter()
                            .map(|variant| EnumVariantInfo {
                                name: variant.name.clone(),
                                payload_len: variant.payload.len(),
                            })
                            .collect(),
                    );
                }
                crate::ast::TypeDeclKind::Alias { .. } => {}
            }
        }

        Self {
            ctx,
            errors: Vec::new(),
            struct_fields,
            enum_variants,
        }
    }

    fn check_module(&mut self) {
        // Visit function bodies only; type decls are handled via cached maps.
        for func in self.ctx.module.funcs() {
            self.visit_expr(&func.body);
        }
    }

    fn check_struct_lit(
        &mut self,
        name: &str,
        fields: &[StructLitField],
        span: crate::diagnostics::Span,
    ) {
        // Enforce struct field existence, duplicates, and missing fields.
        let Some(struct_fields) = self.struct_fields.get(name) else {
            self.errors
                .push(SemCheckError::UnknownStructType(name.to_string(), span));
            return;
        };

        let mut seen = HashSet::new();
        for field in fields {
            if !struct_fields.iter().any(|f| f == &field.name) {
                self.errors.push(SemCheckError::UnknownStructField(
                    field.name.clone(),
                    field.span,
                ));
                continue;
            }
            if !seen.insert(field.name.clone()) {
                self.errors.push(SemCheckError::DuplicateStructField(
                    field.name.clone(),
                    field.span,
                ));
            }
        }

        let missing = struct_fields
            .iter()
            .filter(|name| !seen.contains(*name))
            .cloned()
            .collect::<Vec<_>>();
        if !missing.is_empty() {
            self.errors
                .push(SemCheckError::StructFieldsMissing(missing.join(", "), span));
        }
    }

    fn check_struct_update(&mut self, target: &Expr, fields: &[StructUpdateField]) {
        // Validate field names on struct update expressions.
        let Some(Type::Struct {
            fields: struct_fields,
            ..
        }) = self.ctx.type_map.lookup_node_type(target.id)
        else {
            return;
        };

        let mut seen = HashSet::new();
        for field in fields {
            if !struct_fields.iter().any(|f| f.name == field.name) {
                self.errors.push(SemCheckError::UnknownStructField(
                    field.name.clone(),
                    field.span,
                ));
            } else if !seen.insert(field.name.clone()) {
                self.errors.push(SemCheckError::DuplicateStructField(
                    field.name.clone(),
                    field.span,
                ));
            }
        }
    }

    fn check_pattern(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Ident { .. } => {}
            PatternKind::Array { patterns } | PatternKind::Tuple { patterns } => {
                for pattern in patterns {
                    self.check_pattern(pattern);
                }
            }
            PatternKind::Struct { name, fields } => {
                // Enforce struct pattern fields and recurse into subpatterns.
                let Some(struct_fields) = self.struct_fields.get(name).cloned() else {
                    self.errors
                        .push(SemCheckError::UnknownStructType(name.clone(), pattern.span));
                    return;
                };

                let mut seen = HashSet::new();
                for field in fields {
                    if !struct_fields.iter().any(|f| f == &field.name) {
                        self.errors.push(SemCheckError::UnknownStructField(
                            field.name.clone(),
                            field.span,
                        ));
                    } else if !seen.insert(field.name.clone()) {
                        self.errors.push(SemCheckError::DuplicateStructField(
                            field.name.clone(),
                            field.span,
                        ));
                    }

                    self.check_pattern(&field.pattern);
                }

                let missing = struct_fields
                    .iter()
                    .filter(|name| !seen.contains(*name))
                    .cloned()
                    .collect::<Vec<_>>();
                if !missing.is_empty() {
                    self.errors.push(SemCheckError::StructFieldsMissing(
                        missing.join(", "),
                        pattern.span,
                    ));
                }
            }
        }
    }

    fn check_enum_variant(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        payload_len: usize,
        span: crate::diagnostics::Span,
    ) {
        // Verify enum existence, variant existence, and payload arity.
        let Some(variants) = self.enum_variants.get(enum_name) else {
            self.errors
                .push(SemCheckError::UnknownEnumType(enum_name.to_string(), span));
            return;
        };

        let Some(variant) = variants.iter().find(|v| v.name == variant_name) else {
            self.errors.push(SemCheckError::UnknownEnumVariant(
                enum_name.to_string(),
                variant_name.to_string(),
                span,
            ));
            return;
        };

        if payload_len != variant.payload_len {
            self.errors
                .push(SemCheckError::EnumVariantPayloadArityMismatch(
                    variant_name.to_string(),
                    variant.payload_len,
                    payload_len,
                    span,
                ));
        }
    }

    fn check_match(&mut self, scrutinee: &Expr, arms: &[MatchArm], span: crate::diagnostics::Span) {
        // Enforce match target enum shape, arm uniqueness, and exhaustiveness.
        let Some(scrutinee_ty) = self.ctx.type_map.lookup_node_type(scrutinee.id) else {
            return;
        };
        let Type::Enum { name, variants } = scrutinee_ty else {
            self.errors.push(SemCheckError::MatchTargetNotEnum(
                scrutinee_ty,
                scrutinee.span,
            ));
            return;
        };

        let mut seen_variants = HashSet::new();
        let mut has_wildcard = false;

        for arm in arms {
            match &arm.pattern {
                MatchPattern::Wildcard { .. } => {
                    has_wildcard = true;
                }
                MatchPattern::EnumVariant {
                    enum_name: pat_enum_name,
                    variant_name,
                    bindings,
                    span,
                } => {
                    if let Some(pat_enum_name) = pat_enum_name {
                        if pat_enum_name != &name {
                            self.errors.push(SemCheckError::MatchPatternEnumMismatch(
                                name.clone(),
                                pat_enum_name.clone(),
                                *span,
                            ));
                        }
                    }

                    if !seen_variants.insert(variant_name.clone()) {
                        self.errors.push(SemCheckError::DuplicateMatchVariant(
                            variant_name.clone(),
                            *span,
                        ));
                    }

                    let Some(variant) = variants.iter().find(|v| v.name == *variant_name) else {
                        self.errors.push(SemCheckError::UnknownEnumVariant(
                            name.clone(),
                            variant_name.clone(),
                            *span,
                        ));
                        continue;
                    };

                    if bindings.len() != variant.payload.len() {
                        self.errors
                            .push(SemCheckError::EnumVariantPayloadArityMismatch(
                                variant_name.clone(),
                                variant.payload.len(),
                                bindings.len(),
                                *span,
                            ));
                    }
                }
            }
        }

        if !has_wildcard {
            self.errors.push(SemCheckError::NonExhaustiveMatch(span));
        }
    }
}

impl Visitor for StructuralChecker<'_> {
    fn visit_stmt_expr(&mut self, stmt: &crate::ast::StmtExpr) {
        // Struct patterns are validated here before walking child expressions.
        match &stmt.kind {
            crate::ast::StmtExprKind::LetBind { pattern, .. }
            | crate::ast::StmtExprKind::VarBind { pattern, .. }
            | crate::ast::StmtExprKind::For { pattern, .. } => {
                self.check_pattern(pattern);
            }
            _ => {}
        }
        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::StructLit { name, fields } => {
                self.check_struct_lit(name, fields, expr.span);
            }
            ExprKind::StructUpdate { target, fields } => {
                self.check_struct_update(target, fields);
            }
            ExprKind::StructField { target, field } => {
                // Validate struct field access targets early for clearer errors.
                if let Some(Type::Struct { fields, .. }) =
                    self.ctx.type_map.lookup_node_type(target.id)
                {
                    if !fields.iter().any(|f| f.name == *field) {
                        self.errors
                            .push(SemCheckError::UnknownStructField(field.clone(), expr.span));
                    }
                }
            }
            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                self.check_enum_variant(enum_name, variant, payload.len(), expr.span);
            }
            ExprKind::Match { scrutinee, arms } => {
                self.check_match(scrutinee, arms, expr.span);
            }
            ExprKind::Call { callee, .. } => {
                // Only plain identifiers are valid callees at the AST level for now.
                if !matches!(callee.kind, ExprKind::Var(_)) {
                    self.errors.push(SemCheckError::InvalidCallee(
                        callee.kind.clone(),
                        callee.span,
                    ));
                }
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}
