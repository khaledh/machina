use crate::core::context::NormalizedContext;
use crate::core::diag::Span;
use crate::core::resolve::DefId;
use crate::core::resolve::DefKind;
use crate::core::semck::match_check;
use crate::core::semck::{SemCheckError, SEK};
use crate::core::tree::normalized::{
    BindPattern, BindPatternKind, CallArg, CallArgMode, Expr, ExprKind, FunctionSig, MatchArm,
    MethodSig, Param, ParamMode, StmtExpr, StmtExprKind, StructLitField, StructUpdateField,
    TypeDefKind,
};
use crate::core::tree::visit::{
    Visitor, walk_expr, walk_func_sig, walk_method_sig, walk_stmt_expr,
};
use crate::core::typecheck::type_map::{CallSig, resolve_type_expr};
use crate::core::types::{Type, TypeId};
use std::collections::{HashMap, HashSet};

pub(super) fn check(ctx: &NormalizedContext) -> Vec<SemCheckError> {
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
    ctx: &'a NormalizedContext,
    errors: Vec<SemCheckError>,
    // Cached field/variant shapes from type declarations.
    struct_fields: HashMap<String, Vec<String>>,
    enum_variants: HashMap<String, Vec<EnumVariantInfo>>,
}

impl<'a> StructuralChecker<'a> {
    fn new(ctx: &'a NormalizedContext) -> Self {
        let mut struct_fields = HashMap::new();
        let mut enum_variants = HashMap::new();

        for type_def in ctx.module.type_defs() {
            match &type_def.kind {
                TypeDefKind::Struct { fields } => {
                    // Collect field names for fast membership checks.
                    struct_fields.insert(
                        type_def.name.clone(),
                        fields.iter().map(|f| f.name.clone()).collect(),
                    );
                }
                TypeDefKind::Enum { variants } => {
                    // Collect variant payload arity for enum literals and match patterns.
                    enum_variants.insert(
                        type_def.name.clone(),
                        variants
                            .iter()
                            .map(|variant| EnumVariantInfo {
                                name: variant.name.clone(),
                                payload_len: variant.payload.len(),
                            })
                            .collect(),
                    );
                }
                TypeDefKind::Alias { .. } => {}
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
        self.visit_module(&self.ctx.module);
    }

    fn push_err(errors: &mut Vec<SemCheckError>, span: Span, kind: SEK) {
        errors.push(kind.at(span));
    }

    fn push(&mut self, err: SemCheckError) {
        self.errors.push(err);
    }

    fn check_struct_lit(&mut self, name: &str, fields: &[StructLitField], span: Span) {
        // Enforce struct field existence, duplicates, and missing fields.
        let Some(struct_fields) = self.struct_fields.get(name) else {
            Self::push_err(&mut self.errors, span, SEK::UnknownStructType(name.to_string()));
            return;
        };

        let mut seen = HashSet::new();
        for field in fields {
            if !struct_fields.iter().any(|f| f == &field.name) {
                Self::push_err(&mut self.errors, field.span, SEK::UnknownStructField(field.name.clone()));
                continue;
            }
            if !seen.insert(field.name.clone()) {
                Self::push_err(&mut self.errors, field.span, SEK::DuplicateStructField(field.name.clone()));
            }
        }

        let missing = struct_fields
            .iter()
            .filter(|name| !seen.contains(*name))
            .cloned()
            .collect::<Vec<_>>();
        if !missing.is_empty() {
            Self::push_err(&mut self.errors, span, SEK::StructFieldsMissing(missing.join(", ")));
        }
    }

    fn check_struct_update(&mut self, target: &Expr, fields: &[StructUpdateField]) {
        // Validate field names on struct update expressions.
        let target_ty = self.ctx.type_map.type_table().get(target.ty);
        let Type::Struct {
            fields: struct_fields,
            ..
        } = target_ty
        else {
            return;
        };

        let mut seen = HashSet::new();
        for field in fields {
            if !struct_fields.iter().any(|f| f.name == field.name) {
                Self::push_err(&mut self.errors, field.span, SEK::UnknownStructField(field.name.clone()));
            } else if !seen.insert(field.name.clone()) {
                Self::push_err(&mut self.errors, field.span, SEK::DuplicateStructField(field.name.clone()));
            }
        }
    }

    fn check_pattern(&mut self, pattern: &BindPattern) {
        match &pattern.kind {
            BindPatternKind::Name { .. } => {}
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for pattern in patterns {
                    self.check_pattern(pattern);
                }
            }
            BindPatternKind::Struct { name, fields } => {
                // Enforce struct pattern fields and recurse into subpatterns.
                let Some(struct_fields) = self.struct_fields.get(name).cloned() else {
                    Self::push_err(&mut self.errors, pattern.span, SEK::UnknownStructType(name.clone()));
                    return;
                };

                let mut seen = HashSet::new();
                for field in fields {
                    if !struct_fields.iter().any(|f| f == &field.name) {
                        Self::push_err(&mut self.errors, field.span, SEK::UnknownStructField(field.name.clone()));
                    } else if !seen.insert(field.name.clone()) {
                        Self::push_err(&mut self.errors, field.span, SEK::DuplicateStructField(field.name.clone()));
                    }

                    self.check_pattern(&field.pattern);
                }

                let missing = struct_fields
                    .iter()
                    .filter(|name| !seen.contains(*name))
                    .cloned()
                    .collect::<Vec<_>>();
                if !missing.is_empty() {
                    Self::push_err(&mut self.errors, pattern.span, SEK::StructFieldsMissing(missing.join(", ")));
                }
            }
        }
    }

    fn check_param_modes(&mut self, params: &[Param]) {
        for param in params {
            if let Ok(ty) = resolve_type_expr(&self.ctx.def_table, &self.ctx.module, &param.typ) {
                if param.mode == ParamMode::InOut && !(ty.is_compound() || ty.is_heap()) {
                    // Only aggregate or heap types can be inout parameters.
                    Self::push_err(&mut self.errors, param.span, SEK::InOutParamNotAggregate(ty.clone()));
                }
                if param.mode == ParamMode::Out && !ty.is_compound() {
                    // Only aggregate types can be out parameters (for now).
                    Self::push_err(&mut self.errors, param.span, SEK::OutParamNotAggregate(ty.clone()));
                }
                if param.mode == ParamMode::Sink && !ty.needs_drop() {
                    // Sink params are meant only for heap types.
                    Self::push_err(&mut self.errors, param.span, SEK::SinkParamNotOwned(ty));
                }
            }
        }
    }

    fn check_enum_variant(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        payload_len: usize,
        span: Span,
    ) {
        // Verify enum existence, variant existence, and payload arity.
        let Some(variants) = self.enum_variants.get(enum_name) else {
            Self::push_err(&mut self.errors, span, SEK::UnknownEnumType(enum_name.to_string()));
            return;
        };

        let Some(variant) = variants.iter().find(|v| v.name == variant_name) else {
            Self::push_err(&mut self.errors, span, SEK::UnknownEnumVariant(
                    enum_name.to_string(),
                    variant_name.to_string(),
                ));
            return;
        };

        if payload_len != variant.payload_len {
            Self::push_err(&mut self.errors, span, SEK::EnumVariantPayloadArityMismatch(
                    variant_name.to_string(),
                    variant.payload_len,
                    payload_len,
                ));
        }
    }

    fn check_match(&mut self, scrutinee: &Expr, arms: &[MatchArm], span: Span) {
        match_check::check_match(self.ctx, scrutinee, arms, span, &mut self.errors);
    }

    fn is_mutable_lvalue(&self, expr: &Expr) -> Option<bool> {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => {
                let def = self.ctx.def_table.lookup_def(*def_id)?;
                match def.kind {
                    DefKind::LocalVar { is_mutable, .. } | DefKind::Param { is_mutable, .. } => {
                        Some(is_mutable)
                    }
                    _ => None,
                }
            }
            ExprKind::ArrayIndex { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::StructField { target, .. }
            | ExprKind::Slice { target, .. }
            | ExprKind::Coerce { expr: target, .. } => self.is_mutable_lvalue(target),
            _ => None,
        }
    }

    fn is_lvalue(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Var { def_id, .. } => {
                self.ctx.def_table.lookup_def(*def_id).is_some_and(|def| {
                    matches!(def.kind, DefKind::LocalVar { .. } | DefKind::Param { .. })
                })
            }
            ExprKind::ArrayIndex { target, .. }
            | ExprKind::TupleField { target, .. }
            | ExprKind::StructField { target, .. }
            | ExprKind::Slice { target, .. }
            | ExprKind::Coerce { expr: target, .. } => self.is_lvalue(target),
            _ => false,
        }
    }

    fn check_call_arg_modes(&mut self, sig: &CallSig, args: &[CallArg]) {
        for (param, arg) in sig.params.iter().zip(args) {
            let arg_mode = arg.mode;
            let arg_expr = &arg.expr;
            match param.mode {
                ParamMode::In => match arg_mode {
                    CallArgMode::Default => {}
                    CallArgMode::InOut => {
                        Self::push_err(&mut self.errors, arg.span, SEK::InOutArgUnexpected);
                    }
                    CallArgMode::Out => {
                        Self::push_err(&mut self.errors, arg.span, SEK::OutArgUnexpected);
                    }
                    CallArgMode::Move => {
                        Self::push_err(&mut self.errors, arg.span, SEK::MoveArgUnexpected);
                    }
                },
                ParamMode::InOut => match arg_mode {
                    CallArgMode::InOut => {
                        let err = match self.is_mutable_lvalue(arg_expr) {
                            Some(true) => continue,
                            Some(false) => SEK::InOutArgNotMutable.at(arg.span),
                            None => SEK::InOutArgNotLvalue.at(arg.span),
                        };
                        self.push(err);
                    }
                    CallArgMode::Default => {
                        Self::push_err(&mut self.errors, arg.span, SEK::InOutArgMissingMode);
                    }
                    CallArgMode::Out => {
                        Self::push_err(&mut self.errors, arg.span, SEK::OutArgUnexpected);
                    }
                    CallArgMode::Move => {
                        Self::push_err(&mut self.errors, arg.span, SEK::MoveArgUnexpected);
                    }
                },
                ParamMode::Out => match arg_mode {
                    CallArgMode::Out => {
                        let err = match self.is_mutable_lvalue(arg_expr) {
                            Some(true) => continue,
                            Some(false) => SEK::OutArgNotMutable.at(arg.span),
                            None => SEK::OutArgNotLvalue.at(arg.span),
                        };
                        self.push(err);
                    }
                    CallArgMode::Default => {
                        Self::push_err(&mut self.errors, arg.span, SEK::OutArgMissingMode);
                    }
                    CallArgMode::InOut => {
                        Self::push_err(&mut self.errors, arg.span, SEK::InOutArgUnexpected);
                    }
                    CallArgMode::Move => {
                        Self::push_err(&mut self.errors, arg.span, SEK::MoveArgUnexpected);
                    }
                },
                ParamMode::Sink => match arg_mode {
                    CallArgMode::Move => {}
                    CallArgMode::Default => {
                        Self::push_err(&mut self.errors, arg.span, SEK::SinkArgMissingMove);
                    }
                    CallArgMode::InOut => {
                        Self::push_err(&mut self.errors, arg.span, SEK::InOutArgUnexpected);
                    }
                    CallArgMode::Out => {
                        Self::push_err(&mut self.errors, arg.span, SEK::OutArgUnexpected);
                    }
                },
            }
        }
    }
}

impl Visitor<DefId, TypeId> for StructuralChecker<'_> {
    fn visit_func_sig(&mut self, func_sig: &FunctionSig) {
        self.check_param_modes(&func_sig.params);
        walk_func_sig(self, func_sig);
    }

    fn visit_method_sig(&mut self, method_sig: &MethodSig) {
        if method_sig.self_param.mode == ParamMode::Out {
            Self::push_err(&mut self.errors, method_sig.self_param.span, SEK::OutSelfNotAllowed);
        }
        self.check_param_modes(&method_sig.params);
        walk_method_sig(self, method_sig);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        // Struct patterns are validated here before walking child expressions.
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, .. }
            | StmtExprKind::VarBind { pattern, .. }
            | StmtExprKind::For { pattern, .. } => {
                self.check_pattern(pattern);
            }
            _ => {}
        }
        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::StructLit { name, fields, .. } => {
                self.check_struct_lit(name, fields, expr.span);
            }
            ExprKind::StructUpdate { target, fields } => {
                self.check_struct_update(target, fields);
            }
            ExprKind::StructField { target, field } => {
                // Validate struct field access targets early for clearer errors.
                let target_ty = self.ctx.type_map.type_table().get(target.ty).peel_heap();
                if let Type::Struct { fields, .. } = target_ty
                    && !fields.iter().any(|f| f.name == *field)
                {
                    Self::push_err(&mut self.errors, expr.span, SEK::UnknownStructField(field.clone()));
                }
            }
            ExprKind::Slice { target, .. } => {
                if !self.is_lvalue(target) {
                    Self::push_err(&mut self.errors, expr.span, SEK::SliceTargetNotLvalue);
                }
            }
            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
                ..
            } => {
                self.check_enum_variant(enum_name, variant, payload.len(), expr.span);
            }
            ExprKind::Match { scrutinee, arms } => {
                self.check_match(scrutinee, arms, expr.span);
            }
            ExprKind::Call { callee, args } => {
                // Only plain identifiers are valid callees at the AST level for now.
                if !matches!(callee.kind, ExprKind::Var { .. }) {
                    Self::push_err(&mut self.errors, callee.span, SEK::InvalidCallee(callee.kind.clone()));
                }

                // Validate call-site argument modes and lvalue requirements.
                if let Some(sig) = self.ctx.call_sigs.get(&expr.id) {
                    self.check_call_arg_modes(sig, args);
                }
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.expr);
                }
                return;
            }

            ExprKind::MethodCall { callee, args, .. } => {
                if let Some(sig) = self.ctx.call_sigs.get(&expr.id) {
                    if let Some(receiver) = sig.receiver.as_ref() {
                        match receiver.mode {
                            ParamMode::In => {}
                            ParamMode::InOut | ParamMode::Out => {
                                let err = match self.is_mutable_lvalue(callee) {
                                    Some(true) => None,
                                    Some(false) => {
                                        Some(SEK::InOutArgNotMutable.at(expr.span))
                                    }
                                    None => {
                                        Some(SEK::InOutArgNotLvalue.at(expr.span))
                                    }
                                };
                                if let Some(err) = err {
                                    self.push(err);
                                }
                            }
                            ParamMode::Sink => {}
                        }
                    }
                    self.check_call_arg_modes(sig, args);
                }
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.expr);
                }
                return;
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}
