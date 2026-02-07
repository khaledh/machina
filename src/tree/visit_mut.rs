use crate::tree::*;

/// Tree mutable visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
pub trait VisitorMut<D = String, T = ()> {
    // --- Module ---

    fn visit_module(&mut self, module: &mut Module<D, T>) {
        walk_module(self, module)
    }

    // --- Type Definitions ---

    fn visit_type_def(&mut self, type_def: &mut TypeDef<D>) {
        walk_type_def(self, type_def)
    }

    fn visit_struct_def_fields(&mut self, fields: &mut [StructDefField<D>]) {
        walk_struct_def_fields(self, fields)
    }

    fn visit_struct_def_field(&mut self, field: &mut StructDefField<D>) {
        walk_struct_def_field(self, field)
    }

    fn visit_enum_def_variants(&mut self, variants: &mut [EnumDefVariant<D>]) {
        walk_enum_def_variants(self, variants)
    }

    fn visit_enum_def_variant(&mut self, variant: &mut EnumDefVariant<D>) {
        walk_enum_def_variant(self, variant)
    }

    // --- Type Expressions ---

    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr<D>) {
        walk_type_expr(self, type_expr)
    }

    // --- Function Declarations ---

    fn visit_func_decl(&mut self, func_decl: &mut FuncDecl<D>) {
        walk_func_decl(self, func_decl)
    }

    // --- Functions ---

    fn visit_func_def(&mut self, func_def: &mut FuncDef<D, T>) {
        walk_func_def(self, func_def)
    }

    // --- Function Signatures ---

    fn visit_func_sig(&mut self, func_sig: &mut FunctionSig<D>) {
        walk_func_sig(self, func_sig)
    }

    // --- Type Parameters ---

    fn visit_type_param(&mut self, param: &mut TypeParam<D>) {
        walk_type_param(self, param)
    }

    // --- Method Signatures ---

    fn visit_method_sig(&mut self, method_sig: &mut MethodSig<D>) {
        walk_method_sig(self, method_sig)
    }

    // --- Parameters (common) ---

    fn visit_param(&mut self, param: &mut Param<D>) {
        walk_param(self, param)
    }

    // --- Method Blocks ---

    fn visit_method_block(&mut self, method_block: &mut MethodBlock<D, T>) {
        walk_method_block(self, method_block)
    }

    fn visit_method_item(&mut self, method_item: &mut MethodItem<D, T>) {
        walk_method_item(self, method_item)
    }

    fn visit_method_decl(&mut self, method_decl: &mut MethodDecl<D>) {
        walk_method_decl(self, method_decl)
    }

    fn visit_method_def(&mut self, method_def: &mut MethodDef<D, T>) {
        walk_method_def(self, method_def)
    }

    // --- Closure Definitions ---

    fn visit_closure_def(&mut self, closure_def: &mut ClosureDef<D, T>) {
        walk_closure_def(self, closure_def)
    }

    // --- Blocks ---

    fn visit_block_item(&mut self, item: &mut BlockItem<D, T>) {
        walk_block_item(self, item)
    }

    // --- Bind Patterns ---

    fn visit_bind_pattern(&mut self, pattern: &mut BindPattern<D>) {
        walk_bind_pattern(self, pattern)
    }

    // --- Match Patterns ---

    fn visit_match_pattern(&mut self, pattern: &mut MatchPattern<D>) {
        walk_match_pattern(self, pattern)
    }

    fn visit_match_arm(&mut self, arm: &mut MatchArm<D, T>) {
        walk_match_arm(self, arm)
    }

    fn visit_match_pattern_binding(&mut self, binding: &mut MatchPatternBinding<D>) {
        walk_match_pattern_binding(self, binding)
    }

    // --- Expressions ---

    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr<D, T>) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_expr(&mut self, expr: &mut Expr<D, T>) {
        walk_expr(self, expr)
    }
}

// --- Module ---

pub fn walk_module<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, module: &mut Module<D, T>) {
    for item in &mut module.top_level_items {
        match item {
            TopLevelItem::TypeDef(type_def) => v.visit_type_def(type_def),
            TopLevelItem::FuncDecl(func_decl) => v.visit_func_decl(func_decl),
            TopLevelItem::FuncDef(func_def) => v.visit_func_def(func_def),
            TopLevelItem::MethodBlock(method_block) => v.visit_method_block(method_block),
            TopLevelItem::ClosureDef(closure_def) => v.visit_closure_def(closure_def),
        }
    }
}

// --- Type Definitions ---

pub fn walk_type_def<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, type_def: &mut TypeDef<D>) {
    for param in &mut type_def.type_params {
        v.visit_type_param(param);
    }
    match &mut type_def.kind {
        TypeDefKind::Alias { aliased_ty } => v.visit_type_expr(aliased_ty),
        TypeDefKind::Struct { fields } => v.visit_struct_def_fields(fields),
        TypeDefKind::Enum { variants } => v.visit_enum_def_variants(variants),
    }
}

pub fn walk_struct_def_fields<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    fields: &mut [StructDefField<D>],
) {
    for field in fields {
        v.visit_struct_def_field(field);
    }
}

pub fn walk_struct_def_field<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    field: &mut StructDefField<D>,
) {
    v.visit_type_expr(&mut field.ty);
}

pub fn walk_enum_def_variants<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    variants: &mut [EnumDefVariant<D>],
) {
    for variant in variants {
        v.visit_enum_def_variant(variant);
    }
}

pub fn walk_enum_def_variant<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    variant: &mut EnumDefVariant<D>,
) {
    for payload in &mut variant.payload {
        v.visit_type_expr(payload);
    }
}

// --- Type Expressions ---

pub fn walk_type_expr<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, type_expr: &mut TypeExpr<D>) {
    match &mut type_expr.kind {
        TypeExprKind::Infer => {}
        TypeExprKind::Named { type_args, .. } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
        }
        TypeExprKind::Refined { base_ty_expr, .. } => v.visit_type_expr(base_ty_expr),
        TypeExprKind::Array { elem_ty_expr, .. } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Tuple { field_ty_exprs } => {
            for field in field_ty_exprs {
                v.visit_type_expr(field);
            }
        }
        TypeExprKind::Slice { elem_ty_expr } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Heap { elem_ty_expr } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Ref { elem_ty_expr, .. } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            for param in params {
                v.visit_type_expr(&mut param.ty_expr);
            }
            v.visit_type_expr(ret_ty_expr);
        }
    }
}

// --- Function Declarations ---

pub fn walk_func_decl<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, func_decl: &mut FuncDecl<D>) {
    v.visit_func_sig(&mut func_decl.sig);
}

// --- Functions ---

pub fn walk_func_def<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, func_def: &mut FuncDef<D, T>) {
    v.visit_func_sig(&mut func_def.sig);
    v.visit_expr(&mut func_def.body);
}

// --- Function Signatures ---

pub fn walk_func_sig<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, func_sig: &mut FunctionSig<D>) {
    for param in &mut func_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &mut func_sig.params {
        v.visit_param(param);
    }
    v.visit_type_expr(&mut func_sig.ret_ty_expr);
}

pub fn walk_type_param<V: VisitorMut<D, T> + ?Sized, D, T>(_v: &mut V, _param: &mut TypeParam<D>) {}

// --- Method Signatures ---

pub fn walk_method_sig<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_sig: &mut MethodSig<D>,
) {
    for param in &mut method_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &mut method_sig.params {
        v.visit_param(param);
    }
    v.visit_type_expr(&mut method_sig.ret_ty_expr);
}

// --- Parameters (common) ---

pub fn walk_param<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, param: &mut Param<D>) {
    v.visit_type_expr(&mut param.typ);
}

// --- Method Blocks ---

pub fn walk_method_block<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_block: &mut MethodBlock<D, T>,
) {
    for method_item in &mut method_block.method_items {
        v.visit_method_item(method_item);
    }
}

pub fn walk_method_item<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_item: &mut MethodItem<D, T>,
) {
    match method_item {
        MethodItem::Decl(method_decl) => v.visit_method_decl(method_decl),
        MethodItem::Def(method_def) => v.visit_method_def(method_def),
    }
}

pub fn walk_method_decl<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_decl: &mut MethodDecl<D>,
) {
    v.visit_method_sig(&mut method_decl.sig);
}

pub fn walk_method_def<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_def: &mut MethodDef<D, T>,
) {
    v.visit_method_sig(&mut method_def.sig);
    v.visit_expr(&mut method_def.body);
}

// --- Closure Definitions ---

pub fn walk_closure_def<V: VisitorMut<D, T> + ?Sized, D, T>(
    _v: &mut V,
    _closure_def: &mut ClosureDef<D, T>,
) {
    // Closures are also visited at their expression sites; avoid walking the lifted body twice.
}

// --- Blocks ---

pub fn walk_block_item<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, item: &mut BlockItem<D, T>) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

// --- Bind Patterns ---

pub fn walk_bind_pattern<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    pattern: &mut BindPattern<D>,
) {
    match &mut pattern.kind {
        BindPatternKind::Name { .. } => {}
        BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
            for pattern in patterns {
                v.visit_bind_pattern(pattern);
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                v.visit_bind_pattern(&mut field.pattern);
            }
        }
    }
}

// --- Match Patterns ---

pub fn walk_match_pattern<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    pattern: &mut MatchPattern<D>,
) {
    match pattern {
        MatchPattern::Tuple { patterns, .. } => {
            for pattern in patterns {
                v.visit_match_pattern(pattern);
            }
        }
        MatchPattern::EnumVariant { type_args, .. } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
        }
        _ => {}
    }
}

pub fn walk_match_pattern_bindings<V: VisitorMut<D, T> + ?Sized, D, T>(
    v: &mut V,
    pattern: &mut MatchPattern<D>,
) {
    if let MatchPattern::EnumVariant { bindings, .. } = pattern {
        for binding in bindings {
            v.visit_match_pattern_binding(binding);
        }
    }
}

pub fn walk_match_pattern_binding<V: VisitorMut<D, T> + ?Sized, D, T>(
    _v: &mut V,
    _binding: &mut MatchPatternBinding<D>,
) {
}

pub fn walk_match_arm<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, arm: &mut MatchArm<D, T>) {
    v.visit_match_pattern(&mut arm.pattern);
    v.visit_expr(&mut arm.body);
}

// --- Expressions ---

pub fn walk_stmt_expr<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, stmt: &mut StmtExpr<D, T>) {
    match &mut stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. }
        | StmtExprKind::VarBind { pattern, value, .. } => {
            v.visit_bind_pattern(pattern);
            v.visit_expr(value);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            v.visit_expr(assignee);
            v.visit_expr(value);
        }
        StmtExprKind::While { cond, body } => {
            v.visit_expr(cond);
            v.visit_expr(body);
        }
        StmtExprKind::For {
            pattern,
            iter,
            body,
        } => {
            v.visit_bind_pattern(pattern);
            v.visit_expr(iter);
            v.visit_expr(body);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                v.visit_expr(value);
            }
        }
    }
}

pub fn walk_expr<V: VisitorMut<D, T> + ?Sized, D, T>(v: &mut V, expr: &mut Expr<D, T>) {
    match &mut expr.kind {
        ExprKind::Block { items, tail } => {
            for item in items {
                v.visit_block_item(item);
            }
            if let Some(tail) = tail {
                v.visit_expr(tail);
            }
        }

        ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::UnitLit
        | ExprKind::Var { .. } => {}

        ExprKind::Range { start, end } => {
            v.visit_expr(start);
            v.visit_expr(end);
        }

        ExprKind::StringFmt { segments } => {
            for segment in segments {
                if let StringFmtSegment::Expr { expr, .. } = segment {
                    v.visit_expr(expr);
                }
            }
        }

        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => {
                for elem in elems {
                    v.visit_expr(elem);
                }
            }
            ArrayLitInit::Repeat(expr, _) => {
                v.visit_expr(expr);
            }
        },

        ExprKind::TupleLit(fields) => {
            for field in fields {
                v.visit_expr(field);
            }
        }

        ExprKind::StructLit {
            type_args, fields, ..
        } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
            for field in fields {
                v.visit_expr(&mut field.value);
            }
        }

        ExprKind::EnumVariant {
            type_args, payload, ..
        } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
            for expr in payload {
                v.visit_expr(expr);
            }
        }

        ExprKind::StructUpdate { target, fields } => {
            v.visit_expr(target);
            for field in fields {
                v.visit_expr(&mut field.value);
            }
        }

        ExprKind::BinOp { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        ExprKind::UnaryOp { expr, .. } => {
            v.visit_expr(expr);
        }

        ExprKind::HeapAlloc { expr } => {
            v.visit_expr(expr);
        }

        ExprKind::Move { expr } => {
            v.visit_expr(expr);
        }

        ExprKind::ArrayIndex { target, indices } => {
            v.visit_expr(target);
            for index in indices {
                v.visit_expr(index);
            }
        }

        ExprKind::TupleField { target, .. } => {
            v.visit_expr(target);
        }

        ExprKind::StructField { target, .. } => {
            v.visit_expr(target);
        }

        ExprKind::MethodCall { callee, args, .. } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(&mut arg.expr);
            }
        }

        ExprKind::Closure {
            params,
            return_ty,
            body,
            ..
        } => {
            for param in params {
                v.visit_param(param);
            }
            v.visit_type_expr(return_ty);
            v.visit_expr(body);
        }

        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            v.visit_expr(cond);
            v.visit_expr(then_body);
            v.visit_expr(else_body);
        }

        ExprKind::Slice { target, start, end } => {
            v.visit_expr(target);
            if let Some(start) = start {
                v.visit_expr(start);
            }
            if let Some(end) = end {
                v.visit_expr(end);
            }
        }

        ExprKind::Match { scrutinee, arms } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                v.visit_match_arm(arm);
            }
        }

        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(&mut arg.expr);
            }
        }

        ExprKind::Coerce { expr, .. } => {
            v.visit_expr(expr);
        }

        ExprKind::ImplicitMove { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::AddrOf { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::Deref { expr } => {
            v.visit_expr(expr);
        }
    }
}
