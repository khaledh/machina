use crate::tree::*;

/// Tree visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
/// Example:
/// ```rust
/// use machina::tree::Expr;
/// use machina::tree::visit::{walk_expr, Visitor};
///
/// struct MyVisitor;
/// impl Visitor for MyVisitor {
///     fn visit_expr(&mut self, expr: &Expr) {
///         // pre-visit logic here
///         walk_expr(self, expr);
///         // post-visit logic here
///     }
/// }
/// ```
pub trait Visitor<D = String, T = ()> {
    // --- Module ---

    fn visit_module(&mut self, module: &Module<D, T>) {
        walk_module(self, module)
    }

    // --- Type Definitions ---

    fn visit_type_def(&mut self, type_def: &TypeDef<D>) {
        walk_type_def(self, type_def)
    }

    fn visit_struct_def_fields(&mut self, fields: &[StructDefField<D>]) {
        walk_struct_def_fields(self, fields)
    }

    fn visit_struct_def_field(&mut self, field: &StructDefField<D>) {
        walk_struct_def_field(self, field)
    }

    fn visit_enum_def_variants(&mut self, variants: &[EnumDefVariant<D>]) {
        walk_enum_def_variants(self, variants)
    }

    fn visit_enum_def_variant(&mut self, variant: &EnumDefVariant<D>) {
        walk_enum_def_variant(self, variant)
    }

    // --- Type Expressions ---

    fn visit_type_expr(&mut self, type_expr: &TypeExpr<D>) {
        walk_type_expr(self, type_expr)
    }

    // --- Function Declarations ---

    fn visit_func_decl(&mut self, func_decl: &FuncDecl<D>) {
        walk_func_decl(self, func_decl)
    }

    // --- Functions ---

    fn visit_func_def(&mut self, func_def: &FuncDef<D, T>) {
        walk_func_def(self, func_def)
    }

    // --- Function Signatures ---

    fn visit_func_sig(&mut self, func_sig: &FunctionSig<D>) {
        walk_func_sig(self, func_sig)
    }

    // --- Type Parameters ---

    fn visit_type_param(&mut self, param: &TypeParam<D>) {
        walk_type_param(self, param)
    }

    // --- Method Signatures ---

    fn visit_method_sig(&mut self, method_sig: &MethodSig<D>) {
        walk_method_sig(self, method_sig)
    }

    // --- Parameters (common) ---

    fn visit_param(&mut self, param: &Param<D>) {
        walk_param(self, param)
    }

    // --- Method Blocks ---

    fn visit_method_block(&mut self, method_block: &MethodBlock<D, T>) {
        walk_method_block(self, method_block)
    }

    fn visit_method_item(&mut self, method_item: &MethodItem<D, T>) {
        walk_method_item(self, method_item)
    }

    fn visit_method_decl(&mut self, method_decl: &MethodDecl<D>) {
        walk_method_decl(self, method_decl)
    }

    fn visit_method_def(&mut self, method_def: &MethodDef<D, T>) {
        walk_method_def(self, method_def)
    }

    // --- Closure Definitions ---

    fn visit_closure_def(&mut self, closure_def: &ClosureDef<D, T>) {
        walk_closure_def(self, closure_def)
    }

    // --- Blocks ---

    fn visit_block_item(&mut self, item: &BlockItem<D, T>) {
        walk_block_item(self, item)
    }

    // --- Bind Patterns ---

    fn visit_bind_pattern(&mut self, pattern: &BindPattern<D>) {
        walk_bind_pattern(self, pattern)
    }

    // --- Match Patterns ---

    fn visit_match_pattern(&mut self, pattern: &MatchPattern<D>) {
        walk_match_pattern(self, pattern)
    }

    fn visit_match_arm(&mut self, arm: &MatchArm<D, T>) {
        walk_match_arm(self, arm)
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding<D>) {
        walk_match_pattern_binding(self, binding)
    }

    // --- Expressions ---

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr<D, T>) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_expr(&mut self, expr: &Expr<D, T>) {
        walk_expr(self, expr)
    }
}

// --- Module ---

pub fn walk_module<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, module: &Module<D, T>) {
    for item in &module.top_level_items {
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

pub fn walk_type_def<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, type_def: &TypeDef<D>) {
    match &type_def.kind {
        TypeDefKind::Alias { aliased_ty } => v.visit_type_expr(aliased_ty),
        TypeDefKind::Struct { fields } => v.visit_struct_def_fields(fields),
        TypeDefKind::Enum { variants } => v.visit_enum_def_variants(variants),
    }
}

pub fn walk_struct_def_fields<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    fields: &[StructDefField<D>],
) {
    for field in fields {
        v.visit_struct_def_field(field);
    }
}

pub fn walk_struct_def_field<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    field: &StructDefField<D>,
) {
    v.visit_type_expr(&field.ty);
}

pub fn walk_enum_def_variants<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    variants: &[EnumDefVariant<D>],
) {
    for variant in variants {
        v.visit_enum_def_variant(variant);
    }
}

pub fn walk_enum_def_variant<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    variant: &EnumDefVariant<D>,
) {
    for payload in &variant.payload {
        v.visit_type_expr(payload);
    }
}

// --- Type Expressions ---

pub fn walk_type_expr<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, type_expr: &TypeExpr<D>) {
    match &type_expr.kind {
        TypeExprKind::Named { .. } => {}
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
                v.visit_type_expr(&param.ty_expr);
            }
            v.visit_type_expr(ret_ty_expr);
        }
    }
}

// --- Function Declarations ---

pub fn walk_func_decl<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, func_decl: &FuncDecl<D>) {
    v.visit_func_sig(&func_decl.sig);
}

// --- Functions ---

pub fn walk_func_def<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, func_def: &FuncDef<D, T>) {
    v.visit_func_sig(&func_def.sig);
    v.visit_expr(&func_def.body);
}

// --- Function Signatures ---

pub fn walk_func_sig<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, func_sig: &FunctionSig<D>) {
    for param in &func_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &func_sig.params {
        v.visit_param(param);
    }
}

pub fn walk_type_param<V: Visitor<D, T> + ?Sized, D, T>(_v: &mut V, _param: &TypeParam<D>) {}

// --- Method Signatures ---

pub fn walk_method_sig<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, method_sig: &MethodSig<D>) {
    for param in &method_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &method_sig.params {
        v.visit_param(param);
    }
}

// --- Parameters (common) ---

pub fn walk_param<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, param: &Param<D>) {
    v.visit_type_expr(&param.typ);
}

// --- Method Blocks ---

pub fn walk_method_block<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_block: &MethodBlock<D, T>,
) {
    for method_item in &method_block.method_items {
        v.visit_method_item(method_item);
    }
}

pub fn walk_method_item<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    method_item: &MethodItem<D, T>,
) {
    match method_item {
        MethodItem::Decl(method_decl) => v.visit_method_decl(method_decl),
        MethodItem::Def(method_def) => v.visit_method_def(method_def),
    }
}

pub fn walk_method_decl<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, method_decl: &MethodDecl<D>) {
    v.visit_method_sig(&method_decl.sig);
}

pub fn walk_method_def<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, method_def: &MethodDef<D, T>) {
    v.visit_method_sig(&method_def.sig);
    v.visit_expr(&method_def.body);
}

// --- Closure Definitions ---

pub fn walk_closure_def<V: Visitor<D, T> + ?Sized, D, T>(
    _v: &mut V,
    _closure_def: &ClosureDef<D, T>,
) {
    // Closures are also visited at their expression sites; avoid walking the lifted body twice.
}

// --- Blocks ---

pub fn walk_block_item<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, item: &BlockItem<D, T>) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

// --- Bind Patterns ---

pub fn walk_bind_pattern<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, pattern: &BindPattern<D>) {
    match &pattern.kind {
        BindPatternKind::Name { .. } => {}
        BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
            for pattern in patterns {
                v.visit_bind_pattern(pattern);
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                v.visit_bind_pattern(&field.pattern);
            }
        }
    }
}

// --- Match Patterns ---

pub fn walk_match_pattern<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, pattern: &MatchPattern<D>) {
    if let MatchPattern::Tuple { patterns, .. } = pattern {
        for pattern in patterns {
            v.visit_match_pattern(pattern);
        }
    }
}

pub fn walk_match_pattern_bindings<V: Visitor<D, T> + ?Sized, D, T>(
    v: &mut V,
    pattern: &MatchPattern<D>,
) {
    if let MatchPattern::EnumVariant { bindings, .. } = pattern {
        for binding in bindings {
            v.visit_match_pattern_binding(binding);
        }
    }
}

pub fn walk_match_pattern_binding<V: Visitor<D, T> + ?Sized, D, T>(
    _v: &mut V,
    _binding: &MatchPatternBinding<D>,
) {
}

pub fn walk_match_arm<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, arm: &MatchArm<D, T>) {
    v.visit_match_pattern(&arm.pattern);
    v.visit_expr(&arm.body);
}

// --- Expressions ---

pub fn walk_stmt_expr<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, stmt: &StmtExpr<D, T>) {
    match &stmt.kind {
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

pub fn walk_expr<V: Visitor<D, T> + ?Sized, D, T>(v: &mut V, expr: &Expr<D, T>) {
    match &expr.kind {
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

        ExprKind::StructLit { fields, .. } => {
            for field in fields {
                v.visit_expr(&field.value);
            }
        }

        ExprKind::EnumVariant { payload, .. } => {
            for expr in payload {
                v.visit_expr(expr);
            }
        }

        ExprKind::StructUpdate { target, fields } => {
            v.visit_expr(target);
            for field in fields {
                v.visit_expr(&field.value);
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
                v.visit_expr(&arg.expr);
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
                v.visit_expr(&arg.expr);
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
