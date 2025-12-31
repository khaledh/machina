use super::*;

/// AST visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
/// Example:
/// ```rust
/// use machina::ast::{walk_expr, Expr, Visitor};
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
pub trait Visitor {
    // --- Module ---

    fn visit_module(&mut self, module: &Module) {
        walk_module(self, module)
    }

    // --- Type Declarations ---

    fn visit_type_decl(&mut self, type_decl: &TypeDecl) {
        walk_type_decl(self, type_decl)
    }

    fn visit_struct_fields(&mut self, fields: &[StructField]) {
        walk_struct_fields(self, fields)
    }

    fn visit_struct_field(&mut self, field: &StructField) {
        walk_struct_field(self, field)
    }

    fn visit_enum_variants(&mut self, variants: &[EnumVariant]) {
        walk_enum_variants(self, variants)
    }

    fn visit_enum_variant(&mut self, variant: &EnumVariant) {
        walk_enum_variant(self, variant)
    }

    // --- Type Expressions ---

    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        walk_type_expr(self, type_expr)
    }

    // --- Function Signatures ---

    fn visit_func_sig(&mut self, func_sig: &FunctionSig) {
        walk_func_sig(self, func_sig)
    }

    fn visit_func_param(&mut self, func_param: &FunctionParam) {
        walk_func_param(self, func_param)
    }

    // --- Function Declarations ---

    fn visit_func_decl(&mut self, func_decl: &FunctionDecl) {
        walk_func_decl(self, func_decl)
    }

    // --- Functions ---

    fn visit_func(&mut self, func: &Function) {
        walk_func(self, func)
    }

    fn visit_block_item(&mut self, item: &BlockItem) {
        walk_block_item(self, item)
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr)
    }
}

// --- Module ---

pub fn walk_module<V: Visitor + ?Sized>(v: &mut V, module: &Module) {
    for decl in &module.decls {
        match decl {
            Decl::TypeDecl(type_decl) => v.visit_type_decl(type_decl),
            Decl::FunctionDecl(func_decl) => v.visit_func_decl(func_decl),
            Decl::Function(func) => v.visit_func(func),
        }
    }
}

// --- Type Declarations ---

pub fn walk_type_decl<V: Visitor + ?Sized>(v: &mut V, type_decl: &TypeDecl) {
    match &type_decl.kind {
        TypeDeclKind::Alias { aliased_ty } => v.visit_type_expr(aliased_ty),
        TypeDeclKind::Struct { fields } => v.visit_struct_fields(fields),
        TypeDeclKind::Enum { variants } => v.visit_enum_variants(variants),
    }
}

pub fn walk_struct_fields<V: Visitor + ?Sized>(v: &mut V, fields: &[StructField]) {
    for field in fields {
        v.visit_struct_field(field);
    }
}

pub fn walk_struct_field<V: Visitor + ?Sized>(v: &mut V, field: &StructField) {
    v.visit_type_expr(&field.ty);
}

pub fn walk_enum_variants<V: Visitor + ?Sized>(v: &mut V, variants: &[EnumVariant]) {
    for variant in variants {
        v.visit_enum_variant(variant);
    }
}

pub fn walk_enum_variant<V: Visitor + ?Sized>(v: &mut V, variant: &EnumVariant) {
    for payload in &variant.payload {
        v.visit_type_expr(payload);
    }
}

// --- Type Expressions ---

pub fn walk_type_expr<V: Visitor + ?Sized>(v: &mut V, type_expr: &TypeExpr) {
    match &type_expr.kind {
        TypeExprKind::Named(_) => {}
        TypeExprKind::Array { elem_ty, .. } => v.visit_type_expr(elem_ty),
        TypeExprKind::Tuple { fields } => {
            for field in fields {
                v.visit_type_expr(field);
            }
        }
        TypeExprKind::Range { .. } => {}
        TypeExprKind::Slice { elem_ty } => v.visit_type_expr(elem_ty),
    }
}

// --- Function Signatures ---

pub fn walk_func_sig<V: Visitor + ?Sized>(v: &mut V, func_sig: &FunctionSig) {
    for param in &func_sig.params {
        v.visit_func_param(param);
    }
}

pub fn walk_func_param<V: Visitor + ?Sized>(v: &mut V, func_param: &FunctionParam) {
    v.visit_type_expr(&func_param.typ);
}

// --- Function Declarations ---

pub fn walk_func_decl<V: Visitor + ?Sized>(v: &mut V, func_decl: &FunctionDecl) {
    v.visit_func_sig(&func_decl.sig);
}

// --- Functions ---

pub fn walk_func<V: Visitor + ?Sized>(v: &mut V, func: &Function) {
    v.visit_func_sig(&func.sig);
    v.visit_expr(&func.body);
}

pub fn walk_block_item<V: Visitor + ?Sized>(v: &mut V, item: &BlockItem) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

pub fn walk_stmt_expr<V: Visitor + ?Sized>(v: &mut V, stmt: &StmtExpr) {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            v.visit_expr(value);
        }
        StmtExprKind::Assign { assignee, value } => {
            v.visit_expr(assignee);
            v.visit_expr(value);
        }
        StmtExprKind::While { cond, body } => {
            v.visit_expr(cond);
            v.visit_expr(body);
        }
        StmtExprKind::For { iter, body, .. } => {
            v.visit_expr(iter);
            v.visit_expr(body);
        }
    }
}

pub fn walk_expr<V: Visitor + ?Sized>(v: &mut V, expr: &Expr) {
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
        | ExprKind::Var(_)
        | ExprKind::Range { .. } => {}
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
                v.visit_expr(&arm.body);
            }
        }
        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(arg);
            }
        }
    }
}
