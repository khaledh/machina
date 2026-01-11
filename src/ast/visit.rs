use crate::ast::model::*;

/// AST/HIR visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
/// Example:
/// ```rust
/// use machina::ast::visit::{walk_expr, Expr, Visitor};
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
pub trait Visitor<T = String> {
    // --- Module ---

    fn visit_module(&mut self, module: &Module<T>) {
        walk_module(self, module)
    }

    // --- Type Definitions ---

    fn visit_type_def(&mut self, type_def: &TypeDef<T>) {
        walk_type_def(self, type_def)
    }

    fn visit_struct_fields(&mut self, fields: &[StructField<T>]) {
        walk_struct_fields(self, fields)
    }

    fn visit_struct_field(&mut self, field: &StructField<T>) {
        walk_struct_field(self, field)
    }

    fn visit_enum_variants(&mut self, variants: &[EnumVariant<T>]) {
        walk_enum_variants(self, variants)
    }

    fn visit_enum_variant(&mut self, variant: &EnumVariant<T>) {
        walk_enum_variant(self, variant)
    }

    // --- Type Expressions ---

    fn visit_type_expr(&mut self, type_expr: &TypeExpr<T>) {
        walk_type_expr(self, type_expr)
    }

    // --- Function Declarations ---

    fn visit_func_decl(&mut self, func_decl: &FunctionDecl<T>) {
        walk_func_decl(self, func_decl)
    }

    // --- Functions ---

    fn visit_func_def(&mut self, func_def: &FuncDef<T>) {
        walk_func_def(self, func_def)
    }

    // --- Function Signatures ---

    fn visit_func_sig(&mut self, func_sig: &FunctionSig<T>) {
        walk_func_sig(self, func_sig)
    }

    // --- Method Signatures ---

    fn visit_method_sig(&mut self, method_sig: &MethodSig<T>) {
        walk_method_sig(self, method_sig)
    }

    // --- Parameters (common) ---

    fn visit_param(&mut self, param: &Param<T>) {
        walk_param(self, param)
    }

    // --- Method Blocks ---

    fn visit_method_block(&mut self, method_block: &MethodBlock<T>) {
        walk_method_block(self, method_block)
    }

    fn visit_method(&mut self, method: &Method<T>) {
        walk_method(self, method)
    }

    // --- Blocks ---

    fn visit_block_item(&mut self, item: &BlockItem<T>) {
        walk_block_item(self, item)
    }

    // --- Expressions ---

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr<T>) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_expr(&mut self, expr: &Expr<T>) {
        walk_expr(self, expr)
    }
}

// --- Module ---

pub fn walk_module<V: Visitor<T> + ?Sized, T>(v: &mut V, module: &Module<T>) {
    for decl in &module.decls {
        match decl {
            Decl::TypeDef(type_def) => v.visit_type_def(type_def),
            Decl::FunctionDecl(func_decl) => v.visit_func_decl(func_decl),
            Decl::FuncDef(func_def) => v.visit_func_def(func_def),
            Decl::MethodBlock(method_block) => v.visit_method_block(method_block),
            Decl::ClosureDecl(_) => {} // closures are visited where they are defined
        }
    }
}

// --- Type Definitions ---

pub fn walk_type_def<V: Visitor<T> + ?Sized, T>(v: &mut V, type_def: &TypeDef<T>) {
    match &type_def.kind {
        TypeDefKind::Alias { aliased_ty } => v.visit_type_expr(aliased_ty),
        TypeDefKind::Struct { fields } => v.visit_struct_fields(fields),
        TypeDefKind::Enum { variants } => v.visit_enum_variants(variants),
    }
}

pub fn walk_struct_fields<V: Visitor<T> + ?Sized, T>(v: &mut V, fields: &[StructField<T>]) {
    for field in fields {
        v.visit_struct_field(field);
    }
}

pub fn walk_struct_field<V: Visitor<T> + ?Sized, T>(v: &mut V, field: &StructField<T>) {
    v.visit_type_expr(&field.ty);
}

pub fn walk_enum_variants<V: Visitor<T> + ?Sized, T>(v: &mut V, variants: &[EnumVariant<T>]) {
    for variant in variants {
        v.visit_enum_variant(variant);
    }
}

pub fn walk_enum_variant<V: Visitor<T> + ?Sized, T>(v: &mut V, variant: &EnumVariant<T>) {
    for payload in &variant.payload {
        v.visit_type_expr(payload);
    }
}

// --- Type Expressions ---

pub fn walk_type_expr<V: Visitor<T> + ?Sized, T>(v: &mut V, type_expr: &TypeExpr<T>) {
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
        TypeExprKind::Heap { elem_ty } => v.visit_type_expr(elem_ty),
        TypeExprKind::Fn { params, return_ty } => {
            for param in params {
                v.visit_type_expr(&param.ty);
            }
            v.visit_type_expr(return_ty);
        }
    }
}

// --- Function Declarations ---

pub fn walk_func_decl<V: Visitor<T> + ?Sized, T>(v: &mut V, func_decl: &FunctionDecl<T>) {
    v.visit_func_sig(&func_decl.sig);
}

// --- Functions ---

pub fn walk_func_def<V: Visitor<T> + ?Sized, T>(v: &mut V, func_def: &FuncDef<T>) {
    v.visit_func_sig(&func_def.sig);
    v.visit_expr(&func_def.body);
}

// --- Function Signatures ---

pub fn walk_func_sig<V: Visitor<T> + ?Sized, T>(v: &mut V, func_sig: &FunctionSig<T>) {
    for param in &func_sig.params {
        v.visit_param(param);
    }
}

// --- Method Signatures ---

pub fn walk_method_sig<V: Visitor<T> + ?Sized, T>(v: &mut V, method_sig: &MethodSig<T>) {
    for param in &method_sig.params {
        v.visit_param(param);
    }
}

// --- Parameters (common) ---

pub fn walk_param<V: Visitor<T> + ?Sized, T>(v: &mut V, param: &Param<T>) {
    v.visit_type_expr(&param.typ);
}

// --- Method Blocks ---

pub fn walk_method_block<V: Visitor<T> + ?Sized, T>(v: &mut V, method_block: &MethodBlock<T>) {
    for method in &method_block.methods {
        v.visit_method(method);
    }
}

pub fn walk_method<V: Visitor<T> + ?Sized, T>(v: &mut V, method: &Method<T>) {
    v.visit_method_sig(&method.sig);
    v.visit_expr(&method.body);
}

// --- Blocks ---

pub fn walk_block_item<V: Visitor<T> + ?Sized, T>(v: &mut V, item: &BlockItem<T>) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

// --- Expressions ---

pub fn walk_stmt_expr<V: Visitor<T> + ?Sized, T>(v: &mut V, stmt: &StmtExpr<T>) {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            v.visit_expr(value);
        }
        StmtExprKind::VarDecl { .. } => {}
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

pub fn walk_expr<V: Visitor<T> + ?Sized, T>(v: &mut V, expr: &Expr<T>) {
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
                v.visit_expr(&arm.body);
            }
        }

        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(&arg.expr);
            }
        }
    }
}
