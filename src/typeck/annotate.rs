use crate::tree::resolved as res;
use crate::tree::typed::model as typ;
use crate::typeck::type_map::TypeMap;
use crate::types::TypeId;

pub fn annotate_module(module: &res::Module, type_map: &TypeMap) -> typ::Module {
    let top_level_items = module
        .top_level_items
        .iter()
        .map(|item| annotate_top_level_item(item, type_map))
        .collect();
    typ::Module { top_level_items }
}

fn annotate_top_level_item(item: &res::TopLevelItem, type_map: &TypeMap) -> typ::TopLevelItem {
    match item {
        res::TopLevelItem::TypeDef(type_def) => typ::TopLevelItem::TypeDef(type_def.clone()),
        res::TopLevelItem::FuncDecl(func_decl) => typ::TopLevelItem::FuncDecl(func_decl.clone()),
        res::TopLevelItem::FuncDef(func_def) => {
            typ::TopLevelItem::FuncDef(annotate_func_def(func_def, type_map))
        }
        res::TopLevelItem::MethodBlock(method_block) => {
            typ::TopLevelItem::MethodBlock(annotate_method_block(method_block, type_map))
        }
        res::TopLevelItem::ClosureDef(closure_def) => {
            typ::TopLevelItem::ClosureDef(annotate_closure_def(closure_def, type_map))
        }
    }
}

fn annotate_func_def(func_def: &res::FuncDef, type_map: &TypeMap) -> typ::FuncDef {
    typ::FuncDef {
        id: func_def.id,
        def_id: func_def.def_id,
        sig: func_def.sig.clone(),
        body: annotate_expr(&func_def.body, type_map),
        span: func_def.span,
    }
}

fn annotate_method_block(method_block: &res::MethodBlock, type_map: &TypeMap) -> typ::MethodBlock {
    let method_defs: Vec<typ::MethodDef> = method_block
        .method_defs
        .iter()
        .map(|method_def| annotate_method_def(method_def, type_map))
        .collect();
    typ::MethodBlock {
        id: method_block.id,
        type_name: method_block.type_name.clone(),
        method_defs,
        span: method_block.span,
    }
}

fn annotate_method_def(method_def: &res::MethodDef, type_map: &TypeMap) -> typ::MethodDef {
    typ::MethodDef {
        id: method_def.id,
        def_id: method_def.def_id,
        sig: method_def.sig.clone(),
        body: annotate_expr(&method_def.body, type_map),
        span: method_def.span,
    }
}

fn annotate_closure_def(closure_def: &res::ClosureDef, type_map: &TypeMap) -> typ::ClosureDef {
    typ::ClosureDef {
        id: closure_def.id,
        def_id: closure_def.def_id,
        sig: closure_def.sig.clone(),
        body: annotate_expr(&closure_def.body, type_map),
        span: closure_def.span,
    }
}

fn annotate_block_item(item: &res::BlockItem, type_map: &TypeMap) -> typ::BlockItem {
    match item {
        res::BlockItem::Stmt(stmt) => typ::BlockItem::Stmt(annotate_stmt_expr(stmt, type_map)),
        res::BlockItem::Expr(expr) => typ::BlockItem::Expr(annotate_expr(expr, type_map)),
    }
}

fn annotate_stmt_expr(stmt: &res::StmtExpr, type_map: &TypeMap) -> typ::StmtExpr {
    let kind = match &stmt.kind {
        res::StmtExprKind::LetBind {
            pattern,
            decl_ty,
            value,
        } => typ::StmtExprKind::LetBind {
            pattern: pattern.clone(),
            decl_ty: decl_ty.clone(),
            value: Box::new(annotate_expr(value, type_map)),
        },
        res::StmtExprKind::VarBind {
            pattern,
            decl_ty,
            value,
        } => typ::StmtExprKind::VarBind {
            pattern: pattern.clone(),
            decl_ty: decl_ty.clone(),
            value: Box::new(annotate_expr(value, type_map)),
        },
        res::StmtExprKind::VarDecl {
            ident,
            def_id,
            decl_ty,
        } => typ::StmtExprKind::VarDecl {
            ident: ident.clone(),
            def_id: *def_id,
            decl_ty: decl_ty.clone(),
        },
        res::StmtExprKind::Assign {
            assignee,
            value,
            init,
        } => typ::StmtExprKind::Assign {
            assignee: Box::new(annotate_expr(assignee, type_map)),
            value: Box::new(annotate_expr(value, type_map)),
            init: *init,
        },
        res::StmtExprKind::While { cond, body } => typ::StmtExprKind::While {
            cond: Box::new(annotate_expr(cond, type_map)),
            body: Box::new(annotate_expr(body, type_map)),
        },
        res::StmtExprKind::For {
            pattern,
            iter,
            body,
        } => typ::StmtExprKind::For {
            pattern: pattern.clone(),
            iter: Box::new(annotate_expr(iter, type_map)),
            body: Box::new(annotate_expr(body, type_map)),
        },
    };

    typ::StmtExpr {
        id: stmt.id,
        kind,
        ty: node_type_id(type_map, stmt.id),
        span: stmt.span,
    }
}

fn annotate_expr(expr: &res::Expr, type_map: &TypeMap) -> typ::Expr {
    let kind = match &expr.kind {
        res::ExprKind::Block { items, tail } => typ::ExprKind::Block {
            items: items
                .iter()
                .map(|item| annotate_block_item(item, type_map))
                .collect(),
            tail: tail
                .as_ref()
                .map(|expr| Box::new(annotate_expr(expr, type_map))),
        },
        res::ExprKind::UnitLit => typ::ExprKind::UnitLit,
        res::ExprKind::IntLit(value) => typ::ExprKind::IntLit(*value),
        res::ExprKind::BoolLit(value) => typ::ExprKind::BoolLit(*value),
        res::ExprKind::CharLit(value) => typ::ExprKind::CharLit(*value),
        res::ExprKind::StringLit { value } => typ::ExprKind::StringLit {
            value: value.clone(),
        },
        res::ExprKind::StringFmt { segments } => typ::ExprKind::StringFmt {
            segments: segments
                .iter()
                .map(|segment| annotate_string_segment(segment, type_map))
                .collect(),
        },
        res::ExprKind::ArrayLit { elem_ty, init } => typ::ExprKind::ArrayLit {
            elem_ty: elem_ty.clone(),
            init: annotate_array_lit_init(init, type_map),
        },
        res::ExprKind::TupleLit(items) => {
            typ::ExprKind::TupleLit(items.iter().map(|e| annotate_expr(e, type_map)).collect())
        }
        res::ExprKind::StructLit { name, fields } => typ::ExprKind::StructLit {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| annotate_struct_lit_field(field, type_map))
                .collect(),
        },
        res::ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
        } => typ::ExprKind::EnumVariant {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            payload: payload
                .iter()
                .map(|expr| annotate_expr(expr, type_map))
                .collect(),
        },
        res::ExprKind::StructUpdate { target, fields } => typ::ExprKind::StructUpdate {
            target: Box::new(annotate_expr(target, type_map)),
            fields: fields
                .iter()
                .map(|field| annotate_struct_update_field(field, type_map))
                .collect(),
        },
        res::ExprKind::BinOp { left, op, right } => typ::ExprKind::BinOp {
            left: Box::new(annotate_expr(left, type_map)),
            op: *op,
            right: Box::new(annotate_expr(right, type_map)),
        },
        res::ExprKind::UnaryOp { op, expr } => typ::ExprKind::UnaryOp {
            op: *op,
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        res::ExprKind::HeapAlloc { expr } => typ::ExprKind::HeapAlloc {
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        res::ExprKind::Move { expr } => typ::ExprKind::Move {
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        res::ExprKind::Coerce { kind, expr } => typ::ExprKind::Coerce {
            kind: *kind,
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        res::ExprKind::ImplicitMove { expr } => typ::ExprKind::ImplicitMove {
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        res::ExprKind::Var { ident, def_id } => typ::ExprKind::Var {
            ident: ident.clone(),
            def_id: *def_id,
        },
        res::ExprKind::ArrayIndex { target, indices } => typ::ExprKind::ArrayIndex {
            target: Box::new(annotate_expr(target, type_map)),
            indices: indices
                .iter()
                .map(|expr| annotate_expr(expr, type_map))
                .collect(),
        },
        res::ExprKind::TupleField { target, index } => typ::ExprKind::TupleField {
            target: Box::new(annotate_expr(target, type_map)),
            index: *index,
        },
        res::ExprKind::StructField { target, field } => typ::ExprKind::StructField {
            target: Box::new(annotate_expr(target, type_map)),
            field: field.clone(),
        },
        res::ExprKind::If {
            cond,
            then_body,
            else_body,
        } => typ::ExprKind::If {
            cond: Box::new(annotate_expr(cond, type_map)),
            then_body: Box::new(annotate_expr(then_body, type_map)),
            else_body: Box::new(annotate_expr(else_body, type_map)),
        },
        res::ExprKind::Range { start, end } => typ::ExprKind::Range {
            start: *start,
            end: *end,
        },
        res::ExprKind::Slice { target, start, end } => typ::ExprKind::Slice {
            target: Box::new(annotate_expr(target, type_map)),
            start: start
                .as_ref()
                .map(|expr| Box::new(annotate_expr(expr, type_map))),
            end: end
                .as_ref()
                .map(|expr| Box::new(annotate_expr(expr, type_map))),
        },
        res::ExprKind::Match { scrutinee, arms } => typ::ExprKind::Match {
            scrutinee: Box::new(annotate_expr(scrutinee, type_map)),
            arms: arms
                .iter()
                .map(|arm| annotate_match_arm(arm, type_map))
                .collect(),
        },
        res::ExprKind::Call { callee, args } => typ::ExprKind::Call {
            callee: Box::new(annotate_expr(callee, type_map)),
            args: args
                .iter()
                .map(|arg| annotate_call_arg(arg, type_map))
                .collect(),
        },
        res::ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } => typ::ExprKind::MethodCall {
            callee: Box::new(annotate_expr(callee, type_map)),
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| annotate_call_arg(arg, type_map))
                .collect(),
        },
        res::ExprKind::Closure {
            ident,
            def_id,
            params,
            return_ty,
            body,
        } => typ::ExprKind::Closure {
            ident: ident.clone(),
            def_id: *def_id,
            params: params.clone(),
            return_ty: return_ty.clone(),
            body: Box::new(annotate_expr(body, type_map)),
        },
    };

    typ::Expr {
        id: expr.id,
        kind,
        ty: node_type_id(type_map, expr.id),
        span: expr.span,
    }
}

fn annotate_match_arm(arm: &res::MatchArm, type_map: &TypeMap) -> typ::MatchArm {
    typ::MatchArm {
        id: arm.id,
        pattern: arm.pattern.clone(),
        body: annotate_expr(&arm.body, type_map),
        span: arm.span,
    }
}

fn annotate_call_arg(arg: &res::CallArg, type_map: &TypeMap) -> typ::CallArg {
    typ::CallArg {
        mode: arg.mode,
        expr: annotate_expr(&arg.expr, type_map),
        init: arg.init,
        span: arg.span,
    }
}

fn annotate_array_lit_init(init: &res::ArrayLitInit, type_map: &TypeMap) -> typ::ArrayLitInit {
    match init {
        res::ArrayLitInit::Elems(elems) => typ::ArrayLitInit::Elems(
            elems
                .iter()
                .map(|expr| annotate_expr(expr, type_map))
                .collect(),
        ),
        res::ArrayLitInit::Repeat(expr, count) => {
            typ::ArrayLitInit::Repeat(Box::new(annotate_expr(expr, type_map)), *count)
        }
    }
}

fn annotate_struct_lit_field(
    field: &res::StructLitField,
    type_map: &TypeMap,
) -> typ::StructLitField {
    typ::StructLitField {
        id: field.id,
        name: field.name.clone(),
        value: annotate_expr(&field.value, type_map),
        span: field.span,
    }
}

fn annotate_struct_update_field(
    field: &res::StructUpdateField,
    type_map: &TypeMap,
) -> typ::StructUpdateField {
    typ::StructUpdateField {
        id: field.id,
        name: field.name.clone(),
        value: annotate_expr(&field.value, type_map),
        span: field.span,
    }
}

fn annotate_string_segment(
    segment: &res::StringFmtSegment,
    type_map: &TypeMap,
) -> typ::StringFmtSegment {
    match segment {
        res::StringFmtSegment::Literal { value, span } => typ::StringFmtSegment::Literal {
            value: value.clone(),
            span: *span,
        },
        res::StringFmtSegment::Expr { expr, span } => typ::StringFmtSegment::Expr {
            expr: Box::new(annotate_expr(expr, type_map)),
            span: *span,
        },
    }
}

fn node_type_id(type_map: &TypeMap, id: crate::tree::NodeId) -> TypeId {
    type_map
        .lookup_node_type_id(id)
        .expect("missing type id for node")
}
