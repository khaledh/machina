use crate::hir::model as hir;
use crate::tir::model as tir;
use crate::typeck::type_map::TypeMap;
use crate::types::TypeId;

pub fn annotate_module(module: &hir::Module, type_map: &TypeMap) -> tir::Module {
    let top_level_items = module
        .top_level_items
        .iter()
        .map(|item| annotate_top_level_item(item, type_map))
        .collect();
    tir::Module { top_level_items }
}

fn annotate_top_level_item(item: &hir::TopLevelItem, type_map: &TypeMap) -> tir::TopLevelItem {
    match item {
        hir::TopLevelItem::TypeDef(type_def) => tir::TopLevelItem::TypeDef(type_def.clone()),
        hir::TopLevelItem::FuncDecl(func_decl) => tir::TopLevelItem::FuncDecl(func_decl.clone()),
        hir::TopLevelItem::FuncDef(func_def) => {
            tir::TopLevelItem::FuncDef(annotate_func_def(func_def, type_map))
        }
        hir::TopLevelItem::MethodBlock(method_block) => {
            tir::TopLevelItem::MethodBlock(annotate_method_block(method_block, type_map))
        }
        hir::TopLevelItem::ClosureDecl(closure_decl) => {
            tir::TopLevelItem::ClosureDecl(annotate_closure_decl(closure_decl, type_map))
        }
    }
}

fn annotate_func_def(func_def: &hir::FuncDef, type_map: &TypeMap) -> tir::FuncDef {
    tir::FuncDef {
        id: func_def.id,
        def_id: func_def.def_id,
        sig: func_def.sig.clone(),
        body: annotate_expr(&func_def.body, type_map),
        span: func_def.span,
    }
}

fn annotate_method_block(method_block: &hir::MethodBlock, type_map: &TypeMap) -> tir::MethodBlock {
    let method_defs: Vec<tir::MethodDef> = method_block
        .method_defs
        .iter()
        .map(|method_def| annotate_method_def(method_def, type_map))
        .collect();
    tir::MethodBlock {
        id: method_block.id,
        type_name: method_block.type_name.clone(),
        method_defs,
        span: method_block.span,
    }
}

fn annotate_method_def(method_def: &hir::MethodDef, type_map: &TypeMap) -> tir::MethodDef {
    tir::MethodDef {
        id: method_def.id,
        def_id: method_def.def_id,
        sig: method_def.sig.clone(),
        body: annotate_expr(&method_def.body, type_map),
        span: method_def.span,
    }
}

fn annotate_closure_decl(closure_decl: &hir::ClosureDecl, type_map: &TypeMap) -> tir::ClosureDecl {
    tir::ClosureDecl {
        id: closure_decl.id,
        def_id: closure_decl.def_id,
        sig: closure_decl.sig.clone(),
        body: annotate_expr(&closure_decl.body, type_map),
        span: closure_decl.span,
    }
}

fn annotate_block_item(item: &hir::BlockItem, type_map: &TypeMap) -> tir::BlockItem {
    match item {
        hir::BlockItem::Stmt(stmt) => tir::BlockItem::Stmt(annotate_stmt_expr(stmt, type_map)),
        hir::BlockItem::Expr(expr) => tir::BlockItem::Expr(annotate_expr(expr, type_map)),
    }
}

fn annotate_stmt_expr(stmt: &hir::StmtExpr, type_map: &TypeMap) -> tir::StmtExpr {
    let kind = match &stmt.kind {
        hir::StmtExprKind::LetBind {
            pattern,
            decl_ty,
            value,
        } => tir::StmtExprKind::LetBind {
            pattern: pattern.clone(),
            decl_ty: decl_ty.clone(),
            value: Box::new(annotate_expr(value, type_map)),
        },
        hir::StmtExprKind::VarBind {
            pattern,
            decl_ty,
            value,
        } => tir::StmtExprKind::VarBind {
            pattern: pattern.clone(),
            decl_ty: decl_ty.clone(),
            value: Box::new(annotate_expr(value, type_map)),
        },
        hir::StmtExprKind::VarDecl {
            ident,
            def_id,
            decl_ty,
        } => tir::StmtExprKind::VarDecl {
            ident: ident.clone(),
            def_id: *def_id,
            decl_ty: decl_ty.clone(),
        },
        hir::StmtExprKind::Assign { assignee, value } => tir::StmtExprKind::Assign {
            assignee: Box::new(annotate_expr(assignee, type_map)),
            value: Box::new(annotate_expr(value, type_map)),
        },
        hir::StmtExprKind::While { cond, body } => tir::StmtExprKind::While {
            cond: Box::new(annotate_expr(cond, type_map)),
            body: Box::new(annotate_expr(body, type_map)),
        },
        hir::StmtExprKind::For {
            pattern,
            iter,
            body,
        } => tir::StmtExprKind::For {
            pattern: pattern.clone(),
            iter: Box::new(annotate_expr(iter, type_map)),
            body: Box::new(annotate_expr(body, type_map)),
        },
    };

    tir::StmtExpr {
        id: stmt.id,
        kind,
        ty: node_type_id(type_map, stmt.id),
        span: stmt.span,
    }
}

fn annotate_expr(expr: &hir::Expr, type_map: &TypeMap) -> tir::Expr {
    let kind = match &expr.kind {
        hir::ExprKind::Block { items, tail } => tir::ExprKind::Block {
            items: items
                .iter()
                .map(|item| annotate_block_item(item, type_map))
                .collect(),
            tail: tail
                .as_ref()
                .map(|expr| Box::new(annotate_expr(expr, type_map))),
        },
        hir::ExprKind::UnitLit => tir::ExprKind::UnitLit,
        hir::ExprKind::IntLit(value) => tir::ExprKind::IntLit(*value),
        hir::ExprKind::BoolLit(value) => tir::ExprKind::BoolLit(*value),
        hir::ExprKind::CharLit(value) => tir::ExprKind::CharLit(*value),
        hir::ExprKind::StringLit { value } => tir::ExprKind::StringLit {
            value: value.clone(),
        },
        hir::ExprKind::StringFmt { segments } => tir::ExprKind::StringFmt {
            segments: segments
                .iter()
                .map(|segment| annotate_string_segment(segment, type_map))
                .collect(),
        },
        hir::ExprKind::ArrayLit { elem_ty, init } => tir::ExprKind::ArrayLit {
            elem_ty: elem_ty.clone(),
            init: annotate_array_lit_init(init, type_map),
        },
        hir::ExprKind::TupleLit(items) => {
            tir::ExprKind::TupleLit(items.iter().map(|e| annotate_expr(e, type_map)).collect())
        }
        hir::ExprKind::StructLit { name, fields } => tir::ExprKind::StructLit {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| annotate_struct_lit_field(field, type_map))
                .collect(),
        },
        hir::ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
        } => tir::ExprKind::EnumVariant {
            enum_name: enum_name.clone(),
            variant: variant.clone(),
            payload: payload
                .iter()
                .map(|expr| annotate_expr(expr, type_map))
                .collect(),
        },
        hir::ExprKind::StructUpdate { target, fields } => tir::ExprKind::StructUpdate {
            target: Box::new(annotate_expr(target, type_map)),
            fields: fields
                .iter()
                .map(|field| annotate_struct_update_field(field, type_map))
                .collect(),
        },
        hir::ExprKind::BinOp { left, op, right } => tir::ExprKind::BinOp {
            left: Box::new(annotate_expr(left, type_map)),
            op: *op,
            right: Box::new(annotate_expr(right, type_map)),
        },
        hir::ExprKind::UnaryOp { op, expr } => tir::ExprKind::UnaryOp {
            op: *op,
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        hir::ExprKind::HeapAlloc { expr } => tir::ExprKind::HeapAlloc {
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        hir::ExprKind::Move { expr } => tir::ExprKind::Move {
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        hir::ExprKind::Coerce { kind, expr } => tir::ExprKind::Coerce {
            kind: *kind,
            expr: Box::new(annotate_expr(expr, type_map)),
        },
        hir::ExprKind::Var { ident, def_id } => tir::ExprKind::Var {
            ident: ident.clone(),
            def_id: *def_id,
        },
        hir::ExprKind::ArrayIndex { target, indices } => tir::ExprKind::ArrayIndex {
            target: Box::new(annotate_expr(target, type_map)),
            indices: indices
                .iter()
                .map(|expr| annotate_expr(expr, type_map))
                .collect(),
        },
        hir::ExprKind::TupleField { target, index } => tir::ExprKind::TupleField {
            target: Box::new(annotate_expr(target, type_map)),
            index: *index,
        },
        hir::ExprKind::StructField { target, field } => tir::ExprKind::StructField {
            target: Box::new(annotate_expr(target, type_map)),
            field: field.clone(),
        },
        hir::ExprKind::If {
            cond,
            then_body,
            else_body,
        } => tir::ExprKind::If {
            cond: Box::new(annotate_expr(cond, type_map)),
            then_body: Box::new(annotate_expr(then_body, type_map)),
            else_body: Box::new(annotate_expr(else_body, type_map)),
        },
        hir::ExprKind::Range { start, end } => tir::ExprKind::Range {
            start: *start,
            end: *end,
        },
        hir::ExprKind::Slice { target, start, end } => tir::ExprKind::Slice {
            target: Box::new(annotate_expr(target, type_map)),
            start: start
                .as_ref()
                .map(|expr| Box::new(annotate_expr(expr, type_map))),
            end: end
                .as_ref()
                .map(|expr| Box::new(annotate_expr(expr, type_map))),
        },
        hir::ExprKind::Match { scrutinee, arms } => tir::ExprKind::Match {
            scrutinee: Box::new(annotate_expr(scrutinee, type_map)),
            arms: arms
                .iter()
                .map(|arm| annotate_match_arm(arm, type_map))
                .collect(),
        },
        hir::ExprKind::Call { callee, args } => tir::ExprKind::Call {
            callee: Box::new(annotate_expr(callee, type_map)),
            args: args
                .iter()
                .map(|arg| annotate_call_arg(arg, type_map))
                .collect(),
        },
        hir::ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } => tir::ExprKind::MethodCall {
            callee: Box::new(annotate_expr(callee, type_map)),
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| annotate_call_arg(arg, type_map))
                .collect(),
        },
        hir::ExprKind::Closure {
            ident,
            def_id,
            params,
            return_ty,
            body,
        } => tir::ExprKind::Closure {
            ident: ident.clone(),
            def_id: *def_id,
            params: params.clone(),
            return_ty: return_ty.clone(),
            body: Box::new(annotate_expr(body, type_map)),
        },
    };

    tir::Expr {
        id: expr.id,
        kind,
        ty: node_type_id(type_map, expr.id),
        span: expr.span,
    }
}

fn annotate_match_arm(arm: &hir::MatchArm, type_map: &TypeMap) -> tir::MatchArm {
    tir::MatchArm {
        id: arm.id,
        pattern: arm.pattern.clone(),
        body: annotate_expr(&arm.body, type_map),
        span: arm.span,
    }
}

fn annotate_call_arg(arg: &hir::CallArg, type_map: &TypeMap) -> tir::CallArg {
    tir::CallArg {
        mode: arg.mode,
        expr: annotate_expr(&arg.expr, type_map),
        span: arg.span,
    }
}

fn annotate_array_lit_init(init: &hir::ArrayLitInit, type_map: &TypeMap) -> tir::ArrayLitInit {
    match init {
        hir::ArrayLitInit::Elems(elems) => tir::ArrayLitInit::Elems(
            elems
                .iter()
                .map(|expr| annotate_expr(expr, type_map))
                .collect(),
        ),
        hir::ArrayLitInit::Repeat(expr, count) => {
            tir::ArrayLitInit::Repeat(Box::new(annotate_expr(expr, type_map)), *count)
        }
    }
}

fn annotate_struct_lit_field(
    field: &hir::StructLitField,
    type_map: &TypeMap,
) -> tir::StructLitField {
    tir::StructLitField {
        id: field.id,
        name: field.name.clone(),
        value: annotate_expr(&field.value, type_map),
        span: field.span,
    }
}

fn annotate_struct_update_field(
    field: &hir::StructUpdateField,
    type_map: &TypeMap,
) -> tir::StructUpdateField {
    tir::StructUpdateField {
        id: field.id,
        name: field.name.clone(),
        value: annotate_expr(&field.value, type_map),
        span: field.span,
    }
}

fn annotate_string_segment(
    segment: &hir::StringFmtSegment,
    type_map: &TypeMap,
) -> tir::StringFmtSegment {
    match segment {
        hir::StringFmtSegment::Literal { value, span } => tir::StringFmtSegment::Literal {
            value: value.clone(),
            span: *span,
        },
        hir::StringFmtSegment::Expr { expr, span } => tir::StringFmtSegment::Expr {
            expr: Box::new(annotate_expr(expr, type_map)),
            span: *span,
        },
    }
}

fn node_type_id(type_map: &TypeMap, id: crate::ast::NodeId) -> TypeId {
    type_map
        .lookup_node_type_id(id)
        .expect("missing type id for node")
}
