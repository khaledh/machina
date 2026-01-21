//! Linearization utilities for SSA lowering.
//!
//! Converts semantic value expressions into a linear (single-block) subset
//! that guarantees no control flow is introduced when lowering.

use crate::tree::semantic as sem;

pub fn linearize_expr(expr: &sem::ValueExpr) -> Result<sem::LinearExpr, sem::LinearizeError> {
    match &expr.kind {
        sem::ValueExprKind::Block { items, tail } => {
            let mut linear_items = Vec::with_capacity(items.len());
            for item in items {
                linear_items.push(linearize_block_item(item)?);
            }
            let linear_tail = match tail {
                Some(tail) => Some(Box::new(linearize_expr(tail)?)),
                None => None,
            };
            Ok(sem::LinearExpr {
                id: expr.id,
                kind: sem::LinearExprKind::Block {
                    items: linear_items,
                    tail: linear_tail,
                },
                ty: expr.ty,
                span: expr.span,
            })
        }
        sem::ValueExprKind::UnitLit => Ok(wrap_simple(expr, sem::LinearExprKind::UnitLit)),
        sem::ValueExprKind::IntLit(value) => {
            Ok(wrap_simple(expr, sem::LinearExprKind::IntLit(*value)))
        }
        sem::ValueExprKind::BoolLit(value) => {
            Ok(wrap_simple(expr, sem::LinearExprKind::BoolLit(*value)))
        }
        sem::ValueExprKind::CharLit(value) => {
            Ok(wrap_simple(expr, sem::LinearExprKind::CharLit(*value)))
        }
        sem::ValueExprKind::UnaryOp { op, expr } => {
            let value_expr = linearize_expr(expr)?;
            Ok(sem::LinearExpr {
                id: expr.id,
                kind: sem::LinearExprKind::UnaryOp {
                    op: *op,
                    expr: Box::new(value_expr),
                },
                ty: expr.ty,
                span: expr.span,
            })
        }
        sem::ValueExprKind::BinOp { left, op, right } => {
            let left = Box::new(linearize_expr(left)?);
            let right = Box::new(linearize_expr(right)?);
            Ok(sem::LinearExpr {
                id: expr.id,
                kind: sem::LinearExprKind::BinOp {
                    left,
                    op: *op,
                    right,
                },
                ty: expr.ty,
                span: expr.span,
            })
        }
        sem::ValueExprKind::Load { place } => Ok(sem::LinearExpr {
            id: expr.id,
            kind: sem::LinearExprKind::Load {
                place: place.clone(),
            },
            ty: expr.ty,
            span: expr.span,
        }),
        sem::ValueExprKind::Call { .. } => Err(sem::LinearizeError {
            kind: sem::LinearizeErrorKind::UnsupportedExpr,
            span: expr.span,
        }),
        sem::ValueExprKind::ClosureRef { def_id } => Ok(sem::LinearExpr {
            id: expr.id,
            kind: sem::LinearExprKind::ClosureRef { def_id: *def_id },
            ty: expr.ty,
            span: expr.span,
        }),
        sem::ValueExprKind::HeapAlloc { .. }
        | sem::ValueExprKind::Coerce { .. }
        | sem::ValueExprKind::Len { .. } => Err(sem::LinearizeError {
            kind: sem::LinearizeErrorKind::UnsupportedExpr,
            span: expr.span,
        }),
        sem::ValueExprKind::If { .. }
        | sem::ValueExprKind::MethodCall { .. }
        | sem::ValueExprKind::Match { .. }
        | sem::ValueExprKind::Range { .. }
        | sem::ValueExprKind::Slice { .. }
        | sem::ValueExprKind::StringFmt { .. }
        | sem::ValueExprKind::StringLit { .. }
        | sem::ValueExprKind::ArrayLit { .. }
        | sem::ValueExprKind::TupleLit(_)
        | sem::ValueExprKind::StructLit { .. }
        | sem::ValueExprKind::EnumVariant { .. }
        | sem::ValueExprKind::StructUpdate { .. }
        | sem::ValueExprKind::AddrOf { .. }
        | sem::ValueExprKind::Move { .. }
        | sem::ValueExprKind::ImplicitMove { .. } => Err(sem::LinearizeError {
            kind: sem::LinearizeErrorKind::BranchingExpr,
            span: expr.span,
        }),
    }
}

pub fn linearize_block_item(
    item: &sem::BlockItem,
) -> Result<sem::LinearBlockItem, sem::LinearizeError> {
    match item {
        sem::BlockItem::Stmt(stmt) => Ok(sem::LinearBlockItem::Stmt(linearize_stmt(stmt)?)),
        sem::BlockItem::Expr(expr) => Ok(sem::LinearBlockItem::Expr(linearize_expr(expr)?)),
    }
}

pub fn linearize_stmt(stmt: &sem::StmtExpr) -> Result<sem::LinearStmt, sem::LinearizeError> {
    let kind = match &stmt.kind {
        sem::StmtExprKind::LetBind {
            pattern,
            decl_ty,
            value,
        } => sem::LinearStmtKind::LetBind {
            pattern: pattern.clone(),
            decl_ty: decl_ty.clone(),
            value: Box::new(linearize_expr(value)?),
        },
        sem::StmtExprKind::VarBind {
            pattern,
            decl_ty,
            value,
        } => sem::LinearStmtKind::VarBind {
            pattern: pattern.clone(),
            decl_ty: decl_ty.clone(),
            value: Box::new(linearize_expr(value)?),
        },
        sem::StmtExprKind::VarDecl {
            ident,
            def_id,
            decl_ty,
        } => sem::LinearStmtKind::VarDecl {
            ident: ident.clone(),
            def_id: *def_id,
            decl_ty: decl_ty.clone(),
        },
        sem::StmtExprKind::Assign {
            assignee, value, ..
        } => sem::LinearStmtKind::Assign {
            assignee: assignee.clone(),
            value: Box::new(linearize_expr(value)?),
        },
        sem::StmtExprKind::Return { value } => sem::LinearStmtKind::Return {
            value: match value {
                Some(value) => Some(Box::new(linearize_expr(value)?)),
                None => None,
            },
        },
        sem::StmtExprKind::While { .. }
        | sem::StmtExprKind::For { .. }
        | sem::StmtExprKind::Break
        | sem::StmtExprKind::Continue => {
            return Err(sem::LinearizeError {
                kind: sem::LinearizeErrorKind::BranchingStmt,
                span: stmt.span,
            });
        }
    };

    Ok(sem::LinearStmt {
        id: stmt.id,
        kind,
        span: stmt.span,
    })
}

fn wrap_simple(expr: &sem::ValueExpr, kind: sem::LinearExprKind) -> sem::LinearExpr {
    sem::LinearExpr {
        id: expr.id,
        kind,
        ty: expr.ty,
        span: expr.span,
    }
}
