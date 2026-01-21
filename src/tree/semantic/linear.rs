//! Linear (single-block) semantic expressions for SSA lowering.
//!
//! These nodes represent expressions that are guaranteed to lower without
//! introducing control flow or additional basic blocks.

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree::semantic::{BindPattern, PlaceExpr};
use crate::tree::{BinaryOp, NodeId, UnaryOp};
use crate::types::TypeId;

#[derive(Clone, Debug)]
pub struct LinearExpr {
    pub id: NodeId,
    pub kind: LinearExprKind,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum LinearExprKind {
    Block {
        items: Vec<LinearBlockItem>,
        tail: Option<Box<LinearExpr>>,
    },

    // Literals
    UnitLit,
    IntLit(u64),
    BoolLit(bool),
    CharLit(char),

    // Value ops
    UnaryOp {
        op: UnaryOp,
        expr: Box<LinearExpr>,
    },
    BinOp {
        left: Box<LinearExpr>,
        op: BinaryOp,
        right: Box<LinearExpr>,
    },
    Load {
        place: Box<PlaceExpr>,
    },

    // Calls are kept linear for now (no branching).
    Call {
        callee: Box<LinearExpr>,
        args: Vec<LinearExpr>,
    },

    // Closure reference (lifted to a top-level definition).
    ClosureRef {
        def_id: DefId,
    },
}

#[derive(Clone, Debug)]
pub enum LinearBlockItem {
    Stmt(LinearStmt),
    Expr(LinearExpr),
}

#[derive(Clone, Debug)]
pub struct LinearStmt {
    pub id: NodeId,
    pub kind: LinearStmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum LinearStmtKind {
    LetBind {
        pattern: BindPattern,
        decl_ty: Option<crate::tree::semantic::TypeExpr>,
        value: Box<LinearExpr>,
    },
    VarBind {
        pattern: BindPattern,
        decl_ty: Option<crate::tree::semantic::TypeExpr>,
        value: Box<LinearExpr>,
    },
    VarDecl {
        ident: String,
        def_id: DefId,
        decl_ty: crate::tree::semantic::TypeExpr,
    },
    Assign {
        assignee: Box<PlaceExpr>,
        value: Box<LinearExpr>,
    },
    Return {
        value: Option<Box<LinearExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum LinearizeErrorKind {
    BranchingExpr,
    BranchingStmt,
    UnsupportedExpr,
    UnsupportedStmt,
}

#[derive(Clone, Debug)]
pub struct LinearizeError {
    pub kind: LinearizeErrorKind,
    pub span: Span,
}
