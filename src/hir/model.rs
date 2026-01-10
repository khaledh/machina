//! Resolved HIR model (prototype).
//!
//! This is a minimal, tree-shaped HIR that mirrors the AST but replaces
//! identifier strings with DefIds via `Ident`. It is not wired into the
//! pipeline yet; it's a scaffold for review.

use crate::ast::{BinaryOp, CallArgMode, NodeId, ParamMode, UnaryOp};
use crate::diag::Span;
use crate::resolve::def_map::DefId;

// -----------------------------------------------------------------------------
// Identifiers
// -----------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub def_id: DefId,
    pub span: Span,
}

// -----------------------------------------------------------------------------
// Module
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub enum Decl {
    TypeDecl(TypeDecl),
    FunctionDecl(FunctionDecl),
    Function(Function),
    MethodBlock(MethodBlock),
    Closure(Closure),
}

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct TypeDecl {
    pub id: NodeId,
    pub name: String,
    pub kind: TypeDeclKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeDeclKind {
    Alias { aliased_ty: TypeExpr },
    Struct { fields: Vec<StructField> },
    Enum { variants: Vec<EnumVariant> },
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub id: NodeId,
    pub name: String,
    pub payload: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeExpr {
    pub id: NodeId,
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExprKind {
    Named(Ident),
    Array {
        elem_ty: Box<TypeExpr>,
        dims: Vec<usize>,
    },
    Tuple {
        fields: Vec<TypeExpr>,
    },
    Range {
        min: u64,
        max: u64,
    },
    Slice {
        elem_ty: Box<TypeExpr>,
    },
    Heap {
        elem_ty: Box<TypeExpr>,
    },
    Fn {
        params: Vec<FnTypeParam>,
        return_ty: Box<TypeExpr>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnTypeParam {
    pub mode: ParamMode,
    pub ty: TypeExpr,
}

// -----------------------------------------------------------------------------
// Functions / Methods
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub id: NodeId,
    pub sig: FunctionSig,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub sig: FunctionSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionSig {
    pub id: NodeId,
    pub name: Ident,
    pub params: Vec<Param>,
    pub return_type: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodBlock {
    pub id: NodeId,
    pub name: String,
    pub methods: Vec<Method>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub id: NodeId,
    pub sig: MethodSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodSig {
    pub id: NodeId,
    pub name: String,
    pub self_param: SelfParam,
    pub params: Vec<Param>,
    pub return_type: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SelfParam {
    pub id: NodeId,
    pub mode: ParamMode,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub name: Ident,
    pub typ: TypeExpr,
    pub mode: ParamMode,
    pub span: Span,
}

// -----------------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Block {
        items: Vec<BlockItem>,
        tail: Option<Box<Expr>>,
    },

    // Literals (scalar)
    UnitLit,
    IntLit(u64),
    BoolLit(bool),
    CharLit(char),
    StringLit {
        value: String,
    },
    StringFmt {
        segments: Vec<StringFmtSegment>,
    },

    // Literals (compound)
    ArrayLit {
        elem_ty: Option<TypeExpr>,
        init: ArrayLitInit,
    },
    TupleLit(Vec<Expr>),
    StructLit {
        name: String,
        fields: Vec<StructLitField>,
    },
    EnumVariant {
        enum_name: String,
        variant: String,
        payload: Vec<Expr>,
    },

    // Struct update
    StructUpdate {
        target: Box<Expr>,
        fields: Vec<StructUpdateField>,
    },

    // Operators
    BinOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    // Heap allocation
    HeapAlloc {
        expr: Box<Expr>,
    },

    // Move
    Move {
        expr: Box<Expr>,
    },

    // Var, array index, tuple field, struct field
    Var(Ident),
    ArrayIndex {
        target: Box<Expr>,
        indices: Vec<Expr>,
    },
    TupleField {
        target: Box<Expr>,
        index: usize,
    },
    StructField {
        target: Box<Expr>,
        field: String,
    },

    // Control flow
    If {
        cond: Box<Expr>,
        then_body: Box<Expr>,
        else_body: Box<Expr>,
    },

    // Range
    Range {
        start: u64,
        end: u64,
    },

    // Slice
    Slice {
        target: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    },

    // Match
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    // Function/Method call
    Call {
        callee: Box<Expr>,
        args: Vec<CallArg>,
    },
    MethodCall {
        callee: Box<Expr>,
        method: String,
        args: Vec<CallArg>,
    },

    Closure {
        params: Vec<Param>,
        return_ty: Option<TypeExpr>,
        body: Box<Expr>,
    },
}

// -----------------------------------------------------------------------------
// Call args
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct CallArg {
    pub mode: CallArgMode,
    pub expr: Expr,
    pub span: Span,
}

// -----------------------------------------------------------------------------
// Patterns
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Ident {
        ident: Ident,
    },
    Array {
        patterns: Vec<Pattern>,
    },
    Tuple {
        patterns: Vec<Pattern>,
    },
    Struct {
        name: String,
        fields: Vec<StructPatternField>,
    },
}

#[derive(Clone, Debug)]
pub struct StructPatternField {
    pub name: String,
    pub pattern: Pattern,
    pub span: Span,
}

// -----------------------------------------------------------------------------
// Match patterns
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub id: NodeId,
    pub pattern: MatchPattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MatchPattern {
    Wildcard {
        span: Span,
    },
    BoolLit {
        value: bool,
        span: Span,
    },
    IntLit {
        value: u64,
        span: Span,
    },
    Binding {
        ident: Ident,
    },
    Tuple {
        patterns: Vec<MatchPattern>,
        span: Span,
    },
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        bindings: Vec<MatchPatternBinding>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum MatchPatternBinding {
    Named { ident: Ident },
    Wildcard { span: Span },
}

// -----------------------------------------------------------------------------
// Blocks / statements
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum BlockItem {
    Stmt(StmtExpr),
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct StmtExpr {
    pub id: NodeId,
    pub kind: StmtExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtExprKind {
    LetBind {
        pattern: Pattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },
    VarBind {
        pattern: Pattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },
    VarDecl {
        name: Ident,
        decl_ty: TypeExpr,
    },
    Assign {
        assignee: Box<Expr>,
        value: Box<Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    For {
        pattern: Pattern,
        iter: Box<Expr>,
        body: Box<Expr>,
    },
}

// -----------------------------------------------------------------------------
// Literals / helpers
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum ArrayLitInit {
    Elems(Vec<Expr>),
    Repeat(Box<Expr>, u64),
}

#[derive(Clone, Debug)]
pub struct StructLitField {
    pub id: NodeId,
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructUpdateField {
    pub id: NodeId,
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StringFmtSegment {
    Literal { value: String, span: Span },
    Expr { expr: Box<Expr>, span: Span },
}

// -----------------------------------------------------------------------------
// Closures
// -----------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Closure {
    pub id: NodeId,
    pub params: Vec<Param>,
    pub return_ty: Option<TypeExpr>,
    pub body: Expr,
    pub span: Span,
}
