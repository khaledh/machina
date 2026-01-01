//! Abstract Syntax Tree (AST)

pub mod cfg;
pub mod fold;
mod format;
mod visit;

pub use fold::AstFolder;
pub use visit::*;

use std::fmt;

use crate::diag::Span;

// -- Nodes ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct NodeIdGen {
    next_id: u32,
}

impl Default for NodeIdGen {
    fn default() -> Self {
        Self::new()
    }
}

impl NodeIdGen {
    pub fn new() -> Self {
        Self { next_id: 1 } // NodeId 0 is reserved
    }

    pub fn new_id(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }
}

// -- Module ---

#[derive(Clone, Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}

impl Module {
    pub fn type_decls(&self) -> Vec<&TypeDecl> {
        self.decls
            .iter()
            .filter_map(|decl| {
                if let Decl::TypeDecl(type_decl) = decl {
                    Some(type_decl)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn func_sigs(&self) -> Vec<&FunctionSig> {
        self.decls
            .iter()
            .filter_map(|decl| match decl {
                Decl::FunctionDecl(func_decl) => Some(&func_decl.sig),
                Decl::Function(func) => Some(&func.sig),
                _ => None,
            })
            .collect()
    }

    pub fn func_decls(&self) -> Vec<&FunctionDecl> {
        self.decls
            .iter()
            .filter_map(|decl| match decl {
                Decl::FunctionDecl(func_decl) => Some(func_decl),
                _ => None,
            })
            .collect()
    }

    pub fn funcs(&self) -> Vec<&Function> {
        self.decls
            .iter()
            .filter_map(|decl| {
                if let Decl::Function(function) = decl {
                    Some(function)
                } else {
                    None
                }
            })
            .collect()
    }
}

// -- Declarations ---

#[derive(Clone, Debug)]
pub enum Decl {
    TypeDecl(TypeDecl),
    Function(Function),         // function definition
    FunctionDecl(FunctionDecl), // function declaration
}

// -- Type Declarations ---

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub id: NodeId,
    pub name: String,
    pub payload: Vec<TypeExpr>,
    pub span: Span,
}

// -- Type Expressions ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeExpr {
    pub id: NodeId,
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExprKind {
    Named(String),
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
}

// -- Array Literals ---

#[derive(Clone, Debug)]
pub enum ArrayLitInit {
    Elems(Vec<Expr>),
    Repeat(Box<Expr>, u64),
}

// -- Struct Literals ---

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

// -- String Literals ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringTag {
    Ascii,
    Utf8,
}

#[derive(Clone, Debug)]
pub enum StringFmtSegment {
    Literal { value: String, span: Span },
    Expr { expr: Box<Expr>, span: Span },
}

// -- Functions ---

#[derive(Clone, Debug)]
pub struct FunctionSig {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: TypeExpr,
    pub span: Span,
}

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
pub struct FunctionParam {
    pub id: NodeId,
    pub name: String,
    pub typ: TypeExpr,
    pub mode: FunctionParamMode,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParamMode {
    In,
    Inout,
}

// -- Patterns ---

#[derive(Clone, Debug)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Ident {
        name: String,
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

// -- Match patterns ---

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
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        bindings: Vec<MatchPatternBinding>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct MatchPatternBinding {
    pub id: NodeId,
    pub name: String,
    pub span: Span,
}

// --- Blocks ---

#[derive(Clone, Debug)]
pub enum BlockItem {
    Stmt(StmtExpr),
    Expr(Expr),
}

// --- Statement Expressions ---

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

// -- Expressions ---

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
        tag: StringTag,
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

    // Move
    Move {
        expr: Box<Expr>,
    },

    // Var, array index, tuple field, struct field
    Var(String),
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
        end: u64, // exclusive
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

    // Function call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
}

// -- Operators ---

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison operators
    Eq,
    Ne,
    Lt,
    Gt,
    LtEq,
    GtEq,

    // Bitwise operators
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,

    // Logical operators
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    LogicalNot,
    BitNot,
}
