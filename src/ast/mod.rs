//! Abstract Syntax Tree (AST)

pub mod cfg;
pub mod fold;
mod format;
pub mod model;
pub mod visit;

pub use fold::AstFolder;
pub use visit::*;

use std::fmt;

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

// -- Parameter / call modes ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParamMode {
    In,
    InOut,
    Out,
    Sink,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallArgMode {
    Default,
    InOut,
    Out,
    Move,
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

// -- String-specialized AST aliases ---

pub type Module = model::Module<String>;
pub type Decl = model::Decl<String>;
pub type CallableRef<'a> = model::CallableRef<'a, String>;
pub type TypeDecl = model::TypeDecl<String>;
pub type TypeDeclKind = model::TypeDeclKind<String>;
pub type StructField = model::StructField<String>;
pub type EnumVariant = model::EnumVariant<String>;
pub type TypeExpr = model::TypeExpr<String>;
pub type TypeExprKind = model::TypeExprKind<String>;
pub type FnTypeParam = model::FnTypeParam<String>;
pub type StringFmtSegment = model::StringFmtSegment<String>;
pub type FunctionDecl = model::FunctionDecl<String>;
pub type Function = model::Function<String>;
pub type FunctionSig = model::FunctionSig<String>;
pub type MethodBlock = model::MethodBlock<String>;
pub type Method = model::Method<String>;
pub type MethodSig = model::MethodSig<String>;
pub type SelfParam = model::SelfParam;
pub type Param = model::Param<String>;
pub type CallArg = model::CallArg<String>;
pub type BindPattern = model::BindPattern<String>;
pub type BindPatternKind = model::BindPatternKind<String>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<String>;
pub type MatchArm = model::MatchArm<String>;
pub type MatchPattern = model::MatchPattern<String>;
pub type MatchPatternBinding = model::MatchPatternBinding<String>;
pub type BlockItem = model::BlockItem<String>;
pub type StmtExpr = model::StmtExpr<String>;
pub type StmtExprKind = model::StmtExprKind<String>;
pub type Expr = model::Expr<String>;
pub type ExprKind = model::ExprKind<String>;
pub type ArrayLitInit = model::ArrayLitInit<String>;
pub type StructLitField = model::StructLitField<String>;
pub type StructUpdateField = model::StructUpdateField<String>;
