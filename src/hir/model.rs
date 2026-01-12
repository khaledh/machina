//! HIR model: resolved, tree-shaped nodes with DefIds where available.

use crate::ast::model;
use crate::ast::stage::HirDef;

pub use crate::ast::{BinaryOp, CallArgMode, NodeId, ParamMode, UnaryOp};

// -- HIR type aliases (AST model specialized with DefId) ---

pub type Module = model::Module<HirDef>;
pub type TopLevelItem = model::TopLevelItem<HirDef>;
pub type Decl = model::TopLevelItem<HirDef>;
pub type CallableRef<'a> = model::CallableRef<'a, HirDef>;

pub type TypeDef = model::TypeDef<HirDef>;
pub type TypeDefKind = model::TypeDefKind<HirDef>;
pub type StructDefField = model::StructDefField<HirDef>;
pub type EnumDefVariant = model::EnumDefVariant<HirDef>;

pub type TypeExpr = model::TypeExpr<HirDef>;
pub type TypeExprKind = model::TypeExprKind<HirDef>;
pub type FnTypeParam = model::FnTypeParam<HirDef>;

pub type StringFmtSegment = model::StringFmtSegment<HirDef>;

pub type FuncDecl = model::FuncDecl<HirDef>;
pub type FuncDef = model::FuncDef<HirDef>;
pub type FunctionSig = model::FunctionSig<HirDef>;

pub type MethodBlock = model::MethodBlock<HirDef>;
pub type MethodDef = model::MethodDef<HirDef>;
pub type MethodSig = model::MethodSig<HirDef>;
pub type SelfParam = model::SelfParam<HirDef>;

pub type ClosureDecl = model::ClosureDecl<HirDef>;
pub type ClosureSig = model::ClosureSig<HirDef>;

pub type Param = model::Param<HirDef>;
pub type CallArg = model::CallArg<HirDef>;

pub type BindPattern = model::BindPattern<HirDef>;
pub type BindPatternKind = model::BindPatternKind<HirDef>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<HirDef>;
pub type StructPatternField = model::StructFieldBindPattern<HirDef>;

pub type MatchArm = model::MatchArm<HirDef>;
pub type MatchPattern = model::MatchPattern<HirDef>;
pub type MatchPatternBinding = model::MatchPatternBinding<HirDef>;

pub type BlockItem = model::BlockItem<HirDef>;
pub type StmtExpr = model::StmtExpr<HirDef>;
pub type StmtExprKind = model::StmtExprKind<HirDef>;

pub type Expr = model::Expr<HirDef>;
pub type ExprKind = model::ExprKind<HirDef>;

pub type ArrayLitInit = model::ArrayLitInit<HirDef>;
pub type StructLitField = model::StructLitField<HirDef>;
pub type StructUpdateField = model::StructUpdateField<HirDef>;
