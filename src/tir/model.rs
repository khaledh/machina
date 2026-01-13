//! Typed IR model: tree-shaped nodes with DefIds and TypeIds.

use crate::ast::model;
use crate::resolve::DefId;
use crate::types::TypeId;

pub use crate::ast::{BinaryOp, CallArgMode, CoerceKind, InitInfo, NodeId, ParamMode, UnaryOp};

// -- TIR type aliases (AST model specialized with DefId + TypeId) ---

pub type Module = model::Module<DefId, TypeId>;
pub type TypedModule = model::Module<DefId, TypeId>;
pub type TopLevelItem = model::TopLevelItem<DefId, TypeId>;
pub type TypedTopLevelItem = model::TopLevelItem<DefId, TypeId>;
pub type Decl = model::TopLevelItem<DefId, TypeId>;
pub type CallableRef<'a> = model::CallableRef<'a, DefId, TypeId>;

pub type TypeDef = model::TypeDef<DefId>;
pub type TypeDefKind = model::TypeDefKind<DefId>;
pub type StructDefField = model::StructDefField<DefId>;
pub type EnumDefVariant = model::EnumDefVariant<DefId>;

pub type TypeExpr = model::TypeExpr<DefId>;
pub type TypeExprKind = model::TypeExprKind<DefId>;
pub type FnTypeParam = model::FnTypeParam<DefId>;

pub type StringFmtSegment = model::StringFmtSegment<DefId, TypeId>;
pub type TypedStringFmtSegment = model::StringFmtSegment<DefId, TypeId>;

pub type FuncDecl = model::FuncDecl<DefId>;
pub type FuncDef = model::FuncDef<DefId, TypeId>;
pub type TypedFuncDef = model::FuncDef<DefId, TypeId>;
pub type FunctionSig = model::FunctionSig<DefId>;

pub type MethodBlock = model::MethodBlock<DefId, TypeId>;
pub type TypedMethodBlock = model::MethodBlock<DefId, TypeId>;
pub type MethodDef = model::MethodDef<DefId, TypeId>;
pub type TypedMethodDef = model::MethodDef<DefId, TypeId>;
pub type MethodSig = model::MethodSig<DefId>;
pub type SelfParam = model::SelfParam<DefId>;

pub type ClosureDecl = model::ClosureDecl<DefId, TypeId>;
pub type TypedClosureDecl = model::ClosureDecl<DefId, TypeId>;
pub type ClosureSig = model::ClosureSig<DefId>;

pub type Param = model::Param<DefId>;
pub type CallArg = model::CallArg<DefId, TypeId>;
pub type TypedCallArg = model::CallArg<DefId, TypeId>;

pub type BindPattern = model::BindPattern<DefId>;
pub type BindPatternKind = model::BindPatternKind<DefId>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<DefId>;
pub type StructPatternField = model::StructFieldBindPattern<DefId>;

pub type MatchArm = model::MatchArm<DefId, TypeId>;
pub type TypedMatchArm = model::MatchArm<DefId, TypeId>;
pub type MatchPattern = model::MatchPattern<DefId>;
pub type MatchPatternBinding = model::MatchPatternBinding<DefId>;

pub type BlockItem = model::BlockItem<DefId, TypeId>;
pub type StmtExpr = model::StmtExpr<DefId, TypeId>;
pub type StmtExprKind = model::StmtExprKind<DefId, TypeId>;
pub type TypedBlockItem = model::BlockItem<DefId, TypeId>;
pub type TypedStmtExpr = model::StmtExpr<DefId, TypeId>;
pub type TypedStmtExprKind = model::StmtExprKind<DefId, TypeId>;

pub type Expr = model::Expr<DefId, TypeId>;
pub type ExprKind = model::ExprKind<DefId, TypeId>;
pub type TypedExpr = model::Expr<DefId, TypeId>;
pub type TypedExprKind = model::ExprKind<DefId, TypeId>;

pub type ArrayLitInit = model::ArrayLitInit<DefId, TypeId>;
pub type StructLitField = model::StructLitField<DefId, TypeId>;
pub type StructUpdateField = model::StructUpdateField<DefId, TypeId>;
pub type TypedArrayLitInit = model::ArrayLitInit<DefId, TypeId>;
pub type TypedStructLitField = model::StructLitField<DefId, TypeId>;
pub type TypedStructUpdateField = model::StructUpdateField<DefId, TypeId>;
