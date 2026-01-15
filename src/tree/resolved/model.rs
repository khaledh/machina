//! Resolved tree model: resolved nodes with DefIds where available.

use crate::resolve::DefId;
use crate::tree::model;

pub use crate::tree::{BinaryOp, CallArgMode, NodeId, ParamMode, UnaryOp};

// -- Resolved tree aliases (parsed model specialized with DefId) ---

pub type Module = model::Module<DefId>;

pub type TopLevelItem = model::TopLevelItem<DefId>;

pub type TypeExpr = model::TypeExpr<DefId>;
pub type TypeExprKind = model::TypeExprKind<DefId>;
pub type FnTypeParam = model::FnTypeParam<DefId>;

pub type TypeDef = model::TypeDef<DefId>;
pub type TypeDefKind = model::TypeDefKind<DefId>;
pub type StructDefField = model::StructDefField<DefId>;
pub type EnumDefVariant = model::EnumDefVariant<DefId>;

pub type FuncDecl = model::FuncDecl<DefId>;
pub type FuncDef = model::FuncDef<DefId>;
pub type FunctionSig = model::FunctionSig<DefId>;

pub type MethodBlock = model::MethodBlock<DefId>;
pub type MethodDef = model::MethodDef<DefId>;
pub type MethodSig = model::MethodSig<DefId>;
pub type SelfParam = model::SelfParam<DefId>;

pub type ClosureDef = model::ClosureDef<DefId>;
pub type ClosureSig = model::ClosureSig<DefId>;

pub type Param = model::Param<DefId>;

pub type CallableRef<'a> = model::CallableRef<'a, DefId>;

pub type BlockItem = model::BlockItem<DefId>;

pub type StmtExpr = model::StmtExpr<DefId>;
pub type StmtExprKind = model::StmtExprKind<DefId>;
pub type Expr = model::Expr<DefId>;
pub type ExprKind = model::ExprKind<DefId>;

pub type BindPattern = model::BindPattern<DefId>;
pub type BindPatternKind = model::BindPatternKind<DefId>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<DefId>;
pub type StructPatternField = model::StructFieldBindPattern<DefId>;

pub type MatchArm = model::MatchArm<DefId>;
pub type MatchPattern = model::MatchPattern<DefId>;
pub type MatchPatternBinding = model::MatchPatternBinding<DefId>;

pub type ArrayLitInit = model::ArrayLitInit<DefId>;
pub type StructLitField = model::StructLitField<DefId>;
pub type StructUpdateField = model::StructUpdateField<DefId>;

pub type CallArg = model::CallArg<DefId>;

pub type StringFmtSegment = model::StringFmtSegment<DefId>;
