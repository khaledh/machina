//! Typed tree ir: nodes with DefIds and TypeIds.

use crate::resolve::DefId;
use crate::tree as model;
use crate::types::TypeId;

pub use crate::tree::{BinaryOp, CallArgMode, CoerceKind, InitInfo, NodeId, ParamMode, UnaryOp};

// -- Typed tree aliases (parsed ir specialized with DefId + TypeId) ---

pub type Module = model::Module<DefId, TypeId>;
pub type Require = model::Require;

pub type TopLevelItem = model::TopLevelItem<DefId, TypeId>;

pub type Attribute = model::Attribute;
pub type AttrArg = model::AttrArg;

pub type TypeExpr = model::TypeExpr<DefId>;
pub type TypeExprKind = model::TypeExprKind<DefId>;
pub type FnTypeParam = model::FnTypeParam<DefId>;
pub type TypeParam = model::TypeParam<DefId>;
pub type TypeParamBound = model::TypeParamBound<DefId>;

pub type TypeDef = model::TypeDef<DefId>;
pub type TypeDefKind = model::TypeDefKind<DefId>;
pub type StructDefField = model::StructDefField<DefId>;
pub type EnumDefVariant = model::EnumDefVariant<DefId>;
pub type TraitDef = model::TraitDef<DefId>;
pub type TraitMethod = model::TraitMethod<DefId>;
pub type TraitProperty = model::TraitProperty<DefId>;

pub type FuncDecl = model::FuncDecl<DefId>;
pub type FuncDef = model::FuncDef<DefId, TypeId>;
pub type FunctionSig = model::FunctionSig<DefId>;
pub type MethodBlock = model::MethodBlock<DefId, TypeId>;
pub type MethodItem = model::MethodItem<DefId, TypeId>;
pub type MethodDecl = model::MethodDecl<DefId>;
pub type MethodDef = model::MethodDef<DefId, TypeId>;
pub type MethodSig = model::MethodSig<DefId>;
pub type SelfParam = model::SelfParam<DefId>;
pub type ClosureDef = model::ClosureDef<DefId, TypeId>;
pub type ClosureSig = model::ClosureSig<DefId>;
pub type CaptureSpec = model::CaptureSpec<DefId>;
pub type Param = model::Param<DefId>;

pub type CallableRef<'a> = model::CallableRef<'a, DefId, TypeId>;

pub type BindPattern = model::BindPattern<DefId>;
pub type BindPatternKind = model::BindPatternKind<DefId>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<DefId>;
pub type StructPatternField = model::StructFieldBindPattern<DefId>;

pub type MatchArm = model::MatchArm<DefId, TypeId>;
pub type MatchPattern = model::MatchPattern<DefId>;
pub type MatchPatternBinding = model::MatchPatternBinding<DefId>;

pub type BlockItem = model::BlockItem<DefId, TypeId>;

pub type StmtExpr = model::StmtExpr<DefId, TypeId>;
pub type StmtExprKind = model::StmtExprKind<DefId, TypeId>;
pub type Expr = model::Expr<DefId, TypeId>;
pub type ExprKind = model::ExprKind<DefId, TypeId>;

pub type ArrayLitInit = model::ArrayLitInit<DefId, TypeId>;
pub type StructLitField = model::StructLitField<DefId, TypeId>;
pub type StructUpdateField = model::StructUpdateField<DefId, TypeId>;

pub type CallArg = model::CallArg<DefId, TypeId>;

pub type StringFmtSegment = model::StringFmtSegment<DefId, TypeId>;
