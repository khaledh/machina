use crate::tree::model;
pub use crate::tree::node::*;

// -- String-specialized parsed tree aliases ---

pub type Module = model::Module<()>;

pub type TopLevelItem = model::TopLevelItem<()>;

pub type Attribute = model::Attribute;
pub type AttrArg = model::AttrArg;

pub type TypeExpr = model::TypeExpr<()>;
pub type TypeExprKind = model::TypeExprKind<()>;
pub type FnTypeParam = model::FnTypeParam<()>;
pub type TypeParam = model::TypeParam<()>;

pub type TypeDef = model::TypeDef<()>;
pub type TypeDefKind = model::TypeDefKind<()>;
pub type StructDefField = model::StructDefField<()>;
pub type EnumDefVariant = model::EnumDefVariant<()>;
pub type TraitDef = model::TraitDef<()>;
pub type TraitMethod = model::TraitMethod<()>;

pub type FuncDecl = model::FuncDecl<()>;
pub type FuncDef = model::FuncDef<()>;
pub type FunctionSig = model::FunctionSig<()>;

pub type MethodBlock = model::MethodBlock<()>;
pub type MethodItem = model::MethodItem<()>;
pub type MethodDecl = model::MethodDecl<()>;
pub type MethodDef = model::MethodDef<()>;
pub type MethodSig = model::MethodSig<()>;
pub type SelfParam = model::SelfParam<()>;

pub type ClosureDef = model::ClosureDef<()>;
pub type ClosureSig = model::ClosureSig<()>;
pub type CaptureSpec = model::CaptureSpec<()>;

pub type Param = model::Param<()>;

pub type CallableRef<'a> = model::CallableRef<'a, ()>;

pub type BlockItem = model::BlockItem<()>;

pub type StmtExpr = model::StmtExpr<()>;
pub type StmtExprKind = model::StmtExprKind<()>;
pub type Expr = model::Expr<()>;
pub type ExprKind = model::ExprKind<()>;

pub type BindPattern = model::BindPattern<()>;
pub type BindPatternKind = model::BindPatternKind<()>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<()>;

pub type MatchArm = model::MatchArm<()>;
pub type MatchPattern = model::MatchPattern<()>;
pub type MatchPatternBinding = model::MatchPatternBinding<()>;

pub type ArrayLitInit = model::ArrayLitInit<()>;
pub type StructLitField = model::StructLitField<()>;
pub type StructUpdateField = model::StructUpdateField<()>;

pub type CallArg = model::CallArg<()>;

pub type StringFmtSegment = model::StringFmtSegment<()>;
