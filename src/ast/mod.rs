//! Abstract Syntax Tree (AST)

pub mod cfg;
pub mod fold;
mod format;
pub mod model;
pub mod node;
pub mod visit;
pub mod visit_mut;

pub use fold::AstFolder;
pub use model::*;
pub use node::*;
pub use visit::*;

// -- String-specialized AST aliases ---

pub type Module = model::Module<()>;
pub type TopLevelItem = model::TopLevelItem<()>;
pub type CallableRef<'a> = model::CallableRef<'a, ()>;
pub type TypeDef = model::TypeDef<()>;
pub type TypeDefKind = model::TypeDefKind<()>;
pub type StructDefField = model::StructDefField<()>;
pub type EnumDefVariant = model::EnumDefVariant<()>;
pub type TypeExpr = model::TypeExpr<()>;
pub type TypeExprKind = model::TypeExprKind<()>;
pub type FnTypeParam = model::FnTypeParam<()>;
pub type StringFmtSegment = model::StringFmtSegment<()>;
pub type FuncDecl = model::FuncDecl<()>;
pub type FuncDef = model::FuncDef<()>;
pub type FunctionSig = model::FunctionSig<()>;
pub type MethodBlock = model::MethodBlock<()>;
pub type MethodDef = model::MethodDef<()>;
pub type MethodSig = model::MethodSig<()>;
pub type SelfParam = model::SelfParam<()>;
pub type ClosureDecl = model::ClosureDecl<()>;
pub type ClosureSig = model::ClosureSig<()>;
pub type Param = model::Param<()>;
pub type CallArg = model::CallArg<()>;
pub type BindPattern = model::BindPattern<()>;
pub type BindPatternKind = model::BindPatternKind<()>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<()>;
pub type MatchArm = model::MatchArm<()>;
pub type MatchPattern = model::MatchPattern<()>;
pub type MatchPatternBinding = model::MatchPatternBinding<()>;
pub type BlockItem = model::BlockItem<()>;
pub type StmtExpr = model::StmtExpr<()>;
pub type StmtExprKind = model::StmtExprKind<()>;
pub type Expr = model::Expr<()>;
pub type ExprKind = model::ExprKind<()>;
pub type ArrayLitInit = model::ArrayLitInit<()>;
pub type StructLitField = model::StructLitField<()>;
pub type StructUpdateField = model::StructUpdateField<()>;
