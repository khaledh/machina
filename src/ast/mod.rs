//! Abstract Syntax Tree (AST)

pub mod cfg;
pub mod fold;
mod format;
pub mod model;
pub mod node;
pub mod stage;
pub mod visit;

pub use fold::AstFolder;
pub use model::*;
pub use node::*;
pub use stage::*;
pub use visit::*;

// -- String-specialized AST aliases ---

pub type Module = model::Module<AstDef>;
pub type TopLevelItem = model::TopLevelItem<AstDef>;
pub type CallableRef<'a> = model::CallableRef<'a, AstDef>;
pub type TypeDef = model::TypeDef<AstDef>;
pub type TypeDefKind = model::TypeDefKind<AstDef>;
pub type StructDefField = model::StructDefField<AstDef>;
pub type EnumDefVariant = model::EnumDefVariant<AstDef>;
pub type TypeExpr = model::TypeExpr<AstDef>;
pub type TypeExprKind = model::TypeExprKind<AstDef>;
pub type FnTypeParam = model::FnTypeParam<AstDef>;
pub type StringFmtSegment = model::StringFmtSegment<AstDef>;
pub type FuncDecl = model::FuncDecl<AstDef>;
pub type FuncDef = model::FuncDef<AstDef>;
pub type FunctionSig = model::FunctionSig<AstDef>;
pub type MethodBlock = model::MethodBlock<AstDef>;
pub type MethodDef = model::MethodDef<AstDef>;
pub type MethodSig = model::MethodSig<AstDef>;
pub type SelfParam = model::SelfParam;
pub type ClosureDecl = model::ClosureDecl<AstDef>;
pub type ClosureSig = model::ClosureSig<AstDef>;
pub type Param = model::Param<AstDef>;
pub type CallArg = model::CallArg<AstDef>;
pub type BindPattern = model::BindPattern<AstDef>;
pub type BindPatternKind = model::BindPatternKind<AstDef>;
pub type StructFieldBindPattern = model::StructFieldBindPattern<AstDef>;
pub type MatchArm = model::MatchArm<AstDef>;
pub type MatchPattern = model::MatchPattern<AstDef>;
pub type MatchPatternBinding = model::MatchPatternBinding<AstDef>;
pub type BlockItem = model::BlockItem<AstDef>;
pub type StmtExpr = model::StmtExpr<AstDef>;
pub type StmtExprKind = model::StmtExprKind<AstDef>;
pub type Expr = model::Expr<AstDef>;
pub type ExprKind = model::ExprKind<AstDef>;
pub type ArrayLitInit = model::ArrayLitInit<AstDef>;
pub type StructLitField = model::StructLitField<AstDef>;
pub type StructUpdateField = model::StructUpdateField<AstDef>;
