//! Abstract Syntax Tree (AST)

pub mod cfg;
pub mod fold;
mod format;
pub mod model;
pub mod node;
pub mod visit;

pub use fold::AstFolder;
pub use model::*;
pub use node::*;
pub use visit::*;

// -- String-specialized AST aliases ---

pub type Module = model::Module<String>;
pub type Decl = model::Decl<String>;
pub type CallableRef<'a> = model::CallableRef<'a, String>;
pub type TypeDef = model::TypeDef<String>;
pub type TypeDefKind = model::TypeDefKind<String>;
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
pub type ClosureDecl = model::ClosureDecl<String>;
pub type ClosureSig = model::ClosureSig<String>;
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
