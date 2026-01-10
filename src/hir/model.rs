//! HIR model: resolved, tree-shaped nodes with DefIds where available.

use crate::ast::model;
use crate::diag::Span;
use crate::resolve::def_map::DefId;

pub use crate::ast::{BinaryOp, CallArgMode, NodeId, ParamMode, UnaryOp};

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

    pub fn method_blocks(&self) -> Vec<&MethodBlock> {
        self.decls
            .iter()
            .filter_map(|decl| {
                if let Decl::MethodBlock(method_block) = decl {
                    Some(method_block)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn callables(&self) -> Vec<CallableRef<'_>> {
        let mut callables = Vec::new();
        for decl in &self.decls {
            match decl {
                Decl::FunctionDecl(function_decl) => {
                    callables.push(CallableRef::FunctionDecl(function_decl))
                }
                Decl::Function(function) => callables.push(CallableRef::Function(function)),
                Decl::MethodBlock(method_block) => {
                    for method in &method_block.methods {
                        callables.push(CallableRef::Method {
                            type_name: &method_block.type_name,
                            method,
                        });
                    }
                }
                Decl::Closure(expr) => callables.push(CallableRef::Closure(expr)),
                Decl::TypeDecl(_) => {}
            }
        }
        callables
    }
}

// -- Declarations ---

#[derive(Clone, Debug)]
pub enum Decl {
    TypeDecl(TypeDecl),
    FunctionDecl(FunctionDecl),
    Function(Function),
    MethodBlock(MethodBlock),
    Closure(Expr),
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a> {
    FunctionDecl(&'a FunctionDecl),
    Function(&'a Function),
    Method {
        type_name: &'a str,
        method: &'a Method,
    },
    Closure(&'a Expr),
}

impl<'a> CallableRef<'a> {
    pub fn id(&self) -> NodeId {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.id,
            CallableRef::Function(function) => function.id,
            CallableRef::Method { method, .. } => method.id,
            CallableRef::Closure(expr) => expr.id,
        }
    }

    pub fn name(&self) -> String {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.sig.name.clone(),
            CallableRef::Function(function) => function.sig.name.clone(),
            CallableRef::Method { method, .. } => method.sig.name.clone(),
            CallableRef::Closure(expr) => format!("__mc_closure${}", expr.id),
        }
    }

    pub fn symbol_base_name(&self) -> String {
        match self {
            CallableRef::FunctionDecl(_) => self.name(),
            CallableRef::Function(_) => self.name(),
            CallableRef::Method { type_name, method } => {
                format!("{type_name}${}", method.sig.name)
            }
            CallableRef::Closure(_) => self.name(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.span,
            CallableRef::Function(function) => function.span,
            CallableRef::Method { method, .. } => method.span,
            CallableRef::Closure(expr) => expr.span,
        }
    }
}

// -- Type Declarations ---

pub type TypeDecl = model::TypeDecl<DefId>;
pub type TypeDeclKind = model::TypeDeclKind<DefId>;
pub type StructField = model::StructField<DefId>;
pub type EnumVariant = model::EnumVariant<DefId>;
pub type TypeExpr = model::TypeExpr<DefId>;
pub type TypeExprKind = model::TypeExprKind<DefId>;
pub type FnTypeParam = model::FnTypeParam<DefId>;
pub type StringFmtSegment = model::StringFmtSegment<DefId>;
pub type FunctionSig = model::FunctionSig<DefId>;

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub id: NodeId,
    pub def_id: DefId,
    pub sig: FunctionSig,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub def_id: DefId,
    pub sig: FunctionSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodBlock {
    pub id: NodeId,
    pub type_name: String,
    pub methods: Vec<Method>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub id: NodeId,
    pub def_id: DefId,
    pub sig: MethodSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodSig {
    pub name: String,
    pub self_param: SelfParam,
    pub params: Vec<Param>,
    pub return_type: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SelfParam {
    pub id: NodeId,
    pub def_id: DefId,
    pub mode: ParamMode,
    pub span: Span,
}
pub type Param = model::Param<DefId>;
pub type CallArg = model::CallArg<DefId>;
pub type BindPattern = model::BindPattern<DefId>;
pub type BindPatternKind = model::BindPatternKind<DefId>;
pub type StructPatternField = model::StructFieldBindPattern<DefId>;
pub type MatchArm = model::MatchArm<DefId>;
pub type MatchPattern = model::MatchPattern<DefId>;
pub type MatchPatternBinding = model::MatchPatternBinding<DefId>;
pub type BlockItem = model::BlockItem<DefId>;
pub type StmtExpr = model::StmtExpr<DefId>;
pub type StmtExprKind = model::StmtExprKind<DefId>;
pub type Expr = model::Expr<DefId>;
pub type ExprKind = model::ExprKind<DefId>;
pub type ArrayLitInit = model::ArrayLitInit<DefId>;
pub type StructLitField = model::StructLitField<DefId>;
pub type StructUpdateField = model::StructUpdateField<DefId>;
