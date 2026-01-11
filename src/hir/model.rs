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
    pub fn type_defs(&self) -> Vec<&TypeDef> {
        self.decls
            .iter()
            .filter_map(|decl| {
                if let Decl::TypeDef(type_def) = decl {
                    Some(type_def)
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
                Decl::FuncDef(func_def) => Some(&func_def.sig),
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

    pub fn func_defs(&self) -> Vec<&FuncDef> {
        self.decls
            .iter()
            .filter_map(|decl| {
                if let Decl::FuncDef(func_def) = decl {
                    Some(func_def)
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
                Decl::FuncDef(func_def) => callables.push(CallableRef::FuncDef(func_def)),
                Decl::MethodBlock(method_block) => {
                    for method in &method_block.methods {
                        callables.push(CallableRef::Method {
                            type_name: &method_block.type_name,
                            method,
                        });
                    }
                }
                Decl::ClosureDecl(closure_decl) => {
                    callables.push(CallableRef::ClosureDecl(closure_decl))
                }
                Decl::TypeDef(_) => {}
            }
        }
        callables
    }
}

// -- Declarations ---

#[derive(Clone, Debug)]
pub enum Decl {
    TypeDef(TypeDef),
    FunctionDecl(FunctionDecl),
    FuncDef(FuncDef),
    MethodBlock(MethodBlock),
    ClosureDecl(ClosureDecl),
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a> {
    FunctionDecl(&'a FunctionDecl),
    FuncDef(&'a FuncDef),
    Method {
        type_name: &'a str,
        method: &'a Method,
    },
    ClosureDecl(&'a ClosureDecl),
}

impl<'a> CallableRef<'a> {
    pub fn id(&self) -> NodeId {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.id,
            CallableRef::FuncDef(func_def) => func_def.id,
            CallableRef::Method { method, .. } => method.id,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.id,
        }
    }

    pub fn name(&self) -> String {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.sig.name.clone(),
            CallableRef::FuncDef(func_def) => func_def.sig.name.clone(),
            CallableRef::Method { method, .. } => method.sig.name.clone(),
            CallableRef::ClosureDecl(closure_decl) => closure_decl.sig.name.clone(),
        }
    }

    pub fn symbol_base_name(&self) -> String {
        match self {
            CallableRef::FunctionDecl(_) => self.name(),
            CallableRef::FuncDef(_) => self.name(),
            CallableRef::Method { type_name, method } => {
                format!("{type_name}${}", method.sig.name)
            }
            CallableRef::ClosureDecl(_) => self.name(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.span,
            CallableRef::FuncDef(func_def) => func_def.span,
            CallableRef::Method { method, .. } => method.span,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.span,
        }
    }
}

// -- Type Definitions ---

pub type TypeDef = model::TypeDef<DefId>;
pub type TypeDefKind = model::TypeDefKind<DefId>;
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
pub struct FuncDef {
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

#[derive(Clone, Debug)]
pub struct ClosureDecl {
    pub id: NodeId,
    pub def_id: DefId,
    pub sig: ClosureSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ClosureSig {
    pub name: String,
    pub params: Vec<Param>,
    pub return_ty: TypeExpr,
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
