//! HIR model: resolved, tree-shaped nodes with DefIds where available.

use crate::ast::model;
use crate::diag::Span;
use crate::resolve::DefId;

pub use crate::ast::{BinaryOp, CallArgMode, NodeId, ParamMode, UnaryOp};

// -- Module ---

#[derive(Clone, Debug)]
pub struct Module {
    pub top_level_items: Vec<Decl>,
}

impl Module {
    pub fn type_defs(&self) -> Vec<&TypeDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| {
                if let Decl::TypeDef(type_def) = item {
                    Some(type_def)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn type_def_by_id(&self, def_id: DefId) -> Option<&TypeDef> {
        self.top_level_items.iter().find_map(|item| match item {
            Decl::TypeDef(type_def) if type_def.def_id == def_id => Some(type_def),
            _ => None,
        })
    }

    pub fn func_sigs(&self) -> Vec<&FunctionSig> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                Decl::FuncDecl(func_decl) => Some(&func_decl.sig),
                Decl::FuncDef(func_def) => Some(&func_def.sig),
                _ => None,
            })
            .collect()
    }

    pub fn func_decls(&self) -> Vec<&FuncDecl> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                Decl::FuncDecl(func_decl) => Some(func_decl),
                _ => None,
            })
            .collect()
    }

    pub fn func_defs(&self) -> Vec<&FuncDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| {
                if let Decl::FuncDef(func_def) = item {
                    Some(func_def)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn method_blocks(&self) -> Vec<&MethodBlock> {
        self.top_level_items
            .iter()
            .filter_map(|item| {
                if let Decl::MethodBlock(method_block) = item {
                    Some(method_block)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn callables(&self) -> Vec<CallableRef<'_>> {
        let mut callables = Vec::new();
        for item in &self.top_level_items {
            match item {
                Decl::FuncDecl(func_decl) => callables.push(CallableRef::FuncDecl(func_decl)),
                Decl::FuncDef(func_def) => callables.push(CallableRef::FuncDef(func_def)),
                Decl::MethodBlock(method_block) => {
                    for method_def in &method_block.method_defs {
                        callables.push(CallableRef::MethodDef {
                            type_name: &method_block.type_name,
                            method_def,
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
    FuncDecl(FuncDecl),
    FuncDef(FuncDef),
    MethodBlock(MethodBlock),
    ClosureDecl(ClosureDecl),
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a> {
    FuncDecl(&'a FuncDecl),
    FuncDef(&'a FuncDef),
    MethodDef {
        type_name: &'a str,
        method_def: &'a MethodDef,
    },
    ClosureDecl(&'a ClosureDecl),
}

impl<'a> CallableRef<'a> {
    pub fn id(&self) -> NodeId {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.id,
            CallableRef::FuncDef(func_def) => func_def.id,
            CallableRef::MethodDef { method_def, .. } => method_def.id,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.id,
        }
    }

    pub fn def_id(&self) -> DefId {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.def_id,
            CallableRef::FuncDef(func_def) => func_def.def_id,
            CallableRef::MethodDef { method_def, .. } => method_def.def_id,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.def_id,
        }
    }

    pub fn name(&self) -> String {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.sig.name.clone(),
            CallableRef::FuncDef(func_def) => func_def.sig.name.clone(),
            CallableRef::MethodDef { method_def, .. } => method_def.sig.name.clone(),
            CallableRef::ClosureDecl(closure_decl) => closure_decl.sig.name.clone(),
        }
    }

    pub fn symbol_base_name(&self) -> String {
        match self {
            CallableRef::FuncDecl(_) => self.name(),
            CallableRef::FuncDef(_) => self.name(),
            CallableRef::MethodDef {
                type_name,
                method_def,
            } => {
                format!("{type_name}${}", method_def.sig.name)
            }
            CallableRef::ClosureDecl(_) => self.name(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.span,
            CallableRef::FuncDef(func_def) => func_def.span,
            CallableRef::MethodDef { method_def, .. } => method_def.span,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.span,
        }
    }
}

// -- Type Definitions ---

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub id: NodeId,
    pub def_id: DefId,
    pub name: String,
    pub kind: TypeDefKind,
    pub span: Span,
}

pub type TypeDefKind = model::TypeDefKind<DefId>;
pub type StructDefField = model::StructDefField<DefId>;
pub type EnumDefVariant = model::EnumDefVariant<DefId>;
pub type TypeExpr = model::TypeExpr<DefId>;
pub type TypeExprKind = model::TypeExprKind<DefId>;
pub type FnTypeParam = model::FnTypeParam<DefId>;
pub type StringFmtSegment = model::StringFmtSegment<DefId>;
pub type FunctionSig = model::FunctionSig<DefId>;

#[derive(Clone, Debug)]
pub struct FuncDecl {
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
    pub method_defs: Vec<MethodDef>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodDef {
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
    pub ret_ty_expr: TypeExpr,
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
