//! Semantic tree: place/value split tree used after semantic checks.

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree as ast_model;
use crate::tree::{BinaryOp, CoerceKind, InitInfo, NodeId, ParamMode, UnaryOp};
use crate::types::TypeId;

// -- Semantic tree type aliases (reused parsed fragments) ---

pub type TypeExpr = ast_model::TypeExpr<DefId>;
pub type TypeExprKind = ast_model::TypeExprKind<DefId>;
pub type FnTypeParam = ast_model::FnTypeParam<DefId>;

pub type TypeDef = ast_model::TypeDef<DefId>;
pub type TypeDefKind = ast_model::TypeDefKind<DefId>;
pub type StructDefField = ast_model::StructDefField<DefId>;
pub type EnumDefVariant = ast_model::EnumDefVariant<DefId>;

pub type FunctionSig = ast_model::FunctionSig<DefId>;
pub type MethodSig = ast_model::MethodSig<DefId>;
pub type SelfParam = ast_model::SelfParam<DefId>;
pub type Param = ast_model::Param<DefId>;

pub type BindPattern = ast_model::BindPattern<DefId>;
pub type BindPatternKind = ast_model::BindPatternKind<DefId>;
pub type StructFieldBindPattern = ast_model::StructFieldBindPattern<DefId>;
pub type StructPatternField = ast_model::StructFieldBindPattern<DefId>;

pub type MatchPattern = ast_model::MatchPattern<DefId>;
pub type MatchPatternBinding = ast_model::MatchPatternBinding<DefId>;

// -- Module ---

#[derive(Clone, Debug)]
pub struct Module {
    pub top_level_items: Vec<TopLevelItem>,
}

impl Module {
    pub fn type_defs(&self) -> Vec<&TypeDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::TypeDef(type_def) => Some(type_def),
                _ => None,
            })
            .collect()
    }

    pub fn type_def_by_id(&self, def_id: DefId) -> Option<&TypeDef> {
        self.top_level_items.iter().find_map(|item| match item {
            TopLevelItem::TypeDef(type_def) if type_def.def_id == def_id => Some(type_def),
            _ => None,
        })
    }

    pub fn func_decls(&self) -> Vec<&FuncDecl> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::FuncDecl(func_decl) => Some(func_decl),
                _ => None,
            })
            .collect()
    }

    pub fn func_defs(&self) -> Vec<&FuncDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::FuncDef(func_def) => Some(func_def),
                _ => None,
            })
            .collect()
    }

    pub fn method_blocks(&self) -> Vec<&MethodBlock> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::MethodBlock(method_block) => Some(method_block),
                _ => None,
            })
            .collect()
    }

    pub fn callables(&self) -> Vec<CallableRef<'_>> {
        self.top_level_items
            .iter()
            .flat_map(|item| match item {
                TopLevelItem::FuncDecl(func_decl) => vec![CallableRef::FuncDecl(func_decl)],
                TopLevelItem::FuncDef(func_def) => vec![CallableRef::FuncDef(func_def)],
                TopLevelItem::MethodBlock(method_block) => method_block
                    .method_defs
                    .iter()
                    .map(|method_def| CallableRef::MethodDef {
                        type_name: &method_block.type_name,
                        method_def,
                    })
                    .collect(),
                TopLevelItem::TypeDef(_) => vec![],
            })
            .collect()
    }
}

// -- Top Level Items ---

#[derive(Clone, Debug)]
pub enum TopLevelItem {
    TypeDef(TypeDef),
    FuncDecl(FuncDecl),
    FuncDef(FuncDef),
    MethodBlock(MethodBlock),
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a> {
    FuncDecl(&'a FuncDecl),
    FuncDef(&'a FuncDef),
    MethodDef {
        type_name: &'a str,
        method_def: &'a MethodDef,
    },
}

// -- Functions ---

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
    pub body: ValueExpr,
    pub span: Span,
}

// -- Methods ---

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
    pub body: ValueExpr,
    pub span: Span,
}

// -- String Literals ---

#[derive(Clone, Debug)]
pub enum StringFmtSegment {
    Literal { value: String, span: Span },
    Expr { expr: Box<ValueExpr>, span: Span },
}

// -- Call Args ---

#[derive(Clone, Debug)]
pub enum CallArg {
    In {
        expr: ValueExpr,
        span: Span,
    },
    InOut {
        place: PlaceExpr,
        span: Span,
    },
    Out {
        place: PlaceExpr,
        init: InitInfo,
        span: Span,
    },
    Sink {
        expr: ValueExpr,
        span: Span,
    },
}

impl CallArg {
    pub fn mode(&self) -> ParamMode {
        match self {
            CallArg::In { .. } => ParamMode::In,
            CallArg::InOut { .. } => ParamMode::InOut,
            CallArg::Out { .. } => ParamMode::Out,
            CallArg::Sink { .. } => ParamMode::Sink,
        }
    }
}

#[derive(Clone, Debug)]
pub enum MethodReceiver {
    ValueExpr(Box<ValueExpr>),
    PlaceExpr(PlaceExpr),
}

// -- Match ---

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub id: NodeId,
    pub pattern: MatchPattern,
    pub body: ValueExpr,
    pub span: Span,
}

// -- Block Items ---

#[derive(Clone, Debug)]
pub enum BlockItem {
    Stmt(StmtExpr),
    Expr(ValueExpr),
}

// -- Statements ---

#[derive(Clone, Debug)]
pub struct StmtExpr {
    pub id: NodeId,
    pub kind: StmtExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtExprKind {
    LetBind {
        pattern: BindPattern,
        decl_ty: Option<TypeExpr>,
        value: Box<ValueExpr>,
    },
    VarBind {
        pattern: BindPattern,
        decl_ty: Option<TypeExpr>,
        value: Box<ValueExpr>,
    },
    VarDecl {
        ident: String,
        def_id: DefId,
        decl_ty: TypeExpr,
    },
    Assign {
        assignee: Box<PlaceExpr>,
        value: Box<ValueExpr>,
        init: InitInfo,
    },
    While {
        cond: Box<ValueExpr>,
        body: Box<ValueExpr>,
    },
    For {
        pattern: BindPattern,
        iter: Box<ValueExpr>,
        body: Box<ValueExpr>,
    },
    Break,
    Continue,
    Return {
        value: Option<Box<ValueExpr>>,
    },
}

// -- Places ---

#[derive(Clone, Debug)]
pub struct PlaceExpr {
    pub id: NodeId,
    pub kind: PlaceExprKind,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum PlaceExprKind {
    Var {
        ident: String,
        def_id: DefId,
    },
    Deref {
        value: Box<ValueExpr>,
    },
    ArrayIndex {
        target: Box<PlaceExpr>,
        indices: Vec<ValueExpr>,
    },
    TupleField {
        target: Box<PlaceExpr>,
        index: usize,
    },
    StructField {
        target: Box<PlaceExpr>,
        field: String,
    },
}

// -- Values ---

#[derive(Clone, Debug)]
pub struct ValueExpr {
    pub id: NodeId,
    pub kind: ValueExprKind,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ValueExprKind {
    Block {
        items: Vec<BlockItem>,
        tail: Option<Box<ValueExpr>>,
    },

    // Literals
    UnitLit,
    IntLit(u64),
    BoolLit(bool),
    CharLit(char),
    StringLit {
        value: String,
    },
    StringFmt {
        segments: Vec<StringFmtSegment>,
    },

    // Literals (compound)
    ArrayLit {
        elem_ty: Option<TypeExpr>,
        init: ArrayLitInit,
    },
    TupleLit(Vec<ValueExpr>),
    StructLit {
        name: String,
        fields: Vec<StructLitField>,
    },
    EnumVariant {
        enum_name: String,
        variant: String,
        payload: Vec<ValueExpr>,
    },

    // Struct update
    StructUpdate {
        target: Box<ValueExpr>,
        fields: Vec<StructUpdateField>,
    },

    // Operators
    BinOp {
        left: Box<ValueExpr>,
        op: BinaryOp,
        right: Box<ValueExpr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<ValueExpr>,
    },

    // Heap allocation
    HeapAlloc {
        expr: Box<ValueExpr>,
    },

    // Moves and coercions
    Move {
        place: Box<PlaceExpr>,
    },
    ImplicitMove {
        place: Box<PlaceExpr>,
    },
    Coerce {
        kind: CoerceKind,
        expr: Box<ValueExpr>,
    },

    AddrOf {
        place: Box<PlaceExpr>,
    },

    // Place-to-value
    Load {
        place: Box<PlaceExpr>,
    },

    // Control flow
    If {
        cond: Box<ValueExpr>,
        then_body: Box<ValueExpr>,
        else_body: Box<ValueExpr>,
    },

    // Range
    Range {
        start: u64,
        end: u64, // exclusive
    },

    // Slice
    Slice {
        target: Box<PlaceExpr>,
        start: Option<Box<ValueExpr>>,
        end: Option<Box<ValueExpr>>,
    },

    // Match
    Match {
        scrutinee: Box<ValueExpr>,
        arms: Vec<MatchArm>,
    },

    // Function/Method call
    Call {
        callee: Box<ValueExpr>,
        args: Vec<CallArg>,
    },
    MethodCall {
        receiver: MethodReceiver,
        method_name: String,
        args: Vec<CallArg>,
    },

    // Closure reference (lifted to a top-level definition).
    ClosureRef {
        def_id: DefId,
    },
}

// -- Array literals ---

#[derive(Clone, Debug)]
pub enum ArrayLitInit {
    Elems(Vec<ValueExpr>),
    Repeat(Box<ValueExpr>, u64),
}

impl ArrayLitInit {
    pub fn length(&self) -> usize {
        match self {
            ArrayLitInit::Elems(elems) => elems.len(),
            ArrayLitInit::Repeat(_, count) => *count as usize,
        }
    }
}

// -- Struct literals ---

#[derive(Clone, Debug)]
pub struct StructLitField {
    pub name: String,
    pub value: ValueExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructUpdateField {
    pub name: String,
    pub value: ValueExpr,
    pub span: Span,
}
