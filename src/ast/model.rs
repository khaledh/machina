//! Generic AST model: parameterized over Def/Ty types.

use crate::ast::NodeId;
use crate::diag::Span;

// -- Module ---

#[derive(Clone, Debug)]
pub struct Module<D, T = ()> {
    pub top_level_items: Vec<TopLevelItem<D, T>>,
}

impl<D, T> Module<D, T> {
    pub fn type_defs(&self) -> Vec<&TypeDef<D>> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::TypeDef(type_def) => Some(type_def),
                _ => None,
            })
            .collect()
    }

    pub fn type_def_by_id(&self, def_id: D) -> Option<&TypeDef<D>>
    where
        D: Copy + Eq,
    {
        self.top_level_items.iter().find_map(|item| match item {
            TopLevelItem::TypeDef(type_def) if type_def.def_id == def_id => Some(type_def),
            _ => None,
        })
    }

    pub fn func_sigs(&self) -> Vec<&FunctionSig<D>> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::FuncDecl(func_decl) => Some(&func_decl.sig),
                TopLevelItem::FuncDef(func_def) => Some(&func_def.sig),
                _ => None,
            })
            .collect()
    }

    pub fn func_decls(&self) -> Vec<&FuncDecl<D>> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::FuncDecl(func_decl) => Some(func_decl),
                _ => None,
            })
            .collect()
    }

    pub fn func_defs(&self) -> Vec<&FuncDef<D, T>> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::FuncDef(func_def) => Some(func_def),
                _ => None,
            })
            .collect()
    }

    pub fn method_blocks(&self) -> Vec<&MethodBlock<D, T>> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::MethodBlock(method_block) => Some(method_block),
                _ => None,
            })
            .collect()
    }

    pub fn callables(&self) -> Vec<CallableRef<'_, D, T>> {
        self.top_level_items
            .iter()
            .flat_map(|item| match item {
                TopLevelItem::FuncDecl(func_decl) => {
                    vec![CallableRef::FuncDecl(func_decl)]
                }
                TopLevelItem::FuncDef(func_def) => {
                    vec![CallableRef::FuncDef(func_def)]
                }
                TopLevelItem::MethodBlock(method_block) => method_block
                    .method_defs
                    .iter()
                    .map(|method_def| CallableRef::MethodDef {
                        type_name: &method_block.type_name,
                        method_def,
                    })
                    .collect(),
                TopLevelItem::ClosureDecl(closure_decl) => {
                    vec![CallableRef::ClosureDecl(closure_decl)]
                }
                TopLevelItem::TypeDef(_) => vec![],
            })
            .collect()
    }
}

// -- Top Leve Items ---

#[derive(Clone, Debug)]
pub enum TopLevelItem<D, T = ()> {
    TypeDef(TypeDef<D>),
    FuncDecl(FuncDecl<D>),          // function declaration
    FuncDef(FuncDef<D, T>),         // function definition
    MethodBlock(MethodBlock<D, T>), // method definitions
    ClosureDecl(ClosureDecl<D, T>), // closure declaration (generated)
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a, D, T = ()> {
    FuncDecl(&'a FuncDecl<D>),
    FuncDef(&'a FuncDef<D, T>),
    MethodDef {
        type_name: &'a str,
        method_def: &'a MethodDef<D, T>,
    },
    ClosureDecl(&'a ClosureDecl<D, T>),
}

impl<'a, D, T> CallableRef<'a, D, T> {
    pub fn id(&self) -> NodeId {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.id,
            CallableRef::FuncDef(func_def) => func_def.id,
            CallableRef::MethodDef { method_def, .. } => method_def.id,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.id,
        }
    }

    pub fn def_id(&self) -> D
    where
        D: Copy,
    {
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
pub struct TypeDef<D> {
    pub id: NodeId,
    pub def_id: D,
    pub name: String,
    pub kind: TypeDefKind<D>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeDefKind<D> {
    Alias { aliased_ty: TypeExpr<D> },
    Struct { fields: Vec<StructDefField<D>> },
    Enum { variants: Vec<EnumDefVariant<D>> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructDefField<D> {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr<D>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumDefVariant<D> {
    pub id: NodeId,
    pub name: String,
    pub payload: Vec<TypeExpr<D>>,
    pub span: Span,
}

// -- Type Expressions ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeExpr<D> {
    pub id: NodeId,
    pub kind: TypeExprKind<D>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExprKind<D> {
    Named {
        ident: String,
        def_id: D,
    },
    Array {
        elem_ty_expr: Box<TypeExpr<D>>,
        dims: Vec<usize>,
    },
    Tuple {
        field_ty_exprs: Vec<TypeExpr<D>>,
    },
    Range {
        min: u64,
        max: u64,
    },
    Slice {
        elem_ty_expr: Box<TypeExpr<D>>,
    },
    Heap {
        elem_ty_expr: Box<TypeExpr<D>>,
    },
    Fn {
        params: Vec<FnTypeParam<D>>,
        ret_ty_expr: Box<TypeExpr<D>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnTypeParam<D> {
    pub mode: ParamMode,
    pub ty_expr: TypeExpr<D>,
}

// -- String Literals ---

#[derive(Clone, Debug)]
pub enum StringFmtSegment<D, T = ()> {
    Literal { value: String, span: Span },
    Expr { expr: Box<Expr<D, T>>, span: Span },
}

// -- Functions ---

#[derive(Clone, Debug)]
pub struct FuncDecl<D> {
    pub id: NodeId,
    pub def_id: D,
    pub sig: FunctionSig<D>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FuncDef<D, T = ()> {
    pub id: NodeId,
    pub def_id: D,
    pub sig: FunctionSig<D>,
    pub body: Expr<D, T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionSig<D> {
    pub name: String,
    pub params: Vec<Param<D>>,
    pub ret_ty_expr: TypeExpr<D>,
    pub span: Span,
}

// -- Methods ---

#[derive(Clone, Debug)]
pub struct MethodBlock<D, T = ()> {
    pub id: NodeId,
    pub type_name: String,
    pub method_defs: Vec<MethodDef<D, T>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodDef<D, T = ()> {
    pub id: NodeId,
    pub def_id: D,
    pub sig: MethodSig<D>,
    pub body: Expr<D, T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodSig<D> {
    pub name: String,
    pub self_param: SelfParam<D>,
    pub params: Vec<Param<D>>,
    pub ret_ty_expr: TypeExpr<D>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SelfParam<D> {
    pub id: NodeId,
    pub def_id: D,
    pub mode: ParamMode,
    pub span: Span,
}

// -- Closures Decls ---

#[derive(Clone, Debug)]
pub struct ClosureDecl<D, T = ()> {
    pub id: NodeId,
    pub def_id: D,
    pub sig: ClosureSig<D>,
    pub body: Expr<D, T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ClosureSig<D> {
    pub name: String,
    pub params: Vec<Param<D>>,
    pub return_ty: TypeExpr<D>,
    pub span: Span,
}

// -- Parameters (common) ---

#[derive(Clone, Debug)]
pub struct Param<D> {
    pub id: NodeId,
    pub ident: String,
    pub def_id: D,
    pub typ: TypeExpr<D>,
    pub mode: ParamMode,
    pub span: Span,
}

// -- Call Args --

#[derive(Clone, Debug)]
pub struct CallArg<D, T = ()> {
    pub mode: CallArgMode,
    pub expr: Expr<D, T>,
    pub span: Span,
}

// -- Patterns ---

#[derive(Clone, Debug)]
pub struct BindPattern<D> {
    pub id: NodeId,
    pub kind: BindPatternKind<D>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum BindPatternKind<D> {
    Name {
        ident: String,
        def_id: D,
    },
    Array {
        patterns: Vec<BindPattern<D>>,
    },
    Tuple {
        patterns: Vec<BindPattern<D>>,
    },
    Struct {
        name: String,
        fields: Vec<StructFieldBindPattern<D>>,
    },
}

#[derive(Clone, Debug)]
pub struct StructFieldBindPattern<D> {
    pub name: String,
    pub pattern: BindPattern<D>,
    pub span: Span,
}

// -- Match patterns ---

#[derive(Clone, Debug)]
pub struct MatchArm<D, T = ()> {
    pub id: NodeId,
    pub pattern: MatchPattern<D>,
    pub body: Expr<D, T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MatchPattern<D> {
    Wildcard {
        span: Span,
    },
    BoolLit {
        value: bool,
        span: Span,
    },
    IntLit {
        value: u64,
        span: Span,
    },
    Binding {
        id: NodeId,
        ident: String,
        def_id: D,
        span: Span,
    },
    Tuple {
        patterns: Vec<MatchPattern<D>>,
        span: Span,
    },
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        bindings: Vec<MatchPatternBinding<D>>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum MatchPatternBinding<D> {
    Named {
        id: NodeId,
        ident: String,
        def_id: D,
        span: Span,
    },
    Wildcard {
        span: Span,
    },
}

// --- Blocks ---

#[derive(Clone, Debug)]
pub enum BlockItem<D, T = ()> {
    Stmt(StmtExpr<D, T>),
    Expr(Expr<D, T>),
}

// --- Statement Expressions ---

#[derive(Clone, Debug)]
pub struct StmtExpr<D, T = ()> {
    pub id: NodeId,
    pub kind: StmtExprKind<D, T>,
    pub ty: T,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtExprKind<D, T = ()> {
    LetBind {
        pattern: BindPattern<D>,
        decl_ty: Option<TypeExpr<D>>,
        value: Box<Expr<D, T>>,
    },
    VarBind {
        pattern: BindPattern<D>,
        decl_ty: Option<TypeExpr<D>>,
        value: Box<Expr<D, T>>,
    },
    VarDecl {
        ident: String,
        def_id: D,
        decl_ty: TypeExpr<D>,
    },
    Assign {
        assignee: Box<Expr<D, T>>,
        value: Box<Expr<D, T>>,
    },
    While {
        cond: Box<Expr<D, T>>,
        body: Box<Expr<D, T>>,
    },
    For {
        pattern: BindPattern<D>,
        iter: Box<Expr<D, T>>,
        body: Box<Expr<D, T>>,
    },
}

// -- Expressions ---

#[derive(Clone, Debug)]
pub struct Expr<D, T = ()> {
    pub id: NodeId,
    pub kind: ExprKind<D, T>,
    pub ty: T,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind<D, T = ()> {
    Block {
        items: Vec<BlockItem<D, T>>,
        tail: Option<Box<Expr<D, T>>>,
    },

    // Literals (scalar)
    UnitLit,
    IntLit(u64),
    BoolLit(bool),
    CharLit(char),
    StringLit {
        value: String,
    },
    StringFmt {
        segments: Vec<StringFmtSegment<D, T>>,
    },

    // Literals (compound)
    ArrayLit {
        elem_ty: Option<TypeExpr<D>>,
        init: ArrayLitInit<D, T>,
    },
    TupleLit(Vec<Expr<D, T>>),
    StructLit {
        name: String,
        fields: Vec<StructLitField<D, T>>,
    },
    EnumVariant {
        enum_name: String,
        variant: String,
        payload: Vec<Expr<D, T>>,
    },

    // Struct update
    StructUpdate {
        target: Box<Expr<D, T>>,
        fields: Vec<StructUpdateField<D, T>>,
    },

    // Operators
    BinOp {
        left: Box<Expr<D, T>>,
        op: BinaryOp,
        right: Box<Expr<D, T>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr<D, T>>,
    },

    // Heap allocation
    HeapAlloc {
        expr: Box<Expr<D, T>>,
    },

    // Move
    Move {
        expr: Box<Expr<D, T>>,
    },

    // Var, array index, tuple field, struct field
    Var {
        ident: String,
        def_id: D,
    },
    ArrayIndex {
        target: Box<Expr<D, T>>,
        indices: Vec<Expr<D, T>>,
    },
    TupleField {
        target: Box<Expr<D, T>>,
        index: usize,
    },
    StructField {
        target: Box<Expr<D, T>>,
        field: String,
    },

    // Control flow
    If {
        cond: Box<Expr<D, T>>,
        then_body: Box<Expr<D, T>>,
        else_body: Box<Expr<D, T>>,
    },

    // Range
    Range {
        start: u64,
        end: u64, // exclusive
    },

    // Slice
    Slice {
        target: Box<Expr<D, T>>,
        start: Option<Box<Expr<D, T>>>,
        end: Option<Box<Expr<D, T>>>,
    },

    // Match
    Match {
        scrutinee: Box<Expr<D, T>>,
        arms: Vec<MatchArm<D, T>>,
    },

    // Function/Method call
    Call {
        callee: Box<Expr<D, T>>,
        args: Vec<CallArg<D, T>>,
    },
    MethodCall {
        callee: Box<Expr<D, T>>,
        method_name: String,
        args: Vec<CallArg<D, T>>,
    },

    Closure {
        ident: String,
        def_id: D,
        params: Vec<Param<D>>,
        return_ty: TypeExpr<D>,
        body: Box<Expr<D, T>>,
    },
}

// -- Array literals ---

#[derive(Clone, Debug)]
pub enum ArrayLitInit<D, T = ()> {
    Elems(Vec<Expr<D, T>>),
    Repeat(Box<Expr<D, T>>, u64),
}

impl<D, T> ArrayLitInit<D, T> {
    pub fn length(&self) -> usize {
        match self {
            ArrayLitInit::Elems(elems) => elems.len(),
            ArrayLitInit::Repeat(_, count) => *count as usize,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructLitField<D, T = ()> {
    pub id: NodeId,
    pub name: String,
    pub value: Expr<D, T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructUpdateField<D, T = ()> {
    pub id: NodeId,
    pub name: String,
    pub value: Expr<D, T>,
    pub span: Span,
}

// -- Parameter / call modes ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParamMode {
    In,
    InOut,
    Out,
    Sink,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallArgMode {
    Default,
    InOut,
    Out,
    Move,
}

// -- Operators ---

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison operators
    Eq,
    Ne,
    Lt,
    Gt,
    LtEq,
    GtEq,

    // Bitwise operators
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,

    // Logical operators
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    LogicalNot,
    BitNot,
}
