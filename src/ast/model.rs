//! Generic AST model: parameterized over identifier representation.

use crate::ast::NodeId;
use crate::diag::Span;

// -- Module ---

#[derive(Clone, Debug)]
pub struct Module<T> {
    pub decls: Vec<Decl<T>>,
}

impl<T> Module<T> {
    pub fn type_defs(&self) -> Vec<&TypeDef<T>> {
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

    pub fn func_sigs(&self) -> Vec<&FunctionSig<T>> {
        self.decls
            .iter()
            .filter_map(|decl| match decl {
                Decl::FunctionDecl(func_decl) => Some(&func_decl.sig),
                Decl::Function(func) => Some(&func.sig),
                _ => None,
            })
            .collect()
    }

    pub fn func_decls(&self) -> Vec<&FunctionDecl<T>> {
        self.decls
            .iter()
            .filter_map(|decl| match decl {
                Decl::FunctionDecl(func_decl) => Some(func_decl),
                _ => None,
            })
            .collect()
    }

    pub fn funcs(&self) -> Vec<&Function<T>> {
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

    pub fn method_blocks(&self) -> Vec<&MethodBlock<T>> {
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

    pub fn callables(&self) -> Vec<CallableRef<'_, T>> {
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
pub enum Decl<T> {
    TypeDef(TypeDef<T>),
    FunctionDecl(FunctionDecl<T>), // function declaration
    Function(Function<T>),         // function definition
    MethodBlock(MethodBlock<T>),   // method definitions
    ClosureDecl(ClosureDecl<T>),   // closure declaration (generated)
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a, T> {
    FunctionDecl(&'a FunctionDecl<T>),
    Function(&'a Function<T>),
    Method {
        type_name: &'a str,
        method: &'a Method<T>,
    },
    ClosureDecl(&'a ClosureDecl<T>),
}

impl<'a, T> CallableRef<'a, T> {
    pub fn id(&self) -> NodeId {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.id,
            CallableRef::Function(function) => function.id,
            CallableRef::Method { method, .. } => method.id,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.id,
        }
    }

    pub fn name(&self) -> String
    where
        T: std::fmt::Display,
    {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.sig.name.clone(),
            CallableRef::Function(function) => function.sig.name.clone(),
            CallableRef::Method { method, .. } => method.sig.name.clone(),
            CallableRef::ClosureDecl(closure_decl) => closure_decl.sig.name.clone(),
        }
    }

    pub fn symbol_base_name(&self) -> String
    where
        T: std::fmt::Display,
    {
        match self {
            CallableRef::FunctionDecl(_) => self.name(),
            CallableRef::Function(_) => self.name(),
            CallableRef::Method { type_name, method } => {
                format!("{type_name}${}", method.sig.name)
            }
            CallableRef::ClosureDecl(_) => self.name(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CallableRef::FunctionDecl(func_decl) => func_decl.span,
            CallableRef::Function(function) => function.span,
            CallableRef::Method { method, .. } => method.span,
            CallableRef::ClosureDecl(closure_decl) => closure_decl.span,
        }
    }
}

// -- Type Definitions ---

#[derive(Clone, Debug)]
pub struct TypeDef<T> {
    pub id: NodeId,
    pub name: String,
    pub kind: TypeDefKind<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeDefKind<T> {
    Alias { aliased_ty: TypeExpr<T> },
    Struct { fields: Vec<StructField<T>> },
    Enum { variants: Vec<EnumVariant<T>> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField<T> {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr<T>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant<T> {
    pub id: NodeId,
    pub name: String,
    pub payload: Vec<TypeExpr<T>>,
    pub span: Span,
}

// -- Type Expressions ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeExpr<T> {
    pub id: NodeId,
    pub kind: TypeExprKind<T>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExprKind<T> {
    Named(T),
    Array {
        elem_ty: Box<TypeExpr<T>>,
        dims: Vec<usize>,
    },
    Tuple {
        fields: Vec<TypeExpr<T>>,
    },
    Range {
        min: u64,
        max: u64,
    },
    Slice {
        elem_ty: Box<TypeExpr<T>>,
    },
    Heap {
        elem_ty: Box<TypeExpr<T>>,
    },
    Fn {
        params: Vec<FnTypeParam<T>>,
        return_ty: Box<TypeExpr<T>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnTypeParam<T> {
    pub mode: ParamMode,
    pub ty: TypeExpr<T>,
}

// -- String Literals ---

#[derive(Clone, Debug)]
pub enum StringFmtSegment<T> {
    Literal { value: String, span: Span },
    Expr { expr: Box<Expr<T>>, span: Span },
}

// -- Functions ---

#[derive(Clone, Debug)]
pub struct FunctionDecl<T> {
    pub id: NodeId,
    pub sig: FunctionSig<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Function<T> {
    pub id: NodeId,
    pub sig: FunctionSig<T>,
    pub body: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionSig<T> {
    pub name: String,
    pub params: Vec<Param<T>>,
    pub return_type: TypeExpr<T>,
    pub span: Span,
}

// -- Methods ---

#[derive(Clone, Debug)]
pub struct MethodBlock<T> {
    pub id: NodeId,
    pub type_name: String,
    pub methods: Vec<Method<T>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Method<T> {
    pub id: NodeId,
    pub sig: MethodSig<T>,
    pub body: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodSig<T> {
    pub name: String,
    pub self_param: SelfParam,
    pub params: Vec<Param<T>>,
    pub return_type: TypeExpr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SelfParam {
    pub id: NodeId,
    pub mode: ParamMode,
    pub span: Span,
}

// -- Closures Decls ---

#[derive(Clone, Debug)]
pub struct ClosureDecl<T> {
    pub id: NodeId,
    pub sig: ClosureSig<T>,
    pub body: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ClosureSig<T> {
    pub name: String,
    pub params: Vec<Param<T>>,
    pub return_ty: TypeExpr<T>,
    pub span: Span,
}

// -- Parameters (common) ---

#[derive(Clone, Debug)]
pub struct Param<T> {
    pub id: NodeId,
    pub ident: T,
    pub typ: TypeExpr<T>,
    pub mode: ParamMode,
    pub span: Span,
}

// -- Call Args --

#[derive(Clone, Debug)]
pub struct CallArg<T> {
    pub mode: CallArgMode,
    pub expr: Expr<T>,
    pub span: Span,
}

// -- Patterns ---

#[derive(Clone, Debug)]
pub struct BindPattern<T> {
    pub id: NodeId,
    pub kind: BindPatternKind<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum BindPatternKind<T> {
    Name(T),
    Array {
        patterns: Vec<BindPattern<T>>,
    },
    Tuple {
        patterns: Vec<BindPattern<T>>,
    },
    Struct {
        name: String,
        fields: Vec<StructFieldBindPattern<T>>,
    },
}

#[derive(Clone, Debug)]
pub struct StructFieldBindPattern<T> {
    pub name: String,
    pub pattern: BindPattern<T>,
    pub span: Span,
}

// -- Match patterns ---

#[derive(Clone, Debug)]
pub struct MatchArm<T> {
    pub id: NodeId,
    pub pattern: MatchPattern<T>,
    pub body: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MatchPattern<T> {
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
        ident: T,
        span: Span,
    },
    Tuple {
        patterns: Vec<MatchPattern<T>>,
        span: Span,
    },
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        bindings: Vec<MatchPatternBinding<T>>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum MatchPatternBinding<T> {
    Named { id: NodeId, ident: T, span: Span },
    Wildcard { span: Span },
}

// --- Blocks ---

#[derive(Clone, Debug)]
pub enum BlockItem<T> {
    Stmt(StmtExpr<T>),
    Expr(Expr<T>),
}

// --- Statement Expressions ---

#[derive(Clone, Debug)]
pub struct StmtExpr<T> {
    pub id: NodeId,
    pub kind: StmtExprKind<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtExprKind<T> {
    LetBind {
        pattern: BindPattern<T>,
        decl_ty: Option<TypeExpr<T>>,
        value: Box<Expr<T>>,
    },
    VarBind {
        pattern: BindPattern<T>,
        decl_ty: Option<TypeExpr<T>>,
        value: Box<Expr<T>>,
    },
    VarDecl {
        ident: T,
        decl_ty: TypeExpr<T>,
    },
    Assign {
        assignee: Box<Expr<T>>,
        value: Box<Expr<T>>,
    },
    While {
        cond: Box<Expr<T>>,
        body: Box<Expr<T>>,
    },
    For {
        pattern: BindPattern<T>,
        iter: Box<Expr<T>>,
        body: Box<Expr<T>>,
    },
}

// -- Expressions ---

#[derive(Clone, Debug)]
pub struct Expr<T> {
    pub id: NodeId,
    pub kind: ExprKind<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind<T> {
    Block {
        items: Vec<BlockItem<T>>,
        tail: Option<Box<Expr<T>>>,
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
        segments: Vec<StringFmtSegment<T>>,
    },

    // Literals (compound)
    ArrayLit {
        elem_ty: Option<TypeExpr<T>>,
        init: ArrayLitInit<T>,
    },
    TupleLit(Vec<Expr<T>>),
    StructLit {
        name: String,
        fields: Vec<StructLitField<T>>,
    },
    EnumVariant {
        enum_name: String,
        variant: String,
        payload: Vec<Expr<T>>,
    },

    // Struct update
    StructUpdate {
        target: Box<Expr<T>>,
        fields: Vec<StructUpdateField<T>>,
    },

    // Operators
    BinOp {
        left: Box<Expr<T>>,
        op: BinaryOp,
        right: Box<Expr<T>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr<T>>,
    },

    // Heap allocation
    HeapAlloc {
        expr: Box<Expr<T>>,
    },

    // Move
    Move {
        expr: Box<Expr<T>>,
    },

    // Var, array index, tuple field, struct field
    Var(T),
    ArrayIndex {
        target: Box<Expr<T>>,
        indices: Vec<Expr<T>>,
    },
    TupleField {
        target: Box<Expr<T>>,
        index: usize,
    },
    StructField {
        target: Box<Expr<T>>,
        field: String,
    },

    // Control flow
    If {
        cond: Box<Expr<T>>,
        then_body: Box<Expr<T>>,
        else_body: Box<Expr<T>>,
    },

    // Range
    Range {
        start: u64,
        end: u64, // exclusive
    },

    // Slice
    Slice {
        target: Box<Expr<T>>,
        start: Option<Box<Expr<T>>>,
        end: Option<Box<Expr<T>>>,
    },

    // Match
    Match {
        scrutinee: Box<Expr<T>>,
        arms: Vec<MatchArm<T>>,
    },

    // Function/Method call
    Call {
        callee: Box<Expr<T>>,
        args: Vec<CallArg<T>>,
    },
    MethodCall {
        callee: Box<Expr<T>>,
        method: String,
        args: Vec<CallArg<T>>,
    },

    Closure {
        ident: T,
        params: Vec<Param<T>>,
        return_ty: TypeExpr<T>,
        body: Box<Expr<T>>,
    },
}

// -- Array literals ---

#[derive(Clone, Debug)]
pub enum ArrayLitInit<T> {
    Elems(Vec<Expr<T>>),
    Repeat(Box<Expr<T>>, u64),
}

impl<T> ArrayLitInit<T> {
    pub fn length(&self) -> usize {
        match self {
            ArrayLitInit::Elems(elems) => elems.len(),
            ArrayLitInit::Repeat(_, count) => *count as usize,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructLitField<T> {
    pub id: NodeId,
    pub name: String,
    pub value: Expr<T>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructUpdateField<T> {
    pub id: NodeId,
    pub name: String,
    pub value: Expr<T>,
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
