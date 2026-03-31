//! Abstract Syntax Tree.

use crate::core::ast::NodeId;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable};
use serde::{Deserialize, Serialize};

// -- Attributes --

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttrArg {
    String(String),
    Int(u64),
    Ident(String),
    Named { name: String, value: Box<AttrArg> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<AttrArg>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocComment {
    pub raw: String,
    pub span: Span,
}

// -- Module ---

#[derive(Clone, Debug)]
pub struct Module {
    pub requires: Vec<Require>,
    pub top_level_items: Vec<TopLevelItem>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Require {
    pub id: NodeId,
    pub path: Vec<String>,
    pub alias: Option<String>,
    pub span: Span,
}

impl Module {
    pub fn machine_defs(&self) -> Vec<&MachineDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::MachineDef(machine_def) => Some(machine_def),
                _ => None,
            })
            .collect()
    }

    pub fn trait_defs(&self) -> Vec<&TraitDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::TraitDef(trait_def) => Some(trait_def),
                _ => None,
            })
            .collect()
    }

    pub fn type_defs(&self) -> Vec<&TypeDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::TypeDef(type_def) => Some(type_def),
                _ => None,
            })
            .collect()
    }

    pub fn type_def_by_id(&self, def_table: &DefTable, def_id: DefId) -> Option<&TypeDef> {
        self.type_defs().into_iter().find(|type_def| {
            def_table
                .lookup_node_def_id(type_def.id)
                .is_some_and(|d| d == def_id)
        })
    }

    pub fn func_sigs(&self) -> Vec<&FunctionSig> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::FuncDecl(func_decl) => Some(&func_decl.sig),
                TopLevelItem::FuncDef(func_def) => Some(&func_def.sig),
                _ => None,
            })
            .collect()
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

    pub fn static_defs(&self) -> Vec<&StaticDef> {
        self.top_level_items
            .iter()
            .filter_map(|item| match item {
                TopLevelItem::StaticDef(static_def) => Some(static_def),
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
                TopLevelItem::FuncDecl(func_decl) => {
                    vec![CallableRef::FuncDecl(func_decl)]
                }
                TopLevelItem::FuncDef(func_def) => {
                    vec![CallableRef::FuncDef(func_def)]
                }
                TopLevelItem::MethodBlock(method_block) => method_block
                    .method_items
                    .iter()
                    .map(|method_item| match method_item {
                        MethodItem::Decl(method_decl) => CallableRef::MethodDecl {
                            type_name: &method_block.type_name,
                            method_decl,
                        },
                        MethodItem::Def(method_def) => CallableRef::MethodDef {
                            type_name: &method_block.type_name,
                            method_def,
                        },
                    })
                    .collect(),
                TopLevelItem::ClosureDef(closure_decl) => {
                    vec![CallableRef::ClosureDef(closure_decl)]
                }
                TopLevelItem::StaticDef(_)
                | TopLevelItem::TypeDef(_)
                | TopLevelItem::TraitDef(_)
                | TopLevelItem::MachineDef(_) => vec![],
            })
            .collect()
    }
}

// -- Top Level Items ---

#[derive(Clone, Debug)]
pub enum TopLevelItem {
    TraitDef(TraitDef),
    TypeDef(TypeDef),
    MachineDef(MachineDef),
    StaticDef(StaticDef),     // top-level static definition
    FuncDecl(FuncDecl),       // function declaration
    FuncDef(FuncDef),         // function definition
    MethodBlock(MethodBlock), // method declarations/definitions
    ClosureDef(ClosureDef),   // closure definition (generated)
}

#[derive(Clone, Debug)]
pub struct MachineDef {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub name: String,
    pub host: MachineHost,
    pub items: Vec<MachineItem>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineHost {
    pub id: NodeId,
    pub type_name: String,
    pub key_field: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MachineItem {
    Fields(MachineFields),
    Constructor(FuncDef),
    Action(MachineTransitionHandler),
    Trigger(MachineTransitionHandler),
    On(MachineOnHandler),
}

#[derive(Clone, Debug)]
pub struct MachineFields {
    pub id: NodeId,
    pub fields: Vec<StructDefField>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MachineTransitionHandler {
    pub id: NodeId,
    pub name: String,
    pub instance_param: String,
    pub params: Vec<Param>,
    pub ret_ty_expr: Option<TypeExpr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MachineOnHandler {
    pub id: NodeId,
    pub selector_ty: TypeExpr,
    pub params: Vec<Param>,
    pub provenance: Option<OnHandlerProvenance>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct OnHandlerProvenance {
    pub param: Param,
    /// Optional request-site label from `for RequestType:label(binding)`.
    pub request_site_label: Option<String>,
}

#[derive(Clone, Debug)]
pub struct TraitDef {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub methods: Vec<TraitMethod>,
    pub properties: Vec<TraitProperty>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TraitMethod {
    pub id: NodeId,
    pub sig: MethodSig,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TraitProperty {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr,
    pub has_get: bool,
    pub has_set: bool,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum CallableRef<'a> {
    FuncDecl(&'a FuncDecl),
    FuncDef(&'a FuncDef),
    MethodDecl {
        type_name: &'a str,
        method_decl: &'a MethodDecl,
    },
    MethodDef {
        type_name: &'a str,
        method_def: &'a MethodDef,
    },
    ClosureDef(&'a ClosureDef),
}

impl<'a> CallableRef<'a> {
    pub fn id(&self) -> NodeId {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.id,
            CallableRef::FuncDef(func_def) => func_def.id,
            CallableRef::MethodDecl { method_decl, .. } => method_decl.id,
            CallableRef::MethodDef { method_def, .. } => method_def.id,
            CallableRef::ClosureDef(closure_def) => closure_def.id,
        }
    }

    pub fn name(&self) -> String {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.sig.name.clone(),
            CallableRef::FuncDef(func_def) => func_def.sig.name.clone(),
            CallableRef::MethodDecl { method_decl, .. } => method_decl.sig.name.clone(),
            CallableRef::MethodDef { method_def, .. } => method_def.sig.name.clone(),
            CallableRef::ClosureDef(closure_def) => closure_def.sig.name.clone(),
        }
    }

    pub fn symbol_base_name(&self) -> String {
        match self {
            CallableRef::FuncDecl(_) => self.name(),
            CallableRef::FuncDef(_) => self.name(),
            CallableRef::MethodDecl {
                type_name,
                method_decl,
            } => {
                format!("{type_name}${}", method_decl.sig.name)
            }
            CallableRef::MethodDef {
                type_name,
                method_def,
            } => {
                format!("{type_name}${}", method_def.sig.name)
            }
            CallableRef::ClosureDef(_) => self.name(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CallableRef::FuncDecl(func_decl) => func_decl.span,
            CallableRef::FuncDef(func_def) => func_def.span,
            CallableRef::MethodDecl { method_decl, .. } => method_decl.span,
            CallableRef::MethodDef { method_def, .. } => method_def.span,
            CallableRef::ClosureDef(closure_def) => closure_def.span,
        }
    }
}

// -- Type Definitions ---

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub kind: TypeDefKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeDefKind {
    Alias { aliased_ty: TypeExpr },
    Struct { fields: Vec<StructDefField> },
    Enum { variants: Vec<EnumDefVariant> },
    Linear { linear: LinearTypeDef },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructDefField {
    pub id: NodeId,
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumDefVariant {
    pub id: NodeId,
    pub name: String,
    pub payload: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LinearTypeDef {
    pub fields: Vec<StructDefField>,
    pub states: Vec<LinearStateVariant>,
    pub actions: Vec<LinearTransitionDecl>,
    pub triggers: Vec<LinearTransitionDecl>,
    pub roles: Vec<LinearRoleDecl>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearStateVariant {
    pub id: NodeId,
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub payload: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LinearTransitionDecl {
    pub id: NodeId,
    pub name: String,
    pub params: Vec<LinearTransitionParam>,
    pub source_state: String,
    pub target_state: String,
    pub error_ty_expr: Option<TypeExpr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearTransitionParam {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearRoleDecl {
    pub id: NodeId,
    pub name: String,
    pub allowed_actions: Vec<String>,
    pub span: Span,
}

// -- Type Expressions ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeExpr {
    pub id: NodeId,
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RefinementKind {
    Bounds { min: i128, max: i128 },
    NonZero,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExprKind {
    Infer,
    Union {
        variants: Vec<TypeExpr>,
    },
    Named {
        ident: String,
        type_args: Vec<TypeExpr>,
    },
    Refined {
        base_ty_expr: Box<TypeExpr>,
        refinements: Vec<RefinementKind>,
    },
    Array {
        elem_ty_expr: Box<TypeExpr>,
        dims: Vec<usize>,
    },
    DynArray {
        elem_ty_expr: Box<TypeExpr>,
    },
    Tuple {
        field_ty_exprs: Vec<TypeExpr>,
    },
    Slice {
        elem_ty_expr: Box<TypeExpr>,
    },
    Heap {
        elem_ty_expr: Box<TypeExpr>,
    },
    Ref {
        mutable: bool,
        elem_ty_expr: Box<TypeExpr>,
    },
    Fn {
        params: Vec<FnTypeParam>,
        ret_ty_expr: Box<TypeExpr>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnTypeParam {
    pub mode: ParamMode,
    pub ty_expr: TypeExpr,
}

// -- String Literals ---

#[derive(Clone, Debug)]
pub enum StringFmtSegment {
    Literal { value: String, span: Span },
    Expr { expr: Box<Expr>, span: Span },
}

// -- Functions ---

#[derive(Clone, Debug)]
pub struct FuncDecl {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub sig: FunctionSig,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub sig: FunctionSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StaticMutability {
    Let,
    Var,
}

#[derive(Clone, Debug)]
pub struct StaticDef {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub mutability: StaticMutability,
    pub ty: Option<TypeExpr>,
    pub init: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionSig {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub ret_ty_expr: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub id: NodeId,
    pub ident: String,
    pub bound: Option<TypeParamBound>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeParamBound {
    pub id: NodeId,
    pub name: String,
    pub span: Span,
}

// -- Methods ---

#[derive(Clone, Debug)]
pub struct MethodBlock {
    pub id: NodeId,
    pub type_name: String,
    pub type_args: Vec<TypeExpr>,
    pub trait_name: Option<String>,
    pub method_items: Vec<MethodItem>,
    pub span: Span,
}

#[derive(Clone, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum MethodItem {
    Decl(MethodDecl),
    Def(MethodDef),
}

#[derive(Clone, Debug)]
pub struct MethodDecl {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub sig: MethodSig,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodDef {
    pub id: NodeId,
    pub doc: Option<DocComment>,
    pub attrs: Vec<Attribute>,
    pub sig: MethodSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MethodSig {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub self_param: SelfParam,
    pub params: Vec<Param>,
    pub ret_ty_expr: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SelfParam {
    pub id: NodeId,
    pub mode: ParamMode,
    pub receiver_ty_expr: Option<TypeExpr>,
    pub span: Span,
}

// -- Closure Definitions ---

#[derive(Clone, Debug)]
pub struct ClosureDef {
    pub id: NodeId,
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

#[derive(Clone, Debug)]
pub enum CaptureSpec {
    Move {
        id: NodeId,
        ident: String,
        span: Span,
    },
}

// -- Parameters (common) ---

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub ident: String,
    pub typ: TypeExpr,
    pub mode: ParamMode,
    pub default: Option<Expr>,
    pub span: Span,
}

// -- Call Args --

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgLabel {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct CallArg {
    pub label: Option<ArgLabel>,
    pub mode: CallArgMode,
    pub expr: Expr,
    pub init: InitInfo,
    pub span: Span,
}

// -- Patterns ---

#[derive(Clone, Debug)]
pub struct BindPattern {
    pub id: NodeId,
    pub kind: BindPatternKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum BindPatternKind {
    Wildcard,
    Name {
        ident: String,
    },
    Array {
        prefix: Vec<BindPattern>,
        rest: Option<ArrayRestBindPattern>,
        suffix: Vec<BindPattern>,
    },
    Tuple {
        patterns: Vec<BindPattern>,
    },
    Struct {
        name: String,
        fields: Vec<StructFieldBindPattern>,
    },
}

#[derive(Clone, Debug)]
pub struct ArrayRestBindPattern {
    pub pattern: Option<Box<BindPattern>>,
    pub span: Span,
}

impl BindPatternKind {
    pub fn for_each_child_pattern(&self, mut f: impl FnMut(&BindPattern)) {
        match self {
            Self::Wildcard | Self::Name { .. } => {}
            Self::Array {
                prefix,
                rest,
                suffix,
            } => {
                for pattern in prefix {
                    f(pattern);
                }
                if let Some(rest) = rest
                    && let Some(pattern) = &rest.pattern
                {
                    f(pattern);
                }
                for pattern in suffix {
                    f(pattern);
                }
            }
            Self::Tuple { patterns } => {
                for pattern in patterns {
                    f(pattern);
                }
            }
            Self::Struct { fields, .. } => {
                for field in fields {
                    f(&field.pattern);
                }
            }
        }
    }

    pub fn for_each_child_pattern_mut(&mut self, mut f: impl FnMut(&mut BindPattern)) {
        match self {
            Self::Wildcard | Self::Name { .. } => {}
            Self::Array {
                prefix,
                rest,
                suffix,
            } => {
                for pattern in prefix {
                    f(pattern);
                }
                if let Some(rest) = rest
                    && let Some(pattern) = &mut rest.pattern
                {
                    f(pattern);
                }
                for pattern in suffix {
                    f(pattern);
                }
            }
            Self::Tuple { patterns } => {
                for pattern in patterns {
                    f(pattern);
                }
            }
            Self::Struct { fields, .. } => {
                for field in fields {
                    f(&mut field.pattern);
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructFieldBindPattern {
    pub name: String,
    pub pattern: BindPattern,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct UsingBinding {
    pub id: NodeId,
    pub ident: String,
    pub span: Span,
}

// -- Match patterns ---

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub id: NodeId,
    pub patterns: Vec<MatchPattern>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MatchPattern {
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
        span: Span,
    },
    TypedBinding {
        id: NodeId,
        ident: String,
        ty_expr: TypeExpr,
        span: Span,
    },
    Tuple {
        patterns: Vec<MatchPattern>,
        span: Span,
    },
    EnumVariant {
        id: NodeId,
        enum_name: Option<String>,
        type_args: Vec<TypeExpr>,
        variant_name: String,
        bindings: Vec<MatchPatternBinding>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum MatchPatternBinding {
    Named {
        id: NodeId,
        ident: String,
        span: Span,
    },
    Wildcard {
        span: Span,
    },
}

// --- Blocks ---

#[derive(Clone, Debug)]
pub enum BlockItem {
    Stmt(StmtExpr),
    Expr(Expr),
}

// --- Statement Expressions ---

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
        value: Box<Expr>,
    },
    VarBind {
        pattern: BindPattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },
    VarDecl {
        ident: String,
        decl_ty: TypeExpr,
    },
    Assign {
        assignee: Box<Expr>,
        value: Box<Expr>,
        init: InitInfo,
    },
    CompoundAssign {
        assignee: Box<Expr>,
        op: BinaryOp,
        value: Box<Expr>,
        init: InitInfo,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    For {
        pattern: BindPattern,
        iter: Box<Expr>,
        body: Box<Expr>,
    },
    Defer {
        value: Box<Expr>,
    },
    Using {
        binding: UsingBinding,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Break,
    Continue,
    Return {
        value: Option<Box<Expr>>,
    },
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct InitInfo {
    pub is_init: bool,
    pub promotes_full: bool,
}

// -- Expressions ---

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Block {
        items: Vec<BlockItem>,
        tail: Option<Box<Expr>>,
    },

    // Literals (scalar)
    UnitLit,
    NoneLit,
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
    SetLit {
        elem_ty: Option<TypeExpr>,
        elems: Vec<Expr>,
    },
    MapLit {
        key_ty: Option<TypeExpr>,
        value_ty: Option<TypeExpr>,
        entries: Vec<MapLitEntry>,
    },
    TupleLit(Vec<Expr>),
    StructLit {
        name: String,
        type_args: Vec<TypeExpr>,
        fields: Vec<StructLitField>,
    },
    EnumVariant {
        enum_name: String,
        type_args: Vec<TypeExpr>,
        variant: String,
        payload: Vec<Expr>,
    },

    // Struct update
    StructUpdate {
        target: Box<Expr>,
        fields: Vec<StructUpdateField>,
    },

    // Operators
    BinOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Try {
        fallible_expr: Box<Expr>,
        on_error: Option<Box<Expr>>,
    },

    // Heap allocation
    HeapAlloc {
        expr: Box<Expr>,
    },

    // Move
    Move {
        expr: Box<Expr>,
    },

    // Var, array index, tuple field, struct field
    Var {
        ident: String,
    },
    RoleProjection {
        type_name: String,
        role_name: String,
    },
    ArrayIndex {
        target: Box<Expr>,
        indices: Vec<Expr>,
    },
    TupleField {
        target: Box<Expr>,
        index: usize,
    },
    StructField {
        target: Box<Expr>,
        field: String,
    },

    // Control flow
    If {
        cond: Box<Expr>,
        then_body: Box<Expr>,
        else_body: Box<Expr>,
    },

    // Range
    Range {
        start: Box<Expr>,
        end: Box<Expr>, // exclusive
    },

    // Slice
    Slice {
        target: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    },

    // Match
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    // Function/Method call
    Call {
        callee: Box<Expr>,
        args: Vec<CallArg>,
    },
    MethodCall {
        callee: Box<Expr>,
        method_name: String,
        args: Vec<CallArg>,
    },
    Emit {
        kind: EmitKind,
    },
    Reply {
        cap: Box<Expr>,
        value: Box<Expr>,
    },

    Closure {
        ident: String,
        captures: Vec<CaptureSpec>,
        params: Vec<Param>,
        return_ty: TypeExpr,
        body: Box<Expr>,
    },

    // Semantic-AST only: elaboration inserts these, parser never emits them.
    Coerce {
        kind: CoerceKind,
        expr: Box<Expr>,
    },
    ImplicitMove {
        expr: Box<Expr>,
    },
    // Internal-only: elaboration inserts these, parser never emits them.
    AddrOf {
        expr: Box<Expr>,
    },
    Deref {
        expr: Box<Expr>,
    },
    Load {
        expr: Box<Expr>,
    },
    MapGet {
        target: Box<Expr>,
        key: Box<Expr>,
    },
    Len {
        expr: Box<Expr>,
    },
    ClosureRef {
        ident: String,
    },
}

#[derive(Clone, Debug)]
pub enum EmitKind {
    Send {
        to: Box<Expr>,
        payload: Box<Expr>,
    },
    Request {
        to: Box<Expr>,
        payload: Box<Expr>,
        /// Optional site label from `request:label(...)` sugar.
        request_site_label: Option<String>,
        /// Stable request-site identity used by runtime correlation plumbing.
        /// Populated during elaboration; `None` before that.
        request_site_key: Option<u64>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CoerceKind {
    ArrayToSlice,
    ArrayToDynArray,
    DynArrayToSlice,
}

// -- Array literals ---

#[derive(Clone, Debug)]
pub enum ArrayLitInit {
    Elems(Vec<Expr>),
    Repeat(Box<Expr>, u64),
}

impl ArrayLitInit {
    pub fn length(&self) -> usize {
        match self {
            ArrayLitInit::Elems(elems) => elems.len(),
            ArrayLitInit::Repeat(_, count) => *count as usize,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructLitField {
    pub id: NodeId,
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct MapLitEntry {
    pub id: NodeId,
    pub key: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructUpdateField {
    pub id: NodeId,
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

// -- Parameter / call modes ---

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
