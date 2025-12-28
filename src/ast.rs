use crate::diagnostics::Span;
use std::fmt;

// -- Nodes ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct NodeIdGen {
    next_id: u32,
}

impl NodeIdGen {
    pub fn new() -> Self {
        Self { next_id: 1 } // NodeId 0 is reserved
    }

    pub fn new_id(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }
}

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
}

// -- Declarations ---

#[derive(Clone, Debug)]
pub enum Decl {
    TypeDecl(TypeDecl),
    Function(Function),         // function definition
    FunctionDecl(FunctionDecl), // function declaration
}

// -- Type Declarations ---

#[derive(Clone, Debug)]
pub struct TypeDecl {
    pub id: NodeId,
    pub name: String,
    pub kind: TypeDeclKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeDeclKind {
    Alias { aliased_ty: TypeExpr },
    Struct { fields: Vec<StructField> },
    Enum { variants: Vec<EnumVariant> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub id: NodeId,
    pub name: String,
    pub payload: Vec<TypeExpr>,
    pub span: Span,
}

// -- Type Expressions ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeExpr {
    pub id: NodeId,
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExprKind {
    Named(String),
    Array {
        elem_ty: Box<TypeExpr>,
        dims: Vec<usize>,
    },
    Tuple {
        fields: Vec<TypeExpr>,
    },
    Range {
        min: u64,
        max: u64,
    },
    Slice {
        elem_ty: Box<TypeExpr>,
    },
}

// -- Struct Literals ---

#[derive(Clone, Debug)]
pub struct StructLitField {
    pub id: NodeId,
    pub name: String,
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

// -- String Literals ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringTag {
    Ascii,
    Utf8,
}

// -- Functions ---

#[derive(Clone, Debug)]
pub struct FunctionSig {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub id: NodeId,
    pub sig: FunctionSig,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub sig: FunctionSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub id: NodeId,
    pub name: String,
    pub typ: TypeExpr,
    pub span: Span,
}

// -- Patterns ---

#[derive(Clone, Debug)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Ident {
        name: String,
    },
    Array {
        patterns: Vec<Pattern>,
    },
    Tuple {
        patterns: Vec<Pattern>,
    },
    Struct {
        name: String,
        fields: Vec<StructPatternField>,
    },
}

#[derive(Clone, Debug)]
pub struct StructPatternField {
    pub name: String,
    pub pattern: Pattern,
    pub span: Span,
}

// -- Match patterns ---

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub id: NodeId,
    pub pattern: MatchPattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MatchPattern {
    Wildcard {
        span: Span,
    },
    EnumVariant {
        enum_name: Option<String>,
        variant_name: String,
        bindings: Vec<MatchPatternBinding>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct MatchPatternBinding {
    pub id: NodeId,
    pub name: String,
    pub span: Span,
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
        pattern: Pattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },
    VarBind {
        pattern: Pattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },
    Assign {
        assignee: Box<Expr>,
        value: Box<Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    For {
        pattern: Pattern,
        iter: Box<Expr>,
        body: Box<Expr>,
    },
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
    UInt64Lit(u64),
    BoolLit(bool),
    CharLit(char),
    StringLit {
        value: String,
        tag: StringTag,
    },
    UnitLit,

    // Literals (compound)
    ArrayLit(Vec<Expr>),
    TupleLit(Vec<Expr>),
    StructLit {
        name: String,
        fields: Vec<StructLitField>,
    },
    EnumVariant {
        enum_name: String,
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

    // Var, array index, tuple field, struct field
    Var(String),
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
        start: u64,
        end: u64, // exclusive
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

    // Function call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
}

// -- Operators ---

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,

    // Comparison operators
    Eq,
    Ne,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}

// -- Display ---

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, decl) in self.decls.iter().enumerate() {
            match decl {
                Decl::TypeDecl(type_decl) => type_decl.fmt_with_indent(f, 0)?,
                Decl::Function(func) => func.fmt_with_indent(f, 0)?,
                Decl::FunctionDecl(func_decl) => func_decl.fmt_with_indent(f, 0)?,
            }
            if i + 1 != self.decls.len() {
                writeln!(f, "--------------------------------")?;
            }
        }
        Ok(())
    }
}

impl TypeDecl {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}TypeDecl [{}]", pad, self.id)?;
        let pad1 = indent(level + 1);
        writeln!(f, "{}Name: {}", pad1, self.name)?;
        writeln!(f, "{}Kind:", pad1)?;
        self.kind.fmt_with_indent(f, level + 2)?;
        Ok(())
    }
}

impl TypeDeclKind {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match self {
            TypeDeclKind::Alias { aliased_ty } => {
                writeln!(f, "{}Alias: {} [{}]", pad, aliased_ty, aliased_ty.id)?;
            }
            TypeDeclKind::Struct { fields } => {
                writeln!(f, "{}Struct:", pad)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 2)?;
                }
            }
            TypeDeclKind::Enum { variants } => {
                writeln!(f, "{}Enum:", pad)?;
                for variant in variants {
                    if variant.payload.is_empty() {
                        writeln!(f, "{}- {} [{}]", pad, variant.name, variant.id)?;
                    } else {
                        let payload_str = variant
                            .payload
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<_>>();
                        writeln!(
                            f,
                            "{}- {}({}) [{}]",
                            pad,
                            variant.name,
                            payload_str.join(", "),
                            variant.id
                        )?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl StructField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: {} [{}]", pad1, self.name, self.ty, self.id)?;
        Ok(())
    }
}

impl StructLitField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: {} [{}]", pad1, self.name, self.value, self.id)?;
        Ok(())
    }
}

impl StructUpdateField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: {} [{}]", pad1, self.name, self.value, self.id)?;
        Ok(())
    }
}

impl FunctionSig {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}Name: {}", pad, self.name)?;
        writeln!(f, "{}Return Type: {}", pad, self.return_type)?;
        writeln!(f, "{}Params:", pad)?;
        for param in &self.params {
            writeln!(f, "{}{}", indent(level + 2), param)?;
        }
        Ok(())
    }
}

impl FunctionDecl {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}FunctionDecl [{}]", pad, self.id)?;
        self.sig.fmt_with_indent(f, level + 1)?;
        Ok(())
    }
}

impl Function {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}Function [{}]", pad, self.id)?;
        self.sig.fmt_with_indent(f, level + 1)?;
        self.body.fmt_with_indent(f, level + 1)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl fmt::Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {} [{}]", self.name, self.typ, self.id)?;
        Ok(())
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeExpr::{} [{}]", self.kind, self.id)?;
        Ok(())
    }
}

impl fmt::Display for TypeExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExprKind::Named(name) => {
                write!(f, "Named({})", name)?;
            }
            TypeExprKind::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "Array({}, dims=[{}])", elem_ty, dims_str.join(", "))?;
            }
            TypeExprKind::Tuple { fields } => {
                let fields_str = fields.iter().map(|f| f.to_string()).collect::<Vec<_>>();
                write!(f, "Tuple([{}])", fields_str.join(", "))?;
            }
            TypeExprKind::Range { min, max } => {
                write!(f, "Range({}, {})", min, max)?;
            }
            TypeExprKind::Slice { elem_ty } => {
                write!(f, "Slice({})", elem_ty)?;
            }
        }
        Ok(())
    }
}

impl Pattern {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match &self.kind {
            PatternKind::Ident { name } => {
                writeln!(f, "{}Ident({})", pad, name)?;
            }
            PatternKind::Array { patterns } => {
                writeln!(f, "{}Pattern::Array", pad)?;
                for pattern in patterns {
                    pattern.fmt_with_indent(f, level + 1)?;
                }
            }
            PatternKind::Tuple { patterns } => {
                writeln!(f, "{}Pattern::Tuple", pad)?;
                for pattern in patterns {
                    pattern.fmt_with_indent(f, level + 1)?;
                }
            }
            PatternKind::Struct { name, fields } => {
                writeln!(f, "{}Struct({})", pad, name)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 1)?;
                }
            }
        }
        Ok(())
    }
}

impl StructPatternField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}:", pad1, self.name)?;
        self.pattern.fmt_with_indent(f, level + 2)?;
        Ok(())
    }
}

impl MatchArm {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}MatchArm [{}]", pad, self.id)?;
        self.pattern.fmt_with_indent(f, level + 1)?;
        writeln!(f, "{}Body:", pad)?;
        self.body.fmt_with_indent(f, level + 1)?;
        Ok(())
    }
}

impl MatchPattern {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match self {
            MatchPattern::Wildcard { .. } => {
                writeln!(f, "{}Wildcard", pad)?;
            }
            MatchPattern::EnumVariant {
                enum_name,
                variant_name,
                bindings,
                ..
            } => {
                writeln!(f, "{}EnumVariant", pad)?;
                let pad1 = indent(level + 1);
                if let Some(enum_name) = enum_name {
                    writeln!(f, "{}Enum Name: {}", pad1, enum_name)?;
                }
                writeln!(f, "{}Variant Name: {}", pad1, variant_name)?;
                writeln!(f, "{}Bindings:", pad1)?;
                for binding in bindings {
                    binding.fmt_with_indent(f, level + 2)?;
                }
            }
        }
        Ok(())
    }
}

impl MatchPatternBinding {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: [{}]", pad1, self.name, self.id)?;
        Ok(())
    }
}

impl BlockItem {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        match self {
            BlockItem::Stmt(stmt) => stmt.fmt_with_indent(f, level),
            BlockItem::Expr(expr) => expr.fmt_with_indent(f, level),
        }
    }
}

impl StmtExpr {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match &self.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Let [{}]", pad, self.id)?;
                pattern.fmt_with_indent(f, level + 2)?;
                if let Some(decl_ty) = decl_ty {
                    writeln!(f, "{}Decl Type: {}", pad1, decl_ty)?;
                }
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Var [{}]", pad, self.id)?;
                writeln!(f, "{}Pattern:", pad1)?;
                pattern.fmt_with_indent(f, level + 2)?;
                if let Some(decl_ty) = decl_ty {
                    writeln!(f, "{}Decl Type: {}", pad1, decl_ty)?;
                }
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::Assign { assignee, value } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Assign [{}]", pad, self.id)?;
                writeln!(f, "{}Assignee:", pad1)?;
                assignee.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::While { cond, body } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}While [{}]", pad, self.id)?;
                writeln!(f, "{}Cond:", pad1)?;
                cond.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Body:", pad1)?;
                body.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}For [{}]", pad, self.id)?;
                writeln!(f, "{}Pattern:", pad1)?;
                pattern.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Iter:", pad1)?;
                iter.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Body:", pad1)?;
                body.fmt_with_indent(f, level + 2)?;
            }
        }
        Ok(())
    }
}

impl Expr {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match &self.kind {
            ExprKind::UInt64Lit(value) => {
                writeln!(f, "{}UInt64Lit({}) [{}]", pad, value, self.id)?;
            }
            ExprKind::BoolLit(value) => {
                writeln!(f, "{}BoolLit({}) [{}]", pad, value, self.id)?;
            }
            ExprKind::CharLit(value) => {
                writeln!(f, "{}CharLit({}) [{}]", pad, fmt_char(*value), self.id)?;
            }
            ExprKind::StringLit { value, tag } => {
                let tag_str = match tag {
                    StringTag::Ascii => "ascii",
                    StringTag::Utf8 => "utf8",
                };
                writeln!(
                    f,
                    "{}StringLit(\"{}\", {}) [{}]",
                    pad, value, tag_str, self.id
                )?;
            }
            ExprKind::UnitLit => {
                writeln!(f, "{}UnitLit [{}]", pad, self.id)?;
            }
            ExprKind::ArrayLit(elems) => {
                writeln!(f, "{}ArrayLit [{}]", pad, self.id)?;
                for elem in elems {
                    elem.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::ArrayIndex { target, indices } => {
                writeln!(f, "{}ArrayIndex [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Indices:", pad1)?;
                for index in indices {
                    index.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::TupleLit(elems) => {
                writeln!(f, "{}TupleLit [{}]", pad, self.id)?;
                for elem in elems {
                    elem.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::TupleField { target, index } => {
                writeln!(f, "{}TupleField [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}ArrayIndex: {}", pad1, index)?;
            }
            ExprKind::StructLit { name, fields } => {
                writeln!(f, "{}StructLit [{}]", pad, self.id)?;
                writeln!(f, "{}Name: {}", pad, name)?;
                writeln!(f, "{}Fields:", pad)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::StructField { target, field } => {
                writeln!(f, "{}StructField [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Field: {}", pad1, field)?;
            }
            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                writeln!(f, "{}EnumVariant [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Type: {}", pad1, enum_name)?;
                writeln!(f, "{}Variant: {}", pad1, variant)?;
                if !payload.is_empty() {
                    writeln!(f, "{}Payload:", pad1)?;
                    for expr in payload {
                        expr.fmt_with_indent(f, level + 2)?;
                    }
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}StructUpdate [{}]", pad, self.id)?;
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Fields:", pad1)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::BinOp { left, op, right } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}BinOp [{}]", pad, self.id)?;
                writeln!(f, "{}Op: {}", pad1, op)?;
                writeln!(f, "{}Left:", pad1)?;
                left.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Right:", pad1)?;
                right.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::UnaryOp { op, expr } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}UnaryOp [{}]", pad, self.id)?;
                writeln!(f, "{}Op: {}", pad1, op)?;
                writeln!(f, "{}Operand:", pad1)?;
                expr.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::Block { items, tail } => {
                writeln!(f, "{}Block [{}]", pad, self.id)?;
                for item in items {
                    item.fmt_with_indent(f, level + 1)?;
                }
                if let Some(tail) = tail {
                    tail.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::Var(name) => {
                writeln!(f, "{}Var({}) [{}]", pad, name, self.id)?;
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}If [{}]", pad, self.id)?;
                writeln!(f, "{}Cond:", pad1)?;
                cond.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Then:", pad1)?;
                then_body.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Else:", pad1)?;
                else_body.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::Call { callee: name, args } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Call: [{}]", pad, self.id)?;
                name.fmt_with_indent(f, level + 1)?;
                writeln!(f, "{}Args:", pad1)?;
                for arg in args {
                    arg.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Match [{}]", pad, self.id)?;
                writeln!(f, "{}Scrutinee:", pad1)?;
                scrutinee.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Arms:", pad1)?;
                for arm in arms {
                    arm.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::Range { start, end } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Range [{}]", pad, self.id)?;
                writeln!(f, "{}Start: {}", pad1, start)?;
                writeln!(f, "{}End: {}", pad1, end)?;
            }
            ExprKind::Slice { target, start, end } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Slice [{}]", pad, self.id)?;
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                if let Some(start) = start {
                    writeln!(f, "{}Start:", pad1)?;
                    start.fmt_with_indent(f, level + 2)?;
                } else {
                    writeln!(f, "{}Start: None", pad1)?;
                }
                if let Some(end) = end {
                    writeln!(f, "{}End:", pad1)?;
                    end.fmt_with_indent(f, level + 2)?;
                } else {
                    writeln!(f, "{}End: None", pad1)?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+")?,
            BinaryOp::Sub => write!(f, "-")?,
            BinaryOp::Mul => write!(f, "*")?,
            BinaryOp::Div => write!(f, "/")?,
            BinaryOp::Eq => write!(f, "==")?,
            BinaryOp::Ne => write!(f, "!=")?,
            BinaryOp::Lt => write!(f, "<")?,
            BinaryOp::Gt => write!(f, ">")?,
            BinaryOp::LtEq => write!(f, "<=")?,
            BinaryOp::GtEq => write!(f, ">=")?,
        }
        Ok(())
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-")?,
        }
        Ok(())
    }
}

fn indent(level: usize) -> String {
    "  ".repeat(level)
}

fn fmt_char(value: char) -> String {
    let mut out = String::from("'");
    for ch in value.escape_default() {
        out.push(ch);
    }
    out.push('\'');
    out
}
