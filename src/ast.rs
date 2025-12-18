use crate::diagnostics::Span;
use crate::ids::NodeId;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}

impl Module {
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
}

#[derive(Clone, Debug)]
pub enum Decl {
    TypeDecl(TypeDecl),
    Function(Function),
}

#[derive(Clone, Debug)]
pub struct TypeDecl {
    pub id: NodeId,
    pub name: String,
    pub kind: TypeDeclKind,
}

#[derive(Clone, Debug)]
pub enum TypeDeclKind {
    Alias { aliased_ty: TypeExpr },
    Struct { fields: Vec<StructField> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub id: NodeId,
    pub name: String,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructLitField {
    pub id: NodeId,
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub name: String,
    pub return_type: TypeExpr,
    pub params: Vec<FunctionParam>,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub id: NodeId,
    pub name: String,
    pub typ: TypeExpr,
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Ident { name: String },
    Array { patterns: Vec<Pattern> },
    Tuple { patterns: Vec<Pattern> },
}

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
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Block(Vec<Expr>),

    // Literals (scalar)
    UInt64Lit(u64),
    BoolLit(bool),
    UnitLit,

    // Literals (compound)
    ArrayLit(Vec<Expr>),
    TupleLit(Vec<Expr>),
    StructLit {
        name: String,
        fields: Vec<StructLitField>,
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

    // Bindings
    LetBind {
        // immutable binding
        pattern: Pattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },
    VarBind {
        // mutable binding
        pattern: Pattern,
        decl_ty: Option<TypeExpr>,
        value: Box<Expr>,
    },

    // Assignment
    Assign {
        assignee: Box<Expr>,
        value: Box<Expr>,
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
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },

    // Function call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
}

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

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, decl) in self.decls.iter().enumerate() {
            match decl {
                Decl::TypeDecl(type_decl) => type_decl.fmt_with_indent(f, 0)?,
                Decl::Function(func) => func.fmt_with_indent(f, 0)?,
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

impl Function {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        let pad1 = indent(level + 1);
        writeln!(f, "{}Function [{}]", pad, self.id)?;
        writeln!(f, "{}Name: {}", pad1, self.name)?;
        writeln!(f, "{}Return Type: {}", pad1, self.return_type)?;
        if !self.params.is_empty() {
            writeln!(f, "{}Params:", pad1)?;
            for param in &self.params {
                writeln!(f, "{}{}", indent(level + 2), param)?;
            }
        } else {
            writeln!(f, "{}Params: none", pad1)?;
        }
        self.body.fmt_with_indent(f, level + 1)?;
        Ok(())
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
            ExprKind::BinOp { left, op, right } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}BinOp [{}]", pad, self.id)?;
                writeln!(f, "{}Left:", pad1)?;
                left.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Op: {}", pad1, op)?;
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
            ExprKind::Block(body) => {
                writeln!(f, "{}Block [{}]", pad, self.id)?;
                for expr in body {
                    expr.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::LetBind {
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
            ExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Var [{}]", pad, self.id)?;
                pattern.fmt_with_indent(f, level + 2)?;
                if let Some(decl_ty) = decl_ty {
                    writeln!(f, "{}Decl Type: {}", pad1, decl_ty)?;
                }
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::Assign { assignee, value } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Assign [{}]", pad, self.id)?;
                writeln!(f, "{}Assignee:", pad1)?;
                assignee.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
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
            ExprKind::While { cond, body } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}While [{}]", pad, self.id)?;
                writeln!(f, "{}Cond:", pad1)?;
                cond.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Body:", pad1)?;
                body.fmt_with_indent(f, level + 2)?;
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
