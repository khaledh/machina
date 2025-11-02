#[derive(Clone, Debug)]
pub struct Module {
    pub funcs: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Unit,
    UInt32,
    Bool,
}

#[derive(Clone, Debug)]
pub enum Expr {
    UInt32Lit(u32),
    BoolLit(bool),
    UnitLit,
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Block(Vec<Expr>),
    Let {
        // immutable binding
        name: String,
        value: Box<Expr>,
    },
    Var {
        // mutable binding
        name: String,
        value: Box<Expr>,
    },
    Assign {
        name: String,
        value: Box<Expr>,
    },
    VarRef(String),
    If {
        cond: Box<Expr>,
        then_body: Box<Expr>,
        else_body: Box<Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
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

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Neg,
}
