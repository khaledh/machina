#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub body: Expr,
}

#[derive(Debug)]
pub enum Type {
    UInt32,
}

#[derive(Debug)]
pub enum Expr {
    UInt32Lit(u32),
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}
