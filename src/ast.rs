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
}
