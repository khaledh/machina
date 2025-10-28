use crate::ast;

pub struct Codegen {
    function: ast::Function,
}

impl Codegen {
    pub fn new(function: ast::Function) -> Self {
        Codegen { function }
    }
}

impl Codegen {
    pub fn generate(&self) -> String {
        let mut asm = String::new();
        asm.push_str(".global _main\n");
        asm.push_str(".align 2\n");
        asm.push_str("_main:\n");
        asm.push_str(&self.gen_expr(&self.function.body, 0));
        asm.push_str("  ret\n");
        asm
    }

    fn gen_expr(&self, expr: &ast::Expr, reg: u8) -> String {
        match expr {
            ast::Expr::UInt32Lit(value) => self.gen_u32_imm(value, reg),
            ast::Expr::BinOp { left, op, right } => self.gen_binary_op(*op, left, right, reg),
            ast::Expr::UnaryOp { op, expr } => self.gen_unary_op(*op, expr, reg),
            ast::Expr::Block(body) => self.gen_block(body, reg),
            ast::Expr::Let { .. } => "".to_string(),
            ast::Expr::VarRef(name) => "".to_string(),
        }
    }

    fn gen_u32_imm(&self, value: &u32, reg: u8) -> String {
        format!("  mov w{reg}, #{value}\n")
    }

    fn gen_binary_op(
        &self,
        op: ast::BinOp,
        left: &ast::Expr,
        right: &ast::Expr,
        reg: u8,
    ) -> String {
        let lreg = reg;
        let rreg = reg + 1;
        let mut result = String::new();
        result.push_str(&self.gen_expr(left, lreg));
        result.push_str(&self.gen_expr(right, rreg));
        match op {
            ast::BinOp::Add => result.push_str(&format!("  add w{reg}, w{lreg}, w{rreg}\n")),
            ast::BinOp::Sub => result.push_str(&format!("  sub w{reg}, w{lreg}, w{rreg}\n")),
            ast::BinOp::Mul => result.push_str(&format!("  mul w{reg}, w{lreg}, w{rreg}\n")),
            ast::BinOp::Div => result.push_str(&format!("  div w{reg}, w{lreg}, w{rreg}\n")),
        }
        result
    }

    fn gen_unary_op(&self, op: ast::UnaryOp, expr: &ast::Expr, reg: u8) -> String {
        let mut result = String::new();
        result.push_str(&self.gen_expr(expr, reg));
        match op {
            ast::UnaryOp::Neg => result.push_str(&format!("  neg w{reg}, w{reg}\n")),
        }
        result
    }

    fn gen_block(&self, body: &Vec<ast::Expr>, reg: u8) -> String {
        let mut result = String::new();
        for expr in body {
            result.push_str(&self.gen_expr(expr, reg));
        }
        result
    }
}
