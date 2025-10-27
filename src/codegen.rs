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
        asm.push_str(&self.gen_expr(&self.function.body, "w0"));
        asm.push_str("  ret\n");
        asm
    }

    fn gen_expr(&self, expr: &ast::Expr, reg: &str) -> String {
        match expr {
            ast::Expr::UInt32Lit(value) => self.gen_u32_imm(value, reg),
            ast::Expr::BinOp { left, op, right } => self.gen_binop(*op, left, right),
        }
    }

    fn gen_u32_imm(&self, value: &u32, reg: &str) -> String {
        format!("  mov {reg}, #{value}\n")
    }

    fn gen_binop(&self, op: ast::BinOp, left: &ast::Expr, right: &ast::Expr) -> String {
      let mut result = String::new();
      result.push_str(&self.gen_expr(left, "w0"));
      result.push_str(&self.gen_expr(right, "w1"));
      match op {
        ast::BinOp::Add => result.push_str("  add w0, w0, w1\n"),
        ast::BinOp::Sub => result.push_str("  sub w0, w0, w1\n"),
        ast::BinOp::Mul => result.push_str("  mul w0, w0, w1\n"),
        ast::BinOp::Div => result.push_str("  div w0, w0, w1\n"),
      }
      result
    }
}
