use crate::ast;
use crate::semantic::Symbol;
use std::collections::HashMap;

pub struct Codegen {
    function: ast::Function,
    symbols: HashMap<String, Symbol>,
}

impl Codegen {
    pub fn new(function: ast::Function, symbols: HashMap<String, Symbol>) -> Self {
        Codegen { function, symbols }
    }
}

impl Codegen {
    pub fn generate(&self) -> String {
        let mut asm = String::new();

        let var_count = self.symbols.len();
        // arm64 requires 16-byte stack alignment
        let stack_size = (var_count * 8 + 15) & !15;

        asm.push_str(".align 2\n");

        // Function prologue
        asm.push_str(".global _main\n");
        asm.push_str("_main:\n");
        if stack_size > 0 {
            asm.push_str(&format!("  sub sp, sp, #{stack_size}\n"));
        }

        // Function body
        asm.push_str(&self.gen_expr(&self.function.body, 0));

        // Function epilogue
        if stack_size > 0 {
            asm.push_str(&format!("  add sp, sp, #{stack_size}\n"));
        }
        asm.push_str("  ret\n");
        asm
    }

    fn gen_expr(&self, expr: &ast::Expr, reg: u8) -> String {
        match expr {
            ast::Expr::UInt32Lit(value) => self.gen_u32_imm(value, reg),
            ast::Expr::BoolLit(value) => self.gen_bool_imm(value, reg),
            ast::Expr::BinOp { left, op, right } => self.gen_binary_op(*op, left, right, reg),
            ast::Expr::UnaryOp { op, expr } => self.gen_unary_op(*op, expr, reg),
            ast::Expr::Block(body) => self.gen_block(body, reg),
            ast::Expr::Let { name, value } => {
                if let Some(Symbol::Variable { stack_offset, .. }) = self.symbols.get(name) {
                    let mut result = String::new();
                    result.push_str(&self.gen_expr(value, reg));
                    result.push_str(&format!("  str w{reg}, [sp, #{stack_offset}]\n"));
                    result
                } else {
                    panic!("Variable not found: {name}");
                }
            }
            ast::Expr::VarRef(name) => {
                if let Some(Symbol::Variable { stack_offset, .. }) = self.symbols.get(name) {
                    format!("  ldr w{reg}, [sp, #{stack_offset}]\n")
                } else {
                    panic!("Variable not found: {name}");
                }
            }
        }
    }

    fn gen_u32_imm(&self, value: &u32, reg: u8) -> String {
        format!("  mov w{reg}, #{value}\n")
    }

    fn gen_bool_imm(&self, value: &bool, reg: u8) -> String {
        let value = if *value { 1 } else { 0 };
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
