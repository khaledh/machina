use crate::ast;
use std::cell::Cell;
use std::collections::HashMap;
use thiserror::Error;

pub struct Codegen {
    funcs: Vec<ast::Function>,
    scopes: Vec<CodegenScope>,
    label_counter: Cell<u32>,
    max_stack_offset: Cell<u32>,
}

struct CodegenVar {
    #[allow(unused)]
    name: String,
    stack_offset: u32,
}

struct CodegenScope {
    vars: HashMap<String, CodegenVar>,
    next_offset: Cell<u32>,
}

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Variable not found: {0}")]
    VarNotFound(String),

    #[error("Too many parameters. Only 8 are supported, found: {0}")]
    TooManyParams(usize),

    #[error("Too many arguments. Only 8 are supported, found: {0}")]
    TooManyArgs(usize),

    #[error("No current scope. This is a bug.")]
    NoCurrentScope,
}

impl Codegen {
    pub fn new(module: &ast::Module) -> Self {
        Codegen {
            funcs: module.funcs.clone(),
            scopes: Vec::new(),
            label_counter: Cell::new(0),
            max_stack_offset: Cell::new(0),
        }
    }
}

impl Codegen {
    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();
        asm.push_str(".align 2\n");

        for func in self.funcs.clone() {
            asm.push_str("\n");
            asm.push_str(&self.gen_func(&func)?);
        }
        Ok(asm)
    }

    fn gen_func(&mut self, func: &ast::Function) -> Result<String, CodegenError> {
        let mut asm = String::new();

        self.enter_scope();

        // Generate function parameters
        // ARM64 ABI: first 8 parameters in registers x0-x7, rest on stack
        // TODO: handle more than 8 parameters
        if func.params.len() > 8 {
            return Err(CodegenError::TooManyParams(func.params.len()));
        }
        let mut copy_params_asm = String::new();
        // Copy parameters to stack (TODO: optimize this later to use incoming arguments registers)
        for (i, param) in func.params.iter().enumerate() {
            let stack_offset = self.alloc_stack(8)?;
            copy_params_asm.push_str(&format!("  str x{i}, [sp, #{stack_offset}]\n"));
            self.insert_var(
                &param.name,
                CodegenVar {
                    name: param.name.clone(),
                    stack_offset,
                },
            )?;
        }

        // Generate function body first to get stack size
        let body = func.body.clone();
        let body_asm = self.gen_expr(&body, 0)?;

        // arm64 requires 16-byte stack alignment
        let stack_size = (self.max_stack_offset.get() + 15) & !15;

        // Function prologue
        asm.push_str(&format!(".global _{}\n", func.name));
        asm.push_str(&format!("_{}:\n", func.name));
        // save frame pointer and return address (TODO: omit this for leaf functions)
        asm.push_str("  stp x29, x30, [sp, #-16]!\n");
        if stack_size > 0 {
            asm.push_str(&format!("  sub sp, sp, #{stack_size}\n"));
        }
        if !copy_params_asm.is_empty() {
            asm.push_str(&copy_params_asm);
        }

        // Function body
        asm.push_str(&body_asm);

        // Function epilogue
        if stack_size > 0 {
            asm.push_str(&format!("  add sp, sp, #{stack_size}\n"));
        }
        // restore frame pointer and return address (TODO: omit this for leaf functions)
        asm.push_str("  ldp x29, x30, [sp], #16\n");
        asm.push_str("  ret\n");

        self.exit_scope();

        Ok(asm)
    }

    fn enter_scope(&mut self) {
        let next_offset = match self.scopes.last() {
            Some(scope) => scope.next_offset.clone(),
            None => Cell::new(0),
        };
        let scope = CodegenScope {
            vars: HashMap::new(),
            next_offset,
        };
        self.scopes.push(scope);
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn alloc_stack(&self, size: u32) -> Result<u32, CodegenError> {
        match self.scopes.last() {
            Some(scope) => {
                let offset = scope.next_offset.get();
                scope.next_offset.replace(offset + size);
                if offset + size > self.max_stack_offset.get() {
                    self.max_stack_offset.replace(offset + size);
                }
                Ok(offset)
            }
            None => Err(CodegenError::NoCurrentScope),
        }
    }

    fn insert_var(&mut self, name: &str, var: CodegenVar) -> Result<(), CodegenError> {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.vars.insert(name.to_string(), var);
                Ok(())
            }
            None => return Err(CodegenError::NoCurrentScope),
        }
    }

    fn lookup_var(&self, name: &str) -> Result<&CodegenVar, CodegenError> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.vars.get(name) {
                return Ok(var);
            }
        }
        Err(CodegenError::VarNotFound(name.to_string()))
    }

    fn next_label(&self) -> String {
        let curr = self.label_counter.get();
        self.label_counter.replace(curr + 1);
        format!(".L{}", curr)
    }

    fn gen_expr(&mut self, expr: &ast::Expr, reg: u8) -> Result<String, CodegenError> {
        match expr {
            ast::Expr::UInt32Lit(value) => Ok(self.gen_u32_imm(value, reg)),
            ast::Expr::BoolLit(value) => Ok(self.gen_bool_imm(value, reg)),
            ast::Expr::UnitLit => Ok(format!("  mov w{reg}, 0\n")),
            ast::Expr::BinOp { left, op, right } => self.gen_binary_op(*op, left, right, reg),
            ast::Expr::UnaryOp { op, expr } => self.gen_unary_op(*op, expr, reg),
            ast::Expr::Block(body) => {
                self.enter_scope();
                let result = self.gen_block(body, reg)?;
                self.exit_scope();
                Ok(result)
            }
            ast::Expr::If {
                cond,
                then_body,
                else_body,
            } => {
                let mut result = String::new();
                let else_label = self.next_label();
                let end_label = self.next_label();

                result.push_str(&self.gen_expr(cond, reg)?);
                result.push_str(&format!("  cmp w{reg}, 0\n"));
                result.push_str(&format!("  b.eq {else_label}\n"));
                result.push_str(&self.gen_expr(then_body, reg)?);
                result.push_str(&format!("  b {end_label}\n"));
                result.push_str(&format!("{else_label}:\n"));
                result.push_str(&self.gen_expr(else_body, reg)?);
                result.push_str(&format!("{end_label}:\n"));
                Ok(result)
            }
            ast::Expr::Let { name, value } => {
                // Evaluate initializer in the current scope (before shadowing)
                let mut result = String::new();
                result.push_str(&self.gen_expr(value, reg)?);
                // Now allocate and bind the new variable, then store the value
                let stack_offset = self.alloc_stack(8)?;
                self.insert_var(
                    name,
                    CodegenVar {
                        name: name.to_string(),
                        stack_offset,
                    },
                )?;
                result.push_str(&format!("  str w{reg}, [sp, #{stack_offset}]\n"));
                Ok(result)
            }
            ast::Expr::Var { name, value } => {
                // Evaluate initializer before introducing the new mutable binding
                let mut result = String::new();
                result.push_str(&self.gen_expr(value, reg)?);
                let stack_offset = self.alloc_stack(8)?;
                self.insert_var(
                    name,
                    CodegenVar {
                        name: name.to_string(),
                        stack_offset,
                    },
                )?;
                result.push_str(&format!("  str w{reg}, [sp, #{stack_offset}]\n"));
                Ok(result)
            }
            ast::Expr::VarRef(name) => {
                let stack_offset = self.lookup_var(name)?.stack_offset;
                Ok(format!("  ldr w{reg}, [sp, #{stack_offset}]\n"))
            }
            ast::Expr::Assign { name, value } => {
                let stack_offset = self.lookup_var(name)?.stack_offset;
                let mut result = String::new();
                result.push_str(&self.gen_expr(value, reg)?);
                result.push_str(&format!("  str w{reg}, [sp, #{stack_offset}]\n"));
                Ok(result)
            }
            ast::Expr::While { cond, body } => {
                let mut result = String::new();
                let loop_label = self.next_label();
                let end_label = self.next_label();

                result.push_str(&format!("{loop_label}:\n"));
                result.push_str(&self.gen_expr(cond, reg)?);
                result.push_str(&format!("  cmp w{reg}, 0\n"));
                result.push_str(&format!("  b.eq {end_label}\n"));
                result.push_str(&self.gen_expr(body, reg)?);
                result.push_str(&format!("  b {loop_label}\n"));
                result.push_str(&format!("{end_label}:\n"));
                Ok(result)
            }
            ast::Expr::Call { name, args } => {
                let mut result = String::new();
                // ARM64 ABI: first 8 arguments in registers x0-x7, rest on stack
                if args.len() > 8 {
                    return Err(CodegenError::TooManyArgs(args.len()));
                }
                for (i, arg) in args.iter().enumerate() {
                    result.push_str(&self.gen_expr(arg, i as u8)?);
                }
                result.push_str(&format!("  bl _{}\n", name));
                result.push_str(&format!("  mov w{reg}, w0\n"));
                Ok(result)
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
        &mut self,
        op: ast::BinOp,
        left: &ast::Expr,
        right: &ast::Expr,
        reg: u8,
    ) -> Result<String, CodegenError> {
        let lreg = reg;
        let rreg = reg + 1;
        let mut result = String::new();

        // Evaluate left, spill to stack to preserve across potential calls in right
        result.push_str(&self.gen_expr(left, lreg)?);
        let spill_offset = self.alloc_stack(8)?;
        result.push_str(&format!("  str w{lreg}, [sp, #{spill_offset}]\n"));

        // Evaluate right; this may involve calls that clobber w0
        result.push_str(&self.gen_expr(right, rreg)?);

        // Reload left value after right-hand evaluation
        result.push_str(&format!("  ldr w{lreg}, [sp, #{spill_offset}]\n"));

        match op {
            // Arithmetic operators
            ast::BinOp::Add => result.push_str(&format!("  add w{reg}, w{lreg}, w{rreg}\n")),
            ast::BinOp::Sub => result.push_str(&format!("  sub w{reg}, w{lreg}, w{rreg}\n")),
            ast::BinOp::Mul => result.push_str(&format!("  mul w{reg}, w{lreg}, w{rreg}\n")),
            ast::BinOp::Div => result.push_str(&format!("  udiv w{reg}, w{lreg}, w{rreg}\n")),

            // Comparison operators
            ast::BinOp::Eq => {
                result.push_str(&format!("  cmp w{lreg}, w{rreg}\n"));
                result.push_str(&format!("  cset w{reg}, eq\n"));
            }
            ast::BinOp::Ne => {
                result.push_str(&format!("  cmp w{lreg}, w{rreg}\n"));
                result.push_str(&format!("  cset w{reg}, ne\n"));
            }
            ast::BinOp::Lt => {
                result.push_str(&format!("  cmp w{lreg}, w{rreg}\n"));
                result.push_str(&format!("  cset w{reg}, lt\n"));
            }
            ast::BinOp::Gt => {
                result.push_str(&format!("  cmp w{lreg}, w{rreg}\n"));
                result.push_str(&format!("  cset w{reg}, gt\n"));
            }
            ast::BinOp::LtEq => {
                result.push_str(&format!("  cmp w{lreg}, w{rreg}\n"));
                result.push_str(&format!("  cset w{reg}, le\n"));
            }
            ast::BinOp::GtEq => {
                result.push_str(&format!("  cmp w{lreg}, w{rreg}\n"));
                result.push_str(&format!("  cset w{reg}, ge\n"));
            }
        }
        Ok(result)
    }

    fn gen_unary_op(
        &mut self,
        op: ast::UnaryOp,
        expr: &ast::Expr,
        reg: u8,
    ) -> Result<String, CodegenError> {
        let mut result = String::new();
        result.push_str(&self.gen_expr(expr, reg)?);
        match op {
            ast::UnaryOp::Neg => result.push_str(&format!("  neg w{reg}, w{reg}\n")),
        }
        Ok(result)
    }

    fn gen_block(&mut self, body: &Vec<ast::Expr>, reg: u8) -> Result<String, CodegenError> {
        let mut result = String::new();
        for expr in body {
            result.push_str(&self.gen_expr(expr, reg)?);
        }
        Ok(result)
    }
}
