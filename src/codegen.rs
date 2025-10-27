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
        asm.push_str("  mov w0, #42\n");
        asm.push_str("  ret\n");
        asm
    }
}
