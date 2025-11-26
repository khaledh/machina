use std::collections::HashMap;

use crate::context::LoweredContext;
use crate::ir::types::{IrBlock, IrFunction, IrInst, IrTerminator};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum IrCodegenError {
    #[error("Placeholder error: {0}")]
    PlaceholderError(String),
}

pub struct FuncCodegen<'a> {
    func: &'a IrFunction,
    frame_layout: HashMap<IrAddrId, u32>,
    label_counter: u32,
}

impl<'a> FuncCodegen<'a> {
    pub fn new(func: &'a IrFunction) -> Self {
        Self {
            func,
            frame_layout: HashMap::new(),
            label_counter: 0,
        }
    }

    fn align_up(&self, value: u32, alignment: usize) -> u32 {
        (value + (alignment as u32 - 1)) & !(alignment as u32 - 1)
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn unique_label(&mut self, block: &IrBlock) -> String {
        let lbl = self.next_label();
        format!("{}_{}", lbl, block.name())
    }

    fn gen_func(&mut self) -> Result<String, IrCodegenError> {
        // Build frame layout: addr id -> stack offset (honouring alignment and size)
        let mut stack_offset = 0;
        for (i, addr) in self.func.addrs.iter().enumerate() {
            stack_offset = self.align_up(stack_offset, addr.align);
            self.frame_layout.insert(IrAddrId(i as u32), stack_offset); // addrs are indexed by id
            stack_offset += addr.size as u32;
        }
        // Arm64 requires 16-byte stack alignment
        let stack_size = self.align_up(stack_offset, 16);

        // Function symbol
        let mut asm = String::new();
        asm.push_str(&format!(".global _{}\n", self.func.name));
        asm.push_str(&format!("_{}:\n", self.func.name));

        // Function prologue
        // save frame pointer and return address (TODO: omit this for leaf functions)
        asm.push_str("  stp x29, x30, [sp, #-16]!\n");
        if stack_size > 0 {
            asm.push_str(&format!("  sub sp, sp, #{stack_size}\n"));
        }

        // Function body
        for block in self.func.blocks.values() {
            asm.push_str(&self.gen_block(block)?);
        }

        // Function epilogue
        if stack_size > 0 {
            asm.push_str(&format!("  add sp, sp, #{stack_size}\n"));
        }
        // restore frame pointer and return address (TODO: omit this for leaf functions)
        asm.push_str("  ldp x29, x30, [sp], #16\n");
        asm.push_str("  ret\n");

        Ok(asm)
    }

    fn gen_block(&mut self, block: &IrBlock) -> Result<String, IrCodegenError> {
        let mut asm = String::new();

        // Block label
        asm.push_str(&format!("{}:\n", self.unique_label(block)));

        // Block instructions
        for inst in block.insts().iter() {
            asm.push_str(&self.gen_inst(inst)?);
        }

        // Block terminator
        asm.push_str(&self.gen_terminator(block.term())?);

        Ok(asm)
    }

    fn gen_inst(&mut self, inst: &IrInst) -> Result<String, IrCodegenError> {
        let mut asm = String::new();
        match inst {
            _ => todo!("Codegen for instruction: {:?}", inst),
        }
        Ok(asm)
    }

    fn gen_terminator(&mut self, term: &IrTerminator) -> Result<String, IrCodegenError> {
        let mut asm = String::new();
        match term {
            IrTerminator::Ret { value } => {
                if let Some(value) = value {
                    // TODO: map temps to registers
                    asm.push_str(&format!("  mov w0, %t{}\n", value.id()));
                }
                asm.push_str("  ret\n");
            }
            IrTerminator::Br { target } => {
                asm.push_str(&format!(
                    "  b {}\n",
                    self.unique_label(&self.func.blocks[target])
                ));
            }
            IrTerminator::CondBr {
                cond,
                then_b,
                else_b,
            } => {
                // TODO: map temps to registers
                asm.push_str(&format!("  cmp w{}, 0\n", cond.id()));
                asm.push_str(&format!(
                    "  b.eq {}\n",
                    self.unique_label(&self.func.blocks[then_b])
                ));
                asm.push_str(&format!(
                    "  b {}\n",
                    self.unique_label(&self.func.blocks[else_b])
                ));
            }
            IrTerminator::_Unterminated => panic!("Unterminated block"),
        }

        Ok(asm)
    }
}

pub fn ir_codegen(context: LoweredContext) -> Result<String, IrCodegenError> {
    let mut func_asms = Vec::new();
    for func in &context.ir_funcs {
        let mut cg = FuncCodegen::new(func);
        let func_asm = cg.gen_func()?;
        func_asms.push(func_asm);
        func_asms.push("\n".to_string());
    }
    Ok(func_asms.join("\n"))
}
