//! Lower SSA MemCopy/MemSet instructions into runtime calls.

use crate::ssa::model::ir::{Callee, InstKind, RuntimeFn};
use crate::ssa::opt::Pass;

/// Lowers MemCopy/MemSet into call instructions so call clobbers are modeled.
pub struct MemOps;

impl Pass for MemOps {
    fn name(&self) -> &'static str {
        "memops"
    }

    fn run(&mut self, func: &mut crate::ssa::model::ir::Function) -> bool {
        let mut changed = false;

        for block in &mut func.blocks {
            for inst in &mut block.insts {
                match &inst.kind {
                    InstKind::MemCopy { dst, src, len } => {
                        inst.kind = InstKind::Call {
                            callee: Callee::Runtime(RuntimeFn::MemCopy),
                            args: vec![*dst, *src, *len],
                        };
                        inst.result = None;
                        changed = true;
                    }
                    InstKind::MemSet { dst, byte, len } => {
                        inst.kind = InstKind::Call {
                            callee: Callee::Runtime(RuntimeFn::MemSet),
                            // Runtime ABI is (ptr, len, value).
                            args: vec![*dst, *len, *byte],
                        };
                        inst.result = None;
                        changed = true;
                    }
                    _ => {}
                }
            }
        }

        changed
    }
}
