//! Lower SSA MemCopy/MemSet instructions into runtime calls.

use std::collections::HashMap;

use crate::backend::opt::Pass;
use crate::ir::ir::{Callee, ConstValue, InstKind, RuntimeFn, ValueId};

/// Lowers MemCopy/MemSet into call instructions so call clobbers are modeled.
pub struct MemOps;

impl Pass for MemOps {
    fn name(&self) -> &'static str {
        "memops"
    }

    fn run(&mut self, func: &mut crate::ir::ir::Function) -> bool {
        let mut changed = false;

        for block in &mut func.blocks {
            let mut const_ints: HashMap<ValueId, i128> = HashMap::new();
            let mut new_insts = Vec::with_capacity(block.insts.len());

            for mut inst in block.insts.drain(..) {
                if let (Some(result), InstKind::Const { value }) =
                    (inst.result.as_ref(), &inst.kind)
                {
                    if let ConstValue::Int { value, .. } = value {
                        const_ints.insert(result.id, *value);
                    }
                }

                match inst.kind {
                    InstKind::MemCopy { dst, src, len } => {
                        let len_is_zero = const_ints.get(&len).copied() == Some(0);
                        if dst == src || len_is_zero {
                            changed = true;
                            continue;
                        }

                        inst.kind = InstKind::Call {
                            callee: Callee::Runtime(RuntimeFn::MemCopy),
                            args: vec![dst, src, len],
                        };
                        inst.result = None;
                        changed = true;
                    }
                    InstKind::MemSet { dst, byte, len } => {
                        let len_is_zero = const_ints.get(&len).copied() == Some(0);
                        if len_is_zero {
                            changed = true;
                            continue;
                        }

                        inst.kind = InstKind::Call {
                            callee: Callee::Runtime(RuntimeFn::MemSet),
                            // Runtime ABI is (ptr, len, value).
                            args: vec![dst, len, byte],
                        };
                        inst.result = None;
                        changed = true;
                    }
                    _ => {}
                }

                new_insts.push(inst);
            }

            block.insts = new_insts;
        }

        changed
    }
}
