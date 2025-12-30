use super::Pass;
use crate::mcir::{FuncBody, Operand, Rvalue, Statement};

/// Remove trivial self-copies like `dst = copy dst`.
pub struct RemoveSelfCopies;

impl Pass for RemoveSelfCopies {
    fn name(&self) -> &'static str {
        "remove-self-copies"
    }

    fn run(&mut self, body: &mut FuncBody) -> bool {
        let mut changed = false;
        for block in &mut body.blocks {
            let before = block.stmts.len();
            block.stmts.retain(|stmt| match stmt {
                Statement::CopyScalar { dst, src } => {
                    !matches!(src, Rvalue::Use(Operand::Copy(src_place)) if src_place == dst)
                }
                Statement::CopyAggregate { dst, src } => dst != src,
                _ => true,
            });
            if block.stmts.len() != before {
                changed = true;
            }
        }
        changed
    }
}
