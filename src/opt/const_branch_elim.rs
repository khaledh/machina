use crate::mcir::Const;
use crate::mcir::{FuncBody, Operand, Terminator};
use crate::opt::Pass;

/// Fold constant branch terminators into direct gotos.
pub struct ConstBranchElim;

impl Pass for ConstBranchElim {
    fn name(&self) -> &'static str {
        "const-branch-elim"
    }

    fn run(&mut self, body: &mut FuncBody) -> bool {
        let mut changed = false;
        for block in &mut body.blocks {
            let new_term = match &block.terminator {
                Terminator::If {
                    cond,
                    then_bb,
                    else_bb,
                } => match cond {
                    Operand::Const(Const::Bool(true)) => Some(Terminator::Goto(*then_bb)),
                    Operand::Const(Const::Bool(false)) => Some(Terminator::Goto(*else_bb)),
                    _ => None,
                },
                Terminator::Switch {
                    discr,
                    cases,
                    default,
                } => match discr {
                    Operand::Const(Const::Int { value, signed, .. }) if !signed => {
                        let tag = *value as u64;
                        let target = cases
                            .iter()
                            .find(|c| c.value == tag)
                            .map(|c| c.target)
                            .unwrap_or(*default);
                        Some(Terminator::Goto(target))
                    }
                    _ => None,
                },
                _ => None,
            };

            if let Some(new_term) = new_term {
                if block.terminator != new_term {
                    changed = true;
                }
                block.terminator = new_term;
            }
        }
        changed
    }
}
