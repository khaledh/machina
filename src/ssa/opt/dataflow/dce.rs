//! Dead code elimination for SSA functions.

use std::collections::HashSet;

use crate::ssa::analysis::liveness;
use crate::ssa::model::ir::{
    Function, InstKind, Instruction, Terminator, ValueId, for_each_inst_use,
};
use crate::ssa::opt::Pass;

pub struct DeadCodeElim;

impl Pass for DeadCodeElim {
    fn name(&self) -> &'static str {
        "ssa-dce"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let live_map = liveness::analyze(func);
        let mut changed = false;

        for (block_idx, block) in func.blocks.iter_mut().enumerate() {
            // Start with values that must be live at block exit (including edge args).
            let mut live = live_map[block_idx].live_out.clone();
            // Terminators are always executed; seed their uses before scanning insts.
            add_term_uses(&block.term, &mut live);

            let mut kept = Vec::with_capacity(block.insts.len());

            for inst in block.insts.iter().rev() {
                // Backward sweep: a def is dead if it doesn't reach any use and has no effects.
                if should_keep(inst, &live) {
                    update_live(inst, &mut live);
                    kept.push(inst.clone());
                } else {
                    changed = true;
                }
            }

            kept.reverse();
            block.insts = kept;
        }

        changed
    }
}

fn should_keep(inst: &Instruction, live: &HashSet<ValueId>) -> bool {
    // Conservatively keep side-effecting instructions even if their results are unused.
    if is_side_effecting(&inst.kind) {
        return true;
    }

    match &inst.result {
        Some(result) => live.contains(&result.id),
        None => true,
    }
}

fn update_live(inst: &Instruction, live: &mut HashSet<ValueId>) {
    if let Some(result) = &inst.result {
        live.remove(&result.id);
    }

    // Uses become live because they feed an instruction we've decided to keep.
    for_each_inst_use(&inst.kind, |value| {
        live.insert(value);
    });
}

fn is_side_effecting(kind: &InstKind) -> bool {
    matches!(
        kind,
        InstKind::Store { .. }
            | InstKind::MemCopy { .. }
            | InstKind::MemSet { .. }
            | InstKind::Call { .. }
            | InstKind::Drop { .. }
    )
}

fn add_term_uses(term: &Terminator, live: &mut HashSet<ValueId>) {
    match term {
        Terminator::Br { args, .. } => {
            live.extend(args.iter().cloned());
        }
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            live.insert(*cond);
            live.extend(then_args.iter().cloned());
            live.extend(else_args.iter().cloned());
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            live.insert(*value);
            for case in cases {
                live.extend(case.args.iter().cloned());
            }
            live.extend(default_args.iter().cloned());
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                live.insert(*value);
            }
        }
        Terminator::Unreachable => {}
    }
}
