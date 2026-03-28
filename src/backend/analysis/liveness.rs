//! SSA liveness analysis.

use std::collections::HashSet;

use crate::backend::analysis::cfg::Cfg;
use crate::core::analysis::dataflow::{DataflowGraph, solve_backward};
use crate::ir::{Block, Function, Terminator, ValueId, for_each_inst_use};

#[derive(Debug, Clone, Default)]
pub struct LiveSet {
    pub live_in: HashSet<ValueId>,
    pub live_out: HashSet<ValueId>,
}

pub type LiveMap = Vec<LiveSet>;

#[derive(Debug, Clone)]
struct UseDef {
    use_set: HashSet<ValueId>,
    def_set: HashSet<ValueId>,
}

impl UseDef {
    fn new() -> Self {
        Self {
            use_set: HashSet::new(),
            def_set: HashSet::new(),
        }
    }

    fn add_use(&mut self, value: ValueId) {
        if !self.def_set.contains(&value) {
            self.use_set.insert(value);
        }
    }

    fn add_def(&mut self, value: ValueId) {
        self.def_set.insert(value);
    }
}

/// Compute liveness for a single SSA function.
pub fn analyze(func: &Function) -> LiveMap {
    let cfg = Cfg::new(func);
    let mut use_defs = Vec::with_capacity(func.blocks.len());
    let mut edge_uses = Vec::with_capacity(func.blocks.len());
    for block in &func.blocks {
        use_defs.push(block_use_def(block));
        edge_uses.push(block_edge_uses(block));
    }

    let empty = HashSet::new();
    let result = solve_backward(
        &cfg,
        empty.clone(),
        empty,
        |states| {
            let mut out = HashSet::new();
            for state in states {
                out.extend(state.iter().cloned());
            }
            out
        },
        |block_id, out_state| {
            let idx = cfg.index(block_id);
            let mut out_state = out_state.clone();
            out_state.extend(edge_uses[idx].iter().cloned());

            let use_def = &use_defs[idx];
            let mut in_state: HashSet<_> =
                out_state.difference(&use_def.def_set).cloned().collect();
            in_state.extend(use_def.use_set.iter().cloned());
            in_state
        },
    );

    let mut out_map = result.out_map;
    for (idx, uses) in edge_uses.iter().enumerate() {
        out_map[idx].extend(uses.iter().cloned());
    }

    result
        .in_map
        .into_iter()
        .zip(out_map)
        .map(|(live_in, live_out)| LiveSet { live_in, live_out })
        .collect()
}

fn block_use_def(block: &Block) -> UseDef {
    let mut use_def = UseDef::new();

    // Block params are definitions at the start of the block.
    for param in &block.params {
        use_def.add_def(param.value.id);
    }

    for inst in &block.insts {
        for_each_inst_use(&inst.kind, |value| use_def.add_use(value));
        if let Some(result) = &inst.result {
            use_def.add_def(result.id);
        }
    }

    term_uses(&block.term, &mut use_def);

    use_def
}

fn block_edge_uses(block: &Block) -> HashSet<ValueId> {
    let mut uses = HashSet::new();
    match &block.term {
        Terminator::Br { args, .. } => {
            uses.extend(args.iter().cloned());
        }
        Terminator::CondBr {
            then_args,
            else_args,
            ..
        } => {
            uses.extend(then_args.iter().cloned());
            uses.extend(else_args.iter().cloned());
        }
        Terminator::Switch {
            cases,
            default_args,
            ..
        } => {
            for case in cases {
                uses.extend(case.args.iter().cloned());
            }
            uses.extend(default_args.iter().cloned());
        }
        Terminator::Return { .. } | Terminator::Unreachable => {}
    }
    uses
}

fn term_uses(term: &Terminator, use_def: &mut UseDef) {
    match term {
        Terminator::Br { args, .. } => {
            for arg in args {
                use_def.add_use(*arg);
            }
        }
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            use_def.add_use(*cond);
            for arg in then_args {
                use_def.add_use(*arg);
            }
            for arg in else_args {
                use_def.add_use(*arg);
            }
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            use_def.add_use(*value);
            for case in cases {
                for arg in &case.args {
                    use_def.add_use(*arg);
                }
            }
            for arg in default_args {
                use_def.add_use(*arg);
            }
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                use_def.add_use(*value);
            }
        }
        Terminator::Unreachable => {}
    }
}
