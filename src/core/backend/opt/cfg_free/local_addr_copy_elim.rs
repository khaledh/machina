//! Eliminates redundant local copies used only for calls and drops.

use std::collections::{HashMap, HashSet};

use crate::core::backend::opt::Pass;
use crate::core::ir::{
    Callee, Function, InstKind, ValueId, for_each_inst_use, replace_value_in_func,
};

type DefUseMaps = (
    HashMap<ValueId, (usize, usize)>,
    HashMap<ValueId, Vec<(usize, usize)>>,
);

/// Rewrites `store (load src) -> tmp; call/drop(tmp)` into `call/drop(src)`.
pub struct LocalAddrCopyElim;

impl Pass for LocalAddrCopyElim {
    fn name(&self) -> &'static str {
        "local-addr-copy-elim"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let trace = std::env::var("MACHINA_TRACE_OPT").ok().as_deref() == Some("1");
        let trace_func = trace && func.name == "main";
        let (def_inst, uses) = build_maps(func);
        let mut candidates = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::Store { ptr: dst, value } = &inst.kind else {
                    continue;
                };

                let Some((dst_def_block, dst_def_idx)) = def_inst.get(dst) else {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: dst has no def (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                };
                if *dst_def_block != block_idx {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: dst def in bb{} (bb{}, inst{})",
                            dst, func.name, dst_def_block, block_idx, inst_idx
                        );
                    }
                    continue;
                }
                let dst_def = &func.blocks[*dst_def_block].insts[*dst_def_idx];
                if !matches!(dst_def.kind, InstKind::AddrOfLocal { .. }) {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: dst not addr_of_local (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                }

                let Some((val_def_block, val_def_idx)) = def_inst.get(value) else {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: value has no def (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                };
                if *val_def_block != block_idx {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: value def in bb{} (bb{}, inst{})",
                            dst, func.name, val_def_block, block_idx, inst_idx
                        );
                    }
                    continue;
                }
                let val_def = &func.blocks[*val_def_block].insts[*val_def_idx];
                let InstKind::Load { ptr: src } = val_def.kind else {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: value not load (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                };

                let Some((src_def_block, src_def_idx)) = def_inst.get(&src) else {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: src has no def (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                };
                if *src_def_block != block_idx {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: src def in bb{} (bb{}, inst{})",
                            dst, func.name, src_def_block, block_idx, inst_idx
                        );
                    }
                    continue;
                }
                let src_def = &func.blocks[*src_def_block].insts[*src_def_idx];
                if !matches!(src_def.kind, InstKind::AddrOfLocal { .. }) {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: src not addr_of_local (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                }

                let Some(users) = uses.get(dst) else {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: dst has no users (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                };

                let mut last_drop: Option<usize> = None;
                let mut last_call: Option<usize> = None;
                let mut valid = true;
                for (use_block, use_idx) in users {
                    if *use_block != block_idx || *use_idx < inst_idx {
                        valid = false;
                        break;
                    }
                    if *use_idx == inst_idx {
                        continue;
                    }

                    let use_inst = &func.blocks[*use_block].insts[*use_idx];
                    match &use_inst.kind {
                        InstKind::Call { callee, args } => {
                            if matches!(callee, Callee::Value(value) if *value == *dst) {
                                valid = false;
                                break;
                            }
                            if !args.contains(dst) {
                                valid = false;
                                break;
                            }
                            last_call =
                                Some(last_call.map_or(*use_idx, |prev: usize| prev.max(*use_idx)));
                        }
                        InstKind::Drop { ptr } if ptr == dst => {
                            last_drop =
                                Some(last_drop.map_or(*use_idx, |prev: usize| prev.max(*use_idx)));
                        }
                        _ => {
                            valid = false;
                            break;
                        }
                    }
                }

                let last_use_idx = match (last_call, last_drop) {
                    (Some(call), Some(drop)) => Some(call.max(drop)),
                    (Some(call), None) => Some(call),
                    (None, Some(drop)) => Some(drop),
                    (None, None) => None,
                };

                let Some(last_use_idx) = last_use_idx else {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: no call/drop use (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                };

                if !valid {
                    if trace_func {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: invalid uses (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                }

                if !src_unmodified_between(src, block_idx, *val_def_idx, last_use_idx, func, &uses)
                {
                    if trace {
                        eprintln!(
                            "[local-addr-copy-elim] skip {:?} in {}: src modified between load and use (bb{}, inst{})",
                            dst, func.name, block_idx, inst_idx
                        );
                    }
                    continue;
                }

                if trace {
                    eprintln!(
                        "[local-addr-copy-elim] candidate {:?} -> {:?} in {} (bb{}, inst{})",
                        dst, src, func.name, block_idx, inst_idx
                    );
                }
                candidates.push((block_idx, inst_idx, *dst, src, last_use_idx));
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let mut remove = HashSet::new();
        let replacement_map: HashMap<ValueId, ValueId> = candidates
            .iter()
            .map(|(_, _, dst, src, _)| (*dst, *src))
            .collect();
        for (block_idx, inst_idx, dst, src, _) in &candidates {
            let resolved_src = resolve_replacement_target(*src, &replacement_map);
            replace_value_in_func(func, *dst, resolved_src, Some((*block_idx, *inst_idx)));
            remove.insert((*block_idx, *inst_idx));
        }

        for (block_idx, block) in func.blocks.iter_mut().enumerate() {
            if !remove.iter().any(|(b, _)| *b == block_idx) {
                continue;
            }
            let mut new_insts = Vec::with_capacity(block.insts.len());
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                if !remove.contains(&(block_idx, inst_idx)) {
                    new_insts.push(inst.clone());
                }
            }
            block.insts = new_insts;
        }

        true
    }
}

fn resolve_replacement_target(
    start: ValueId,
    replacement_map: &HashMap<ValueId, ValueId>,
) -> ValueId {
    let mut curr = start;
    let mut seen = HashSet::new();
    while let Some(next) = replacement_map.get(&curr).copied() {
        if !seen.insert(curr) {
            break;
        }
        curr = next;
    }
    curr
}

fn build_maps(func: &Function) -> DefUseMaps {
    let mut def_inst = HashMap::new();
    let mut uses: HashMap<ValueId, Vec<(usize, usize)>> = HashMap::new();

    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            if let Some(result) = &inst.result {
                def_inst.insert(result.id, (block_idx, inst_idx));
            }

            for_each_inst_use(&inst.kind, |value| {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            });
        }
    }

    (def_inst, uses)
}

fn src_unmodified_between(
    src: ValueId,
    block_idx: usize,
    start_idx: usize,
    end_idx: usize,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
) -> bool {
    src_unmodified_between_inner(
        src,
        block_idx,
        start_idx,
        end_idx,
        func,
        uses,
        &mut HashSet::new(),
    )
}

fn src_unmodified_between_inner(
    value: ValueId,
    block_idx: usize,
    start_idx: usize,
    end_idx: usize,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
    visiting: &mut HashSet<ValueId>,
) -> bool {
    if !visiting.insert(value) {
        return true;
    }

    let Some(users) = uses.get(&value) else {
        return true;
    };

    for (use_block, use_idx) in users {
        if *use_block != block_idx {
            return false;
        }
        if *use_idx <= start_idx {
            continue;
        }
        if *use_idx > end_idx {
            return false;
        }

        let inst = &func.blocks[*use_block].insts[*use_idx];
        match &inst.kind {
            InstKind::Load { .. } => {}
            InstKind::FieldAddr { .. } | InstKind::IndexAddr { .. } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !src_unmodified_between_inner(
                    result.id, block_idx, start_idx, end_idx, func, uses, visiting,
                ) {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}
