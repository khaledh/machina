//! Shared helpers for pointer-tracking dataflow passes.

use std::collections::{HashMap, HashSet};

use crate::ssa::model::ir::{CastKind, Function, InstKind, ValueId};

pub(super) fn peel_ptr_cast(
    value: ValueId,
    func: &Function,
    def_inst: &HashMap<ValueId, (usize, usize)>,
) -> ValueId {
    let mut current = value;
    let mut seen = HashSet::new();
    while seen.insert(current) {
        let Some((block_idx, inst_idx)) = def_inst.get(&current) else {
            break;
        };
        let inst = &func.blocks[*block_idx].insts[*inst_idx];
        match inst.kind {
            InstKind::Cast {
                kind: CastKind::PtrToPtr,
                value,
                ..
            } => current = value,
            _ => break,
        }
    }
    current
}

pub(super) fn is_read_only_ptr(
    value: ValueId,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
    ignore: Option<(usize, usize)>,
    visiting: &mut HashSet<ValueId>,
) -> bool {
    if !visiting.insert(value) {
        return true;
    }

    let Some(users) = uses.get(&value) else {
        return true;
    };

    for (block_idx, inst_idx) in users {
        if Some((*block_idx, *inst_idx)) == ignore {
            continue;
        }

        let inst = &func.blocks[*block_idx].insts[*inst_idx];
        match &inst.kind {
            InstKind::Load { .. } => {}
            InstKind::FieldAddr { .. } | InstKind::IndexAddr { .. } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !is_read_only_ptr(result.id, func, uses, ignore, visiting) {
                    return false;
                }
            }
            InstKind::Cast {
                kind: CastKind::PtrToPtr,
                ..
            } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !is_read_only_ptr(result.id, func, uses, ignore, visiting) {
                    return false;
                }
            }
            InstKind::MemCopy { dst, src, .. } => {
                if dst == &value {
                    return false;
                }
                if src != &value {
                    return false;
                }
            }
            InstKind::MemSet { dst, .. } => {
                if dst == &value {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}

pub(super) fn source_stable_after(
    value: ValueId,
    block_idx: usize,
    inst_idx: usize,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
    ignore: Option<(usize, usize)>,
    visiting: &mut HashSet<ValueId>,
) -> bool {
    if !visiting.insert(value) {
        return true;
    }

    let Some(users) = uses.get(&value) else {
        return true;
    };

    for (use_block, use_idx) in users {
        if Some((*use_block, *use_idx)) == ignore {
            continue;
        }

        let inst = &func.blocks[*use_block].insts[*use_idx];
        match &inst.kind {
            InstKind::Load { .. } => {}
            InstKind::FieldAddr { .. } | InstKind::IndexAddr { .. } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !source_stable_after(
                    result.id, block_idx, inst_idx, func, uses, ignore, visiting,
                ) {
                    return false;
                }
            }
            InstKind::Cast {
                kind: CastKind::PtrToPtr,
                ..
            } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !source_stable_after(
                    result.id, block_idx, inst_idx, func, uses, ignore, visiting,
                ) {
                    return false;
                }
            }
            InstKind::Store { .. } | InstKind::Drop { .. } | InstKind::Call { .. } => {
                if *use_block != block_idx || *use_idx > inst_idx {
                    return false;
                }
            }
            InstKind::MemCopy { dst, src, .. } => {
                if dst == &value {
                    if *use_block != block_idx || *use_idx > inst_idx {
                        return false;
                    }
                } else if src != &value {
                    return false;
                }
            }
            InstKind::MemSet { dst, .. } => {
                if dst == &value && (*use_block != block_idx || *use_idx > inst_idx) {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}
