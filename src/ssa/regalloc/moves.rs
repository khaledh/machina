//! Move planning for SSA register allocation.

use std::collections::{HashMap, HashSet};

use crate::regalloc::target::TargetSpec;
use crate::ssa::IrTypeCache;
use crate::ssa::IrTypeKind;
use crate::ssa::model::ir::{BlockId, Function, InstKind, Terminator, ValueId};

use super::{Location, ValueAllocMap};

/// A single move between two allocated locations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveOp {
    pub src: Location,
    pub dst: Location,
    pub size: u32,
}

/// Moves needed on a control-flow edge from `from` to `to`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeMove {
    pub from: BlockId,
    pub to: BlockId,
    pub moves: Vec<MoveOp>,
}

/// Moves needed around a call instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallMove {
    pub block: BlockId,
    pub inst_index: usize,
    pub pre_moves: Vec<MoveOp>,
    pub post_moves: Vec<MoveOp>,
}

/// Collection of planned moves for one function.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MovePlan {
    pub edge_moves: Vec<EdgeMove>,
    pub call_moves: Vec<CallMove>,
}

impl MovePlan {
    /// Orders parallel move lists and breaks register cycles using a scratch register.
    pub fn resolve_parallel_moves(&mut self, scratch_regs: &[crate::regalloc::target::PhysReg]) {
        if scratch_regs.is_empty() {
            return;
        }
        let scratch = scratch_regs[0];

        for edge in &mut self.edge_moves {
            resolve_move_list(&mut edge.moves, scratch);
        }

        for call in &mut self.call_moves {
            resolve_move_list(&mut call.pre_moves, scratch);
            resolve_move_list(&mut call.post_moves, scratch);
        }
    }
}

/// Build move plans for block arguments and call ABI requirements.
pub fn build_move_plan(
    func: &Function,
    alloc_map: &ValueAllocMap,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
) -> MovePlan {
    let mut plan = MovePlan::default();
    let param_reg_count = param_reg_count(target);
    let value_types = build_value_types(func);

    let mut block_params: HashMap<BlockId, Vec<ValueId>> = HashMap::new();
    for block in &func.blocks {
        let params = block.params.iter().map(|param| param.value.id).collect();
        block_params.insert(block.id, params);
    }

    for block in &func.blocks {
        for (inst_index, inst) in block.insts.iter().enumerate() {
            if let InstKind::Call { args, .. } = &inst.kind {
                let mut pre_moves = Vec::new();
                let mut post_moves = Vec::new();

                if let Some(result) = &inst.result {
                    if needs_sret(types, result.ty) {
                        let reg = target.indirect_result_reg().unwrap_or_else(|| {
                            panic!("ssa regalloc: call sret requires indirect result reg")
                        });
                        let loc = alloc_map.get(&result.id).copied().unwrap_or_else(|| {
                            panic!("ssa regalloc: missing alloc for {:?}", result.id)
                        });
                        let src = match loc {
                            Location::Stack(slot) => Location::StackAddr(slot),
                            _ => {
                                panic!(
                                    "ssa regalloc: sret result must be stack-backed, got {:?}",
                                    loc
                                );
                            }
                        };
                        let size = move_size_for(types, result.ty, src, Location::Reg(reg));
                        pre_moves.push(MoveOp {
                            src,
                            dst: Location::Reg(reg),
                            size,
                        });
                    }
                }

                for (idx, arg) in args.iter().enumerate() {
                    let arg_ty = value_types
                        .get(arg)
                        .copied()
                        .unwrap_or_else(|| panic!("ssa regalloc: missing type for arg {:?}", arg));
                    let src = alloc_map
                        .get(arg)
                        .copied()
                        .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", arg));
                    let is_reg = matches!(
                        types.kind(arg_ty),
                        crate::ssa::IrTypeKind::Unit
                            | crate::ssa::IrTypeKind::Bool
                            | crate::ssa::IrTypeKind::Int { .. }
                            | crate::ssa::IrTypeKind::Ptr { .. }
                    );
                    let src = if is_reg {
                        src
                    } else {
                        match src {
                            Location::Stack(slot) => Location::StackAddr(slot),
                            Location::StackAddr(slot) => Location::StackAddr(slot),
                            other => {
                                panic!(
                                    "ssa regalloc: aggregate arg must be stack-backed, got {:?}",
                                    other
                                );
                            }
                        }
                    };
                    if idx < param_reg_count {
                        let dst = target.param_reg(idx as u32).unwrap_or_else(|| {
                            panic!("ssa regalloc: call arg {} has no ABI reg", idx)
                        });
                        let dst = Location::Reg(dst);
                        if src != dst {
                            let size = move_size_for(types, arg_ty, src, dst);
                            pre_moves.push(MoveOp { src, dst, size });
                        }
                    } else {
                        let offset = ((idx - param_reg_count) as u32) * 8;
                        let dst = Location::OutgoingArg(offset);
                        let size = move_size_for(types, arg_ty, src, dst);
                        pre_moves.push(MoveOp { src, dst, size });
                    }
                }

                if let Some(result) = &inst.result {
                    if !needs_sret(types, result.ty) {
                        let src = Location::Reg(target.result_reg());
                        let dst = alloc_map.get(&result.id).copied().unwrap_or_else(|| {
                            panic!("ssa regalloc: missing alloc for {:?}", result.id)
                        });
                        if src != dst {
                            let size = move_size_for(types, result.ty, src, dst);
                            post_moves.push(MoveOp { src, dst, size });
                        }
                    }
                }

                if !pre_moves.is_empty() || !post_moves.is_empty() {
                    plan.call_moves.push(CallMove {
                        block: block.id,
                        inst_index,
                        pre_moves,
                        post_moves,
                    });
                }
            }
        }

        match &block.term {
            Terminator::Br { target, args } => {
                let moves = edge_moves_for(
                    block.id,
                    *target,
                    args,
                    &block_params,
                    alloc_map,
                    &value_types,
                    types,
                );
                if !moves.is_empty() {
                    plan.edge_moves.push(EdgeMove {
                        from: block.id,
                        to: *target,
                        moves,
                    });
                }
            }
            Terminator::CondBr {
                then_bb,
                then_args,
                else_bb,
                else_args,
                ..
            } => {
                let then_moves = edge_moves_for(
                    block.id,
                    *then_bb,
                    then_args,
                    &block_params,
                    alloc_map,
                    &value_types,
                    types,
                );
                if !then_moves.is_empty() {
                    plan.edge_moves.push(EdgeMove {
                        from: block.id,
                        to: *then_bb,
                        moves: then_moves,
                    });
                }

                let else_moves = edge_moves_for(
                    block.id,
                    *else_bb,
                    else_args,
                    &block_params,
                    alloc_map,
                    &value_types,
                    types,
                );
                if !else_moves.is_empty() {
                    plan.edge_moves.push(EdgeMove {
                        from: block.id,
                        to: *else_bb,
                        moves: else_moves,
                    });
                }
            }
            Terminator::Switch {
                cases,
                default,
                default_args,
                ..
            } => {
                for case in cases {
                    let moves = edge_moves_for(
                        block.id,
                        case.target,
                        &case.args,
                        &block_params,
                        alloc_map,
                        &value_types,
                        types,
                    );
                    if !moves.is_empty() {
                        plan.edge_moves.push(EdgeMove {
                            from: block.id,
                            to: case.target,
                            moves,
                        });
                    }
                }

                let default_moves = edge_moves_for(
                    block.id,
                    *default,
                    default_args,
                    &block_params,
                    alloc_map,
                    &value_types,
                    types,
                );
                if !default_moves.is_empty() {
                    plan.edge_moves.push(EdgeMove {
                        from: block.id,
                        to: *default,
                        moves: default_moves,
                    });
                }
            }
            Terminator::Return { .. } | Terminator::Unreachable => {}
        }
    }

    plan
}

fn build_value_types(func: &Function) -> HashMap<ValueId, crate::ssa::IrTypeId> {
    let mut map = HashMap::new();
    for block in &func.blocks {
        for param in &block.params {
            map.insert(param.value.id, param.value.ty);
        }
        for inst in &block.insts {
            if let Some(result) = &inst.result {
                map.insert(result.id, result.ty);
            }
        }
    }
    map
}

fn param_reg_count(target: &dyn TargetSpec) -> usize {
    const MAX_PARAM_REGS: u32 = 32;
    let mut count = 0usize;
    let mut seen = HashSet::new();
    for idx in 0..MAX_PARAM_REGS {
        match target.param_reg(idx) {
            Some(reg) => {
                if !seen.insert(reg) {
                    break;
                }
                count += 1;
            }
            None => break,
        }
    }
    count
}

fn needs_sret(types: &mut IrTypeCache, ty: crate::ssa::IrTypeId) -> bool {
    match types.kind(ty) {
        IrTypeKind::Unit | IrTypeKind::Bool | IrTypeKind::Int { .. } | IrTypeKind::Ptr { .. } => {
            false
        }
        _ => true,
    }
}

fn edge_moves_for(
    from: BlockId,
    to: BlockId,
    args: &[ValueId],
    block_params: &HashMap<BlockId, Vec<ValueId>>,
    alloc_map: &ValueAllocMap,
    value_types: &HashMap<ValueId, crate::ssa::IrTypeId>,
    types: &mut IrTypeCache,
) -> Vec<MoveOp> {
    let params = block_params.get(&to).unwrap_or_else(|| {
        panic!(
            "ssa regalloc: missing params for target block {:?} from {:?}",
            to, from
        )
    });

    if params.len() != args.len() {
        panic!(
            "ssa regalloc: block {:?} expects {} args, got {}",
            to,
            params.len(),
            args.len()
        );
    }

    let mut moves = Vec::new();
    for (arg, param) in args.iter().zip(params.iter()) {
        let src = alloc_map
            .get(arg)
            .copied()
            .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", arg));
        let dst = alloc_map
            .get(param)
            .copied()
            .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", param));
        if src != dst {
            let arg_ty = value_types
                .get(arg)
                .copied()
                .or_else(|| value_types.get(param).copied())
                .unwrap_or_else(|| {
                    panic!(
                        "ssa regalloc: missing type for block arg {:?} (param {:?})",
                        arg, param
                    )
                });
            let size = move_size_for(types, arg_ty, src, dst);
            moves.push(MoveOp { src, dst, size });
        }
    }

    moves
}

fn resolve_move_list(moves: &mut Vec<MoveOp>, scratch: crate::regalloc::target::PhysReg) {
    if moves.len() <= 1 {
        return;
    }

    let mut pending = std::mem::take(moves);
    let mut ordered = Vec::with_capacity(pending.len());

    while !pending.is_empty() {
        let mut ready_idx = None;
        for (idx, mov) in pending.iter().enumerate() {
            match mov.dst {
                Location::Reg(dst_reg) => {
                    let mut used_elsewhere = false;
                    for (other_idx, other) in pending.iter().enumerate() {
                        if other_idx == idx {
                            continue;
                        }
                        if let Location::Reg(src_reg) = other.src {
                            if src_reg == dst_reg {
                                used_elsewhere = true;
                                break;
                            }
                        }
                    }

                    if !used_elsewhere {
                        ready_idx = Some(idx);
                        break;
                    }
                }
                _ => {
                    ready_idx = Some(idx);
                    break;
                }
            }
        }

        if let Some(idx) = ready_idx {
            ordered.push(pending.remove(idx));
            continue;
        }

        let cycle_idx = pending
            .iter()
            .position(|mov| matches!((&mov.src, &mov.dst), (Location::Reg(_), Location::Reg(_))))
            .expect("cycle resolution requires reg-to-reg move");
        let mut mov = pending.remove(cycle_idx);
        let Location::Reg(src_reg) = mov.src else {
            unreachable!("cycle candidate must be reg -> reg");
        };

        ordered.push(MoveOp {
            src: Location::Reg(src_reg),
            dst: Location::Reg(scratch),
            size: mov.size,
        });

        mov.src = Location::Reg(scratch);
        pending.push(mov);
    }

    *moves = ordered;
}

fn move_size_for(
    types: &mut IrTypeCache,
    ty: crate::ssa::IrTypeId,
    src: Location,
    dst: Location,
) -> u32 {
    if matches!(src, Location::StackAddr(_)) || matches!(dst, Location::StackAddr(_)) {
        return 8;
    }

    types.layout(ty).size() as u32
}
