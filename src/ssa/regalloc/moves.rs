//! Move planning for SSA register allocation.

use std::collections::HashMap;

use crate::regalloc::target::TargetSpec;
use crate::ssa::model::ir::{BlockId, Function, InstKind, Terminator, ValueId};

use super::{Location, ValueAllocMap};

/// A single move between two allocated locations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveOp {
    pub src: Location,
    pub dst: Location,
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

/// Build move plans for block arguments and call ABI requirements.
pub fn build_move_plan(
    func: &Function,
    alloc_map: &ValueAllocMap,
    target: &dyn TargetSpec,
) -> MovePlan {
    let mut plan = MovePlan::default();

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

                for (idx, arg) in args.iter().enumerate() {
                    let dst = target
                        .param_reg(idx as u32)
                        .unwrap_or_else(|| panic!("ssa regalloc: call arg {} has no ABI reg", idx));
                    let src = alloc_map
                        .get(arg)
                        .copied()
                        .unwrap_or_else(|| panic!("ssa regalloc: missing alloc for {:?}", arg));
                    let dst = Location::Reg(dst);
                    if src != dst {
                        pre_moves.push(MoveOp { src, dst });
                    }
                }

                if let Some(result) = &inst.result {
                    let src = Location::Reg(target.result_reg());
                    let dst = alloc_map.get(&result.id).copied().unwrap_or_else(|| {
                        panic!("ssa regalloc: missing alloc for {:?}", result.id)
                    });
                    if src != dst {
                        post_moves.push(MoveOp { src, dst });
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
                let moves = edge_moves_for(block.id, *target, args, &block_params, alloc_map);
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
                let then_moves =
                    edge_moves_for(block.id, *then_bb, then_args, &block_params, alloc_map);
                if !then_moves.is_empty() {
                    plan.edge_moves.push(EdgeMove {
                        from: block.id,
                        to: *then_bb,
                        moves: then_moves,
                    });
                }

                let else_moves =
                    edge_moves_for(block.id, *else_bb, else_args, &block_params, alloc_map);
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
                    let moves =
                        edge_moves_for(block.id, case.target, &case.args, &block_params, alloc_map);
                    if !moves.is_empty() {
                        plan.edge_moves.push(EdgeMove {
                            from: block.id,
                            to: case.target,
                            moves,
                        });
                    }
                }

                let default_moves =
                    edge_moves_for(block.id, *default, default_args, &block_params, alloc_map);
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

fn edge_moves_for(
    from: BlockId,
    to: BlockId,
    args: &[ValueId],
    block_params: &HashMap<BlockId, Vec<ValueId>>,
    alloc_map: &ValueAllocMap,
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
            moves.push(MoveOp { src, dst });
        }
    }

    moves
}
