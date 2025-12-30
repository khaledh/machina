use crate::mcir::types::{BasicBlock, BlockId, FuncBody, Terminator};

#[derive(Debug, Clone)]
pub struct DataflowResult<T> {
    pub in_map: Vec<T>,
    pub out_map: Vec<T>,
}

pub fn solve_forward<T, FMeet, FTransfer>(
    body: &FuncBody,
    entry: T,
    bottom: T,
    meet: FMeet,
    transfer: FTransfer,
) -> DataflowResult<T>
where
    T: Clone + PartialEq,
    FMeet: Fn(&[T]) -> T,
    FTransfer: Fn(&BasicBlock, &T, BlockId) -> T,
{
    let preds = compute_preds(body);

    let mut in_map = vec![bottom.clone(); body.blocks.len()];
    let mut out_map = vec![bottom.clone(); body.blocks.len()];

    let entry_idx = body.entry.index();
    in_map[entry_idx] = entry.clone();

    let mut changed = true;
    while changed {
        changed = false;
        for (idx, block) in body.blocks.iter().enumerate() {
            let in_state = if idx == entry_idx {
                entry.clone()
            } else if preds[idx].is_empty() {
                bottom.clone()
            } else {
                let pred_states = preds[idx]
                    .iter()
                    .map(|pred| out_map[pred.index()].clone())
                    .collect::<Vec<_>>();
                meet(&pred_states)
            };

            let out_state = transfer(block, &in_state, BlockId(idx as u32));

            if in_state != in_map[idx] {
                in_map[idx] = in_state;
                changed = true;
            }
            if out_state != out_map[idx] {
                out_map[idx] = out_state;
                changed = true;
            }
        }
    }

    DataflowResult { in_map, out_map }
}

fn compute_preds(body: &FuncBody) -> Vec<Vec<BlockId>> {
    let mut preds: Vec<Vec<BlockId>> = vec![vec![]; body.blocks.len()];
    for (idx, block) in body.blocks.iter().enumerate() {
        let src = BlockId(idx as u32);
        for succ in succs(block) {
            preds[succ.index()].push(src);
        }
    }
    preds
}

fn succs(block: &BasicBlock) -> Vec<BlockId> {
    match &block.terminator {
        Terminator::Goto(target) => vec![*target],
        Terminator::If {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        Terminator::Switch { cases, default, .. } => {
            let mut out = Vec::with_capacity(cases.len() + 1);
            for case in cases {
                out.push(case.target);
            }
            out.push(*default);
            out
        }
        Terminator::Return | Terminator::Unreachable | Terminator::Unterminated => vec![],
    }
}
