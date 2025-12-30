//! Generic dataflow analysis utilities.
//! Concrete analyses (liveness, move-check, etc) should build on this.
//! Use `solve_forward` for forward problems and `solve_backward` for backward ones.

pub trait DataflowGraph {
    type Node: Copy + Eq;

    fn num_nodes(&self) -> usize;
    fn index(&self, node: Self::Node) -> usize;
    fn node_at(&self, idx: usize) -> Self::Node;
    fn preds(&self, node: Self::Node) -> &[Self::Node];
    fn succs(&self, node: Self::Node) -> &[Self::Node];
}

#[derive(Debug, Clone)]
pub struct DataflowResult<T> {
    pub in_map: Vec<T>,
    pub out_map: Vec<T>,
}

pub fn solve_forward<T, G, FMeet, FTransfer>(
    graph: &G,
    entry_node: G::Node,
    entry_state: T,
    bottom: T,
    meet: FMeet,
    transfer: FTransfer,
) -> DataflowResult<T>
where
    T: Clone + PartialEq,
    G: DataflowGraph,
    FMeet: Fn(&[T]) -> T,
    FTransfer: Fn(G::Node, &T) -> T,
{
    let num_nodes = graph.num_nodes();
    let mut in_map = vec![bottom.clone(); num_nodes];
    let mut out_map = vec![bottom.clone(); num_nodes];
    let mut in_worklist = vec![true; num_nodes];
    let mut worklist: Vec<G::Node> = (0..num_nodes).map(|idx| graph.node_at(idx)).collect();

    while let Some(node) = worklist.pop() {
        let idx = graph.index(node);
        in_worklist[idx] = false;

        let preds = graph.preds(node);
        let in_state = if node == entry_node {
            entry_state.clone()
        } else if preds.is_empty() {
            bottom.clone()
        } else {
            let mut pred_states = Vec::with_capacity(preds.len());
            for &pred in preds {
                let p_idx = graph.index(pred);
                pred_states.push(out_map[p_idx].clone());
            }
            meet(&pred_states)
        };

        let out_state = transfer(node, &in_state);

        let mut changed = false;
        if in_state != in_map[idx] {
            in_map[idx] = in_state;
            changed = true;
        }
        if out_state != out_map[idx] {
            out_map[idx] = out_state;
            changed = true;
        }

        if changed {
            for &succ in graph.succs(node) {
                let s_idx = graph.index(succ);
                if !in_worklist[s_idx] {
                    in_worklist[s_idx] = true;
                    worklist.push(succ);
                }
            }
        }
    }

    DataflowResult { in_map, out_map }
}

pub fn solve_backward<T, G, FMeet, FTransfer>(
    graph: &G,
    entry_state: T,
    bottom: T,
    meet: FMeet,
    transfer: FTransfer,
) -> DataflowResult<T>
where
    T: Clone + PartialEq,
    G: DataflowGraph,
    FMeet: Fn(&[T]) -> T,
    FTransfer: Fn(G::Node, &T) -> T,
{
    let num_nodes = graph.num_nodes();
    let mut in_map = vec![bottom.clone(); num_nodes];
    let mut out_map = vec![bottom.clone(); num_nodes];
    let mut in_worklist = vec![true; num_nodes];
    let mut worklist: Vec<G::Node> = (0..num_nodes).map(|idx| graph.node_at(idx)).collect();

    while let Some(node) = worklist.pop() {
        let idx = graph.index(node);
        in_worklist[idx] = false;

        let succs = graph.succs(node);
        let out_state = if succs.is_empty() {
            entry_state.clone()
        } else {
            let mut succ_states = Vec::with_capacity(succs.len());
            for &succ in succs {
                let s_idx = graph.index(succ);
                succ_states.push(in_map[s_idx].clone());
            }
            meet(&succ_states)
        };

        let in_state = transfer(node, &out_state);

        let mut changed = false;
        if in_state != in_map[idx] {
            in_map[idx] = in_state;
            changed = true;
        }
        if out_state != out_map[idx] {
            out_map[idx] = out_state;
            changed = true;
        }

        if changed {
            for &pred in graph.preds(node) {
                let p_idx = graph.index(pred);
                if !in_worklist[p_idx] {
                    in_worklist[p_idx] = true;
                    worklist.push(pred);
                }
            }
        }
    }

    DataflowResult { in_map, out_map }
}

#[cfg(test)]
#[path = "../../tests/t_analysis_dataflow.rs"]
mod tests;
