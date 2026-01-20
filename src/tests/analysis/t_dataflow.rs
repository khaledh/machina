use crate::analysis::dataflow::{DataflowGraph, solve_backward, solve_forward};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ToyNode(usize);

struct ToyGraph {
    preds: Vec<Vec<ToyNode>>,
    succs: Vec<Vec<ToyNode>>,
}

impl ToyGraph {
    fn new_linear(len: usize) -> Self {
        let mut preds = vec![vec![]; len];
        let mut succs = vec![vec![]; len];
        for idx in 0..len {
            if idx > 0 {
                preds[idx].push(ToyNode(idx - 1));
            }
            if idx + 1 < len {
                succs[idx].push(ToyNode(idx + 1));
            }
        }
        Self { preds, succs }
    }
}

impl DataflowGraph for ToyGraph {
    type Node = ToyNode;

    fn num_nodes(&self) -> usize {
        self.preds.len()
    }

    fn index(&self, node: Self::Node) -> usize {
        node.0
    }

    fn node_at(&self, idx: usize) -> Self::Node {
        ToyNode(idx)
    }

    fn preds(&self, node: Self::Node) -> &[Self::Node] {
        &self.preds[node.0]
    }

    fn succs(&self, node: Self::Node) -> &[Self::Node] {
        &self.succs[node.0]
    }
}

#[test]
fn solve_forward_chain() {
    let graph = ToyGraph::new_linear(3);
    let entry_node = ToyNode(0);

    let result = solve_forward(
        &graph,
        entry_node,
        0i32,
        -1i32,
        |states| states.iter().cloned().max().unwrap_or(-1),
        |_, in_state| in_state + 1,
    );

    assert_eq!(result.in_map, vec![0, 1, 2]);
    assert_eq!(result.out_map, vec![1, 2, 3]);
}

#[test]
fn solve_backward_chain() {
    let graph = ToyGraph::new_linear(3);

    let result = solve_backward(
        &graph,
        0i32,
        -1i32,
        |states| states.iter().cloned().max().unwrap_or(-1),
        |_, out_state| out_state + 1,
    );

    assert_eq!(result.out_map, vec![2, 1, 0]);
    assert_eq!(result.in_map, vec![3, 2, 1]);
}
