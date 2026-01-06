use crate::lower::errors::LowerError;
use crate::mcir::types::{BlockId, Operand, PlaceAny, Terminator};

#[derive(Clone)]
pub struct Test {
    pub place: PlaceAny,
    pub kind: TestKind,
}

#[derive(Clone)]
pub enum TestKind {
    Bool { value: bool },
    Int { value: u64, signed: bool, bits: u8 },
    Enum { tag: u64, is_scalar: bool },
}

pub struct ArmDecision {
    pub tests: Vec<Test>,
    pub target: BlockId,
}

pub enum DecisionNode {
    // Evaluate `tests` as a conjunction; on success follow `on_match`, else `on_fail`.
    Tests {
        tests: Vec<Test>,
        on_match: Box<DecisionNode>,
        on_fail: Box<DecisionNode>,
    },
    Leaf {
        target: BlockId,
    },
    Unreachable,
}

pub fn build_decision_tree(arms: &[ArmDecision]) -> DecisionNode {
    if arms.is_empty() {
        return DecisionNode::Unreachable;
    }

    // Short-circuit: the first arm with no tests is an unconditional match.
    let first = &arms[0];
    if first.tests.is_empty() {
        return DecisionNode::Leaf {
            target: first.target,
        };
    }

    // A simple left-to-right tree: test the first arm's conjunction, then fall through.
    DecisionNode::Tests {
        tests: first.tests.clone(),
        on_match: Box::new(DecisionNode::Leaf {
            target: first.target,
        }),
        on_fail: Box::new(build_decision_tree(&arms[1..])),
    }
}

pub trait DecisionTreeEmitter {
    fn new_block(&mut self) -> BlockId;
    fn enter_block(&mut self, block: BlockId);
    fn emit_test(&mut self, test: &Test) -> Result<Operand, LowerError>;
    fn set_terminator(&mut self, block: BlockId, term: Terminator);
}

pub fn emit_decision_tree(
    tree: &DecisionNode,
    start_bb: BlockId,
    emitter: &mut impl DecisionTreeEmitter,
) -> Result<(), LowerError> {
    emit_node(tree, start_bb, emitter)
}

fn emit_node(
    node: &DecisionNode,
    start_bb: BlockId,
    emitter: &mut impl DecisionTreeEmitter,
) -> Result<(), LowerError> {
    match node {
        DecisionNode::Leaf { target } => {
            emitter.set_terminator(start_bb, Terminator::Goto(*target));
            Ok(())
        }
        DecisionNode::Unreachable => {
            emitter.set_terminator(start_bb, Terminator::Unreachable);
            Ok(())
        }
        DecisionNode::Tests {
            tests,
            on_match,
            on_fail,
        } => {
            let then_bb = emitter.new_block();
            let else_bb = emitter.new_block();
            // Emit a chain of `if` tests; success falls into `then_bb`, failure into `else_bb`.
            emit_test_chain(start_bb, then_bb, else_bb, tests, emitter)?;
            emit_node(on_match, then_bb, emitter)?;
            emit_node(on_fail, else_bb, emitter)?;
            Ok(())
        }
    }
}

fn emit_test_chain(
    start_bb: BlockId,
    success_bb: BlockId,
    failure_bb: BlockId,
    tests: &[Test],
    emitter: &mut impl DecisionTreeEmitter,
) -> Result<(), LowerError> {
    let mut curr_bb = start_bb;
    for (index, test) in tests.iter().enumerate() {
        let then_bb = if index + 1 == tests.len() {
            success_bb
        } else {
            emitter.new_block()
        };

        // Each test branches to the next test on success or to the failure block.
        emitter.enter_block(curr_bb);
        let cond = emitter.emit_test(test)?;
        emitter.set_terminator(
            curr_bb,
            Terminator::If {
                cond,
                then_bb,
                else_bb: failure_bb,
            },
        );

        if index + 1 == tests.len() {
            break;
        }

        curr_bb = then_bb;
    }

    Ok(())
}
