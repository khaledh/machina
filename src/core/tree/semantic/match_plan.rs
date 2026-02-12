//! Semantic match plan consumed by lowering to avoid re-deriving pattern logic.

use std::collections::HashMap;

use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::types::Type;

#[derive(Clone, Debug)]
pub struct MatchPlan {
    pub scrutinee_ty: Type,
    pub decision: MatchDecision,
    pub arms: Vec<MatchArmPlan>,
}

#[derive(Clone, Debug)]
pub struct MatchArmPlan {
    pub bindings: Vec<MatchBinding>,
}

#[derive(Clone, Debug)]
pub struct MatchBinding {
    pub def_id: DefId,
    pub node_id: NodeId,
    pub source: MatchPlace,
}

#[derive(Clone, Debug)]
pub enum MatchDecision {
    Switch(MatchSwitch),
    DecisionTree(MatchDecisionNode),
}

#[derive(Clone, Debug)]
pub struct MatchSwitch {
    pub discr: MatchPlace,
    pub cases: Vec<MatchSwitchCase>,
    pub default: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct MatchSwitchCase {
    pub value: u64,
    pub arm_index: usize,
}

#[derive(Clone, Debug)]
pub enum MatchDecisionNode {
    Tests {
        tests: Vec<MatchTest>,
        on_match: Box<MatchDecisionNode>,
        on_fail: Box<MatchDecisionNode>,
    },
    Leaf {
        arm_index: usize,
    },
    Unreachable,
}

#[derive(Clone, Debug)]
pub struct MatchTest {
    pub place: MatchPlace,
    pub kind: MatchTestKind,
}

#[derive(Clone, Debug)]
pub enum MatchTestKind {
    Bool { value: bool },
    Int { value: u64, signed: bool, bits: u8 },
    EnumTag { tag: u64, is_scalar: bool },
}

#[derive(Clone, Debug)]
pub struct MatchPlace {
    pub base: MatchBase,
    pub projections: Vec<MatchProjection>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum MatchBase {
    Scrutinee,
}

#[derive(Clone, Debug)]
pub enum MatchProjection {
    Deref,
    Field { index: usize },
    ByteOffset { offset: usize },
}

pub type MatchPlanMap = HashMap<NodeId, MatchPlan>;
