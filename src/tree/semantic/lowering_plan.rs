//! Lowering plans for SSA emission.
//!
//! Classifies each value expression as either linear (single-block) or
//! branching (may introduce control flow), so lowering can dispatch directly.

use std::collections::HashMap;

use crate::tree::NodeId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LoweringPlan {
    Linear,
    Branching,
}

pub type LoweringPlanMap = HashMap<NodeId, LoweringPlan>;
