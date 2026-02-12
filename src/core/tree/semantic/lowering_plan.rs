//! Lowering plans for SSA emission.
//!
//! Classifies each value expression as either linear (single-block) or
//! branching (may introduce control flow), so lowering can dispatch directly.

use crate::tree::NodeId;
use crate::tree::semantic::{CallPlanMap, IndexPlanMap, MatchPlanMap, SlicePlanMap};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LoweringPlan {
    Linear,
    Branching,
}

#[derive(Clone, Debug, Default)]
pub struct LoweringPlanMap {
    pub value_plans: HashMap<NodeId, LoweringPlan>,
    pub call_plans: CallPlanMap,
    pub index_plans: IndexPlanMap,
    pub match_plans: MatchPlanMap,
    pub slice_plans: SlicePlanMap,
}

impl LoweringPlanMap {
    pub fn lookup_value_plan(&self, node: NodeId) -> Option<LoweringPlan> {
        self.value_plans.get(&node).cloned()
    }

    pub fn lookup_call_plan(&self, node: NodeId) -> Option<crate::tree::semantic::CallPlan> {
        self.call_plans.get(&node).cloned()
    }

    pub fn lookup_index_plan(&self, node: NodeId) -> Option<crate::tree::semantic::IndexPlan> {
        self.index_plans.get(&node).cloned()
    }

    pub fn lookup_match_plan(&self, node: NodeId) -> Option<crate::tree::semantic::MatchPlan> {
        self.match_plans.get(&node).cloned()
    }

    pub fn lookup_slice_plan(&self, node: NodeId) -> Option<crate::tree::semantic::SlicePlan> {
        self.slice_plans.get(&node).cloned()
    }

    pub fn insert_value_plan(&mut self, node: NodeId, plan: LoweringPlan) {
        self.value_plans.insert(node, plan);
    }
}
