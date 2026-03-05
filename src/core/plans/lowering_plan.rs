//! Lowering plans for SSA emission.
//!
//! Classifies each value expression as either linear (single-block) or
//! branching (may introduce control flow), so lowering can dispatch directly.

use super::{CallPlanMap, IndexPlanMap, MatchPlanMap, SlicePlanMap, StringFmtPlan};
use crate::core::ast::{Expr, NodeId};
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
    pub try_cleanup_plans: HashMap<NodeId, Vec<Expr>>,
    pub string_fmt_plans: HashMap<NodeId, StringFmtPlan>,
}

impl LoweringPlanMap {
    pub fn lookup_value_plan(&self, node: NodeId) -> Option<LoweringPlan> {
        self.value_plans.get(&node).cloned()
    }

    pub fn lookup_call_plan(&self, node: NodeId) -> Option<super::CallPlan> {
        self.call_plans.get(&node).cloned()
    }

    pub fn lookup_index_plan(&self, node: NodeId) -> Option<super::IndexPlan> {
        self.index_plans.get(&node).cloned()
    }

    pub fn lookup_match_plan(&self, node: NodeId) -> Option<super::MatchPlan> {
        self.match_plans.get(&node).cloned()
    }

    pub fn lookup_slice_plan(&self, node: NodeId) -> Option<super::SlicePlan> {
        self.slice_plans.get(&node).cloned()
    }

    pub fn lookup_try_cleanup_plan(&self, node: NodeId) -> Option<Vec<Expr>> {
        self.try_cleanup_plans.get(&node).cloned()
    }

    pub fn insert_value_plan(&mut self, node: NodeId, plan: LoweringPlan) {
        self.value_plans.insert(node, plan);
    }
}
