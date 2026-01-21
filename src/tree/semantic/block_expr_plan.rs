//! Block expression plans used to classify statement-position expressions.

use std::collections::HashMap;

use crate::tree::NodeId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockExprPlan {
    Linear,
    Branching,
}

pub type BlockExprPlanMap = HashMap<NodeId, BlockExprPlan>;
