//! Indexing and slicing plans computed during elaboration.

use std::collections::HashMap;

use crate::tree::NodeId;

#[derive(Clone, Debug)]
pub struct IndexPlan {
    pub base: IndexBaseKind,
}

#[derive(Clone, Debug)]
pub enum IndexBaseKind {
    Array { dims: Vec<u64>, deref_count: usize },
    DynArray { deref_count: usize },
    Slice { deref_count: usize },
    String { deref_count: usize },
}

#[derive(Clone, Debug)]
pub struct SlicePlan {
    pub base: SliceBaseKind,
    pub elem_size: u64,
}

#[derive(Clone, Debug)]
pub enum SliceBaseKind {
    Array { len: u64, deref_count: usize },
    DynArray { deref_count: usize },
    Slice { deref_count: usize },
    String { deref_count: usize },
}

pub type IndexPlanMap = HashMap<NodeId, IndexPlan>;
pub type SlicePlanMap = HashMap<NodeId, SlicePlan>;
