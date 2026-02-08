//! Semantic f-string plans consumed by lowering.

use super::ValueExpr;

#[derive(Clone, Debug)]
pub struct StringFmtPlan {
    pub kind: FmtKind,
    pub segments: Vec<SegmentKind>,
    pub reserve_terms: Vec<LenTerm>,
}

#[derive(Clone, Debug)]
pub enum FmtKind {
    View,
    Owned,
}

#[derive(Clone, Debug)]
pub enum SegmentKind {
    LiteralBytes(String),
    Bool {
        expr: Box<ValueExpr>,
    },
    Int {
        expr: Box<ValueExpr>,
        signed: bool,
        bits: u8,
    },
    StringValue {
        expr: Box<ValueExpr>,
    },
}

#[derive(Clone, Debug)]
pub enum LenTerm {
    Literal(usize),
    StringValue { segment_index: usize },
}
