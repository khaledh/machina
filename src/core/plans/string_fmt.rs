//! Semantic f-string plans consumed by lowering.

use crate::core::ast::Expr;

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
        expr: Box<Expr>,
    },
    Int {
        expr: Box<Expr>,
        signed: bool,
        bits: u8,
    },
    StringValue {
        expr: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub enum LenTerm {
    Literal(usize),
    StringValue { segment_index: usize },
}
