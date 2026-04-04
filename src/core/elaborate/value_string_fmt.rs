//! String interpolation planning.
//!
//! Pre-computes the strategy for formatting f-strings (`f"..."`) so that
//! lowering can emit efficient code without re-analyzing segment types.
//!
//! ## Formatting strategy
//!
//! Most dynamic f-strings lower through the owned-string path. Borrowed string
//! contexts can select a stack-backed view plan when every segment has a
//! statically bounded size.
//!
//! ## Reserve length calculation
//!
//! The plan includes reserve terms that let lowering pre-allocate the output
//! buffer. Literal segments contribute their exact length; integer segments
//! use a conservative upper bound (20 chars for u64); string segments require
//! runtime length queries.

use crate::core::ast::{ExprKind, StringFmtSegment};
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::plans::{FmtKind, LenTerm, SegmentKind, StringFmtPlan};
use crate::core::types::Type;

/// Maximum decimal digits in a u64 (used for reserve length estimation).
const MAX_U64_DEC_LEN: usize = 20;
/// Maximum string length of a boolean formatted as `true`/`false`.
const MAX_BOOL_LEN: usize = 5;

impl<'a> Elaborator<'a> {
    /// Build a formatting plan for a string interpolation expression.
    pub(in crate::core::elaborate::value) fn elab_string_fmt_plan(
        &mut self,
        segments: &[StringFmtSegment],
        expected: Option<&Type>,
    ) -> StringFmtPlan {
        let mut plan_segments = Vec::with_capacity(segments.len());
        let mut reserve_terms = Vec::new();

        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => {
                    // Literal bytes contribute directly to both plan segments and reserve length.
                    plan_segments.push(SegmentKind::LiteralBytes(value.clone()));
                    reserve_terms.push(LenTerm::Literal(value.len()));
                }
                StringFmtSegment::Expr { expr, .. } => {
                    if let ExprKind::StringLit { value } = &expr.kind {
                        // Inline string literals as literal bytes for cheaper view formatting.
                        plan_segments.push(SegmentKind::LiteralBytes(value.clone()));
                        reserve_terms.push(LenTerm::Literal(value.len()));
                        continue;
                    }

                    let ty = self
                        .type_map
                        .type_table()
                        .get(self.type_id_for(expr.id))
                        .clone();
                    match ty.peel_borrow() {
                        Type::String => {
                            // String values require owned formatting and a dynamic reserve term.
                            let expr = self.elab_value(expr);
                            let segment_index = plan_segments.len();
                            plan_segments.push(SegmentKind::StringValue {
                                expr: Box::new(expr),
                            });
                            reserve_terms.push(LenTerm::StringValue { segment_index });
                        }
                        Type::Int { signed, bits, .. } => {
                            // Integers contribute a conservative literal reserve bound.
                            plan_segments.push(SegmentKind::Int {
                                expr: Box::new(self.elab_value(expr)),
                                signed: *signed,
                                bits: *bits,
                            });
                            reserve_terms.push(LenTerm::Literal(MAX_U64_DEC_LEN));
                        }
                        Type::Bool => {
                            plan_segments.push(SegmentKind::Bool {
                                expr: Box::new(self.elab_value(expr)),
                            });
                            reserve_terms.push(LenTerm::Literal(MAX_BOOL_LEN));
                        }
                        _ => {
                            panic!("compiler bug: unsupported f-string expr type");
                        }
                    }
                }
            }
        }

        // Borrowed string contexts can safely consume stack-backed formatting
        // results as long as every segment has a statically bounded size.
        let kind = if expects_borrowed_string(expected) && can_lower_fmt_plan_as_view(&plan_segments)
        {
            FmtKind::View
        } else {
            FmtKind::Owned
        };

        StringFmtPlan {
            kind,
            segments: plan_segments,
            reserve_terms,
        }
    }
}

fn expects_borrowed_string(expected: Option<&Type>) -> bool {
    matches!(
        expected,
        Some(Type::Borrow { elem_ty }) if matches!(elem_ty.as_ref(), Type::String)
    )
}

fn can_lower_fmt_plan_as_view(segments: &[SegmentKind]) -> bool {
    segments
        .iter()
        .all(|segment| !matches!(segment, SegmentKind::StringValue { .. }))
}
