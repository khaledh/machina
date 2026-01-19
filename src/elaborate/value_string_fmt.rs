use crate::elaborate::elaborator::Elaborator;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::types::Type;

const MAX_U64_DEC_LEN: usize = 20;

impl<'a> Elaborator<'a> {
    pub(in crate::elaborate::value) fn elab_string_fmt_plan(
        &mut self,
        segments: &[norm::StringFmtSegment],
    ) -> sem::StringFmtPlan {
        let mut plan_segments = Vec::with_capacity(segments.len());
        let mut reserve_terms = Vec::new();

        for segment in segments {
            match segment {
                norm::StringFmtSegment::Literal { value, .. } => {
                    // Literal bytes contribute directly to both plan segments and reserve length.
                    plan_segments.push(sem::SegmentKind::LiteralBytes(value.clone()));
                    reserve_terms.push(sem::LenTerm::Literal(value.len()));
                }
                norm::StringFmtSegment::Expr { expr, .. } => {
                    if let norm::ExprKind::StringLit { value } = &expr.kind {
                        // Inline string literals as literal bytes for cheaper view formatting.
                        plan_segments.push(sem::SegmentKind::LiteralBytes(value.clone()));
                        reserve_terms.push(sem::LenTerm::Literal(value.len()));
                        continue;
                    }

                    let ty = self.type_map.type_table().get(expr.ty).clone();
                    match ty {
                        Type::String => {
                            // String values require owned formatting and a dynamic reserve term.
                            let expr = self.elab_value(expr);
                            let segment_index = plan_segments.len();
                            plan_segments.push(sem::SegmentKind::StringValue {
                                expr: Box::new(expr),
                            });
                            reserve_terms.push(sem::LenTerm::StringValue { segment_index });
                        }
                        Type::Int { signed, bits } => {
                            // Integers contribute a conservative literal reserve bound.
                            plan_segments.push(sem::SegmentKind::Int {
                                expr: Box::new(self.elab_value(expr)),
                                signed,
                                bits,
                            });
                            reserve_terms.push(sem::LenTerm::Literal(MAX_U64_DEC_LEN));
                        }
                        _ => {
                            panic!("compiler bug: unsupported f-string expr type");
                        }
                    }
                }
            }
        }

        // Any dynamic string segment forces the owned formatter path.
        let kind = if plan_segments
            .iter()
            .any(|segment| matches!(segment, sem::SegmentKind::StringValue { .. }))
        {
            sem::FmtKind::Owned
        } else {
            sem::FmtKind::View
        };

        sem::StringFmtPlan {
            kind,
            segments: plan_segments,
            reserve_terms,
        }
    }
}
