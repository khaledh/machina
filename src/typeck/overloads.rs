use crate::ast::{CallArg, FunctionParamMode};
use crate::diag::Span;
use crate::resolve::def_map::DefId;
use crate::typeck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::types::{Type, TypeAssignability, ValueAssignability, value_assignable};

pub(super) struct FuncParamSig {
    #[allow(dead_code)]
    pub(super) name: String,
    pub(super) ty: Type,
    #[allow(dead_code)]
    pub(super) mode: FunctionParamMode,
}

pub(super) struct FuncOverloadSig {
    pub(super) def_id: DefId,
    pub(super) params: Vec<FuncParamSig>,
    pub(super) return_type: Type,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(super) enum ArgOverloadRank {
    Exact = 0,
    Assignable = 1,
}

pub(super) struct ResolvedOverload<'a> {
    pub(super) def_id: DefId,
    pub(super) sig: &'a FuncOverloadSig,
    pub(super) arg_ranks: Vec<ArgOverloadRank>,
}

impl ResolvedOverload<'_> {
    fn score(&self) -> u32 {
        self.arg_ranks.iter().map(|r| *r as u32).sum()
    }
}

pub(super) struct FuncOverloadResolver<'a> {
    call_span: Span,
    name: &'a str,
    args: &'a [CallArg],
    arg_types: &'a [Type],
}

impl<'a> FuncOverloadResolver<'a> {
    pub(super) fn new(
        name: &'a str,
        args: &'a [CallArg],
        arg_types: &'a [Type],
        call_span: Span,
    ) -> Self {
        Self {
            call_span,
            name,
            args,
            arg_types,
        }
    }

    pub(super) fn resolve(
        self,
        overloads: &'a [FuncOverloadSig],
    ) -> Result<ResolvedOverload<'a>, TypeCheckError> {
        // Score each candidate by per-arg assignability; keep the best score.
        // If only out-of-range errors are seen, surface that instead of "no match."
        let mut candidates = Vec::new();
        let mut range_err: Option<TypeCheckError> = None;

        for cand in overloads {
            match self.rank_overload(cand) {
                Ok(Some(ranks)) => candidates.push(ResolvedOverload {
                    def_id: cand.def_id,
                    sig: cand,
                    arg_ranks: ranks,
                }),
                Ok(None) => {}
                Err(err) => {
                    if matches!(err.kind(), TypeCheckErrorKind::ValueOutOfRange(_, _, _, _)) {
                        range_err.get_or_insert(err);
                    } else {
                        return Err(err);
                    }
                }
            }
        }

        if candidates.is_empty() {
            return Err(range_err.unwrap_or_else(|| {
                TypeCheckErrorKind::FuncOverloadNoMatch(self.name.to_string(), self.call_span)
                    .into()
            }));
        }

        // Lower score wins (Exact < Assignable); ties are ambiguous.
        let best_score = candidates.iter().map(|c| c.score()).min().unwrap();
        let mut best: Vec<_> = candidates
            .into_iter()
            .filter(|c| c.score() == best_score)
            .collect();

        if best.len() != 1 {
            return Err(TypeCheckErrorKind::FuncOverloadAmbiguous(
                self.name.to_string(),
                self.call_span,
            )
            .into());
        }

        Ok(best.pop().unwrap())
    }

    fn rank_overload(
        &self,
        sig: &FuncOverloadSig,
    ) -> Result<Option<Vec<ArgOverloadRank>>, TypeCheckError> {
        // Reject wrong arity; otherwise classify each argument against its param.
        if sig.params.len() != self.arg_types.len() {
            return Ok(None);
        }

        let mut ranks = Vec::with_capacity(self.arg_types.len());
        for ((arg, arg_ty), param) in self
            .args
            .iter()
            .zip(self.arg_types.iter())
            .zip(sig.params.iter())
        {
            // Use value-aware assignability for literal narrowing and range checks.
            match value_assignable(&arg.expr, arg_ty, &param.ty) {
                ValueAssignability::Assignable(assignability) => match assignability {
                    TypeAssignability::Exact => ranks.push(ArgOverloadRank::Exact),
                    TypeAssignability::Incompatible => return Ok(None),
                    _ => ranks.push(ArgOverloadRank::Assignable),
                },
                ValueAssignability::ValueOutOfRange { value, min, max } => {
                    return Err(
                        TypeCheckErrorKind::ValueOutOfRange(value, min, max, arg.span).into(),
                    );
                }
                ValueAssignability::Incompatible => return Ok(None),
            }
        }

        Ok(Some(ranks))
    }
}
