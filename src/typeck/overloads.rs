use crate::diagnostics::Span;
use crate::resolve::def_map::DefId;
use crate::type_rel::{TypeAssignability, type_assignable};
use crate::typeck::errors::TypeCheckError;
use crate::types::Type;

pub(super) struct FuncParamSig {
    #[allow(dead_code)]
    pub(super) name: String,
    pub(super) ty: Type,
}

pub(super) struct FuncOverloadSig {
    pub(super) def_id: DefId,
    pub(super) params: Vec<FuncParamSig>,
    pub(super) return_type: Type,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(super) enum OverloadRank {
    Exact = 0,
    Assignable = 1,
}

pub(super) struct ResolvedOverload<'a> {
    pub(super) def_id: DefId,
    pub(super) sig: &'a FuncOverloadSig,
    pub(super) ranks: Vec<OverloadRank>,
}

impl ResolvedOverload<'_> {
    fn score(&self) -> u32 {
        self.ranks.iter().map(|r| *r as u32).sum()
    }
}

pub(super) struct FuncOverloadResolver<'a> {
    call_span: Span,
    name: &'a str,
    arg_types: &'a [Type],
}

impl<'a> FuncOverloadResolver<'a> {
    pub(super) fn new(name: &'a str, arg_types: &'a [Type], call_span: Span) -> Self {
        Self {
            call_span,
            name,
            arg_types,
        }
    }

    pub(super) fn resolve(
        self,
        overloads: &'a [FuncOverloadSig],
    ) -> Result<ResolvedOverload<'a>, TypeCheckError> {
        let mut candidates = Vec::new();

        for cand in overloads {
            if let Some(ranks) = self.rank_overload(cand) {
                candidates.push(ResolvedOverload {
                    def_id: cand.def_id,
                    sig: cand,
                    ranks,
                });
            }
        }

        if candidates.is_empty() {
            return Err(TypeCheckError::FuncOverloadNoMatch(
                self.name.to_string(),
                self.call_span,
            ));
        }

        let best_score = candidates.iter().map(|c| c.score()).min().unwrap();
        let mut best: Vec<_> = candidates
            .into_iter()
            .filter(|c| c.score() == best_score)
            .collect();
        if best.len() != 1 {
            return Err(TypeCheckError::FuncOverloadAmbiguous(
                self.name.to_string(),
                self.call_span,
            ));
        }

        Ok(best.pop().unwrap())
    }

    fn rank_overload(&self, sig: &FuncOverloadSig) -> Option<Vec<OverloadRank>> {
        if sig.params.len() != self.arg_types.len() {
            return None;
        }

        let mut ranks = Vec::with_capacity(self.arg_types.len());
        for (arg_ty, param) in self.arg_types.iter().zip(sig.params.iter()) {
            match type_assignable(arg_ty, &param.ty) {
                TypeAssignability::Exact => ranks.push(OverloadRank::Exact),
                TypeAssignability::Incompatible => return None,
                _ => ranks.push(OverloadRank::Assignable),
            }
        }

        Some(ranks)
    }
}
