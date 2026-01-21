//! SSA lowering state and shared helpers.

use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::DefId;
use crate::ssa::lower::LoweredFunction;
use crate::ssa::lower::ctx::LowerCtx;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{BlockId, FunctionSig, TypeId, ValueId};
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;

pub(super) type LinearValue = ValueId;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct LocalValue {
    pub(super) value: ValueId,
    pub(super) ty: TypeId,
}

pub(super) struct BranchingValue {
    pub(super) value: ValueId,
    pub(super) block: BlockId,
}

pub(super) struct FuncLowerer<'a> {
    pub(super) ctx: LowerCtx<'a>,
    pub(super) builder: FunctionBuilder,
    pub(super) locals: HashMap<DefId, LocalValue>,
}

impl<'a> FuncLowerer<'a> {
    pub(super) fn new(func: &sem::FuncDef, type_map: &'a TypeMap) -> Self {
        let mut ctx = LowerCtx::new(type_map);
        let ret_id = ctx.ssa_type_for_type_id(func.body.ty);
        let sig = FunctionSig {
            params: Vec::new(),
            ret: ret_id,
        };
        let builder = FunctionBuilder::new(func.def_id, func.sig.name.clone(), sig);
        Self {
            ctx,
            builder,
            locals: HashMap::new(),
        }
    }

    pub(super) fn finish(self) -> LoweredFunction {
        LoweredFunction {
            func: self.builder.finish(),
            types: self.ctx.types,
        }
    }

    pub(super) fn ordered_locals(&self) -> Vec<(DefId, LocalValue)> {
        let mut locals: Vec<_> = self
            .locals
            .iter()
            .map(|(def, local)| (*def, *local))
            .collect();
        locals.sort_by_key(|(def, _)| def.0);
        locals
    }

    pub(super) fn locals_args(
        &self,
        defs: &[DefId],
        span: Span,
    ) -> Result<Vec<ValueId>, sem::LinearizeError> {
        if self.locals.len() != defs.len() {
            return Err(self.err_span(span, sem::LinearizeErrorKind::UnsupportedExpr));
        }
        let mut args = Vec::with_capacity(defs.len());
        for def in defs {
            let Some(local) = self.locals.get(def) else {
                return Err(self.err_span(span, sem::LinearizeErrorKind::UnsupportedExpr));
            };
            args.push(local.value);
        }
        Ok(args)
    }

    pub(super) fn set_locals_from_params(
        &mut self,
        defs: &[DefId],
        tys: &[TypeId],
        params: &[ValueId],
    ) {
        self.locals.clear();
        for ((def, ty), value) in defs.iter().zip(tys.iter()).zip(params.iter()) {
            self.locals.insert(
                *def,
                LocalValue {
                    value: *value,
                    ty: *ty,
                },
            );
        }
    }

    pub(super) fn lookup_local(&self, def_id: DefId) -> LocalValue {
        *self
            .locals
            .get(&def_id)
            .unwrap_or_else(|| panic!("ssa lower_func missing local {:?}", def_id))
    }

    pub(super) fn err_span(
        &self,
        span: Span,
        kind: sem::LinearizeErrorKind,
    ) -> sem::LinearizeError {
        sem::LinearizeError { kind, span }
    }

    pub(super) fn err_stmt(
        &self,
        stmt: &sem::LinearStmt,
        kind: sem::LinearizeErrorKind,
    ) -> sem::LinearizeError {
        sem::LinearizeError {
            kind,
            span: stmt.span,
        }
    }
}
