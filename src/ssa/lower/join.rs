//! Join-plan and join-session helpers for branching lowering.

use crate::diag::Span;
use crate::resolve::DefId;
use crate::ssa::IrTypeId;
use crate::ssa::lower::LoweringError;
use crate::ssa::lower::locals::LocalSnapshot;
use crate::ssa::model::ir::{BlockId, Terminator, ValueId};
use crate::tree::semantic as sem;

/// Plan for joining control flow from multiple branches (e.g., if/else arms).
///
/// Contains the join block, the phi-like parameter for the expression value,
/// and parameters for threading local variable values through the join point.
pub(super) struct JoinPlan {
    pub(super) join_bb: BlockId,
    pub(super) join_value: ValueId,
    pub(super) defs: Vec<DefId>,
    pub(super) tys: Vec<IrTypeId>,
    pub(super) join_local_params: Vec<ValueId>,
}

pub(super) struct JoinSession {
    plan: JoinPlan,
    saved_locals: LocalSnapshot,
}

impl crate::ssa::lower::lowerer::FuncLowerer<'_> {
    /// Builds a join plan for merging control flow from multiple branches.
    ///
    /// Creates the join block with:
    /// 1. A parameter for the expression's result value
    /// 2. Parameters for each local variable (to thread SSA values through)
    ///
    /// The caller is responsible for emitting branches to this join block.
    pub(super) fn build_join_plan(&mut self, expr: &sem::ValueExpr) -> JoinPlan {
        let join_bb = self.builder.add_block();

        // Add parameter for the expression's result value.
        let join_ty = self.type_lowerer.lower_type_id(expr.ty);
        let join_value = self.builder.add_block_param(join_bb, join_ty);

        // Snapshot current locals in deterministic order.
        let locals_snapshot = self.locals.ordered();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let tys: Vec<IrTypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();

        // Add parameters for threading local variable values.
        let join_local_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(join_bb, *ty))
            .collect();

        JoinPlan {
            join_bb,
            join_value,
            defs,
            tys,
            join_local_params,
        }
    }

    pub(super) fn begin_join(&mut self, expr: &sem::ValueExpr) -> JoinSession {
        JoinSession::new(self.build_join_plan(expr), self.locals.snapshot())
    }

    /// Emits a branch from the current block to the join block.
    ///
    /// Passes the branch's result value followed by current local variable values
    /// as arguments to the join block's parameters.
    pub(super) fn emit_join_branch(
        &mut self,
        plan: &JoinPlan,
        value: ValueId,
        _span: Span,
    ) -> Result<(), LoweringError> {
        // Build arguments: result value + locals in stable order.
        let Some(local_args) = self.locals.args_for(&plan.defs) else {
            panic!("ssa emit_join_branch missing locals args for join");
        };
        let mut args = Vec::with_capacity(1 + local_args.len());
        args.push(value);
        args.extend(local_args);

        self.builder.terminate(Terminator::Br {
            target: plan.join_bb,
            args,
        });
        Ok(())
    }
}

impl JoinSession {
    pub(super) fn new(plan: JoinPlan, saved_locals: LocalSnapshot) -> Self {
        Self { plan, saved_locals }
    }

    pub(super) fn emit_branch(
        &self,
        lowerer: &mut crate::ssa::lower::lowerer::FuncLowerer<'_>,
        value: ValueId,
        span: Span,
    ) -> Result<(), LoweringError> {
        lowerer.emit_join_branch(&self.plan, value, span)
    }

    pub(super) fn restore_locals(&self, lowerer: &mut crate::ssa::lower::lowerer::FuncLowerer<'_>) {
        lowerer.locals.restore(&self.saved_locals);
    }

    pub(super) fn join_value(&self) -> ValueId {
        self.plan.join_value
    }

    pub(super) fn finalize(self, lowerer: &mut crate::ssa::lower::lowerer::FuncLowerer<'_>) {
        lowerer.builder.select_block(self.plan.join_bb);
        lowerer.locals.set_from_params(
            &self.plan.defs,
            &self.plan.tys,
            &self.plan.join_local_params,
        );
    }
}
