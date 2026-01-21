//! SSA lowering state and shared helpers.

use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::{DefId, DefTable};
use crate::ssa::lower::LoweredFunction;
use crate::ssa::lower::ctx::LowerCtx;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{BlockId, FunctionSig, Terminator, TypeId, ValueId};
use crate::tree::NodeId;
use crate::tree::ParamMode;
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;
use crate::types::Type;

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

pub(super) struct JoinPlan {
    pub(super) join_bb: BlockId,
    pub(super) join_value: ValueId,
    pub(super) defs: Vec<DefId>,
    pub(super) tys: Vec<TypeId>,
    pub(super) join_local_params: Vec<ValueId>,
}

pub(super) enum BranchResult {
    Value(BranchingValue),
    Return,
}

pub(super) enum StmtOutcome {
    Continue(BlockId),
    Return,
}

pub(super) struct FuncLowerer<'a> {
    pub(super) ctx: LowerCtx<'a>,
    pub(super) builder: FunctionBuilder,
    pub(super) locals: HashMap<DefId, LocalValue>,
    pub(super) block_expr_plans: &'a HashMap<NodeId, sem::BlockExprPlan>,
    pub(super) param_defs: Vec<DefId>,
    pub(super) param_tys: Vec<TypeId>,
}

impl<'a> FuncLowerer<'a> {
    pub(super) fn new(
        func: &sem::FuncDef,
        def_table: &DefTable,
        type_map: &'a TypeMap,
        block_expr_plans: &'a HashMap<NodeId, sem::BlockExprPlan>,
    ) -> Self {
        let mut ctx = LowerCtx::new(type_map);

        // Look up the function signature type to seed the SSA signature.
        let def = def_table
            .lookup_def(func.def_id)
            .unwrap_or_else(|| panic!("ssa lower_func missing def {:?}", func.def_id));
        let func_ty = type_map
            .lookup_def_type(def)
            .unwrap_or_else(|| panic!("ssa lower_func missing def type {:?}", func.def_id));
        let ret_ty = match func_ty {
            Type::Fn { ret_ty, .. } => ret_ty,
            other => panic!("ssa lower_func expected fn type, found {:?}", other),
        };

        // Collect parameter defs/types and map them into SSA types.
        let mut param_defs = Vec::with_capacity(func.sig.params.len());
        let mut param_tys = Vec::with_capacity(func.sig.params.len());
        for param in &func.sig.params {
            if param.mode != ParamMode::In {
                panic!(
                    "ssa lower_func only supports in params, found {:?} for {:?}",
                    param.mode, param.ident
                );
            }
            let def = def_table
                .lookup_def(param.def_id)
                .unwrap_or_else(|| panic!("ssa lower_func missing param def {:?}", param.def_id));
            let param_ty = type_map
                .lookup_def_type(def)
                .unwrap_or_else(|| panic!("ssa lower_func missing param type {:?}", param.def_id));
            let param_ty_id = ctx.ssa_type_for_type(&param_ty);
            param_defs.push(param.def_id);
            param_tys.push(param_ty_id);
        }

        // Build the SSA function signature from parameter and return types.
        let ret_id = ctx.ssa_type_for_type(&ret_ty);
        let sig = FunctionSig {
            params: param_tys.clone(),
            ret: ret_id,
        };
        let builder = FunctionBuilder::new(func.def_id, func.sig.name.clone(), sig);
        Self {
            ctx,
            builder,
            locals: HashMap::new(),
            block_expr_plans,
            param_defs,
            param_tys,
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
        // Emit the current SSA values for a fixed def ordering.
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
        // Reset locals to a fresh mapping based on block params.
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

    pub(super) fn build_join_plan(&mut self, expr: &sem::ValueExpr) -> JoinPlan {
        // Create the join block and capture a stable locals ordering.
        let join_bb = self.builder.add_block();
        let join_ty = self.ctx.ssa_type_for_expr(expr);
        let join_value = self.builder.add_block_param(join_bb, join_ty);

        let locals_snapshot = self.ordered_locals();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let tys: Vec<TypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();

        // Thread the current locals through the join block.
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

    pub(super) fn emit_join_branch(
        &mut self,
        from_bb: BlockId,
        plan: &JoinPlan,
        value: ValueId,
        span: Span,
    ) -> Result<(), sem::LinearizeError> {
        // Append the locals in a stable order after the branch value.
        let local_args = self.locals_args(&plan.defs, span)?;
        let mut args = Vec::with_capacity(1 + local_args.len());
        args.push(value);
        args.extend(local_args);

        self.builder.set_terminator(
            from_bb,
            Terminator::Br {
                target: plan.join_bb,
                args,
            },
        );
        Ok(())
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
