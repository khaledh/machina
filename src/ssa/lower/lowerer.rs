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

/// An SSA value produced by linear (single-block) expression lowering.
pub(super) type LinearValue = ValueId;

/// A local variable's current SSA value and type.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct LocalValue {
    pub(super) value: ValueId,
    pub(super) ty: TypeId,
}

/// Result of lowering a branching expression: the produced value and the block it ends in.
pub(super) struct BranchingValue {
    pub(super) value: ValueId,
    pub(super) block: BlockId,
}

/// Plan for joining control flow from multiple branches (e.g., if/else arms).
///
/// Contains the join block, the phi-like parameter for the expression value,
/// and parameters for threading local variable values through the join point.
pub(super) struct JoinPlan {
    pub(super) join_bb: BlockId,
    pub(super) join_value: ValueId,
    pub(super) defs: Vec<DefId>,
    pub(super) tys: Vec<TypeId>,
    pub(super) join_local_params: Vec<ValueId>,
}

/// Result of lowering a branching expression.
pub(super) enum BranchResult {
    /// Expression produced a value in the given block.
    Value(BranchingValue),
    /// Expression terminates with a return (no continuation).
    Return,
}

/// Outcome of lowering a statement.
pub(super) enum StmtOutcome {
    /// Continue execution in the given block.
    Continue(BlockId),
    /// Statement terminates with a return.
    Return,
}

/// Main state for lowering a single function to SSA IR.
///
/// Tracks:
/// - Type lowering context for converting types
/// - SSA function builder for emitting instructions
/// - Current SSA values for local variables (updated on assignment/join)
/// - Expression plans from semantic analysis (linear vs branching)
pub(super) struct FuncLowerer<'a> {
    pub(super) ctx: LowerCtx<'a>,
    pub(super) builder: FunctionBuilder,
    /// Maps definition IDs to their current SSA values (mutable during lowering).
    pub(super) locals: HashMap<DefId, LocalValue>,
    pub(super) block_expr_plans: &'a HashMap<NodeId, sem::BlockExprPlan>,
    pub(super) param_defs: Vec<DefId>,
    pub(super) param_tys: Vec<TypeId>,
}

impl<'a> FuncLowerer<'a> {
    /// Creates a new function lowerer for the given semantic function definition.
    ///
    /// Initializes the type context, extracts the function signature, and prepares
    /// parameter information for later mapping to SSA block parameters.
    pub(super) fn new(
        func: &sem::FuncDef,
        def_table: &DefTable,
        type_map: &'a TypeMap,
        block_expr_plans: &'a HashMap<NodeId, sem::BlockExprPlan>,
    ) -> Self {
        let mut ctx = LowerCtx::new(type_map);

        // Look up the function's type to extract parameter and return types.
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

        // Convert each parameter to SSA types. Only `in` mode is supported for now.
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

        // Build the SSA function signature and initialize the builder.
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

    /// Returns locals in a deterministic order (sorted by DefId).
    /// This ensures consistent ordering when threading locals through join blocks.
    pub(super) fn ordered_locals(&self) -> Vec<(DefId, LocalValue)> {
        let mut locals: Vec<_> = self
            .locals
            .iter()
            .map(|(def, local)| (*def, *local))
            .collect();
        locals.sort_by_key(|(def, _)| def.0);
        locals
    }

    /// Collects current SSA values for the given defs in order.
    /// Used when building branch arguments for join blocks.
    pub(super) fn locals_args(
        &self,
        defs: &[DefId],
        span: Span,
    ) -> Result<Vec<ValueId>, sem::LinearizeError> {
        let mut args = Vec::with_capacity(defs.len());
        for def in defs {
            let Some(local) = self.locals.get(def) else {
                return Err(self.err_span(span, sem::LinearizeErrorKind::UnsupportedExpr));
            };
            args.push(local.value);
        }
        Ok(args)
    }

    /// Resets the locals map to use values from block parameters.
    /// Called after entering a new block (entry, loop header, join block).
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
        let join_ty = self.ctx.ssa_type_for_expr(expr);
        let join_value = self.builder.add_block_param(join_bb, join_ty);

        // Snapshot current locals in deterministic order.
        let locals_snapshot = self.ordered_locals();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let tys: Vec<TypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();

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

    /// Emits a branch from a source block to the join block.
    ///
    /// Passes the branch's result value followed by current local variable values
    /// as arguments to the join block's parameters.
    pub(super) fn emit_join_branch(
        &mut self,
        from_bb: BlockId,
        plan: &JoinPlan,
        value: ValueId,
        span: Span,
    ) -> Result<(), sem::LinearizeError> {
        // Build arguments: result value + locals in stable order.
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
