//! Lowering from AST to MCIR.
//!
//! Design notes:
//! - The lowering is place-based: aggregate values are modeled as places (locations),
//!   while scalars are modeled as operands.
//! - Lowering distinguishes value position from block-item position. Expressions that
//!   only make sense for side effects (let/var/assign/while) are rejected in value
//!   context.
//! - Aggregate construction is done in-place via projections whenever possible to
//!   avoid extra temporaries and copies (NRVO/RVO-style behavior).
//! - Control-flow expressions (if/block) are lowered by emitting side effects in
//!   each branch and joining with a destination place or a temp.
//!
//! Implementation notes:
//! - `lower_agg_value_into` is the central entry for writing aggregate values into
//!   a destination place.
//! - `emit_call_into` centralizes call emission so call sites can reuse it for both
//!   scalar and aggregate destinations.

use std::collections::HashMap;

use crate::ast::*;
use crate::context::{AnalyzedContext, LoweredMcirContext};
use crate::lower::errors::LowerError;
use crate::lower::lower_ty::TyLowerer;
use crate::mcir::func_builder::FuncBuilder;
use crate::mcir::interner::GlobalInterner;
use crate::mcir::types::*;
use crate::resolve::def_map::DefId;

pub(super) enum ExprValue {
    Scalar(Operand),
    Aggregate(Place<Aggregate>),
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    Scalar,
    Aggregate,
}

#[derive(Debug)]
pub struct FuncLowerer<'a> {
    pub(super) ctx: &'a AnalyzedContext,
    pub(super) global_interner: &'a mut GlobalInterner,
    pub(super) func: &'a Function,
    pub(super) fb: FuncBuilder,
    pub(super) locals: HashMap<DefId, LocalId>,
    pub(super) ty_lowerer: TyLowerer,
    pub(super) curr_block: BlockId,
}

impl<'a> FuncLowerer<'a> {
    /// --- Entry points ---

    /// Create a lowering context for one function.
    pub fn new(
        ctx: &'a AnalyzedContext,
        func: &'a Function,
        global_interner: &'a mut GlobalInterner,
    ) -> Self {
        let mut ty_lowerer = TyLowerer::new();

        // Lower the function return type
        let ast_ret_ty = ctx
            .type_map
            .lookup_node_type(func.id)
            .expect("Function return type not found");
        let ret_ty_id = ty_lowerer.lower_ty(&ast_ret_ty);

        // Initialize the function builder
        let fb = FuncBuilder::new(ret_ty_id);
        let entry = fb.body.entry;

        Self {
            ctx,
            global_interner,
            func,
            fb,
            locals: HashMap::new(),
            ty_lowerer,
            curr_block: entry,
        }
    }

    /// Lower the function AST into MCIR.
    pub fn lower(&mut self) -> Result<FuncBody, LowerError> {
        // Create locals for params.
        for (i, param) in self.func.params.iter().enumerate() {
            let ty = self.ty_for_node(param.id)?;
            let ty_id = self.ty_lowerer.lower_ty(&ty);
            let def_id = self.def_for_node(param.id)?.id;
            let local_id = self.fb.new_local(
                ty_id,
                LocalKind::Param { index: i as u32 },
                Some(param.name.clone()),
            );
            self.locals.insert(def_id, local_id);
        }

        // Lower the body into the return local (scalar vs aggregate).
        let ret_id = self.fb.body.ret_local;
        let ret_ty = self.fb.body.locals[ret_id.0 as usize].ty;

        let body_ty = self.ty_for_node(self.func.body.id)?;
        if body_ty.is_scalar() {
            let ret_place = Place::<Scalar>::new(ret_id, ret_ty, vec![]);
            let body_value = self.lower_expr_value(&self.func.body)?;
            if let ExprValue::Scalar(op) = body_value {
                self.emit_copy_scalar(ret_place, Rvalue::Use(op));
            }
        } else {
            let ret_place = Place::<Aggregate>::new(ret_id, ret_ty, vec![]);
            self.lower_agg_value_into(ret_place, &self.func.body)?;
        }

        // Terminate the entry block
        self.fb.set_terminator(self.curr_block, Terminator::Return);

        self.fb.body.types = std::mem::take(&mut self.ty_lowerer.table);

        Ok(self.fb.body.clone())
    }
}

/// Lower all functions in a module into MCIR bodies.
pub fn lower_ast(ctx: AnalyzedContext) -> Result<LoweredMcirContext, LowerError> {
    let mut bodies = Vec::new();

    // Interned globals
    let mut global_interner = GlobalInterner::new();

    // Lower all functions
    for func in ctx.module.funcs() {
        let body = FuncLowerer::new(&ctx, func, &mut global_interner).lower()?;
        bodies.push(body);
    }

    Ok(ctx.with_func_bodies(bodies, global_interner.take()))
}

#[cfg(test)]
#[path = "../tests/t_lower_ast.rs"]
mod tests;
