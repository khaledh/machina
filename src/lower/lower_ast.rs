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

use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::context::{AnalyzedContext, LoweredMcirContext};
use crate::lower::drop_glue::DropGlueRegistry;
use crate::lower::errors::LowerError;
use crate::lower::lower_drop::DropScope;
use crate::lower::lower_ty::TyLowerer;
use crate::mcir::func_builder::FuncBuilder;
use crate::mcir::interner::GlobalInterner;
use crate::mcir::types::*;
use crate::resolve::def_map::DefId;
use crate::types::Type;

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
    pub(super) func_id: NodeId,
    pub(super) func_name: &'a str,
    pub(super) func_body: &'a Expr,
    pub(super) params: Vec<LowerParam>,
    pub(super) fb: FuncBuilder,
    pub(super) locals: HashMap<DefId, LocalId>,
    pub(super) out_param_defs: HashSet<DefId>,
    pub(super) ty_lowerer: TyLowerer,
    pub(super) curr_block: BlockId,
    pub(super) drop_scopes: Vec<DropScope>,
    pub(super) drop_glue: &'a mut DropGlueRegistry,
    pub(super) trace_alloc: bool,
}

#[derive(Debug, Clone)]
pub struct LowerParam {
    pub id: NodeId,
    pub name: String,
    pub mode: ParamMode,
}

impl<'a> FuncLowerer<'a> {
    /// Create a lowering context for one function.
    pub fn new_function(
        ctx: &'a AnalyzedContext,
        func: &'a Function,
        global_interner: &'a mut GlobalInterner,
        drop_glue: &'a mut DropGlueRegistry,
        trace_alloc: bool,
    ) -> Self {
        let params = func
            .sig
            .params
            .iter()
            .map(|param| LowerParam {
                id: param.id,
                name: param.name.clone(),
                mode: param.mode.clone(),
            })
            .collect::<Vec<_>>();
        Self::new(
            ctx,
            func.id,
            &func.sig.name,
            &func.body,
            params,
            global_interner,
            drop_glue,
            trace_alloc,
        )
    }

    pub fn new_method(
        ctx: &'a AnalyzedContext,
        method: &'a Method,
        global_interner: &'a mut GlobalInterner,
        drop_glue: &'a mut DropGlueRegistry,
        trace_alloc: bool,
    ) -> Self {
        let mut params = Vec::with_capacity(method.sig.params.len() + 1);
        params.push(LowerParam {
            id: method.sig.self_param.id,
            name: "self".to_string(),
            mode: method.sig.self_param.mode.clone(),
        });
        for param in &method.sig.params {
            params.push(LowerParam {
                id: param.id,
                name: param.name.clone(),
                mode: param.mode.clone(),
            });
        }
        Self::new(
            ctx,
            method.id,
            &method.sig.name,
            &method.body,
            params,
            global_interner,
            drop_glue,
            trace_alloc,
        )
    }

    fn new(
        ctx: &'a AnalyzedContext,
        func_id: NodeId,
        func_name: &'a str,
        func_body: &'a Expr,
        params: Vec<LowerParam>,
        global_interner: &'a mut GlobalInterner,
        drop_glue: &'a mut DropGlueRegistry,
        trace_alloc: bool,
    ) -> Self {
        let mut ty_lowerer = TyLowerer::new();

        // Lower the function return type
        let ast_ret_ty = ctx
            .type_map
            .lookup_node_type(func_id)
            .expect("Function return type not found");
        let ret_ty_id = ty_lowerer.lower_ty(&ast_ret_ty);

        // Initialize the function builder
        let fb = FuncBuilder::new(ret_ty_id);
        let entry = fb.body.entry;

        // Track `out` params so we can suppress overwrite drops for their assignments.
        let mut out_param_defs = HashSet::new();
        for param in &params {
            if param.mode != ParamMode::Out {
                continue;
            }
            if let Some(def) = ctx.def_map.lookup_def(param.id) {
                out_param_defs.insert(def.id);
            }
        }

        Self {
            ctx,
            global_interner,
            func_id,
            func_name,
            func_body,
            params,
            fb,
            locals: HashMap::new(),
            out_param_defs,
            ty_lowerer,
            curr_block: entry,
            drop_scopes: Vec::new(),
            drop_glue,
            trace_alloc,
        }
    }

    /// Lower the function AST into MCIR.
    pub fn lower(&mut self) -> Result<FuncBody, LowerError> {
        // Function-level scope for owned locals/params.
        self.enter_drop_scope();

        if self.trace_alloc && self.func_name == "main" {
            self.emit_runtime_set_alloc_trace(true);
        }

        // Create locals for params.
        let params = self.params.clone();
        for (i, param) in params.iter().enumerate() {
            let ty = self.ty_for_node(param.id)?;
            let ty_id = self.ty_lowerer.lower_ty(&ty);
            let def_id = self.def_for_node(param.id)?.id;
            let local_id = self.fb.new_local(
                ty_id,
                LocalKind::Param { index: i as u32 },
                Some(param.name.clone()),
            );
            self.locals.insert(def_id, local_id);
            // `sink` params are treated as owned values; register for drop.
            // (`in`/`inout` params are borrowed)
            if param.mode == ParamMode::Sink {
                let is_initialized = self.create_is_initialized(&param.name, &ty, true);
                self.register_drop(def_id, &ty, is_initialized);
            }

            if matches!(ty, Type::Range { .. }) {
                let param_place = Place::<Scalar>::new(local_id, ty_id, vec![]);
                let op = Operand::Copy(param_place);
                self.emit_conversion_check(&Type::uint(64), &ty, &op);
            }
        }

        // Lower the body into the return local (scalar vs aggregate).
        let ret_id = self.fb.body.ret_local;
        let ret_ty = self.fb.body.locals[ret_id.0 as usize].ty;

        let body_ty = self.ty_for_node(self.func_body.id)?;
        if body_ty.is_scalar() {
            let ret_place = Place::<Scalar>::new(ret_id, ret_ty, vec![]);
            let body_value = self.lower_expr_value(self.func_body)?;
            if let ExprValue::Scalar(op) = body_value {
                let ret_ast_ty = self.ty_for_node(self.func_id)?;
                self.emit_conversion_check(&body_ty, &ret_ast_ty, &op);
                self.emit_copy_scalar(ret_place, Rvalue::Use(op));
            }
        } else {
            let ret_place = Place::<Aggregate>::new(ret_id, ret_ty, vec![]);
            self.lower_agg_value_into(ret_place, self.func_body)?;
        }

        // Drop any remaining owned locals before returning.
        self.exit_drop_scope()?;

        // Terminate the entry block
        self.fb.set_terminator(self.curr_block, Terminator::Return);

        self.fb.body.types = std::mem::take(&mut self.ty_lowerer.table);

        Ok(self.fb.body.clone())
    }
}

/// Lower all functions in a module into MCIR bodies.
pub fn lower_ast(
    ctx: AnalyzedContext,
    trace_alloc: bool,
) -> Result<LoweredMcirContext, LowerError> {
    let mut bodies = Vec::new();

    // Interned globals
    let mut global_interner = GlobalInterner::new();

    // Drop glue registry
    let mut drop_glue = DropGlueRegistry::new(ctx.def_map.next_def_id());

    // Lower all callables (functions + methods).
    for callable in ctx.module.callables() {
        let body = match callable {
            CallableRef::Function(function) => FuncLowerer::new_function(
                &ctx,
                function,
                &mut global_interner,
                &mut drop_glue,
                trace_alloc,
            )
            .lower()?,
            CallableRef::Method { method, .. } => FuncLowerer::new_method(
                &ctx,
                method,
                &mut global_interner,
                &mut drop_glue,
                trace_alloc,
            )
            .lower()?,
        };
        bodies.push(body);
    }

    // Register generated drop glue functions
    let mut ctx = ctx;
    for generated in drop_glue.drain() {
        ctx.symbols
            .register_generated_def(generated.def_id, generated.name);
        bodies.push(generated.body);
    }

    Ok(ctx.with_func_bodies(bodies, global_interner.take()))
}

#[cfg(test)]
#[path = "../tests/t_lower_ast.rs"]
mod tests;
