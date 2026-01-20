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

use crate::context::{AnalyzedContext, LoweredMcirContext};
use crate::lower::drop_glue::DropGlueRegistry;
use crate::lower::errors::LowerError;
use crate::lower::lower_drop::DropScope;
use crate::lower::lower_ty::TyLowerer;
use crate::mcir::func_builder::FuncBuilder;
use crate::mcir::interner::GlobalInterner;
use crate::mcir::types::*;
use crate::resolve::DefId;
use crate::tree::semantic::{CallableRef, FuncDef, MethodDef, ValueExpr};
use crate::tree::{NodeId, ParamMode};
use crate::types::Type;

pub(super) enum Value {
    Scalar(Operand),
    Aggregate(Place<Aggregate>),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct LoopContext {
    pub(super) break_bb: BlockId,
    pub(super) continue_bb: BlockId,
    pub(super) drop_depth: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    Scalar,
    Aggregate,
}

#[derive(Clone)]
pub struct LoweredFunc {
    pub def_id: DefId,
    pub body: FuncBody,
}

#[derive(Debug, Clone)]
pub struct LoweredParam {
    pub id: NodeId,
    pub def_id: DefId,
    pub name: String,
    pub mode: ParamMode,
}

#[derive(Debug)]
pub struct FuncLowerer<'a> {
    pub(super) ctx: &'a AnalyzedContext,
    pub(super) global_interner: &'a mut GlobalInterner,
    pub(super) func_id: NodeId,
    pub(super) func_name: &'a str,
    pub(super) func_body: &'a ValueExpr,
    pub(super) params: Vec<LoweredParam>,
    pub(super) fb: FuncBuilder,
    pub(super) locals: HashMap<DefId, LocalId>,
    pub(super) out_param_defs: HashSet<DefId>,
    pub(super) ty_lowerer: TyLowerer,
    pub(super) curr_block: BlockId,
    pub(super) drop_scopes: Vec<DropScope>,
    pub(super) loop_stack: Vec<LoopContext>,
    pub(super) drop_glue: &'a mut DropGlueRegistry,
    pub(super) trace_alloc: bool,
    func_return_ty: Option<Type>,
}

impl<'a> FuncLowerer<'a> {
    /// Create a lowering context for one function.
    pub fn new_function(
        ctx: &'a AnalyzedContext,
        func_def: &'a FuncDef,
        global_interner: &'a mut GlobalInterner,
        drop_glue: &'a mut DropGlueRegistry,
        trace_alloc: bool,
    ) -> Self {
        let params = func_def
            .sig
            .params
            .iter()
            .map(|param| {
                let def = ctx.def_table.lookup_def(param.def_id).unwrap_or_else(|| {
                    panic!("compiler bug: param def {:?} not found", param.def_id)
                });
                LoweredParam {
                    id: param.id,
                    def_id: param.def_id,
                    name: def.name.clone(),
                    mode: param.mode.clone(),
                }
            })
            .collect::<Vec<_>>();
        Self::new(
            ctx,
            func_def.id,
            &func_def.sig.name,
            &func_def.body,
            params,
            global_interner,
            drop_glue,
            trace_alloc,
            None,
        )
    }

    pub fn new_method(
        ctx: &'a AnalyzedContext,
        method_def: &'a MethodDef,
        global_interner: &'a mut GlobalInterner,
        drop_glue: &'a mut DropGlueRegistry,
        trace_alloc: bool,
    ) -> Self {
        let mut params = Vec::with_capacity(method_def.sig.params.len() + 1);
        let self_def = ctx
            .def_table
            .lookup_def(method_def.sig.self_param.def_id)
            .unwrap_or_else(|| {
                panic!(
                    "compiler bug: self def {:?} not found",
                    method_def.sig.self_param.def_id
                )
            });
        params.push(LoweredParam {
            id: method_def.sig.self_param.id,
            def_id: method_def.sig.self_param.def_id,
            name: self_def.name.clone(),
            mode: method_def.sig.self_param.mode.clone(),
        });
        for param in &method_def.sig.params {
            let def = ctx
                .def_table
                .lookup_def(param.def_id)
                .unwrap_or_else(|| panic!("compiler bug: param def {:?} not found", param.def_id));
            params.push(LoweredParam {
                id: param.id,
                def_id: param.def_id,
                name: def.name.clone(),
                mode: param.mode.clone(),
            });
        }
        Self::new(
            ctx,
            method_def.id,
            &method_def.sig.name,
            &method_def.body,
            params,
            global_interner,
            drop_glue,
            trace_alloc,
            None,
        )
    }

    fn new(
        ctx: &'a AnalyzedContext,
        func_id: NodeId,
        func_name: &'a str,
        func_body: &'a ValueExpr,
        params: Vec<LoweredParam>,
        global_interner: &'a mut GlobalInterner,
        drop_glue: &'a mut DropGlueRegistry,
        trace_alloc: bool,
        func_return_ty: Option<Type>,
    ) -> Self {
        let mut ty_lowerer = TyLowerer::new();

        // Lower the function return type
        let ast_ret_ty = func_return_ty.clone().unwrap_or_else(|| {
            ctx.type_map
                .lookup_node_type(func_id)
                .expect("Function return type not found")
        });
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
            out_param_defs.insert(param.def_id);
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
            loop_stack: Vec::new(),
            drop_glue,
            trace_alloc,
            func_return_ty,
        }
    }

    pub(super) fn ret_type(&self) -> Result<Type, LowerError> {
        if let Some(ty) = &self.func_return_ty {
            Ok(ty.clone())
        } else {
            self.ty_for_node(self.func_id)
        }
    }

    pub(super) fn push_loop_context(&mut self, break_bb: BlockId, continue_bb: BlockId) {
        let drop_depth = self.drop_scopes.len();
        self.loop_stack.push(LoopContext {
            break_bb,
            continue_bb,
            drop_depth,
        });
    }

    pub(super) fn pop_loop_context(&mut self) {
        self.loop_stack.pop();
    }

    pub(super) fn current_loop_context(&self) -> Option<LoopContext> {
        self.loop_stack.last().copied()
    }

    pub(super) fn is_block_terminated(&self, block: BlockId) -> bool {
        !matches!(
            self.fb.body.blocks[block.index()].terminator,
            Terminator::Unterminated
        )
    }

    pub(super) fn is_curr_block_terminated(&self) -> bool {
        self.is_block_terminated(self.curr_block)
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
            let ty = self.def_ty_for_id(param.def_id, param.id)?;
            let ty_id = self.ty_lowerer.lower_ty(&ty);
            let local_id = self.fb.new_local(
                ty_id,
                LocalKind::Param { index: i as u32 },
                Some(param.name.clone()),
            );
            self.locals.insert(param.def_id, local_id);
            // `sink` params are treated as owned values; register for drop.
            // (`in`/`inout` params are borrowed)
            if param.mode == ParamMode::Sink {
                let is_initialized = self.create_is_initialized(&param.name, &ty, true);
                self.register_drop(param.def_id, &ty, is_initialized);
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
            if !self.is_curr_block_terminated() {
                if let Value::Scalar(op) = body_value {
                    let ret_ast_ty = self.ret_type()?;
                    self.emit_conversion_check(&body_ty, &ret_ast_ty, &op);
                    self.emit_copy_scalar(ret_place, Rvalue::Use(op));
                }
            }
        } else if !self.is_curr_block_terminated() {
            let ret_place = Place::<Aggregate>::new(ret_id, ret_ty, vec![]);
            self.lower_agg_value_into(ret_place, self.func_body)?;
        }

        // Drop any remaining owned locals before returning.
        if self.is_curr_block_terminated() {
            self.drop_scopes.clear();
        } else {
            self.exit_drop_scope()?;
            self.fb.set_terminator(self.curr_block, Terminator::Return);
        }

        self.fb.body.types = std::mem::take(&mut self.ty_lowerer.table);

        Ok(self.fb.body.clone())
    }
}

/// Lower all functions in a module into MCIR funcs.
pub fn lower_ast(
    ctx: AnalyzedContext,
    trace_alloc: bool,
) -> Result<LoweredMcirContext, LowerError> {
    let mut funcs = Vec::new();

    // Interned globals
    let mut global_interner = GlobalInterner::new();

    // Drop glue registry
    let mut drop_glue = DropGlueRegistry::new(ctx.def_table.next_def_id());

    // Lower all callables (functions + methods).
    for callable in ctx.module.callables() {
        let (def_id, body) = match callable {
            CallableRef::FuncDecl(_) => continue,
            CallableRef::MethodDecl { .. } => continue,
            CallableRef::FuncDef(func_def) => (
                func_def.def_id,
                FuncLowerer::new_function(
                    &ctx,
                    func_def,
                    &mut global_interner,
                    &mut drop_glue,
                    trace_alloc,
                )
                .lower()?,
            ),
            CallableRef::MethodDef { method_def, .. } => (
                method_def.def_id,
                FuncLowerer::new_method(
                    &ctx,
                    method_def,
                    &mut global_interner,
                    &mut drop_glue,
                    trace_alloc,
                )
                .lower()?,
            ),
        };

        funcs.push(LoweredFunc { def_id, body });
    }

    // Register generated drop glue functions
    let mut ctx = ctx;
    for generated in drop_glue.drain() {
        ctx.symbols
            .register_generated_def(generated.def_id, generated.name);
        funcs.push(LoweredFunc {
            def_id: generated.def_id,
            body: generated.body,
        });
    }

    Ok(ctx.with_funcs(funcs, global_interner.take()))
}

#[cfg(test)]
#[path = "../tests/lower/t_lower_ast.rs"]
mod tests;
