//! For loop desugaring.
//!
//! Transforms `for` loops into `while` loops with explicit index management.
//! This simplifies lowering by eliminating the need for special iterator
//! handling—lowering only needs to handle `while` loops.
//!
//! ## Desugaring pattern
//!
//! A for loop over an array or slice:
//! ```text
//! for elem in arr { body }
//! ```
//!
//! Becomes:
//! ```text
//! {
//!     let iter = arr;
//!     let len = len(iter);    // or constant for fixed-size arrays
//!     var idx: u64 = 0;
//!     while idx < len {
//!         let cur = idx;
//!         idx = idx + 1;      // increment before body (so `continue` works)
//!         let elem = iter[cur];
//!         body
//!     }
//! }
//! ```
//!
//! For range loops (`for i in 0..10`), the iterator binding is omitted
//! and the element is just the current index value.

use crate::core::analysis::facts::SyntheticReason;
use crate::core::ast::{
    ArrayLitInit, BinaryOp, BindPattern, BindPatternKind, BlockItem, CallArg, CallArgMode,
    EmitKind, Expr, ExprKind, InitInfo, MatchArm, MatchPattern, MethodItem, Module, ParamMode,
    StmtExpr, StmtExprKind, TopLevelItem, UnaryOp, UsingBinding,
};
use crate::core::diag::Span;
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::plans::{
    AbstractIterableForKernel, ArgLowering, CallInput, CallPlan, CallTarget, ForKernel,
    IntrinsicForKernel, ProtocolForKernel, RuntimeCall,
};
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::types::Type;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// Metadata for a synthesized local variable in the desugared loop.
struct ForLocal {
    def_id: DefId,
    name: String,
    ty: Type,
    pattern: BindPattern,
}

/// Metadata for a synthesized match binding used inside desugared protocol loops.
struct MatchBindingLocal {
    def_id: DefId,
    name: String,
    pattern_id: crate::core::ast::NodeId,
}

/// Known `using` cleanup entrypoints keyed by resource type name.
struct UsingCleanupMethods {
    close_ignore_error_by_type: HashMap<String, DefId>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CleanupBoundary {
    Normal,
    Loop,
}

struct CleanupFrame {
    deferred: Vec<Expr>,
    boundary: CleanupBoundary,
}

struct SyntaxDesugarCtx<'a, 'b> {
    elaborator: &'a mut Elaborator<'b>,
    cleanup_methods: UsingCleanupMethods,
    cleanup_frames: Vec<CleanupFrame>,
}

impl<'a, 'b> Deref for SyntaxDesugarCtx<'a, 'b> {
    type Target = Elaborator<'b>;

    fn deref(&self) -> &Self::Target {
        self.elaborator
    }
}

impl<'a, 'b> DerefMut for SyntaxDesugarCtx<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.elaborator
    }
}

pub(super) fn run(elaborator: &mut Elaborator<'_>, module: &mut Module) {
    let cleanup_methods = collect_using_cleanup_methods(module, &*elaborator.def_table);
    let mut ctx = SyntaxDesugarCtx {
        elaborator,
        cleanup_methods,
        cleanup_frames: Vec::new(),
    };
    ctx.desugar_module(module);
}

fn collect_using_cleanup_methods(
    module: &Module,
    def_table: &impl Deref<Target = DefTable>,
) -> UsingCleanupMethods {
    let mut close_ignore_error_by_type = HashMap::new();
    for block in module.method_blocks() {
        for item in &block.method_items {
            let MethodItem::Def(def) = item else {
                continue;
            };
            if def.sig.name == "close_ignore_error" {
                close_ignore_error_by_type
                    .insert(block.type_name.clone(), def_table.def_id(def.id));
            }
        }
    }
    UsingCleanupMethods {
        close_ignore_error_by_type,
    }
}

impl<'a, 'b> SyntaxDesugarCtx<'a, 'b> {
    fn desugar_module(&mut self, module: &mut Module) {
        for item in &mut module.top_level_items {
            self.desugar_top_level_item(item);
        }
    }

    fn desugar_top_level_item(&mut self, item: &mut TopLevelItem) {
        match item {
            TopLevelItem::FuncDef(def) => self.desugar_value_expr(&mut def.body),
            TopLevelItem::MethodBlock(block) => {
                for method_item in &mut block.method_items {
                    if let MethodItem::Def(def) = method_item {
                        self.desugar_value_expr(&mut def.body);
                    }
                }
            }
            TopLevelItem::TraitDef(_)
            | TopLevelItem::TypeDef(_)
            | TopLevelItem::FuncDecl(_)
            | TopLevelItem::MachineDef(_)
            | TopLevelItem::ClosureDef(_) => {}
        }
    }

    fn collect_cleanup_for_return(&self) -> Vec<Expr> {
        self.cleanup_frames
            .iter()
            .rev()
            .flat_map(|frame| frame.deferred.iter().rev().cloned())
            .collect()
    }

    fn collect_cleanup_for_loop_exit(&self) -> Vec<Expr> {
        let mut values = Vec::new();
        for frame in self.cleanup_frames.iter().rev() {
            values.extend(frame.deferred.iter().rev().cloned());
            if frame.boundary == CleanupBoundary::Loop {
                break;
            }
        }
        values
    }

    fn collect_cleanup_for_try_propagate(&self) -> Vec<Expr> {
        self.collect_cleanup_for_return()
    }

    fn append_cleanup_before_stmt(&self, rewritten: &mut Vec<BlockItem>, cleanup: Vec<Expr>) {
        // Cleanup runs in LIFO order before the control transfer leaves the
        // scopes represented by the active cleanup frames.
        for value in cleanup {
            rewritten.push(BlockItem::Expr(value));
        }
    }

    /// Desugar a for loop into a while loop with explicit index management.
    fn desugar_for_stmt(
        &mut self,
        stmt: &StmtExpr,
        pattern: &BindPattern,
        iter: &Expr,
        body: &Expr,
    ) -> Expr {
        let span = stmt.span;
        let u64_ty = Type::uint(64);
        let bool_ty = Type::Bool;
        let for_plan = self.for_plan_or_panic(stmt.id, "for desugaring");

        let mut items = Vec::new();

        let (iter_place, idx_place, len_value, elem_ty, is_range) = match &for_plan.kernel {
            ForKernel::Intrinsic(IntrinsicForKernel::Range) => {
                let ExprKind::Range { start, end } = &iter.kind else {
                    panic!("compiler bug: range for-plan on non-range iterator");
                };
                let start_expr = (**start).clone();
                let len_expr = (**end).clone();

                let idx_info = self.new_for_local("idx", u64_ty.clone(), true, span);
                let idx_stmt = self.make_var_bind_stmt(idx_info.pattern.clone(), start_expr, span);
                items.push(BlockItem::Stmt(idx_stmt));

                let len_info = self.new_for_local("len", u64_ty.clone(), false, span);
                let len_stmt = self.make_let_bind_stmt(len_info.pattern.clone(), len_expr, span);
                items.push(BlockItem::Stmt(len_stmt));

                let idx_place = self.make_var_place(&idx_info, span);
                let len_place = self.make_var_place(&len_info, span);
                (
                    None,
                    idx_place,
                    self.make_len_load(len_place, span),
                    u64_ty.clone(),
                    true,
                )
            }
            ForKernel::Intrinsic(kernel) => {
                let iter_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(iter.id))
                    .clone();
                let elem_ty = for_plan.item_ty.clone();
                let iter_info = self.new_for_local("iter", iter_ty.clone(), false, span);
                let iter_value = iter.clone();
                let iter_stmt =
                    self.make_let_bind_stmt(iter_info.pattern.clone(), iter_value, span);
                items.push(BlockItem::Stmt(iter_stmt));

                let len_expr = match kernel {
                    IntrinsicForKernel::Array => {
                        let Type::Array { dims, .. } = &iter_ty else {
                            panic!("compiler bug: array for-plan on non-array iterator");
                        };
                        let len = dims
                            .first()
                            .copied()
                            .unwrap_or_else(|| panic!("compiler bug: empty array dims"));
                        self.make_u64_lit(len as u64, span)
                    }
                    IntrinsicForKernel::Slice | IntrinsicForKernel::DynArray => {
                        let iter_place = self.make_var_place(&iter_info, span);
                        self.make_len_expr(iter_place, span)
                    }
                    IntrinsicForKernel::String => {
                        let iter_place = self.make_var_place(&iter_info, span);
                        self.make_len_expr(iter_place, span)
                    }
                    IntrinsicForKernel::Map => {
                        panic!(
                            "compiler bug: map for-loops use the dedicated cursor desugaring path"
                        )
                    }
                    IntrinsicForKernel::Range => {
                        panic!("compiler bug: range iteration should take the range path")
                    }
                };

                let len_info = self.new_for_local("len", u64_ty.clone(), false, span);
                let len_stmt = self.make_let_bind_stmt(len_info.pattern.clone(), len_expr, span);
                items.push(BlockItem::Stmt(len_stmt));

                let idx_info = self.new_for_local("idx", u64_ty.clone(), true, span);
                let idx_init = self.make_u64_lit(0, span);
                let idx_stmt = self.make_var_bind_stmt(idx_info.pattern.clone(), idx_init, span);
                items.push(BlockItem::Stmt(idx_stmt));

                let idx_place = self.make_var_place(&idx_info, span);
                let len_place = self.make_var_place(&len_info, span);
                let iter_place = self.make_var_place(&iter_info, span);
                (
                    Some(iter_place),
                    idx_place,
                    self.make_len_load(len_place, span),
                    elem_ty,
                    false,
                )
            }
            ForKernel::AbstractIterable(_) | ForKernel::Protocol(_) => {
                panic!(
                    "compiler bug: protocol for-loops use the dedicated protocol desugaring path"
                )
            }
        };

        let idx_load = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
        let cond_expr = self.make_binop_expr(BinaryOp::Lt, idx_load, len_value, bool_ty, span);

        let mut loop_items = Vec::new();

        let cur_info = self.new_for_local("cur", u64_ty.clone(), false, span);
        let cur_load = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
        let cur_stmt = self.make_let_bind_stmt(cur_info.pattern.clone(), cur_load, span);
        loop_items.push(BlockItem::Stmt(cur_stmt));

        // Increment before the body so `continue` doesn't skip the step.
        let idx_next = {
            let idx_val = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
            let one = self.make_u64_lit(1, span);
            self.make_binop_expr(BinaryOp::Add, idx_val, one, u64_ty.clone(), span)
        };
        let idx_assign = self.make_assign_stmt(idx_place.clone(), idx_next, span);
        loop_items.push(BlockItem::Stmt(idx_assign));

        let elem_expr = if is_range {
            let cur_place = self.make_var_place(&cur_info, span);
            self.make_load_expr(cur_place, u64_ty.clone(), span)
        } else {
            let iter_place = iter_place
                .unwrap_or_else(|| panic!("compiler bug: missing iter place for iterable loop"));
            let cur_place = self.make_var_place(&cur_info, span);
            let cur_value = self.make_load_expr(cur_place, u64_ty.clone(), span);
            match &for_plan.kernel {
                ForKernel::Intrinsic(IntrinsicForKernel::String) => {
                    let index_place =
                        self.make_index_place(iter_place, cur_value, Type::uint(8), span);
                    self.make_load_expr(index_place, Type::uint(8), span)
                }
                ForKernel::Intrinsic(IntrinsicForKernel::Map) => {
                    panic!("compiler bug: map for-loops use the dedicated cursor desugaring path")
                }
                ForKernel::AbstractIterable(_) | ForKernel::Protocol(_) => {
                    panic!(
                        "compiler bug: protocol for-loops use the dedicated protocol desugaring path"
                    )
                }
                _ => {
                    let index_place =
                        self.make_index_place(iter_place, cur_value, elem_ty.clone(), span);
                    self.make_load_expr(index_place, elem_ty.clone(), span)
                }
            }
        };

        let pattern_stmt = self.make_let_bind_stmt(pattern.clone(), elem_expr, span);
        loop_items.push(BlockItem::Stmt(pattern_stmt));

        loop_items.push(BlockItem::Expr(body.clone()));

        let loop_body = self.make_block_expr(loop_items, span);
        let while_stmt = self.make_stmt(
            StmtExprKind::While {
                cond: Box::new(cond_expr),
                body: Box::new(loop_body),
            },
            span,
        );
        items.push(BlockItem::Stmt(while_stmt));

        self.make_block_expr(items, span)
    }

    fn desugar_map_for_stmt(
        &mut self,
        stmt: &StmtExpr,
        pattern: &BindPattern,
        iter: &Expr,
        body: &Expr,
    ) -> Expr {
        let span = stmt.span;
        let for_plan = self.for_plan_or_panic(stmt.id, "map for desugaring");
        let ForKernel::Intrinsic(IntrinsicForKernel::Map) = &for_plan.kernel else {
            panic!("compiler bug: expected map for-plan for {:?}", stmt.id);
        };

        let iter_ty = self
            .type_map
            .type_table()
            .get(self.type_id_for(iter.id))
            .clone();
        let Type::Map { key_ty, value_ty } = iter_ty.clone() else {
            panic!("compiler bug: map for-plan on non-map iterator");
        };

        let cursor_ty = Type::uint(32);
        let bool_ty = Type::Bool;

        let iter_info = self.new_for_local("iter", iter_ty, false, span);
        let cursor_info = self.new_for_local("cursor", cursor_ty.clone(), true, span);

        let mut items = Vec::new();
        items.push(BlockItem::Stmt(self.make_let_bind_stmt(
            iter_info.pattern.clone(),
            iter.clone(),
            span,
        )));

        let iter_place = self.make_var_place(&iter_info, span);
        let init_arg = self.make_call_arg(CallArgMode::Default, iter_place.clone(), span);
        let cursor_init = self.make_runtime_call_expr(
            RuntimeCall::MapIterInit,
            vec![init_arg],
            cursor_ty.clone(),
            vec![ParamMode::In],
            span,
        );
        items.push(BlockItem::Stmt(self.make_var_bind_stmt(
            cursor_info.pattern.clone(),
            cursor_init,
            span,
        )));

        let cursor_place = self.make_var_place(&cursor_info, span);
        let is_done_cursor = self.make_load_expr(cursor_place.clone(), cursor_ty.clone(), span);
        let is_done_args = vec![
            self.make_call_arg(CallArgMode::Default, iter_place.clone(), span),
            self.make_call_arg(CallArgMode::Default, is_done_cursor, span),
        ];
        let is_done = self.make_runtime_call_expr(
            RuntimeCall::MapIterIsDone,
            is_done_args,
            bool_ty.clone(),
            vec![ParamMode::In, ParamMode::In],
            span,
        );
        let cond_expr = self.make_unary_expr(UnaryOp::LogicalNot, is_done, bool_ty, span);

        let key_info = self.new_for_local("key", (*key_ty).clone(), true, span);
        let value_info = self.new_for_local("value", (*value_ty).clone(), true, span);
        let key_place = self.make_var_place(&key_info, span);
        let value_place = self.make_var_place(&value_info, span);

        let load_cursor = self.make_load_expr(cursor_place.clone(), cursor_ty.clone(), span);
        let mut load_args = vec![
            self.make_call_arg(CallArgMode::Default, iter_place.clone(), span),
            self.make_call_arg(CallArgMode::Default, load_cursor, span),
        ];
        let mut load_modes = vec![ParamMode::In, ParamMode::In];
        let load_runtime = if matches!(key_ty.as_ref(), Type::String) {
            RuntimeCall::MapIterLoadStringKey
        } else {
            let key_size = self.make_u64_lit(key_ty.size_of() as u64, span);
            load_args.push(self.make_call_arg(CallArgMode::Default, key_size, span));
            load_modes.push(ParamMode::In);
            RuntimeCall::MapIterLoadBytes
        };
        let value_size = self.make_u64_lit(value_ty.size_of() as u64, span);
        load_args.push(self.make_call_arg(CallArgMode::Default, value_size, span));
        load_modes.push(ParamMode::In);
        load_args.push(self.make_call_arg(CallArgMode::Out, key_place.clone(), span));
        load_args.push(self.make_call_arg(CallArgMode::Out, value_place.clone(), span));
        load_modes.push(ParamMode::Out);
        load_modes.push(ParamMode::Out);
        let load_entry =
            self.make_runtime_call_expr(load_runtime, load_args, Type::Unit, load_modes, span);

        let advance_cursor = self.make_load_expr(cursor_place.clone(), cursor_ty.clone(), span);
        let advance_args = vec![
            self.make_call_arg(CallArgMode::Default, iter_place, span),
            self.make_call_arg(CallArgMode::Default, advance_cursor, span),
        ];
        let advance = self.make_runtime_call_expr(
            RuntimeCall::MapIterAdvance,
            advance_args,
            cursor_ty.clone(),
            vec![ParamMode::In, ParamMode::In],
            span,
        );

        let moved_key = self.make_move_expr(key_place, (*key_ty).clone(), span);
        let moved_value = self.make_move_expr(value_place, (*value_ty).clone(), span);
        let item_tuple =
            self.make_tuple_expr(vec![moved_key, moved_value], for_plan.item_ty.clone(), span);

        let loop_items = vec![
            BlockItem::Stmt(self.make_var_decl_stmt(&key_info, span)),
            BlockItem::Stmt(self.make_var_decl_stmt(&value_info, span)),
            BlockItem::Expr(load_entry),
            BlockItem::Stmt(self.make_assign_stmt(cursor_place, advance, span)),
            BlockItem::Stmt(self.make_let_bind_stmt(pattern.clone(), item_tuple, span)),
            BlockItem::Expr(body.clone()),
        ];

        let loop_body = self.make_block_expr(loop_items, span);
        items.push(BlockItem::Stmt(self.make_stmt(
            StmtExprKind::While {
                cond: Box::new(cond_expr),
                body: Box::new(loop_body),
            },
            span,
        )));

        self.make_block_expr(items, span)
    }

    fn desugar_protocol_for_stmt(
        &mut self,
        stmt: &StmtExpr,
        pattern: &BindPattern,
        iter: &Expr,
        body: &Expr,
    ) -> Expr {
        let span = stmt.span;
        let bool_ty = Type::Bool;
        let for_plan = self.for_plan_or_panic(stmt.id, "protocol for desugaring");
        let (iter_ty, done_ty, propagated_err_tys, iter_method, next_method) = match &for_plan
            .kernel
        {
            ForKernel::Protocol(ProtocolForKernel {
                iter_ty,
                done_ty,
                propagated_err_tys,
                iter_method,
                next_method,
                ..
            }) => (
                iter_ty,
                done_ty,
                propagated_err_tys,
                Some(*iter_method),
                Some(*next_method),
            ),
            ForKernel::AbstractIterable(AbstractIterableForKernel { .. }) => {
                panic!(
                    "compiler bug: abstract Iterable<T> for-plan must be specialized before elaboration for {:?}",
                    stmt.id
                );
            }
            _ => panic!("compiler bug: expected protocol for-plan for {:?}", stmt.id),
        };

        let src_ty = self
            .type_map
            .type_table()
            .get(self.type_id_for(iter.id))
            .clone();
        let mut step_err_tys = propagated_err_tys.clone();
        step_err_tys.push(done_ty.clone());
        let src_info = self.new_for_local("iter_src", src_ty.clone(), false, span);
        let iter_info = self.new_for_local("iter_state", iter_ty.clone(), true, span);
        let step_info = self.new_for_local(
            "iter_step",
            Type::ErrorUnion {
                ok_ty: Box::new(for_plan.item_ty.clone()),
                err_tys: step_err_tys,
            },
            false,
            span,
        );
        let item_binding = self.new_match_binding("iter_item", &for_plan.item_ty, span);
        let done_binding = self.new_match_binding("iter_done", done_ty, span);
        let err_bindings = propagated_err_tys
            .iter()
            .map(|err_ty| {
                (
                    err_ty.clone(),
                    self.new_match_binding("iter_err", err_ty, span),
                )
            })
            .collect::<Vec<_>>();

        let mut items = Vec::new();
        items.push(BlockItem::Stmt(self.make_let_bind_stmt(
            src_info.pattern.clone(),
            iter.clone(),
            span,
        )));

        let src_place = self.make_var_place(&src_info, span);
        let src_value = self.make_load_expr(src_place, src_ty, span);
        let iter_init = self.make_direct_method_call_expr(
            iter_method.expect("protocol for-plan must have iter method"),
            "iter",
            src_value,
            ParamMode::In,
            iter_ty.clone(),
            span,
        );
        items.push(BlockItem::Stmt(self.make_var_bind_stmt(
            iter_info.pattern.clone(),
            iter_init,
            span,
        )));

        let cond_expr = self.make_value_expr(ExprKind::BoolLit(true), bool_ty, span);
        let iter_place = self.make_var_place(&iter_info, span);
        let next_step = self.make_direct_method_call_expr(
            next_method.expect("protocol for-plan must have next method"),
            "next",
            iter_place.clone(),
            ParamMode::InOut,
            step_info.ty.clone(),
            span,
        );

        let step_stmt = self.make_let_bind_stmt(step_info.pattern.clone(), next_step, span);
        let step_place = self.make_var_place(&step_info, span);
        let step_value = self.make_load_expr(step_place, step_info.ty.clone(), span);

        let break_stmt = self.make_stmt(StmtExprKind::Break, span);
        let done_body = self.make_block_expr(vec![BlockItem::Stmt(break_stmt)], span);
        let done_arm = MatchArm {
            id: self.node_id_gen.new_id(),
            patterns: vec![MatchPattern::TypedBinding {
                id: done_binding.pattern_id,
                ident: done_binding.name.clone(),
                ty_expr: self.type_expr_from_type(done_ty, span),
                span,
            }],
            body: done_body,
            span,
        };

        let item_place = self.make_place_expr(
            ExprKind::Var {
                ident: item_binding.name.clone(),
            },
            for_plan.item_ty.clone(),
            span,
            Some(item_binding.def_id),
        );
        let item_value = self.make_load_expr(item_place, for_plan.item_ty.clone(), span);
        let pattern_bind = self.make_let_bind_stmt(pattern.clone(), item_value, span);
        let item_body = self.make_block_expr(
            vec![BlockItem::Stmt(pattern_bind), BlockItem::Expr(body.clone())],
            span,
        );
        let item_arm = MatchArm {
            id: self.node_id_gen.new_id(),
            patterns: vec![MatchPattern::TypedBinding {
                id: item_binding.pattern_id,
                ident: item_binding.name.clone(),
                ty_expr: self.type_expr_from_type(&for_plan.item_ty, span),
                span,
            }],
            body: item_body,
            span,
        };

        let mut match_arms = vec![done_arm];
        for (err_ty, err_binding) in err_bindings {
            let err_place = self.make_place_expr(
                ExprKind::Var {
                    ident: err_binding.name.clone(),
                },
                err_ty.clone(),
                span,
                Some(err_binding.def_id),
            );
            let err_value = self.make_load_expr(err_place, err_ty.clone(), span);
            let return_stmt = self.make_return_stmt(Some(err_value), span);
            let err_body = self.make_block_expr(vec![BlockItem::Stmt(return_stmt)], span);
            match_arms.push(MatchArm {
                id: self.node_id_gen.new_id(),
                patterns: vec![MatchPattern::TypedBinding {
                    id: err_binding.pattern_id,
                    ident: err_binding.name.clone(),
                    ty_expr: self.type_expr_from_type(&err_ty, span),
                    span,
                }],
                body: err_body,
                span,
            });
        }
        match_arms.push(item_arm);
        let step_match = self.make_value_expr(
            ExprKind::Match {
                scrutinee: Box::new(step_value),
                arms: match_arms.clone(),
            },
            Type::Unit,
            span,
        );
        let match_plan = self.build_match_plan(step_match.id, step_info.ty.clone(), &match_arms);
        self.record_match_plan(step_match.id, match_plan);

        let loop_body = self.make_block_expr(
            vec![BlockItem::Stmt(step_stmt), BlockItem::Expr(step_match)],
            span,
        );
        items.push(BlockItem::Stmt(self.make_stmt(
            StmtExprKind::While {
                cond: Box::new(cond_expr),
                body: Box::new(loop_body),
            },
            span,
        )));

        self.make_block_expr(items, span)
    }

    fn desugar_block_items(&mut self, items: &mut Vec<BlockItem>, boundary: CleanupBoundary) {
        let frame_start = self.cleanup_frames.len();
        self.cleanup_frames.push(CleanupFrame {
            deferred: Vec::new(),
            boundary,
        });
        let mut rewritten = Vec::with_capacity(items.len());
        for item in items.drain(..) {
            match item {
                BlockItem::Expr(mut expr) => {
                    self.desugar_value_expr(&mut expr);
                    rewritten.push(BlockItem::Expr(expr));
                }
                BlockItem::Stmt(mut stmt) => {
                    self.desugar_stmt_expr(&mut stmt);
                    match &stmt.kind {
                        StmtExprKind::For {
                            pattern,
                            iter,
                            body,
                        } => {
                            let for_plan =
                                self.for_plan_or_panic(stmt.id, "block-item for desugaring");
                            let mut expr = match &for_plan.kernel {
                                ForKernel::Intrinsic(IntrinsicForKernel::Map) => {
                                    self.desugar_map_for_stmt(&stmt, pattern, iter, body)
                                }
                                ForKernel::AbstractIterable(_) | ForKernel::Protocol(_) => {
                                    self.desugar_protocol_for_stmt(&stmt, pattern, iter, body)
                                }
                                _ => self.desugar_for_stmt(&stmt, pattern, iter, body),
                            };
                            self.desugar_value_expr(&mut expr);
                            rewritten.push(BlockItem::Expr(expr));
                        }
                        StmtExprKind::Defer { value } => {
                            self.cleanup_frames
                                .last_mut()
                                .expect("cleanup frame must exist while desugaring a block")
                                .deferred
                                .push((**value).clone());
                        }
                        StmtExprKind::Using {
                            binding,
                            value,
                            body,
                        } => {
                            let mut expr = self.desugar_using_stmt(&stmt, binding, value, body);
                            self.desugar_value_expr(&mut expr);
                            rewritten.push(BlockItem::Expr(expr));
                        }
                        StmtExprKind::Return { .. } => {
                            self.append_cleanup_before_stmt(
                                &mut rewritten,
                                self.collect_cleanup_for_return(),
                            );
                            rewritten.push(BlockItem::Stmt(stmt));
                        }
                        StmtExprKind::Break | StmtExprKind::Continue => {
                            self.append_cleanup_before_stmt(
                                &mut rewritten,
                                self.collect_cleanup_for_loop_exit(),
                            );
                            rewritten.push(BlockItem::Stmt(stmt));
                        }
                        _ => rewritten.push(BlockItem::Stmt(stmt)),
                    }
                }
            }
        }
        let frame = self
            .cleanup_frames
            .pop()
            .expect("cleanup frame must exist while finishing a block");
        // Ordinary block fallthrough runs only the defers registered in this
        // block. Outer frames stay active for their own surrounding scopes.
        for value in frame.deferred.into_iter().rev() {
            rewritten.push(BlockItem::Expr(value));
        }
        debug_assert_eq!(self.cleanup_frames.len(), frame_start);
        *items = rewritten;
    }

    fn desugar_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        match &mut stmt.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                self.desugar_value_expr(value)
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.desugar_place_expr(assignee);
                self.desugar_value_expr(value);
            }
            StmtExprKind::While { cond, body } => {
                self.desugar_value_expr(cond);
                self.desugar_loop_body(body);
            }
            StmtExprKind::For { iter, body, .. } => {
                self.desugar_value_expr(iter);
                self.desugar_loop_body(body);
            }
            StmtExprKind::Defer { value } => {
                self.desugar_value_expr(value);
            }
            StmtExprKind::Using { value, body, .. } => {
                self.desugar_value_expr(value);
                self.desugar_value_expr(body);
            }
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.desugar_value_expr(value);
                }
            }
            StmtExprKind::VarDecl { .. }
            | StmtExprKind::Break
            | StmtExprKind::Continue
            | StmtExprKind::CompoundAssign { .. } => {}
        }
    }

    fn desugar_place_expr(&mut self, place: &mut Expr) {
        match &mut place.kind {
            ExprKind::Var { .. } => {}
            ExprKind::Deref { expr } => self.desugar_value_expr(expr),
            ExprKind::ArrayIndex { target, indices } => {
                self.desugar_place_expr(target);
                for index in indices {
                    self.desugar_value_expr(index);
                }
            }
            ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
                self.desugar_place_expr(target)
            }
            _ => {}
        }
    }

    fn desugar_call_arg(&mut self, arg: &mut CallArg) {
        self.desugar_value_expr(&mut arg.expr);
    }

    fn desugar_using_stmt(
        &mut self,
        stmt: &StmtExpr,
        binding: &UsingBinding,
        value: &Expr,
        body: &Expr,
    ) -> Expr {
        let pattern = BindPattern {
            id: binding.id,
            kind: BindPatternKind::Name {
                ident: binding.ident.clone(),
            },
            span: binding.span,
        };

        let cleanup = self
            .make_using_cleanup_expr(&pattern, value, stmt.span)
            .unwrap_or_else(|| {
                panic!(
                    "compiler bug: missing `close_ignore_error` cleanup for using-bound type {}",
                    self.type_map.type_table().get(self.type_id_for(value.id))
                )
            });

        // `using` lowers to a dedicated block so the binding and its deferred
        // cleanup stay scoped to the body region.
        let items = vec![
            BlockItem::Stmt(self.make_let_bind_stmt(pattern.clone(), value.clone(), stmt.span)),
            BlockItem::Stmt(self.make_stmt(
                StmtExprKind::Defer {
                    value: Box::new(cleanup),
                },
                stmt.span,
            )),
            BlockItem::Expr(body.clone()),
        ];
        self.make_block_expr(items, stmt.span)
    }

    fn desugar_value_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Block { items, tail } => {
                self.desugar_block_items(items, CleanupBoundary::Normal);
                if let Some(tail) = tail {
                    self.desugar_value_expr(tail);
                }
            }
            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.desugar_value_expr(elem);
                    }
                }
                ArrayLitInit::Repeat(value, ..) => self.desugar_value_expr(value),
            },
            ExprKind::SetLit { elems, .. } => {
                for elem in elems {
                    self.desugar_value_expr(elem);
                }
            }
            ExprKind::MapLit { entries, .. } => {
                for entry in entries {
                    self.desugar_value_expr(&mut entry.key);
                    self.desugar_value_expr(&mut entry.value);
                }
            }
            ExprKind::TupleLit(items) | ExprKind::EnumVariant { payload: items, .. } => {
                for item in items {
                    self.desugar_value_expr(item);
                }
            }
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.desugar_value_expr(&mut field.value);
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                self.desugar_value_expr(target);
                for field in fields {
                    self.desugar_value_expr(&mut field.value);
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.desugar_value_expr(left);
                self.desugar_value_expr(right);
            }
            ExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                self.desugar_value_expr(fallible_expr);
                if let Some(handler) = on_error {
                    self.desugar_value_expr(handler);
                } else {
                    let cleanup = self.collect_cleanup_for_try_propagate();
                    if !cleanup.is_empty() {
                        // Bare `?` still returns from the surrounding callable,
                        // so record the active cleanup set for backend return
                        // lowering instead of rewriting the expression shape.
                        self.record_try_cleanup_plan(expr.id, cleanup);
                    }
                }
            }
            ExprKind::UnaryOp { expr, .. }
            | ExprKind::HeapAlloc { expr }
            | ExprKind::Coerce { expr, .. } => self.desugar_value_expr(expr),
            ExprKind::Move { expr }
            | ExprKind::ImplicitMove { expr }
            | ExprKind::AddrOf { expr }
            | ExprKind::Load { expr }
            | ExprKind::Len { expr } => self.desugar_place_expr(expr),
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.desugar_value_expr(cond);
                self.desugar_value_expr(then_body);
                self.desugar_value_expr(else_body);
            }
            ExprKind::Range { start, end } => {
                self.desugar_value_expr(start);
                self.desugar_value_expr(end);
            }
            ExprKind::Slice { target, start, end } => {
                self.desugar_place_expr(target);
                if let Some(start) = start {
                    self.desugar_value_expr(start);
                }
                if let Some(end) = end {
                    self.desugar_value_expr(end);
                }
            }
            ExprKind::MapGet { target, key } => {
                self.desugar_value_expr(target);
                self.desugar_value_expr(key);
            }
            ExprKind::Match { scrutinee, arms } => {
                self.desugar_value_expr(scrutinee);
                for arm in arms {
                    self.desugar_value_expr(&mut arm.body);
                }
            }
            ExprKind::Call { callee, args } => {
                self.desugar_value_expr(callee);
                for arg in args {
                    self.desugar_call_arg(arg);
                }
            }
            ExprKind::MethodCall { callee, args, .. } => {
                self.desugar_value_expr(callee);
                for arg in args {
                    self.desugar_call_arg(arg);
                }
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { to, payload } => {
                    self.desugar_value_expr(to);
                    self.desugar_value_expr(payload);
                }
                EmitKind::Request { to, payload, .. } => {
                    self.desugar_value_expr(to);
                    self.desugar_value_expr(payload);
                }
            },
            ExprKind::Reply { cap, value } => {
                self.desugar_value_expr(cap);
                self.desugar_value_expr(value);
            }
            ExprKind::UnitLit
            | ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::StringFmt { .. }
            | ExprKind::ClosureRef { .. }
            | ExprKind::RoleProjection { .. }
            | ExprKind::Var { .. }
            | ExprKind::Deref { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. }
            | ExprKind::ArrayIndex { .. }
            | ExprKind::Closure { .. } => {}
        }
    }

    fn desugar_loop_body(&mut self, body: &mut Expr) {
        match &mut body.kind {
            ExprKind::Block { items, tail } => {
                self.desugar_block_items(items, CleanupBoundary::Loop);
                if let Some(tail) = tail {
                    self.desugar_value_expr(tail);
                }
            }
            _ => self.desugar_value_expr(body),
        }
    }

    fn make_using_cleanup_expr(
        &mut self,
        pattern: &BindPattern,
        value: &Expr,
        span: Span,
    ) -> Option<Expr> {
        let ident = match &pattern.kind {
            BindPatternKind::Name { ident } => ident.clone(),
            _ => return None,
        };
        let def_id = self.def_table.def_id(pattern.id);
        let value_ty_id = self.type_id_for(value.id);
        let Type::Struct { name, .. } = self.type_map.type_table().get(value_ty_id).clone() else {
            return None;
        };
        let method_def_id = *self.cleanup_methods.close_ignore_error_by_type.get(&name)?;

        let receiver_ty = self.type_map.type_table().get(value_ty_id).clone();
        let receiver_place = self.make_place_expr(
            ExprKind::Var { ident },
            receiver_ty.clone(),
            span,
            Some(def_id),
        );
        let receiver_value = self.make_load_expr(receiver_place, receiver_ty, span);
        let expr_id = self.node_id_gen.new_id();
        let _ty_id = self.insert_synth_node_type(expr_id, Type::Unit);
        self.record_call_plan(
            expr_id,
            CallPlan {
                target: CallTarget::Direct(method_def_id),
                args: vec![ArgLowering::Direct(CallInput::Receiver)],
                drop_mask: vec![false],
                input_modes: vec![ParamMode::Sink],
                has_receiver: true,
            },
        );
        Some(Expr {
            id: expr_id,
            kind: ExprKind::MethodCall {
                callee: Box::new(receiver_value),
                method_name: "close_ignore_error".to_string(),
                args: vec![],
            },
            span,
        })
    }

    fn new_for_local(&mut self, suffix: &str, ty: Type, is_mutable: bool, span: Span) -> ForLocal {
        let hint = self.next_synthetic_def_id_hint();
        let name = format!("__for_{}_{}", suffix, hint.0);
        let def_id = self.add_typed_synthetic_def(
            name.clone(),
            DefKind::LocalVar {
                nrvo_eligible: false,
                is_mutable,
            },
            ty.clone(),
            SyntheticReason::ElaborateSyntheticNode,
        );
        let pattern_id = self.node_id_gen.new_id();
        self.def_table.record_use(pattern_id, def_id);
        let pattern = BindPattern {
            id: pattern_id,
            kind: BindPatternKind::Name {
                ident: name.clone(),
            },
            span,
        };
        ForLocal {
            def_id,
            name,
            ty,
            pattern,
        }
    }

    fn new_match_binding(&mut self, suffix: &str, ty: &Type, _span: Span) -> MatchBindingLocal {
        let hint = self.next_synthetic_def_id_hint();
        let name = format!("__for_{}_{}", suffix, hint.0);
        let def_id = self.add_typed_synthetic_def(
            name.clone(),
            DefKind::LocalVar {
                nrvo_eligible: false,
                is_mutable: false,
            },
            ty.clone(),
            SyntheticReason::ElaborateSyntheticNode,
        );
        let pattern_id = self.node_id_gen.new_id();
        self.def_table.record_use(pattern_id, def_id);
        MatchBindingLocal {
            def_id,
            name,
            pattern_id,
        }
    }

    fn make_var_place(&mut self, info: &ForLocal, span: Span) -> Expr {
        self.make_place_expr(
            ExprKind::Var {
                ident: info.name.clone(),
            },
            info.ty.clone(),
            span,
            Some(info.def_id),
        )
    }

    fn make_len_expr(&mut self, place: Expr, span: Span) -> Expr {
        self.make_value_expr(
            ExprKind::Len {
                expr: Box::new(place),
            },
            Type::uint(64),
            span,
        )
    }

    fn make_len_load(&mut self, place: Expr, span: Span) -> Expr {
        self.make_load_expr(place, Type::uint(64), span)
    }

    fn make_u64_lit(&mut self, value: u64, span: Span) -> Expr {
        self.make_value_expr(ExprKind::IntLit(value), Type::uint(64), span)
    }

    fn make_call_arg(&mut self, mode: CallArgMode, expr: Expr, span: Span) -> CallArg {
        let init = if matches!(mode, CallArgMode::Out) {
            self.init_info_for_id(expr.id)
        } else {
            InitInfo::default()
        };
        CallArg {
            label: None,
            mode,
            expr,
            init,
            span,
        }
    }

    fn make_unary_expr(&mut self, op: UnaryOp, expr: Expr, ty: Type, span: Span) -> Expr {
        self.make_value_expr(
            ExprKind::UnaryOp {
                op,
                expr: Box::new(expr),
            },
            ty,
            span,
        )
    }

    fn make_tuple_expr(&mut self, items: Vec<Expr>, ty: Type, span: Span) -> Expr {
        self.make_value_expr(ExprKind::TupleLit(items), ty, span)
    }

    fn make_move_expr(&mut self, place: Expr, ty: Type, span: Span) -> Expr {
        self.make_value_expr(
            ExprKind::Move {
                expr: Box::new(place),
            },
            ty,
            span,
        )
    }

    fn make_binop_expr(
        &mut self,
        op: BinaryOp,
        left: Expr,
        right: Expr,
        result_ty: Type,
        span: Span,
    ) -> Expr {
        self.make_value_expr(
            ExprKind::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            result_ty,
            span,
        )
    }

    fn make_index_place(&mut self, target: Expr, index: Expr, elem_ty: Type, span: Span) -> Expr {
        let id = self.node_id_gen.new_id();
        let target_ty = self
            .type_map
            .type_table()
            .get(self.type_id_for(target.id))
            .clone();
        let plan = self.build_index_plan(&target_ty);
        self.record_index_plan(id, plan);
        let _ty_id = self.insert_synth_node_type(id, elem_ty);

        Expr {
            id,
            kind: ExprKind::ArrayIndex {
                target: Box::new(target),
                indices: vec![index],
            },
            span,
        }
    }

    fn make_load_expr(&mut self, place: Expr, ty: Type, span: Span) -> Expr {
        self.make_value_expr(
            ExprKind::Load {
                expr: Box::new(place),
            },
            ty,
            span,
        )
    }

    fn make_let_bind_stmt(&mut self, pattern: BindPattern, value: Expr, span: Span) -> StmtExpr {
        self.make_stmt(
            StmtExprKind::LetBind {
                pattern,
                decl_ty: None,
                value: Box::new(value),
            },
            span,
        )
    }

    fn make_var_bind_stmt(&mut self, pattern: BindPattern, value: Expr, span: Span) -> StmtExpr {
        self.make_stmt(
            StmtExprKind::VarBind {
                pattern,
                decl_ty: None,
                value: Box::new(value),
            },
            span,
        )
    }

    fn make_var_decl_stmt(&mut self, info: &ForLocal, span: Span) -> StmtExpr {
        let decl_ty = self.type_expr_from_type(&info.ty, span);
        let stmt = self.make_stmt(
            StmtExprKind::VarDecl {
                ident: info.name.clone(),
                decl_ty,
            },
            span,
        );
        self.def_table.record_use(stmt.id, info.def_id);
        stmt
    }

    fn make_assign_stmt(&mut self, assignee: Expr, value: Expr, span: Span) -> StmtExpr {
        let init = self.init_info_for_id(assignee.id);
        self.make_stmt(
            StmtExprKind::Assign {
                assignee: Box::new(assignee),
                value: Box::new(value),
                init,
            },
            span,
        )
    }

    fn make_return_stmt(&mut self, value: Option<Expr>, span: Span) -> StmtExpr {
        self.make_stmt(
            StmtExprKind::Return {
                value: value.map(Box::new),
            },
            span,
        )
    }

    fn make_block_expr(&mut self, items: Vec<BlockItem>, span: Span) -> Expr {
        self.make_value_expr(ExprKind::Block { items, tail: None }, Type::Unit, span)
    }

    fn make_stmt(&mut self, kind: StmtExprKind, span: Span) -> StmtExpr {
        StmtExpr {
            id: self.node_id_gen.new_id(),
            kind,
            span,
        }
    }

    fn make_value_expr(&mut self, kind: ExprKind, ty: Type, span: Span) -> Expr {
        let id = self.node_id_gen.new_id();
        let _ty_id = self.insert_synth_node_type(id, ty);
        Expr { id, kind, span }
    }

    fn make_runtime_call_expr(
        &mut self,
        runtime: RuntimeCall,
        args: Vec<CallArg>,
        ret_ty: Type,
        input_modes: Vec<ParamMode>,
        span: Span,
    ) -> Expr {
        let expr_id = self.node_id_gen.new_id();
        let _ty_id = self.insert_synth_node_type(expr_id, ret_ty);
        self.record_call_plan(
            expr_id,
            CallPlan {
                target: CallTarget::Runtime(runtime),
                args: (0..args.len())
                    .map(|index| ArgLowering::Direct(CallInput::Arg(index)))
                    .collect(),
                drop_mask: vec![false; args.len()],
                input_modes,
                has_receiver: false,
            },
        );
        let callee = self.make_value_expr(ExprKind::UnitLit, Type::Unit, span);
        Expr {
            id: expr_id,
            kind: ExprKind::Call {
                callee: Box::new(callee),
                args,
            },
            span,
        }
    }

    fn make_direct_method_call_expr(
        &mut self,
        method_def_id: DefId,
        method_name: &str,
        receiver: Expr,
        receiver_mode: ParamMode,
        ret_ty: Type,
        span: Span,
    ) -> Expr {
        let expr_id = self.node_id_gen.new_id();
        let _ty_id = self.insert_synth_node_type(expr_id, ret_ty.clone());
        let receiver_needs_drop = self
            .type_map
            .type_table()
            .get(self.type_id_for(receiver.id))
            .needs_drop();
        self.record_call_plan(
            expr_id,
            CallPlan {
                target: CallTarget::Direct(method_def_id),
                args: vec![ArgLowering::Direct(CallInput::Receiver)],
                drop_mask: vec![receiver_mode == ParamMode::In && receiver_needs_drop],
                input_modes: vec![receiver_mode],
                has_receiver: true,
            },
        );
        Expr {
            id: expr_id,
            kind: ExprKind::MethodCall {
                callee: Box::new(receiver),
                method_name: method_name.to_string(),
                args: vec![],
            },
            span,
        }
    }

    fn make_place_expr(
        &mut self,
        kind: ExprKind,
        ty: Type,
        span: Span,
        def_id: Option<DefId>,
    ) -> Expr {
        let id = self.node_id_gen.new_id();
        let _ty_id = self.insert_synth_node_type(id, ty);
        if let Some(def_id) = def_id {
            self.def_table.record_use(id, def_id);
        }
        Expr { id, kind, span }
    }
}
