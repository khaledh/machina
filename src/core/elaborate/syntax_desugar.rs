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
use crate::core::diag::Span;
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::resolve::{DefId, DefKind};
use crate::core::tree::semantic as sem;
use crate::core::tree::{BinaryOp, ParamMode};
use crate::core::types::Type;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// Metadata for a synthesized local variable in the desugared loop.
struct ForLocal {
    def_id: DefId,
    name: String,
    ty: Type,
    pattern: sem::BindPattern,
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
    deferred: Vec<sem::ValueExpr>,
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

pub(super) fn run(elaborator: &mut Elaborator<'_>, module: &mut sem::Module) {
    let mut ctx = SyntaxDesugarCtx {
        elaborator,
        cleanup_methods: collect_using_cleanup_methods(module),
        cleanup_frames: Vec::new(),
    };
    ctx.desugar_module(module);
}

fn collect_using_cleanup_methods(module: &sem::Module) -> UsingCleanupMethods {
    let mut close_ignore_error_by_type = HashMap::new();
    for block in module.method_blocks() {
        for item in &block.method_items {
            let sem::MethodItem::Def(def) = item else {
                continue;
            };
            if def.sig.name == "close_ignore_error" {
                close_ignore_error_by_type.insert(block.type_name.clone(), def.def_id);
            }
        }
    }
    UsingCleanupMethods {
        close_ignore_error_by_type,
    }
}

impl<'a, 'b> SyntaxDesugarCtx<'a, 'b> {
    fn desugar_module(&mut self, module: &mut sem::Module) {
        for item in &mut module.top_level_items {
            self.desugar_top_level_item(item);
        }
    }

    fn desugar_top_level_item(&mut self, item: &mut sem::TopLevelItem) {
        match item {
            sem::TopLevelItem::FuncDef(def) => self.desugar_value_expr(&mut def.body),
            sem::TopLevelItem::MethodBlock(block) => {
                for method_item in &mut block.method_items {
                    if let sem::MethodItem::Def(def) = method_item {
                        self.desugar_value_expr(&mut def.body);
                    }
                }
            }
            sem::TopLevelItem::TraitDef(_)
            | sem::TopLevelItem::TypeDef(_)
            | sem::TopLevelItem::FuncDecl(_) => {}
        }
    }

    fn collect_cleanup_for_return(&self) -> Vec<sem::ValueExpr> {
        self.cleanup_frames
            .iter()
            .rev()
            .flat_map(|frame| frame.deferred.iter().rev().cloned())
            .collect()
    }

    fn collect_cleanup_for_loop_exit(&self) -> Vec<sem::ValueExpr> {
        let mut values = Vec::new();
        for frame in self.cleanup_frames.iter().rev() {
            values.extend(frame.deferred.iter().rev().cloned());
            if frame.boundary == CleanupBoundary::Loop {
                break;
            }
        }
        values
    }

    fn collect_cleanup_for_try_propagate(&self) -> Vec<sem::ValueExpr> {
        self.collect_cleanup_for_return()
    }

    fn append_cleanup_before_stmt(
        &self,
        rewritten: &mut Vec<sem::BlockItem>,
        cleanup: Vec<sem::ValueExpr>,
    ) {
        // Cleanup runs in LIFO order before the control transfer leaves the
        // scopes represented by the active cleanup frames.
        for value in cleanup {
            rewritten.push(sem::BlockItem::Expr(value));
        }
    }

    /// Desugar a for loop into a while loop with explicit index management.
    fn desugar_for_stmt(
        &mut self,
        stmt: &sem::StmtExpr,
        pattern: &sem::BindPattern,
        iter: &sem::ValueExpr,
        body: &sem::ValueExpr,
    ) -> sem::ValueExpr {
        let span = stmt.span;
        let u64_ty = Type::uint(64);
        let bool_ty = Type::Bool;

        let mut items = Vec::new();

        let (iter_place, idx_place, len_value, elem_ty, is_range) = match &iter.kind {
            sem::ValueExprKind::Range { start, end } => {
                let start_expr = (**start).clone();
                let len_expr = (**end).clone();

                let idx_info = self.new_for_local("idx", u64_ty.clone(), true, span);
                let idx_stmt = self.make_var_bind_stmt(idx_info.pattern.clone(), start_expr, span);
                items.push(sem::BlockItem::Stmt(idx_stmt));

                let len_info = self.new_for_local("len", u64_ty.clone(), false, span);
                let len_stmt = self.make_let_bind_stmt(len_info.pattern.clone(), len_expr, span);
                items.push(sem::BlockItem::Stmt(len_stmt));

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
            _ => {
                let iter_ty = self.type_map.type_table().get(iter.ty).clone();
                let elem_ty =
                    self.iter_elem_type_or_panic(&iter_ty, "for desugaring iterable path");

                let iter_info = self.new_for_local("iter", iter_ty.clone(), false, span);
                let iter_value = iter.clone();
                let iter_stmt =
                    self.make_let_bind_stmt(iter_info.pattern.clone(), iter_value, span);
                items.push(sem::BlockItem::Stmt(iter_stmt));

                let len_expr = match iter_ty {
                    Type::Array { dims, .. } => {
                        let len = dims
                            .first()
                            .copied()
                            .unwrap_or_else(|| panic!("compiler bug: empty array dims"));
                        self.make_u64_lit(len as u64, span)
                    }
                    Type::Slice { .. } => {
                        let iter_place = self.make_var_place(&iter_info, span);
                        self.make_len_expr(iter_place, span)
                    }
                    Type::DynArray { .. } => {
                        let iter_place = self.make_var_place(&iter_info, span);
                        self.make_len_expr(iter_place, span)
                    }
                    Type::Range { .. } => {
                        panic!("compiler bug: range iteration should take the range path")
                    }
                    _ => unreachable!("checked above"),
                };

                let len_info = self.new_for_local("len", u64_ty.clone(), false, span);
                let len_stmt = self.make_let_bind_stmt(len_info.pattern.clone(), len_expr, span);
                items.push(sem::BlockItem::Stmt(len_stmt));

                let idx_info = self.new_for_local("idx", u64_ty.clone(), true, span);
                let idx_init = self.make_u64_lit(0, span);
                let idx_stmt = self.make_var_bind_stmt(idx_info.pattern.clone(), idx_init, span);
                items.push(sem::BlockItem::Stmt(idx_stmt));

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
        };

        let idx_load = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
        let cond_expr = self.make_binop_expr(BinaryOp::Lt, idx_load, len_value, bool_ty, span);

        let mut loop_items = Vec::new();

        let cur_info = self.new_for_local("cur", u64_ty.clone(), false, span);
        let cur_load = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
        let cur_stmt = self.make_let_bind_stmt(cur_info.pattern.clone(), cur_load, span);
        loop_items.push(sem::BlockItem::Stmt(cur_stmt));

        // Increment before the body so `continue` doesn't skip the step.
        let idx_next = {
            let idx_val = self.make_load_expr(idx_place.clone(), u64_ty.clone(), span);
            let one = self.make_u64_lit(1, span);
            self.make_binop_expr(BinaryOp::Add, idx_val, one, u64_ty.clone(), span)
        };
        let idx_assign = self.make_assign_stmt(idx_place.clone(), idx_next, span);
        loop_items.push(sem::BlockItem::Stmt(idx_assign));

        let elem_expr = if is_range {
            let cur_place = self.make_var_place(&cur_info, span);
            self.make_load_expr(cur_place, u64_ty.clone(), span)
        } else {
            let iter_place = iter_place
                .unwrap_or_else(|| panic!("compiler bug: missing iter place for iterable loop"));
            let cur_place = self.make_var_place(&cur_info, span);
            let cur_value = self.make_load_expr(cur_place, u64_ty.clone(), span);
            let index_place = self.make_index_place(iter_place, cur_value, elem_ty.clone(), span);
            self.make_load_expr(index_place, elem_ty.clone(), span)
        };

        let pattern_stmt = self.make_let_bind_stmt(pattern.clone(), elem_expr, span);
        loop_items.push(sem::BlockItem::Stmt(pattern_stmt));

        loop_items.push(sem::BlockItem::Expr(body.clone()));

        let loop_body = self.make_block_expr(loop_items, span);
        let while_stmt = self.make_stmt(
            sem::StmtExprKind::While {
                cond: Box::new(cond_expr),
                body: Box::new(loop_body),
            },
            span,
        );
        items.push(sem::BlockItem::Stmt(while_stmt));

        self.make_block_expr(items, span)
    }

    fn desugar_block_items(&mut self, items: &mut Vec<sem::BlockItem>, boundary: CleanupBoundary) {
        let frame_start = self.cleanup_frames.len();
        self.cleanup_frames.push(CleanupFrame {
            deferred: Vec::new(),
            boundary,
        });
        let mut rewritten = Vec::with_capacity(items.len());
        for item in items.drain(..) {
            match item {
                sem::BlockItem::Expr(mut expr) => {
                    self.desugar_value_expr(&mut expr);
                    rewritten.push(sem::BlockItem::Expr(expr));
                }
                sem::BlockItem::Stmt(mut stmt) => {
                    self.desugar_stmt_expr(&mut stmt);
                    match &stmt.kind {
                        sem::StmtExprKind::For {
                            pattern,
                            iter,
                            body,
                        } => {
                            let mut expr = self.desugar_for_stmt(&stmt, pattern, iter, body);
                            self.desugar_value_expr(&mut expr);
                            rewritten.push(sem::BlockItem::Expr(expr));
                        }
                        sem::StmtExprKind::Defer { value } => {
                            self.cleanup_frames
                                .last_mut()
                                .expect("cleanup frame must exist while desugaring a block")
                                .deferred
                                .push((**value).clone());
                        }
                        sem::StmtExprKind::Using {
                            pattern,
                            value,
                            body,
                        } => {
                            let mut expr = self.desugar_using_stmt(&stmt, pattern, value, body);
                            self.desugar_value_expr(&mut expr);
                            rewritten.push(sem::BlockItem::Expr(expr));
                        }
                        sem::StmtExprKind::Return { .. } => {
                            self.append_cleanup_before_stmt(
                                &mut rewritten,
                                self.collect_cleanup_for_return(),
                            );
                            rewritten.push(sem::BlockItem::Stmt(stmt));
                        }
                        sem::StmtExprKind::Break | sem::StmtExprKind::Continue => {
                            self.append_cleanup_before_stmt(
                                &mut rewritten,
                                self.collect_cleanup_for_loop_exit(),
                            );
                            rewritten.push(sem::BlockItem::Stmt(stmt));
                        }
                        _ => rewritten.push(sem::BlockItem::Stmt(stmt)),
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
            rewritten.push(sem::BlockItem::Expr(value));
        }
        debug_assert_eq!(self.cleanup_frames.len(), frame_start);
        *items = rewritten;
    }

    fn desugar_stmt_expr(&mut self, stmt: &mut sem::StmtExpr) {
        match &mut stmt.kind {
            sem::StmtExprKind::LetBind { value, .. } | sem::StmtExprKind::VarBind { value, .. } => {
                self.desugar_value_expr(value)
            }
            sem::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.desugar_place_expr(assignee);
                self.desugar_value_expr(value);
            }
            sem::StmtExprKind::While { cond, body } => {
                self.desugar_value_expr(cond);
                self.desugar_loop_body(body);
            }
            sem::StmtExprKind::For { iter, body, .. } => {
                self.desugar_value_expr(iter);
                self.desugar_loop_body(body);
            }
            sem::StmtExprKind::Defer { value } => {
                self.desugar_value_expr(value);
            }
            sem::StmtExprKind::Using { value, body, .. } => {
                self.desugar_value_expr(value);
                self.desugar_value_expr(body);
            }
            sem::StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.desugar_value_expr(value);
                }
            }
            sem::StmtExprKind::VarDecl { .. }
            | sem::StmtExprKind::Break
            | sem::StmtExprKind::Continue => {}
        }
    }

    fn desugar_place_expr(&mut self, place: &mut sem::PlaceExpr) {
        match &mut place.kind {
            sem::PlaceExprKind::Var { .. } => {}
            sem::PlaceExprKind::Deref { value } => self.desugar_value_expr(value),
            sem::PlaceExprKind::ArrayIndex { target, indices } => {
                self.desugar_place_expr(target);
                for index in indices {
                    self.desugar_value_expr(index);
                }
            }
            sem::PlaceExprKind::TupleField { target, .. }
            | sem::PlaceExprKind::StructField { target, .. } => self.desugar_place_expr(target),
        }
    }

    fn desugar_call_arg(&mut self, arg: &mut sem::CallArg) {
        match arg {
            sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                self.desugar_value_expr(expr)
            }
            sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                self.desugar_place_expr(place)
            }
        }
    }

    fn desugar_using_stmt(
        &mut self,
        stmt: &sem::StmtExpr,
        pattern: &sem::BindPattern,
        value: &sem::ValueExpr,
        body: &sem::ValueExpr,
    ) -> sem::ValueExpr {
        let cleanup = self
            .make_using_cleanup_expr(pattern, value, stmt.span)
            .unwrap_or_else(|| {
                panic!(
                    "compiler bug: missing `close_ignore_error` cleanup for using-bound type {}",
                    self.type_map.type_table().get(value.ty)
                )
            });

        // `using` lowers to a dedicated block so the binding and its deferred
        // cleanup stay scoped to the body region.
        let items = vec![
            sem::BlockItem::Stmt(self.make_let_bind_stmt(
                pattern.clone(),
                value.clone(),
                stmt.span,
            )),
            sem::BlockItem::Stmt(self.make_stmt(
                sem::StmtExprKind::Defer {
                    value: Box::new(cleanup),
                },
                stmt.span,
            )),
            sem::BlockItem::Expr(body.clone()),
        ];
        self.make_block_expr(items, stmt.span)
    }

    fn desugar_value_expr(&mut self, expr: &mut sem::ValueExpr) {
        match &mut expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                self.desugar_block_items(items, CleanupBoundary::Normal);
                if let Some(tail) = tail {
                    self.desugar_value_expr(tail);
                }
            }
            sem::ValueExprKind::ArrayLit { init, .. } => match init {
                sem::ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.desugar_value_expr(elem);
                    }
                }
                sem::ArrayLitInit::Repeat(value, ..) => self.desugar_value_expr(value),
            },
            sem::ValueExprKind::SetLit { elems, .. } => {
                for elem in elems {
                    self.desugar_value_expr(elem);
                }
            }
            sem::ValueExprKind::MapLit { entries, .. } => {
                for entry in entries {
                    self.desugar_value_expr(&mut entry.key);
                    self.desugar_value_expr(&mut entry.value);
                }
            }
            sem::ValueExprKind::TupleLit(items)
            | sem::ValueExprKind::EnumVariant { payload: items, .. } => {
                for item in items {
                    self.desugar_value_expr(item);
                }
            }
            sem::ValueExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.desugar_value_expr(&mut field.value);
                }
            }
            sem::ValueExprKind::StructUpdate { target, fields } => {
                self.desugar_value_expr(target);
                for field in fields {
                    self.desugar_value_expr(&mut field.value);
                }
            }
            sem::ValueExprKind::BinOp { left, right, .. } => {
                self.desugar_value_expr(left);
                self.desugar_value_expr(right);
            }
            sem::ValueExprKind::Try {
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
            sem::ValueExprKind::UnaryOp { expr, .. }
            | sem::ValueExprKind::HeapAlloc { expr }
            | sem::ValueExprKind::Coerce { expr, .. } => self.desugar_value_expr(expr),
            sem::ValueExprKind::Move { place }
            | sem::ValueExprKind::ImplicitMove { place }
            | sem::ValueExprKind::AddrOf { place }
            | sem::ValueExprKind::Load { place }
            | sem::ValueExprKind::Len { place } => self.desugar_place_expr(place),
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.desugar_value_expr(cond);
                self.desugar_value_expr(then_body);
                self.desugar_value_expr(else_body);
            }
            sem::ValueExprKind::Range { start, end } => {
                self.desugar_value_expr(start);
                self.desugar_value_expr(end);
            }
            sem::ValueExprKind::Slice { target, start, end } => {
                self.desugar_place_expr(target);
                if let Some(start) = start {
                    self.desugar_value_expr(start);
                }
                if let Some(end) = end {
                    self.desugar_value_expr(end);
                }
            }
            sem::ValueExprKind::MapGet { target, key } => {
                self.desugar_value_expr(target);
                self.desugar_value_expr(key);
            }
            sem::ValueExprKind::Match { scrutinee, arms } => {
                self.desugar_value_expr(scrutinee);
                for arm in arms {
                    self.desugar_value_expr(&mut arm.body);
                }
            }
            sem::ValueExprKind::Call { callee, args } => {
                self.desugar_value_expr(callee);
                for arg in args {
                    self.desugar_call_arg(arg);
                }
            }
            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                match receiver {
                    sem::MethodReceiver::ValueExpr(value) => self.desugar_value_expr(value),
                    sem::MethodReceiver::PlaceExpr(place) => self.desugar_place_expr(place),
                }
                for arg in args {
                    self.desugar_call_arg(arg);
                }
            }
            sem::ValueExprKind::EmitSend { to, payload }
            | sem::ValueExprKind::EmitRequest {
                to,
                payload,
                request_site_key: _,
            } => {
                self.desugar_value_expr(to);
                self.desugar_value_expr(payload);
            }
            sem::ValueExprKind::Reply { cap, value } => {
                self.desugar_value_expr(cap);
                self.desugar_value_expr(value);
            }
            sem::ValueExprKind::UnitLit
            | sem::ValueExprKind::IntLit(_)
            | sem::ValueExprKind::BoolLit(_)
            | sem::ValueExprKind::CharLit(_)
            | sem::ValueExprKind::StringLit { .. }
            | sem::ValueExprKind::StringFmt { .. }
            | sem::ValueExprKind::ClosureRef { .. } => {}
        }
    }

    fn desugar_loop_body(&mut self, body: &mut sem::ValueExpr) {
        match &mut body.kind {
            sem::ValueExprKind::Block { items, tail } => {
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
        pattern: &sem::BindPattern,
        value: &sem::ValueExpr,
        span: Span,
    ) -> Option<sem::ValueExpr> {
        let (ident, def_id) = match &pattern.kind {
            sem::BindPatternKind::Name { ident, def_id } => (ident.clone(), *def_id),
            _ => return None,
        };
        let Type::Struct { name, .. } = self.type_map.type_table().get(value.ty).clone() else {
            return None;
        };
        let method_def_id = *self.cleanup_methods.close_ignore_error_by_type.get(&name)?;

        let receiver_ty = self.type_map.type_table().get(value.ty).clone();
        let receiver_place = self.make_place_expr(
            sem::PlaceExprKind::Var { ident, def_id },
            receiver_ty.clone(),
            span,
        );
        let receiver_value = self.make_load_expr(receiver_place, receiver_ty, span);
        let expr_id = self.node_id_gen.new_id();
        let ty_id = self.insert_synth_node_type(expr_id, Type::Unit);
        self.record_call_plan(
            expr_id,
            sem::CallPlan {
                target: sem::CallTarget::Direct(method_def_id),
                args: vec![sem::ArgLowering::Direct(sem::CallInput::Receiver)],
                drop_mask: vec![false],
                input_modes: vec![ParamMode::Sink],
                has_receiver: true,
            },
        );
        Some(sem::ValueExpr {
            id: expr_id,
            kind: sem::ValueExprKind::MethodCall {
                receiver: sem::MethodReceiver::ValueExpr(Box::new(receiver_value)),
                method_name: "close_ignore_error".to_string(),
                args: vec![],
            },
            ty: ty_id,
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
        let pattern = sem::BindPattern {
            id: self.node_id_gen.new_id(),
            kind: sem::BindPatternKind::Name {
                ident: name.clone(),
                def_id,
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

    fn make_var_place(&mut self, info: &ForLocal, span: Span) -> sem::PlaceExpr {
        self.make_place_expr(
            sem::PlaceExprKind::Var {
                ident: info.name.clone(),
                def_id: info.def_id,
            },
            info.ty.clone(),
            span,
        )
    }

    fn make_len_expr(&mut self, place: sem::PlaceExpr, span: Span) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::Len {
                place: Box::new(place),
            },
            Type::uint(64),
            span,
        )
    }

    fn make_len_load(&mut self, place: sem::PlaceExpr, span: Span) -> sem::ValueExpr {
        self.make_load_expr(place, Type::uint(64), span)
    }

    fn make_u64_lit(&mut self, value: u64, span: Span) -> sem::ValueExpr {
        self.make_value_expr(sem::ValueExprKind::IntLit(value), Type::uint(64), span)
    }

    fn make_binop_expr(
        &mut self,
        op: BinaryOp,
        left: sem::ValueExpr,
        right: sem::ValueExpr,
        result_ty: Type,
        span: Span,
    ) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            result_ty,
            span,
        )
    }

    fn make_index_place(
        &mut self,
        target: sem::PlaceExpr,
        index: sem::ValueExpr,
        elem_ty: Type,
        span: Span,
    ) -> sem::PlaceExpr {
        let id = self.node_id_gen.new_id();
        let target_ty = self.type_map.type_table().get(target.ty).clone();
        let plan = self.build_index_plan(&target_ty);
        self.record_index_plan(id, plan);
        let ty_id = self.insert_synth_node_type(id, elem_ty);

        sem::PlaceExpr {
            id,
            kind: sem::PlaceExprKind::ArrayIndex {
                target: Box::new(target),
                indices: vec![index],
            },
            ty: ty_id,
            span,
        }
    }

    fn make_load_expr(&mut self, place: sem::PlaceExpr, ty: Type, span: Span) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::Load {
                place: Box::new(place),
            },
            ty,
            span,
        )
    }

    fn make_let_bind_stmt(
        &mut self,
        pattern: sem::BindPattern,
        value: sem::ValueExpr,
        span: Span,
    ) -> sem::StmtExpr {
        self.make_stmt(
            sem::StmtExprKind::LetBind {
                pattern,
                decl_ty: None,
                value: Box::new(value),
            },
            span,
        )
    }

    fn make_var_bind_stmt(
        &mut self,
        pattern: sem::BindPattern,
        value: sem::ValueExpr,
        span: Span,
    ) -> sem::StmtExpr {
        self.make_stmt(
            sem::StmtExprKind::VarBind {
                pattern,
                decl_ty: None,
                value: Box::new(value),
            },
            span,
        )
    }

    fn make_assign_stmt(
        &mut self,
        assignee: sem::PlaceExpr,
        value: sem::ValueExpr,
        span: Span,
    ) -> sem::StmtExpr {
        self.make_stmt(
            sem::StmtExprKind::Assign {
                assignee: Box::new(assignee.clone()),
                value: Box::new(value),
                init: self.init_info_for_id(assignee.id),
            },
            span,
        )
    }

    fn make_block_expr(&mut self, items: Vec<sem::BlockItem>, span: Span) -> sem::ValueExpr {
        self.make_value_expr(
            sem::ValueExprKind::Block { items, tail: None },
            Type::Unit,
            span,
        )
    }

    fn make_stmt(&mut self, kind: sem::StmtExprKind, span: Span) -> sem::StmtExpr {
        sem::StmtExpr {
            id: self.node_id_gen.new_id(),
            kind,
            span,
        }
    }

    fn make_value_expr(
        &mut self,
        kind: sem::ValueExprKind,
        ty: Type,
        span: Span,
    ) -> sem::ValueExpr {
        let id = self.node_id_gen.new_id();
        let ty_id = self.insert_synth_node_type(id, ty);
        sem::ValueExpr {
            id,
            kind,
            ty: ty_id,
            span,
        }
    }

    fn make_place_expr(
        &mut self,
        kind: sem::PlaceExprKind,
        ty: Type,
        span: Span,
    ) -> sem::PlaceExpr {
        let id = self.node_id_gen.new_id();
        let ty_id = self.insert_synth_node_type(id, ty);
        sem::PlaceExpr {
            id,
            kind,
            ty: ty_id,
            span,
        }
    }
}
