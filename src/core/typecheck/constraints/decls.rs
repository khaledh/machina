//! Module/function/method declaration orchestration.

use std::collections::HashMap;

use super::*;
use crate::core::ast::{FuncDecl, MethodBlock, MethodDecl, MethodDef, MethodItem, Param, TypeParam};
use crate::core::typecheck::errors::TEK;

fn as_opaque_iterable_type(ty: &Type) -> Option<Type> {
    matches!(ty, Type::Iterable { .. }).then(|| ty.clone())
}

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_module(&mut self) {
        self.check_local_function_overload_arity_ranges();
        self.check_local_method_overload_arity_ranges();
        // Declarations first so callable defs are available when encountered by
        // later expressions in the same module.
        for func_decl in self.ctx.module.func_decls() {
            self.collect_func_decl(func_decl);
        }
        for func_def in self.ctx.module.func_defs() {
            self.collect_func_def(func_def);
        }
        for method_block in self.ctx.module.method_blocks() {
            let receiver_type_params = method_block_type_params(&self.ctx.def_table, method_block);
            for method_item in &method_block.method_items {
                match method_item {
                    MethodItem::Decl(method_decl) => {
                        self.collect_method_decl(method_block, &receiver_type_params, method_decl);
                    }
                    MethodItem::Def(method_def) => {
                        self.collect_method_def(method_block, &receiver_type_params, method_def);
                    }
                }
            }
        }
    }

    fn collect_func_decl(&mut self, func_decl: &FuncDecl) {
        self.with_type_params(&func_decl.sig.type_params, |this| {
            let func_def_id = this.ctx.def_table.def_id(func_decl.id);
            if let Some(fn_ty) = this.collect_function_signature(&func_decl.sig) {
                let def_term = this.def_term(func_def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(func_def_id, func_decl.span),
                );
            }
            this.collect_param_default_constraints(&func_decl.sig.params);
        });
    }

    fn collect_func_def(&mut self, func_def: &FuncDef) {
        self.with_type_params(&func_def.sig.type_params, |this| {
            let func_def_id = this.ctx.def_table.def_id(func_def.id);
            let mut declared_ret_ty = None;
            let mut opaque_ret_ty = None;
            if let Some(fn_ty) = this.collect_function_signature(&func_def.sig) {
                declared_ret_ty = fn_type_return(&fn_ty);
                opaque_ret_ty = declared_ret_ty.as_ref().and_then(as_opaque_iterable_type);
                let def_term = this.def_term(func_def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(func_def_id, func_def.span),
                );
            }
            let ret_ty = this.resolve_return_type_in_scope(&func_def.sig.ret_ty_expr);
            if opaque_ret_ty.is_none() {
                opaque_ret_ty = ret_ty.as_ref().ok().and_then(as_opaque_iterable_type);
            }
            this.collect_param_default_constraints(&func_def.sig.params);
            let return_term = if opaque_ret_ty.is_some() {
                this.fresh_var_term()
            } else {
                ret_ty
                    .ok()
                    .or(declared_ret_ty)
                    .unwrap_or_else(|| this.fresh_var_term())
            };
            let func_node_term = this.node_term(func_def.id);
            this.push_eq(
                func_node_term,
                return_term.clone(),
                ConstraintReason::Decl(func_def_id, func_def.span),
            );
            this.enter_callable(func_def_id, return_term.clone(), func_def.span);

            for param in &func_def.sig.params {
                let node_term = this.node_term(param.id);
                let declared_ty = this.resolve_param_type_in_scope(&param.typ).ok();
                if let Some(param_def_id) = this.lookup_def_id(param.id) {
                    let def_term = this.def_term(param_def_id);
                    if let Some(ty) = declared_ty.clone() {
                        this.push_eq(
                            def_term.clone(),
                            ty,
                            ConstraintReason::Decl(param_def_id, param.span),
                        );
                    }
                    this.push_eq(
                        node_term,
                        def_term,
                        ConstraintReason::Decl(param_def_id, param.span),
                    );
                } else if let Some(ty) = declared_ty {
                    this.push_eq(node_term, ty, ConstraintReason::Expr(param.id, param.span));
                }
            }

            let body_ty = if opaque_ret_ty.is_some() {
                this.collect_expr(&func_def.body, None)
            } else {
                this.collect_expr(&func_def.body, Some(return_term.clone()))
            };
            this.push_assignable(
                body_ty,
                return_term,
                ConstraintReason::Expr(func_def.body.id, func_def.body.span),
            );
            if let Some(exposed_ty) = opaque_ret_ty {
                this.out.opaque_facts.push(OpaqueFact::CallableReturn {
                    def_id: func_def_id,
                    binding_node: func_def.id,
                    exposed_ty,
                    span: func_def.sig.ret_ty_expr.span,
                });
            }
            this.exit_callable(func_def_id, func_def.span);
        });
    }

    fn collect_method_decl(
        &mut self,
        method_block: &MethodBlock,
        receiver_type_params: &[TypeParam],
        method_decl: &MethodDecl,
    ) {
        let method_def_id = self.ctx.def_table.def_id(method_decl.id);
        self.with_type_params(receiver_type_params, |this| {
            this.with_type_params(&method_decl.sig.type_params, |this| {
                if let Some(fn_ty) = this.collect_method_signature(method_block, &method_decl.sig) {
                    let def_term = this.def_term(method_def_id);
                    this.push_eq(
                        def_term,
                        fn_ty,
                        ConstraintReason::Decl(method_def_id, method_decl.span),
                    );
                }
                this.collect_param_default_constraints(&method_decl.sig.params);
            });
        });
    }

    fn collect_method_def(
        &mut self,
        method_block: &MethodBlock,
        receiver_type_params: &[TypeParam],
        method_def: &MethodDef,
    ) {
        let method_def_id = self.ctx.def_table.def_id(method_def.id);
        let method_span = method_def.span;
        let sig = &method_def.sig;

        self.with_type_params(receiver_type_params, |this| {
            this.with_type_params(&sig.type_params, |this| {
                let mut declared_ret_ty = None;
                let mut opaque_ret_ty = None;
                if let Some(fn_ty) = this.collect_method_signature(method_block, sig) {
                    declared_ret_ty = fn_type_return(&fn_ty);
                    opaque_ret_ty = declared_ret_ty.as_ref().and_then(as_opaque_iterable_type);
                    let def_term = this.def_term(method_def_id);
                    this.push_eq(
                        def_term,
                        fn_ty,
                        ConstraintReason::Decl(method_def_id, method_span),
                    );
                }
                let ret_ty = this.resolve_return_type_in_scope(&sig.ret_ty_expr);
                if opaque_ret_ty.is_none() {
                    opaque_ret_ty = ret_ty.as_ref().ok().and_then(as_opaque_iterable_type);
                }
                this.collect_param_default_constraints(&sig.params);
                let return_term = if opaque_ret_ty.is_some() {
                    this.fresh_var_term()
                } else {
                    ret_ty
                        .ok()
                        .or(declared_ret_ty)
                        .unwrap_or_else(|| this.fresh_var_term())
                };
                let method_node_term = this.node_term(method_def.id);
                this.push_eq(
                    method_node_term,
                    return_term.clone(),
                    ConstraintReason::Decl(method_def_id, method_def.span),
                );
                this.enter_callable(method_def_id, return_term.clone(), method_span);

                let self_node = this.node_term(sig.self_param.id);
                if let Some(self_ty) = this.resolve_method_block_self_type(method_block) {
                    if let Some(self_param_def_id) = this.lookup_def_id(sig.self_param.id) {
                        let self_term = this.def_term(self_param_def_id);
                        this.push_eq(
                            self_term.clone(),
                            self_ty.clone(),
                            ConstraintReason::Decl(self_param_def_id, sig.self_param.span),
                        );
                        this.push_eq(
                            self_node.clone(),
                            self_term,
                            ConstraintReason::Decl(self_param_def_id, sig.self_param.span),
                        );
                    } else {
                        this.push_eq(
                            self_node.clone(),
                            self_ty,
                            ConstraintReason::Expr(sig.self_param.id, sig.self_param.span),
                        );
                    }
                }

                for param in &sig.params {
                    let node_term = this.node_term(param.id);
                    let declared_ty = this.resolve_param_type_in_scope(&param.typ).ok();
                    if let Some(param_def_id) = this.lookup_def_id(param.id) {
                        let def_term = this.def_term(param_def_id);
                        if let Some(ty) = declared_ty.clone() {
                            this.push_eq(
                                def_term.clone(),
                                ty,
                                ConstraintReason::Decl(param_def_id, param.span),
                            );
                        }
                        this.push_eq(
                            node_term,
                            def_term,
                            ConstraintReason::Decl(param_def_id, param.span),
                        );
                    } else if let Some(ty) = declared_ty {
                        this.push_eq(node_term, ty, ConstraintReason::Expr(param.id, param.span));
                    }
                }

                let body_ty = if opaque_ret_ty.is_some() {
                    this.collect_expr(&method_def.body, None)
                } else {
                    this.collect_expr(&method_def.body, Some(return_term.clone()))
                };
                this.push_assignable(
                    body_ty,
                    return_term,
                    ConstraintReason::Expr(method_def.body.id, method_def.body.span),
                );
                if let Some(exposed_ty) = opaque_ret_ty {
                    this.out.opaque_facts.push(OpaqueFact::CallableReturn {
                        def_id: method_def_id,
                        binding_node: method_def.id,
                        exposed_ty,
                        span: sig.ret_ty_expr.span,
                    });
                }

                this.exit_callable(method_def_id, method_span);
            });
        });
    }

    fn collect_param_default_constraints(&mut self, params: &[Param]) {
        for param in params {
            let Some(default) = &param.default else {
                continue;
            };
            let Some(param_ty) = self.resolve_param_type_in_scope(&param.typ).ok() else {
                continue;
            };
            let default_ty = self.collect_expr(default, Some(param_ty.clone()));
            self.push_assignable(
                default_ty,
                param_ty,
                ConstraintReason::Expr(default.id, default.span),
            );
        }
    }

    fn check_local_function_overload_arity_ranges(&mut self) {
        let mut by_name: HashMap<String, Vec<(&[Param], Span)>> = HashMap::new();
        for func_decl in self.ctx.module.func_decls() {
            by_name
                .entry(func_decl.sig.name.clone())
                .or_default()
                .push((&func_decl.sig.params, func_decl.span));
        }
        for func_def in self.ctx.module.func_defs() {
            by_name
                .entry(func_def.sig.name.clone())
                .or_default()
                .push((&func_def.sig.params, func_def.span));
        }
        for (name, overloads) in by_name {
            self.check_overload_arity_ranges(&name, &overloads);
        }
    }

    fn check_local_method_overload_arity_ranges(&mut self) {
        let mut by_name: HashMap<String, Vec<(&[Param], Span)>> = HashMap::new();
        for method_block in self.ctx.module.method_blocks() {
            for method_item in &method_block.method_items {
                let (label, params, span) = match method_item {
                    MethodItem::Decl(method_decl) => (
                        format!("{}::{}", method_block.type_name, method_decl.sig.name),
                        method_decl.sig.params.as_slice(),
                        method_decl.span,
                    ),
                    MethodItem::Def(method_def) => (
                        format!("{}::{}", method_block.type_name, method_def.sig.name),
                        method_def.sig.params.as_slice(),
                        method_def.span,
                    ),
                };
                by_name.entry(label).or_default().push((params, span));
            }
        }
        for (name, overloads) in by_name {
            self.check_overload_arity_ranges(&name, &overloads);
        }
    }

    fn check_overload_arity_ranges(&mut self, name: &str, overloads: &[(&[Param], Span)]) {
        for (index, (left_params, _)) in overloads.iter().enumerate() {
            let left_has_defaults = left_params.iter().any(|param| param.default.is_some());
            let left_required = required_param_count(left_params);
            let left_total = left_params.len();
            for (right_params, right_span) in overloads.iter().skip(index + 1) {
                let right_has_defaults = right_params.iter().any(|param| param.default.is_some());
                if !left_has_defaults && !right_has_defaults {
                    continue;
                }
                let right_required = required_param_count(right_params);
                let right_total = right_params.len();
                if left_required <= right_total && right_required <= left_total {
                    self.out
                        .immediate_errors
                        .push(TEK::OverloadArityOverlap(name.to_string()).at(*right_span));
                }
            }
        }
    }
}

fn required_param_count(params: &[Param]) -> usize {
    params.iter().filter(|param| param.default.is_none()).count()
}

fn method_block_type_params(
    def_table: &crate::core::resolve::DefTable,
    method_block: &MethodBlock,
) -> Vec<TypeParam> {
    method_block
        .type_args
        .iter()
        .filter_map(|type_arg| {
            let crate::core::ast::TypeExprKind::Named { ident, type_args } = &type_arg.kind else {
                return None;
            };
            if !type_args.is_empty() {
                return None;
            }
            let def_id = def_table.lookup_node_def_id(type_arg.id)?;
            let def = def_table.lookup_def(def_id)?;
            if def.kind != crate::core::resolve::DefKind::TypeParam {
                return None;
            }
            Some(TypeParam {
                id: type_arg.id,
                ident: ident.clone(),
                bound: None,
                span: type_arg.span,
            })
        })
        .collect()
}
