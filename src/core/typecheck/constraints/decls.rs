//! Module/function/method declaration orchestration.

use super::*;
use crate::core::tree::{FuncDecl, MethodItem};

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_module(&mut self) {
        // Declarations first so callable defs are available when encountered by
        // later expressions in the same module.
        for func_decl in self.ctx.module.func_decls() {
            self.collect_func_decl(func_decl);
        }
        for func_def in self.ctx.module.func_defs() {
            self.collect_func_def(func_def);
        }
        for method_block in self.ctx.module.method_blocks() {
            for method_item in &method_block.method_items {
                let sig = match method_item {
                    MethodItem::Decl(method_decl) => &method_decl.sig,
                    MethodItem::Def(method_def) => &method_def.sig,
                };
                self.collect_method_def(&method_block.type_name, method_item, sig);
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
        });
    }

    fn collect_func_def(&mut self, func_def: &FuncDef) {
        self.with_type_params(&func_def.sig.type_params, |this| {
            let func_def_id = this.ctx.def_table.def_id(func_def.id);
            let mut declared_ret_ty = None;
            if let Some(fn_ty) = this.collect_function_signature(&func_def.sig) {
                declared_ret_ty = fn_type_return(&fn_ty);
                let def_term = this.def_term(func_def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(func_def_id, func_def.span),
                );
            }
            let ret_ty = this.resolve_return_type_in_scope(&func_def.sig.ret_ty_expr);
            let return_term = ret_ty
                .ok()
                .or(declared_ret_ty)
                .unwrap_or_else(|| this.fresh_var_term());
            let func_node_term = this.node_term(func_def.id);
            this.push_eq(
                func_node_term,
                return_term.clone(),
                ConstraintReason::Decl(func_def_id, func_def.span),
            );
            this.enter_callable(func_def_id, return_term.clone(), func_def.span);

            for param in &func_def.sig.params {
                let node_term = this.node_term(param.id);
                let declared_ty = this.resolve_type_in_scope(&param.typ).ok();
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

            let body_ty = this.collect_expr(&func_def.body, Some(return_term.clone()));
            this.push_assignable(
                body_ty,
                return_term,
                ConstraintReason::Expr(func_def.body.id, func_def.body.span),
            );
            this.exit_callable(func_def_id, func_def.span);
        });
    }

    fn collect_method_def(&mut self, type_name: &str, method_item: &MethodItem, sig: &MethodSig) {
        let method_def_id = match method_item {
            MethodItem::Decl(method_decl) => self.ctx.def_table.def_id(method_decl.id),
            MethodItem::Def(method_def) => self.ctx.def_table.def_id(method_def.id),
        };
        let method_span = match method_item {
            MethodItem::Decl(method_decl) => method_decl.span,
            MethodItem::Def(method_def) => method_def.span,
        };

        self.with_type_params(&sig.type_params, |this| {
            let mut declared_ret_ty = None;
            if let Some(fn_ty) = this.collect_method_signature(type_name, sig) {
                declared_ret_ty = fn_type_return(&fn_ty);
                let def_term = this.def_term(method_def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(method_def_id, method_span),
                );
            }
            let ret_ty = this.resolve_return_type_in_scope(&sig.ret_ty_expr);
            let return_term = ret_ty
                .ok()
                .or(declared_ret_ty)
                .unwrap_or_else(|| this.fresh_var_term());
            if let MethodItem::Def(method_def) = method_item {
                let method_node_term = this.node_term(method_def.id);
                this.push_eq(
                    method_node_term,
                    return_term.clone(),
                    ConstraintReason::Decl(method_def_id, method_def.span),
                );
            }
            this.enter_callable(method_def_id, return_term.clone(), method_span);

            let self_node = this.node_term(sig.self_param.id);
            if let Some(self_ty) = this.type_defs.get(type_name).cloned() {
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
                let declared_ty = this.resolve_type_in_scope(&param.typ).ok();
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

            if let MethodItem::Def(method_def) = method_item {
                let body_ty = this.collect_expr(&method_def.body, Some(return_term.clone()));
                this.push_assignable(
                    body_ty,
                    return_term,
                    ConstraintReason::Expr(method_def.body.id, method_def.body.span),
                );
            }

            this.exit_callable(method_def_id, method_span);
        });
    }
}
