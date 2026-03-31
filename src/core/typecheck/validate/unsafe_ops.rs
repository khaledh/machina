//! Validation for operations that require an explicit `unsafe` block.

use std::collections::HashMap;

use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::{Expr, ExprKind, NodeId};
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::types::Type;

type NodeTypeMap = HashMap<NodeId, Type>;
type ResolvedCallMap = HashMap<NodeId, DefId>;

pub(super) fn check_unsafe_ops(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut checker = UnsafeOpsChecker {
        def_table: &engine.context().def_table,
        node_types: &engine.state().solve.resolved_node_types,
        resolved_call_defs: &engine.state().solve.resolved_call_defs,
        errors: Vec::new(),
        unsafe_depth: 0,
    };
    checker.visit_module(&engine.context().module);
    checker.errors
}

struct UnsafeOpsChecker<'a> {
    def_table: &'a DefTable,
    node_types: &'a NodeTypeMap,
    resolved_call_defs: &'a ResolvedCallMap,
    errors: Vec<TypeCheckError>,
    unsafe_depth: usize,
}

impl Visitor for UnsafeOpsChecker<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Unsafe { body } => {
                self.unsafe_depth += 1;
                self.visit_expr(body);
                self.unsafe_depth -= 1;
            }
            ExprKind::Call { .. } => {
                self.check_unsafe_intrinsic_call(expr);
                visit::walk_expr(self, expr);
            }
            ExprKind::MethodCall {
                callee,
                method_name,
                ..
            } => {
                self.check_raw_ptr_method_call(expr, callee, method_name);
                visit::walk_expr(self, expr);
            }
            _ => visit::walk_expr(self, expr),
        }
    }
}

impl UnsafeOpsChecker<'_> {
    fn in_unsafe_context(&self) -> bool {
        self.unsafe_depth > 0
    }

    fn check_unsafe_intrinsic_call(&mut self, expr: &Expr) {
        if self.in_unsafe_context() {
            return;
        }
        let Some(def_id) = self.resolved_call_defs.get(&expr.id).copied() else {
            return;
        };
        let Some(def) = self.def_table.lookup_def(def_id) else {
            return;
        };
        let op_name = match def.link_name().unwrap_or(def.name.as_str()) {
            "ptr_at" => Some("ptr_at"),
            "view_at" => Some("view_at"),
            "view_slice_at" => Some("view_slice_at"),
            "view_array_at" => Some("view_array_at"),
            _ => None,
        };
        if let Some(op_name) = op_name.filter(|_| def.is_intrinsic()) {
            self.errors
                .push(TEK::UnsafeOperationRequiresUnsafeBlock(op_name.into()).at(expr.span));
        }
    }

    fn check_raw_ptr_method_call(&mut self, expr: &Expr, callee: &Expr, method_name: &str) {
        if self.in_unsafe_context() {
            return;
        }
        let Some(receiver_ty) = self.node_types.get(&callee.id) else {
            return;
        };
        if !matches!(receiver_ty, Type::RawPtr { .. }) {
            return;
        }
        let op_name = match method_name {
            "read" => Some("raw pointer read"),
            "write" => Some("raw pointer write"),
            _ => None,
        };
        if let Some(op_name) = op_name {
            self.errors
                .push(TEK::UnsafeOperationRequiresUnsafeBlock(op_name.into()).at(expr.span));
        }
    }
}
