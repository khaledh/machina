//! Closure lifting: transform closures into struct types with invoke methods.
//!
//! Closures in the source language capture variables from their environment.
//! This module "lifts" each closure into:
//!
//! 1. A struct type containing the captured variables as fields
//! 2. An `invoke` method that takes the struct as `self` and the original params
//!
//! The closure literal becomes a struct literal that initializes the capture
//! fields, and closure calls become method calls on that struct. Captureless
//! closures are lifted directly to top-level functions and referenced via
//! `ClosureRef`.
//!
//! ## Capture modes
//!
//! - **Move captures**: For aggregate types, the value is moved into the
//!   struct field. Syntax: `[move x] |params| body`. Access inside the
//!   closure body is `env.<field>`.
//!
//! - **Borrow captures**: A reference is stored in the struct field.
//!   Access inside the closure body is `*env.<field>`.
//!
//! ## Example transformation
//!
//! ```text
//! fn main() {
//!     let data = SomeStruct { ... };
//!     let f = [move data] |y| data.field + y;
//!     f(10);
//! }
//! ```
//!
//! Becomes:
//!
//! ```text
//! struct main$closure$1 { data: SomeStruct }
//! impl main$closure$1 { fn invoke(env: Self, y: i32) -> i32 { env.data.field + y } }
//! fn main() {
//!     let data = SomeStruct { ... };
//!     let f = main$closure$1 { data: move data };
//!     f.invoke(10);
//! }
//! ```

use crate::core::analysis::facts::SyntheticReason;
use crate::core::ast::{
    BindPattern, BindPatternKind, Expr, ExprKind, FuncDef, FunctionSig, MethodBlock, MethodDef,
    MethodItem, MethodSig, Module, NodeId, Param, ParamMode, SelfParam, StructDefField,
    TopLevelItem, TypeDef, TypeDefKind, TypeExpr,
};
use crate::core::codegen_names::CodegenNameTable;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefKind, TypeAttrs};
use crate::core::semck::closure::capture::CaptureMode;
use crate::core::types::{StructField, Type, TypeId};

use super::elaborator::{CaptureField, ClosureContext, ClosureInfo, Elaborator};
use std::collections::HashSet;

/// Register generated symbol names for lifted closure methods.
///
/// Closure conversion synthesizes method defs that do not exist in the initial
/// resolved symbol table. Add stable backend names so downstream IR/codegen
/// formatting can print and reference them.
pub(super) fn register_lifted_method_symbols(
    module: &Module,
    def_table: &crate::core::analysis::facts::DefTableOverlay,
    symbols: &mut CodegenNameTable,
) {
    let mut used_names: HashSet<String> = symbols.def_names.values().cloned().collect();
    for method_block in module.method_blocks() {
        let type_name = method_block.type_name.as_str();
        for method_item in &method_block.method_items {
            let method_def = match method_item {
                MethodItem::Def(method_def) => method_def,
                MethodItem::Decl(_) => continue,
            };
            let method_def_id = def_table.def_id(method_def.id);
            if symbols.def_names.contains_key(&method_def_id) {
                continue;
            }
            let base_name = format!("{type_name}${}", method_def.sig.name);
            let name = if used_names.contains(&base_name) {
                format!("{base_name}${}", method_def_id.0)
            } else {
                base_name
            };
            used_names.insert(name.clone());
            symbols.register_generated_def(method_def_id, name);
        }
    }
}

impl<'a> Elaborator<'a> {
    /// Append lifted closure artifacts (types/methods/functions) to module
    /// items. This is the closure-conversion materialization boundary.
    pub(super) fn append_lifted_closure_items(&mut self, top_level_items: &mut Vec<TopLevelItem>) {
        top_level_items.extend(self.closure_types.drain(..).map(TopLevelItem::TypeDef));
        top_level_items.extend(
            self.closure_methods
                .drain(..)
                .map(TopLevelItem::MethodBlock),
        );
        top_level_items.extend(self.closure_funcs.drain(..).map(TopLevelItem::FuncDef));
    }

    /// Returns true when a closure has no captures (or no capture metadata).
    pub(super) fn is_captureless_closure(&self, def_id: DefId) -> bool {
        self.closure_captures
            .get(&def_id)
            .is_none_or(|captures| captures.is_empty())
    }

    /// Build capture field metadata for all variables captured by a closure.
    fn capture_fields_for(&mut self, closure_def_id: DefId, span: Span) -> Vec<CaptureField> {
        let Some(captures) = self.closure_captures.get(&closure_def_id) else {
            return Vec::new();
        };
        let mut fields = Vec::with_capacity(captures.len());
        for capture in captures {
            let def_id = capture.def_id;
            let def = self.def_or_panic(def_id, "closure capture");
            let name = def.name.clone();
            let base_ty = self.def_type_or_panic(&def, "closure capture");
            // Move captures store the value itself; borrow captures store refs.
            let (field_ty, field_ty_expr) = match capture.mode {
                CaptureMode::Move => (base_ty.clone(), self.type_expr_from_type(&base_ty, span)),
                CaptureMode::ImmBorrow | CaptureMode::MutBorrow => {
                    let field_ty = Type::Ref {
                        mutable: capture.mode == CaptureMode::MutBorrow,
                        elem_ty: Box::new(base_ty.clone()),
                    };
                    let field_ty_expr = self.type_expr_from_type(&field_ty, span);
                    (field_ty, field_ty_expr)
                }
            };
            fields.push(CaptureField {
                def_id,
                name,
                mode: capture.mode,
                base_ty,
                field_ty,
                field_ty_expr,
            });
        }
        fields
    }

    /// Ensure that a captureless closure is lifted to a top-level function.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn ensure_closure_func(
        &mut self,
        ident: &str,
        def_id: DefId,
        params: &[Param],
        return_ty: &TypeExpr,
        body: &Expr,
        span: Span,
        expr_id: NodeId,
    ) {
        if !self.is_captureless_closure(def_id) {
            return;
        }

        // Avoid re-emitting the same lifted function for a closure def.
        if !self.closure_func_ids.insert(def_id) {
            return;
        }

        // The closure expression already typechecks as a function type.
        let fn_ty = self.node_type_or_panic(expr_id, "captureless closure expression");
        let return_ty_val = match &fn_ty {
            Type::Fn { ret_ty, .. } => *ret_ty.clone(),
            other => panic!(
                "compiler bug: closure expr {:?} has non-fn type {:?}",
                expr_id, other
            ),
        };

        let def = self.def_or_panic(def_id, "captureless closure");

        // The resolver registers a def_id for closures but doesn't assign a def type.
        if !self.has_def_type(&def) {
            self.insert_def_type_for(def, fn_ty.clone(), SyntheticReason::ClosureLowering);
        }

        // The generated function gets a fresh node id but reuses the closure def id.
        let func_id = self.node_id_gen.new_id();
        self.insert_node_type_for(
            func_id,
            return_ty_val.clone(),
            SyntheticReason::ClosureLowering,
        );

        self.def_table.record_use(func_id, def_id);
        let func_def = FuncDef {
            id: func_id,
            attrs: Vec::new(),
            sig: FunctionSig {
                name: ident.to_string(),
                type_params: Vec::new(),
                params: params.to_vec(),
                ret_ty_expr: return_ty.clone(),
                span,
            },
            body: self.elab_value(body),
            span,
        };

        self.closure_funcs.push(func_def);
    }

    /// Generate the struct type definition for a lifted closure.
    /// Returns the type name, type ID, Type value, and the `env` parameter's DefId.
    fn make_closure_type(
        &mut self,
        ident: &str,
        span: Span,
        captures: &[CaptureField],
    ) -> (String, TypeId, Type, DefId) {
        let type_name = ident.to_string();
        let type_def_id = self.add_synthetic_def(
            type_name.clone(),
            DefKind::TypeDef {
                attrs: TypeAttrs::default(),
            },
            SyntheticReason::ClosureLowering,
        );
        let fields = captures
            .iter()
            .map(|capture| StructDefField {
                id: self.node_id_gen.new_id(),
                name: capture.name.clone(),
                ty: capture.field_ty_expr.clone(),
                span,
            })
            .collect();
        let type_def = TypeDef {
            id: self.node_id_gen.new_id(),
            attrs: Vec::new(),
            name: type_name.clone(),
            type_params: Vec::new(),
            kind: TypeDefKind::Struct { fields },
            span,
        };
        self.def_table
            .record_def_node(type_def_id, type_def.id, span);
        self.closure_types.push(type_def);

        let closure_fields: Vec<StructField> = captures
            .iter()
            .map(|capture| StructField {
                name: capture.name.clone(),
                ty: capture.field_ty.clone(),
            })
            .collect();
        let closure_ty = Type::Struct {
            name: type_name.clone(),
            fields: closure_fields,
        };
        let self_def_name = "env".to_string();
        let self_def_id = self.add_typed_synthetic_def(
            self_def_name,
            DefKind::Param {
                index: 0,
                is_mutable: false,
            },
            closure_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        let closure_ty_node = self.node_id_gen.new_id();
        let ty_id = self.insert_node_type_for(
            closure_ty_node,
            closure_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        (type_name, ty_id, closure_ty, self_def_id)
    }

    /// Ensure that closure metadata exists for a given closure definition.
    ///
    /// On first encounter, this creates the closure struct type, generates
    /// the `invoke` method, and caches the result. Subsequent calls return
    /// the cached info. This lazy approach handles forward references to
    /// closures that haven't been elaborated yet.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn ensure_closure_info(
        &mut self,
        ident: &str,
        def_id: DefId,
        params: &[Param],
        return_ty: &TypeExpr,
        body: &Expr,
        span: Span,
        expr_id: NodeId,
    ) -> ClosureInfo {
        if let Some(info) = self.closure_info.get(&def_id) {
            return info.clone();
        }

        // First encounter: synthesize the closure struct and its invoke method.
        let captures = self.capture_fields_for(def_id, span);
        let (type_name, type_id, closure_ty, self_def_id) =
            self.make_closure_type(ident, span, &captures);
        let param_modes = params.iter().map(|param| param.mode.clone()).collect();

        let return_ty_val = match self.node_type_or_panic(expr_id, "closure expression") {
            Type::Fn { ret_ty, .. } => *ret_ty,
            other => {
                panic!(
                    "compiler bug: closure expr {:?} has non-fn type {:?}",
                    expr_id, other
                )
            }
        };

        let info = ClosureInfo {
            type_name,
            type_id,
            param_modes,
            ty: closure_ty,
            self_def_id,
            captures,
        };
        self.closure_info.insert(def_id, info.clone());

        let self_param = SelfParam {
            id: self.node_id_gen.new_id(),
            mode: ParamMode::In,
            span,
        };
        self.def_table
            .record_def_node(info.self_def_id, self_param.id, span);
        let method_id = self.node_id_gen.new_id();
        self.insert_node_type_for(
            method_id,
            return_ty_val.clone(),
            SyntheticReason::ClosureLowering,
        );

        self.closure_stack.push(ClosureContext::new(&info));
        self.def_table.record_use(method_id, def_id);
        let method_def = MethodDef {
            id: method_id,
            attrs: Vec::new(),
            sig: MethodSig {
                name: "invoke".to_string(),
                type_params: Vec::new(),
                self_param,
                params: params.to_vec(),
                ret_ty_expr: return_ty.clone(),
                span,
            },
            body: self.elab_value(body),
            span,
        };
        self.closure_stack.pop();

        self.closure_methods.push(MethodBlock {
            id: self.node_id_gen.new_id(),
            type_name: info.type_name.clone(),
            trait_name: None,
            method_items: vec![MethodItem::Def(method_def)],
            span,
        });
        info
    }

    /// Record a mapping from bound variable(s) to the closure they hold.
    ///
    /// When a closure is bound to a variable (or destructured into multiple),
    /// this records the mapping so that later references to those variables
    /// can be resolved to the closure's struct type.
    pub(super) fn record_closure_binding(
        &mut self,
        pattern: &BindPattern,
        closure_def_id: DefId,
        info: &ClosureInfo,
    ) {
        let mut defs = Vec::new();
        self.collect_bind_pattern_defs(pattern, &mut defs);
        for def_id in defs {
            self.closure_bindings.insert(def_id, closure_def_id);
            self.insert_def_id_type(def_id, info.ty.clone(), SyntheticReason::ClosureLowering);
        }
    }

    fn collect_bind_pattern_defs(&self, pattern: &BindPattern, out: &mut Vec<DefId>) {
        match &pattern.kind {
            BindPatternKind::Name { .. } => out.push(self.def_id_for(pattern.id)),
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for pattern in patterns {
                    self.collect_bind_pattern_defs(pattern, out);
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.collect_bind_pattern_defs(&field.pattern, out);
                }
            }
        }
    }

    pub(super) fn closure_type_id_for_def(&self, def_id: DefId) -> Option<TypeId> {
        let closure_def_id = self.closure_bindings.get(&def_id)?;
        self.closure_info
            .get(closure_def_id)
            .map(|info| info.type_id)
    }

    /// Check if a callee expression refers to a closure and return its info.
    ///
    /// Handles direct closure literals, variables bound to closures, and
    /// move/implicit-move wrappers around either.
    pub(super) fn closure_call_info(&mut self, callee: &Expr) -> Option<(DefId, ClosureInfo)> {
        match &callee.kind {
            ExprKind::Closure {
                ident,
                params,
                return_ty,
                body,
                captures: _,
            } => {
                let def_id = self.def_id_for(callee.id);
                if self.is_captureless_closure(def_id) {
                    return None;
                }
                Some((
                    def_id,
                    self.ensure_closure_info(
                        ident,
                        def_id,
                        params,
                        return_ty,
                        body,
                        callee.span,
                        callee.id,
                    ),
                ))
            }
            ExprKind::Var { .. } => {
                let def_id = self.def_id_for(callee.id);
                self.closure_bindings
                    .get(&def_id)
                    .and_then(|closure_def_id| {
                        self.closure_info
                            .get(closure_def_id)
                            .map(|info| (*closure_def_id, info.clone()))
                    })
            }
            ExprKind::Move { expr } | ExprKind::ImplicitMove { expr } => {
                self.closure_call_info(expr)
            }
            _ => None,
        }
    }

    /// Rewrite a reference to a captured variable as access through `env`.
    ///
    /// Returns `Some(place)` if `def_id` refers to a captured variable in the
    /// current closure context. The place is either `env.<field>` for move
    /// captures or `*env.<field>` for borrow captures.
    pub(super) fn capture_place_for(
        &mut self,
        def_id: DefId,
        place_id: NodeId,
        span: Span,
    ) -> Option<Expr> {
        let ctx = self.closure_stack.last()?.clone();
        let field = ctx.capture_field(def_id)?.clone();
        let env_id = self.node_id_gen.new_id();
        self.insert_node_type_for(env_id, ctx.ty.clone(), SyntheticReason::ClosureLowering);
        self.def_table.record_use(env_id, ctx.self_def_id);
        let env_place = Expr {
            id: env_id,
            kind: ExprKind::Var {
                ident: "env".to_string(),
            },
            span,
        };

        let field_place_id = self.node_id_gen.new_id();
        self.insert_node_type_for(
            field_place_id,
            field.field_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        let field_place = Expr {
            id: field_place_id,
            kind: ExprKind::StructField {
                target: Box::new(env_place.clone()),
                field: field.name.clone(),
            },
            span,
        };

        if field.mode == CaptureMode::Move {
            // For move captures, env.<field> is already the place.
            self.insert_node_type_for(
                place_id,
                field.base_ty.clone(),
                SyntheticReason::ClosureLowering,
            );
            return Some(Expr {
                id: place_id,
                kind: ExprKind::StructField {
                    target: Box::new(env_place),
                    field: field.name.clone(),
                },
                span,
            });
        }

        let load_id = self.node_id_gen.new_id();
        self.insert_node_type_for(
            load_id,
            field.field_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        let load = Expr {
            id: load_id,
            kind: ExprKind::Load {
                expr: Box::new(field_place),
            },
            span,
        };

        self.insert_node_type_for(
            place_id,
            field.base_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        Some(Expr {
            id: place_id,
            kind: ExprKind::Deref {
                expr: Box::new(load),
            },
            span,
        })
    }

    /// Generate the value expression for a capture field initializer.
    ///
    /// For move captures, produces `move <var>`. For borrow captures,
    /// produces `addr_of <var>`.
    pub(super) fn capture_value_for_def(&mut self, capture: &CaptureField, span: Span) -> Expr {
        let place_id = self.node_id_gen.new_id();
        self.insert_node_type_for(
            place_id,
            capture.base_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        self.def_table.record_use(place_id, capture.def_id);
        let place = Expr {
            id: place_id,
            kind: ExprKind::Var {
                ident: capture.name.clone(),
            },
            span,
        };

        if capture.mode == CaptureMode::Move {
            let move_id = self.node_id_gen.new_id();
            self.insert_node_type_for(
                move_id,
                capture.base_ty.clone(),
                SyntheticReason::ClosureLowering,
            );
            return Expr {
                id: move_id,
                kind: ExprKind::Move {
                    expr: Box::new(place),
                },
                span,
            };
        }

        let addr_id = self.node_id_gen.new_id();
        self.insert_node_type_for(
            addr_id,
            capture.field_ty.clone(),
            SyntheticReason::ClosureLowering,
        );
        Expr {
            id: addr_id,
            kind: ExprKind::AddrOf {
                expr: Box::new(place),
            },
            span,
        }
    }
}
