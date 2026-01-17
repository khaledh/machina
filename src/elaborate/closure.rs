use crate::diag::Span;
use crate::resolve::{DefId, DefKind};
use crate::semck::closure::capture::CaptureMode;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{NodeId, ParamMode};
use crate::types::{StructField, Type, TypeId};

use super::elaborator::{CaptureField, ClosureContext, ClosureInfo, Elaborator};

impl<'a> Elaborator<'a> {
    fn capture_fields_for(&mut self, closure_def_id: DefId, span: Span) -> Vec<CaptureField> {
        let Some(captures) = self.closure_captures.get(&closure_def_id) else {
            return Vec::new();
        };
        let mut fields = Vec::with_capacity(captures.len());
        for capture in captures {
            let def_id = capture.def_id;
            let def = self
                .def_table
                .lookup_def(def_id)
                .unwrap_or_else(|| panic!("compiler bug: missing def for capture {def_id}"))
                .clone();
            let name = def.name.clone();
            let base_ty = self
                .type_map
                .lookup_def_type(&def)
                .unwrap_or_else(|| panic!("compiler bug: missing type for capture {def_id}"));
            let base_ty_id = self
                .type_map
                .lookup_def_type_id(&def)
                .unwrap_or_else(|| panic!("compiler bug: missing type id for capture {def_id}"));
            // Move captures store the value itself; borrow captures store refs.
            let (field_ty, field_ty_id, field_ty_expr) = match capture.mode {
                CaptureMode::Move => (
                    base_ty.clone(),
                    base_ty_id,
                    self.type_expr_from_type(&base_ty, span),
                ),
                CaptureMode::ImmBorrow | CaptureMode::MutBorrow => {
                    let field_ty = Type::Ref {
                        mutable: capture.mode == CaptureMode::MutBorrow,
                        elem_ty: Box::new(base_ty.clone()),
                    };
                    let field_ty_id = self
                        .type_map
                        .insert_node_type(self.node_id_gen.new_id(), field_ty.clone());
                    let field_ty_expr = self.type_expr_from_type(&field_ty, span);
                    (field_ty, field_ty_id, field_ty_expr)
                }
            };
            fields.push(CaptureField {
                def_id,
                name,
                mode: capture.mode,
                base_ty,
                base_ty_id,
                field_ty,
                field_ty_id,
                field_ty_expr,
            });
        }
        fields
    }

    fn make_closure_type(
        &mut self,
        ident: &str,
        span: Span,
        captures: &[CaptureField],
    ) -> (String, TypeId, Type, DefId) {
        let type_name = ident.to_string();
        let type_def_id = self.def_table.add_def(type_name.clone(), DefKind::TypeDef);
        let fields = captures
            .iter()
            .map(|capture| sem::StructDefField {
                id: self.node_id_gen.new_id(),
                name: capture.name.clone(),
                ty: capture.field_ty_expr.clone(),
                span,
            })
            .collect();
        let type_def = sem::TypeDef {
            id: self.node_id_gen.new_id(),
            def_id: type_def_id,
            name: type_name.clone(),
            kind: sem::TypeDefKind::Struct { fields },
            span,
        };
        self.closure_types.push(type_def);

        let self_def_name = "env".to_string();
        let self_def_id = self.def_table.add_def(
            self_def_name,
            DefKind::Param {
                index: 0,
                is_mutable: false,
            },
        );

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
        let ty_id = self
            .type_map
            .insert_node_type(self.node_id_gen.new_id(), closure_ty.clone());
        if let Some(def) = self.def_table.lookup_def(self_def_id) {
            let _ = self
                .type_map
                .insert_def_type(def.clone(), closure_ty.clone());
        }

        (type_name, ty_id, closure_ty, self_def_id)
    }

    pub(super) fn ensure_closure_info(
        &mut self,
        ident: &str,
        def_id: DefId,
        params: &[norm::Param],
        return_ty: &norm::TypeExpr,
        body: &norm::Expr,
        span: Span,
        expr_id: NodeId,
    ) -> ClosureInfo {
        if let Some(info) = self.closure_info.get(&def_id) {
            return info.clone();
        }

        // Synthesize the closure struct and its invoke method on first sight.
        let captures = self.capture_fields_for(def_id, span);
        let (type_name, type_id, closure_ty, self_def_id) =
            self.make_closure_type(ident, span, &captures);
        let param_modes = params.iter().map(|param| param.mode.clone()).collect();

        let return_ty_val = match self.type_map.lookup_node_type(expr_id) {
            Some(Type::Fn { ret_ty, .. }) => *ret_ty,
            Some(other) => {
                panic!(
                    "compiler bug: closure expr {:?} has non-fn type {:?}",
                    expr_id, other
                )
            }
            None => panic!("compiler bug: missing type for closure expr {:?}", expr_id),
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

        let self_param = sem::SelfParam {
            id: self.node_id_gen.new_id(),
            def_id: self_def_id,
            mode: ParamMode::In,
            span,
        };
        let method_id = self.node_id_gen.new_id();
        self.type_map
            .insert_node_type(method_id, return_ty_val.clone());

        self.closure_stack.push(ClosureContext::new(&info));
        let method_def = sem::MethodDef {
            id: method_id,
            def_id,
            sig: sem::MethodSig {
                name: "invoke".to_string(),
                self_param,
                params: params.to_vec(),
                ret_ty_expr: return_ty.clone(),
                span,
            },
            body: self.elab_value(body),
            span,
        };
        self.closure_stack.pop();

        self.closure_methods.push(sem::MethodBlock {
            id: self.node_id_gen.new_id(),
            type_name: info.type_name.clone(),
            method_defs: vec![method_def],
            span,
        });
        info
    }

    pub(super) fn record_closure_binding(
        &mut self,
        pattern: &norm::BindPattern,
        closure_def_id: DefId,
        info: &ClosureInfo,
    ) {
        let mut defs = Vec::new();
        self.collect_bind_pattern_defs(pattern, &mut defs);
        for def_id in defs {
            self.closure_bindings.insert(def_id, closure_def_id);
            if let Some(def) = self.def_table.lookup_def(def_id) {
                let _ = self.type_map.insert_def_type(def.clone(), info.ty.clone());
            }
        }
    }

    fn collect_bind_pattern_defs(&self, pattern: &norm::BindPattern, out: &mut Vec<DefId>) {
        match &pattern.kind {
            norm::BindPatternKind::Name { def_id, .. } => out.push(*def_id),
            norm::BindPatternKind::Array { patterns }
            | norm::BindPatternKind::Tuple { patterns } => {
                for pattern in patterns {
                    self.collect_bind_pattern_defs(pattern, out);
                }
            }
            norm::BindPatternKind::Struct { fields, .. } => {
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

    pub(super) fn closure_call_info(
        &mut self,
        callee: &norm::Expr,
    ) -> Option<(DefId, ClosureInfo)> {
        match &callee.kind {
            norm::ExprKind::Closure {
                ident,
                def_id,
                params,
                return_ty,
                body,
                captures: _,
            } => Some((
                *def_id,
                self.ensure_closure_info(
                    ident,
                    *def_id,
                    params,
                    return_ty,
                    body,
                    callee.span,
                    callee.id,
                ),
            )),
            norm::ExprKind::Var { def_id, .. } => {
                self.closure_bindings
                    .get(def_id)
                    .and_then(|closure_def_id| {
                        self.closure_info
                            .get(closure_def_id)
                            .map(|info| (*closure_def_id, info.clone()))
                    })
            }
            norm::ExprKind::Move { expr } | norm::ExprKind::ImplicitMove { expr } => {
                self.closure_call_info(expr)
            }
            _ => None,
        }
    }

    pub(super) fn capture_place_for(
        &mut self,
        def_id: DefId,
        place_id: NodeId,
        span: Span,
    ) -> Option<sem::PlaceExpr> {
        let ctx = self.closure_stack.last()?;
        let field = ctx.capture_field(def_id)?;
        // Rewrite captured var uses to env.<field> for move captures,
        // or *env.<field> for borrow captures.
        let env_id = self.node_id_gen.new_id();
        let env_place = sem::PlaceExpr {
            id: env_id,
            kind: sem::PlaceExprKind::Var {
                ident: "env".to_string(),
                def_id: ctx.self_def_id,
            },
            ty: ctx.type_id,
            span,
        };
        self.type_map.insert_node_type(env_id, ctx.ty.clone());

        let field_place_id = self.node_id_gen.new_id();
        let field_place = sem::PlaceExpr {
            id: field_place_id,
            kind: sem::PlaceExprKind::StructField {
                target: Box::new(env_place.clone()),
                field: field.name.clone(),
            },
            ty: field.field_ty_id,
            span,
        };
        self.type_map
            .insert_node_type(field_place_id, field.field_ty.clone());

        if field.mode == CaptureMode::Move {
            // For move captures, env.<field> is already the place.
            self.type_map
                .insert_node_type(place_id, field.base_ty.clone());
            return Some(sem::PlaceExpr {
                id: place_id,
                kind: sem::PlaceExprKind::StructField {
                    target: Box::new(env_place),
                    field: field.name.clone(),
                },
                ty: field.base_ty_id,
                span,
            });
        }

        let load_id = self.node_id_gen.new_id();
        let load = sem::ValueExpr {
            id: load_id,
            kind: sem::ValueExprKind::Load {
                place: Box::new(field_place),
            },
            ty: field.field_ty_id,
            span,
        };
        self.type_map
            .insert_node_type(load_id, field.field_ty.clone());

        self.type_map
            .insert_node_type(place_id, field.base_ty.clone());
        Some(sem::PlaceExpr {
            id: place_id,
            kind: sem::PlaceExprKind::Deref {
                value: Box::new(load),
            },
            ty: field.base_ty_id,
            span,
        })
    }

    pub(super) fn capture_value_for_def(
        &mut self,
        capture: &CaptureField,
        span: Span,
    ) -> sem::ValueExpr {
        let place_id = self.node_id_gen.new_id();
        let place = sem::PlaceExpr {
            id: place_id,
            kind: sem::PlaceExprKind::Var {
                ident: capture.name.clone(),
                def_id: capture.def_id,
            },
            ty: capture.base_ty_id,
            span,
        };
        self.type_map
            .insert_node_type(place_id, capture.base_ty.clone());

        if capture.mode == CaptureMode::Move {
            // Move captures materialize the env field by moving the base.
            let move_id = self.node_id_gen.new_id();
            self.type_map
                .insert_node_type(move_id, capture.base_ty.clone());
            return sem::ValueExpr {
                id: move_id,
                kind: sem::ValueExprKind::Move {
                    place: Box::new(place),
                },
                ty: capture.base_ty_id,
                span,
            };
        }

        let addr_id = self.node_id_gen.new_id();
        self.type_map
            .insert_node_type(addr_id, capture.field_ty.clone());
        sem::ValueExpr {
            id: addr_id,
            kind: sem::ValueExprKind::AddrOf {
                place: Box::new(place),
            },
            ty: capture.field_ty_id,
            span,
        }
    }
}
