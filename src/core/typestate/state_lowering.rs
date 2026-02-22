//! Lowering helpers that convert validated typestate state members into
//! ordinary method blocks and rewrite state-literal transitions.

use std::collections::{HashMap, HashSet};

use crate::core::diag::Span;
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::*;

/// Shared lowering inputs for methods/handlers within one source state.
pub(super) struct StateLoweringContext<'a> {
    pub(super) source_state_names: &'a HashSet<String>,
    pub(super) local_fields_by_state: &'a HashMap<String, Vec<StructDefField>>,
    pub(super) carried_field_names: &'a [String],
    pub(super) state_name_map: &'a HashMap<String, String>,
    pub(super) span: Span,
}

pub(super) fn collect_first_state_fields_block(
    items: &[TypestateStateItem],
) -> Vec<StructDefField> {
    items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Fields(TypestateFields { fields, .. }) => Some(fields.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

pub(super) fn lower_state_method(
    mut method: FuncDef,
    node_id_gen: &mut NodeIdGen,
    ctx: &StateLoweringContext<'_>,
) -> MethodDef {
    // Run carried-field transition rewrites while state names are still in
    // source form (`Disconnected`, `Connected`, ...).
    rewrite_transition_literals_in_method(
        &mut method,
        node_id_gen,
        ctx.source_state_names,
        ctx.local_fields_by_state,
        ctx.carried_field_names,
    );
    // Then rewrite state names to generated nominal names.
    rewrite_state_refs_in_func(&mut method, ctx.state_name_map);
    MethodDef {
        id: method.id,
        attrs: method.attrs,
        sig: MethodSig {
            name: method.sig.name,
            type_params: method.sig.type_params,
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                mode: ParamMode::Sink,
                span: ctx.span,
            },
            params: method.sig.params,
            ret_ty_expr: method.sig.ret_ty_expr,
            span: method.sig.span,
        },
        body: method.body,
        span: method.span,
    }
}

fn rewrite_transition_literals_in_method(
    method: &mut FuncDef,
    node_id_gen: &mut NodeIdGen,
    source_state_names: &HashSet<String>,
    local_fields_by_state: &HashMap<String, Vec<StructDefField>>,
    carried_field_names: &[String],
) {
    // This rewrite is intentionally method-local; it should not affect
    // constructor bodies or unrelated free functions.
    let mut rewriter = TransitionLiteralRewriter {
        node_id_gen,
        source_state_names,
        local_fields_by_state,
        carried_field_names,
    };
    rewriter.visit_expr(&mut method.body);
}

struct TransitionLiteralRewriter<'a> {
    // Used to synthesize AST nodes while preserving node-id uniqueness.
    node_id_gen: &'a mut NodeIdGen,
    // Source state names for quick match on `State` / `State { ... }`.
    source_state_names: &'a HashSet<String>,
    // Local fields for each state. Needed for shorthand `State` legality.
    local_fields_by_state: &'a HashMap<String, Vec<StructDefField>>,
    // Carried field names copied from typestate-level `fields { ... }`.
    carried_field_names: &'a [String],
}

impl TransitionLiteralRewriter<'_> {
    // Builds `self.<field>` expression used for implicit carried-field transfer.
    fn build_self_field_expr(&mut self, field_name: &str, span: Span) -> Expr {
        Expr {
            id: self.node_id_gen.new_id(),
            kind: ExprKind::StructField {
                target: Box::new(Expr {
                    id: self.node_id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: "self".to_string(),
                        // Resolved later in the normal resolver pass.
                    },
                    span,
                }),
                field: field_name.to_string(),
            },
            span,
        }
    }

    fn carried_struct_fields(&mut self, span: Span) -> Vec<StructLitField> {
        // Materialize all carried fields as `name: self.name`.
        self.carried_field_names
            .iter()
            .cloned()
            .map(|name| StructLitField {
                id: self.node_id_gen.new_id(),
                name: name.clone(),
                value: self.build_self_field_expr(&name, span),
                span,
            })
            .collect()
    }
}

impl VisitorMut for TransitionLiteralRewriter<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        // Visit children first so nested transition literals are rewritten
        // before parent transforms potentially replace this node.
        visit_mut::walk_expr(self, expr);

        match &mut expr.kind {
            ExprKind::StructLit { name, fields, .. } if self.source_state_names.contains(name) => {
                // `State { ... }`: inject only missing carried fields.
                let existing: HashSet<String> =
                    fields.iter().map(|field| field.name.clone()).collect();
                for field_name in self.carried_field_names {
                    if existing.contains(field_name) {
                        // User-provided value wins over implicit carry.
                        continue;
                    }
                    fields.push(StructLitField {
                        id: self.node_id_gen.new_id(),
                        name: field_name.clone(),
                        value: self.build_self_field_expr(field_name, expr.span),
                        span: expr.span,
                    });
                }
            }
            ExprKind::Var { ident, .. } if self.source_state_names.contains(ident) => {
                // `State` shorthand is only valid when the target state has no
                // local fields. We rewrite it to an explicit struct literal.
                if self
                    .local_fields_by_state
                    .get(ident)
                    .is_some_and(|fields| fields.is_empty())
                {
                    expr.kind = ExprKind::StructLit {
                        name: ident.clone(),
                        type_args: Vec::new(),
                        fields: self.carried_struct_fields(expr.span),
                    };
                }
            }
            _ => {}
        }
    }
}

pub(super) fn rewrite_state_refs_in_func(
    func: &mut FuncDef,
    state_name_map: &HashMap<String, String>,
) {
    // Generic state-name rewrite used by lowered constructors and methods.
    let mut rewriter = StateRefRewriter::new(state_name_map);
    rewriter.visit_func_def(func);
}

struct StateRefRewriter<'a> {
    state_name_map: &'a HashMap<String, String>,
}

impl<'a> StateRefRewriter<'a> {
    fn new(state_name_map: &'a HashMap<String, String>) -> Self {
        Self { state_name_map }
    }

    fn rewrite_name(&self, name: &mut String) {
        // Only rewrite known state names; non-state symbols are untouched.
        if let Some(new_name) = self.state_name_map.get(name) {
            *name = new_name.clone();
        }
    }
}

impl VisitorMut for StateRefRewriter<'_> {
    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        if let TypeExprKind::Named { ident, .. } = &mut type_expr.kind {
            self.rewrite_name(ident);
        }
        visit_mut::walk_type_expr(self, type_expr);
    }

    fn visit_bind_pattern(&mut self, pattern: &mut BindPattern) {
        if let BindPatternKind::Struct { name, .. } = &mut pattern.kind {
            self.rewrite_name(name);
        }
        visit_mut::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &mut MatchPattern) {
        if let MatchPattern::EnumVariant {
            enum_name: Some(enum_name),
            ..
        } = pattern
        {
            self.rewrite_name(enum_name);
        }
        visit_mut::walk_match_pattern(self, pattern);
    }

    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        // `decl_ty` in let/var is not traversed by generic walk; handle it here.
        match &mut stmt.kind {
            StmtExprKind::LetBind {
                decl_ty: Some(decl_ty),
                ..
            }
            | StmtExprKind::VarBind {
                decl_ty: Some(decl_ty),
                ..
            } => self.visit_type_expr(decl_ty),
            StmtExprKind::VarDecl { decl_ty, .. } => self.visit_type_expr(decl_ty),
            _ => {}
        }
        visit_mut::walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::ArrayLit {
                elem_ty: Some(elem_ty),
                ..
            } => self.visit_type_expr(elem_ty),
            ExprKind::StructLit { name, .. } => self.rewrite_name(name),
            ExprKind::EnumVariant { enum_name, .. } => self.rewrite_name(enum_name),
            _ => {}
        }
        visit_mut::walk_expr(self, expr);
    }
}
