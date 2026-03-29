//! Shared runtime-intrinsic declaration helpers for machine-backed features.
//!
//! Linear hosted machines need to declare runtime intrinsics into the module
//! before resolve/typecheck. The declaration shape is:
//! `fn name(u64, u64, ...) -> u64`.

use std::collections::HashSet;

use crate::core::ast::{
    FuncDecl, FunctionSig, Module, NodeIdGen, Param, ParamMode, TopLevelItem, TypeExpr,
    TypeExprKind,
};
use crate::core::diag::Span;

pub(crate) fn ensure_u64_runtime_intrinsics(
    module: &mut Module,
    node_id_gen: &mut NodeIdGen,
    intrinsics: &[(&str, &[&str])],
) {
    let existing_callables: HashSet<String> = module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::FuncDecl(decl) => Some(decl.sig.name.clone()),
            TopLevelItem::FuncDef(def) => Some(def.sig.name.clone()),
            _ => None,
        })
        .collect();

    let span = Span::default();
    let mut append = Vec::new();

    for &(name, param_names) in intrinsics {
        if existing_callables.contains(name) {
            continue;
        }
        append.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            doc: None,
            attrs: Vec::new(),
            sig: FunctionSig {
                name: name.to_string(),
                type_params: Vec::new(),
                params: param_names
                    .iter()
                    .map(|param_name| Param {
                        id: node_id_gen.new_id(),
                        ident: (*param_name).to_string(),
                        typ: u64_type_expr(node_id_gen, span),
                        mode: ParamMode::In,
                        default: None,
                        span,
                    })
                    .collect(),
                ret_ty_expr: u64_type_expr(node_id_gen, span),
                span,
            },
            span,
        }));
    }

    module.top_level_items.extend(append);
}

fn u64_type_expr(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: "u64".to_string(),
            type_args: Vec::new(),
        },
        span,
    }
}
