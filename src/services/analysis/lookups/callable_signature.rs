//! Shared source-level callable signature rendering for hover/signature-help.
//!
//! This keeps one formatting path for:
//! - source-style labels (`fn foo<T>(x: T) -> T`)
//! - source param names/modes/types
//! - typestate-name demangling

use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::tree::typed as typed_tree;
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::{Type, TypeRenderConfig, render_type};

use super::TypestateNameDemangler;

pub(super) struct CallableSignature {
    pub label: String,
    pub parameters: Vec<String>,
}

pub(super) fn format_source_callable_signature(
    def_id: Option<DefId>,
    typed_module: Option<&typed_tree::Module>,
    type_map: Option<&TypeMap>,
    def_table: &DefTable,
    demangler: &TypestateNameDemangler,
) -> Option<CallableSignature> {
    let def_id = def_id?;
    let typed_module = typed_module?;
    let type_map = type_map?;
    let def = def_table.lookup_def(def_id)?;
    if !matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
        return None;
    }
    let fn_ty = type_map.lookup_def_type(def)?;
    let Type::Fn { params, ret_ty } = fn_ty else {
        return None;
    };

    let callable = typed_module
        .callables()
        .into_iter()
        .find(|callable| match callable {
            typed_tree::CallableRef::FuncDecl(func_decl) => func_decl.def_id == def_id,
            typed_tree::CallableRef::FuncDef(func_def) => func_def.def_id == def_id,
            typed_tree::CallableRef::MethodDecl { method_decl, .. } => method_decl.def_id == def_id,
            typed_tree::CallableRef::MethodDef { method_def, .. } => method_def.def_id == def_id,
            typed_tree::CallableRef::ClosureDef(closure_def) => closure_def.def_id == def_id,
        })?;

    let map_type_params = |params: &[typed_tree::TypeParam]| {
        params
            .iter()
            .map(|param| param.ident.clone())
            .collect::<Vec<_>>()
    };
    let map_params = |params: &[typed_tree::Param]| {
        params
            .iter()
            .map(|param| {
                (
                    param.ident.clone(),
                    param.mode.clone(),
                    param.def_id,
                    param.typ.clone(),
                )
            })
            .collect::<Vec<_>>()
    };

    let (name, type_params, params_src): (
        String,
        Vec<String>,
        Vec<(String, crate::core::tree::ParamMode, DefId, typed_tree::TypeExpr)>,
    ) = match callable {
        typed_tree::CallableRef::FuncDecl(func_decl) => (
            func_decl.sig.name.clone(),
            map_type_params(&func_decl.sig.type_params),
            map_params(&func_decl.sig.params),
        ),
        typed_tree::CallableRef::FuncDef(func_def) => (
            func_def.sig.name.clone(),
            map_type_params(&func_def.sig.type_params),
            map_params(&func_def.sig.params),
        ),
        typed_tree::CallableRef::MethodDecl { method_decl, .. } => (
            method_decl.sig.name.clone(),
            map_type_params(&method_decl.sig.type_params),
            map_params(&method_decl.sig.params),
        ),
        typed_tree::CallableRef::MethodDef { method_def, .. } => (
            method_def.sig.name.clone(),
            map_type_params(&method_def.sig.type_params),
            map_params(&method_def.sig.params),
        ),
        typed_tree::CallableRef::ClosureDef(closure_def) => (
            "<closure>".to_string(),
            Vec::new(),
            map_params(&closure_def.sig.params),
        ),
    };

    let type_var_names = type_map.lookup_def_type_param_names(def_id);
    let nominal_name_map = |name: &str| {
        if let Some(state_name) = demangler.demangle_state_qualified(name) {
            state_name
        } else {
            demangler.demangle_text(name)
        }
    };
    let render = |ty: &Type| {
        render_type(
            ty,
            &TypeRenderConfig {
                show_in_mode: false,
                type_var_names,
                nominal_name_map: Some(&nominal_name_map),
            },
        )
    };

    let mut rendered_params = Vec::with_capacity(params_src.len());
    for (idx, (param_name, mode, param_def_id, param_ty_expr)) in params_src.into_iter().enumerate()
    {
        let mode_prefix = match mode {
            crate::core::tree::ParamMode::In => "",
            crate::core::tree::ParamMode::InOut => "inout ",
            crate::core::tree::ParamMode::Out => "out ",
            crate::core::tree::ParamMode::Sink => "sink ",
        };
        let param_ty = format_type_expr_for_signature(&param_ty_expr, demangler).unwrap_or_else(
            || {
                let param_ty = params
                    .get(idx)
                    .map(|param| param.ty.clone())
                    .or_else(|| {
                        def_table
                            .lookup_def(param_def_id)
                            .and_then(|param_def| type_map.lookup_def_type(param_def))
                    })
                    .unwrap_or(Type::Unknown);
                render(&param_ty)
            },
        );
        rendered_params.push(format!("{mode_prefix}{param_name}: {param_ty}"));
    }
    let rendered_name = demangler.demangle_text(&name);
    let rendered_ret = render(&ret_ty);
    let rendered_tparams = if type_params.is_empty() {
        String::new()
    } else {
        format!("<{}>", type_params.join(", "))
    };
    let label = format!(
        "fn {rendered_name}{rendered_tparams}({}) -> {rendered_ret}",
        rendered_params.join(", ")
    );
    Some(CallableSignature {
        label,
        parameters: rendered_params,
    })
}

fn format_type_expr_for_signature(
    ty_expr: &typed_tree::TypeExpr,
    demangler: &TypestateNameDemangler,
) -> Option<String> {
    use crate::core::tree::TypeExprKind;
    Some(match &ty_expr.kind {
        TypeExprKind::Infer => "_".to_string(),
        TypeExprKind::Named {
            ident, type_args, ..
        } => {
            if type_args.is_empty() {
                demangler.demangle_text(ident)
            } else {
                let args: Vec<_> = type_args
                    .iter()
                    .filter_map(|arg| format_type_expr_for_signature(arg, demangler))
                    .collect();
                format!("{}<{}>", demangler.demangle_text(ident), args.join(", "))
            }
        }
        TypeExprKind::Array { elem_ty_expr, dims } => {
            let mut elem = format_type_expr_for_signature(elem_ty_expr, demangler)?;
            for dim in dims {
                elem = format!("{elem}[{dim}]");
            }
            elem
        }
        TypeExprKind::DynArray { elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr, demangler)?;
            format!("{elem}[*]")
        }
        TypeExprKind::Slice { elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr, demangler)?;
            format!("{elem}[]")
        }
        TypeExprKind::Heap { elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr, demangler)?;
            format!("{elem}^")
        }
        TypeExprKind::Tuple { field_ty_exprs } => {
            let fields: Vec<_> = field_ty_exprs
                .iter()
                .filter_map(|field| format_type_expr_for_signature(field, demangler))
                .collect();
            format!("({})", fields.join(", "))
        }
        TypeExprKind::Union { variants } => {
            let fields: Vec<_> = variants
                .iter()
                .filter_map(|field| format_type_expr_for_signature(field, demangler))
                .collect();
            fields.join(" | ")
        }
        TypeExprKind::Fn { params, ret_ty_expr } => {
            let params: Vec<_> = params
                .iter()
                .filter_map(|param| {
                    let ty = format_type_expr_for_signature(&param.ty_expr, demangler)?;
                    let mode = match param.mode {
                        crate::core::tree::ParamMode::In => "",
                        crate::core::tree::ParamMode::InOut => "inout ",
                        crate::core::tree::ParamMode::Out => "out ",
                        crate::core::tree::ParamMode::Sink => "sink ",
                    };
                    Some(format!("{mode}{ty}"))
                })
                .collect();
            let ret = format_type_expr_for_signature(ret_ty_expr, demangler)?;
            format!("fn({}) -> {ret}", params.join(", "))
        }
        TypeExprKind::Refined {
            base_ty_expr,
            refinements: _,
        } => format_type_expr_for_signature(base_ty_expr, demangler)?,
        TypeExprKind::Ref { mutable, elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr, demangler)?;
            if *mutable {
                format!("mut {elem}")
            } else {
                elem
            }
        }
    })
}
