//! Shared source-level callable signature rendering for hover/signature-help.
//!
//! This keeps one formatting path for:
//! - source-style labels (`fn foo<T>(x: T) -> T`)
//! - source param names/modes/types

use crate::core::ast::visit::{Visitor, walk_module};
use crate::core::ast::{BindPatternKind, NodeId};
use crate::core::ast::{
    CallableRef, EnumDefVariant, MatchPattern, Module, Param, ParamMode, StmtExprKind,
    StructDefField, TypeDefKind, TypeExpr, TypeExprKind, TypeParam,
};
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::{Type, TypeRenderConfig, render_type};

pub(super) struct CallableSignature {
    pub label: String,
    pub parameters: Vec<String>,
}

pub(super) fn format_source_callable_signature(
    def_id: Option<DefId>,
    typed_module: Option<&Module>,
    type_map: Option<&TypeMap>,
    def_table: &DefTable,
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
        .find(|callable| def_table.def_id(callable.id()) == def_id)?;

    let map_type_params = |params: &[TypeParam]| {
        params
            .iter()
            .map(format_type_param_for_signature)
            .collect::<Vec<_>>()
    };
    let map_params = |params: &[Param]| {
        params
            .iter()
            .map(|param| {
                (
                    param.ident.clone(),
                    param.mode.clone(),
                    def_table.def_id(param.id),
                    param.typ.clone(),
                )
            })
            .collect::<Vec<_>>()
    };

    let (name, type_params, self_mode, params_src): (
        String,
        Vec<String>,
        Option<ParamMode>,
        Vec<(String, ParamMode, DefId, TypeExpr)>,
    ) = match callable {
        CallableRef::FuncDecl(func_decl) => (
            func_decl.sig.name.clone(),
            map_type_params(&func_decl.sig.type_params),
            None,
            map_params(&func_decl.sig.params),
        ),
        CallableRef::FuncDef(func_def) => (
            func_def.sig.name.clone(),
            map_type_params(&func_def.sig.type_params),
            None,
            map_params(&func_def.sig.params),
        ),
        CallableRef::MethodDecl { method_decl, .. } => (
            method_decl.sig.name.clone(),
            map_type_params(&method_decl.sig.type_params),
            Some(method_decl.sig.self_param.mode.clone()),
            map_params(&method_decl.sig.params),
        ),
        CallableRef::MethodDef { method_def, .. } => (
            method_def.sig.name.clone(),
            map_type_params(&method_def.sig.type_params),
            Some(method_def.sig.self_param.mode.clone()),
            map_params(&method_def.sig.params),
        ),
        CallableRef::ClosureDef(closure_def) => (
            "<closure>".to_string(),
            Vec::new(),
            None,
            map_params(&closure_def.sig.params),
        ),
    };

    let type_var_names = type_map.lookup_def_type_param_names(def_id);
    let render = |ty: &Type| {
        render_type(
            ty,
            &TypeRenderConfig {
                show_in_mode: false,
                type_var_names,
                nominal_name_map: None,
            },
        )
    };

    let mut rendered_params =
        Vec::with_capacity(params_src.len() + usize::from(self_mode.is_some()));
    if let Some(self_mode) = self_mode {
        let mode_prefix = match self_mode {
            ParamMode::In => "",
            ParamMode::InOut => "inout ",
            ParamMode::Out => "out ",
            ParamMode::Sink => "sink ",
        };
        rendered_params.push(format!("{mode_prefix}self"));
    }
    for (idx, (param_name, mode, param_def_id, param_ty_expr)) in params_src.into_iter().enumerate()
    {
        let mode_prefix = match mode {
            ParamMode::In => "",
            ParamMode::InOut => "inout ",
            ParamMode::Out => "out ",
            ParamMode::Sink => "sink ",
        };
        let param_ty = format_type_expr_for_signature(&param_ty_expr).unwrap_or_else(|| {
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
        });
        rendered_params.push(format!("{mode_prefix}{param_name}: {param_ty}"));
    }
    let rendered_ret = format_type_expr_for_signature(ret_ty_expr_for_callable(callable))
        .unwrap_or_else(|| render(&ret_ty));
    let rendered_tparams = if type_params.is_empty() {
        String::new()
    } else {
        format!("<{}>", type_params.join(", "))
    };
    let label = format!(
        "fn {name}{rendered_tparams}({}) -> {rendered_ret}",
        rendered_params.join(", ")
    );
    Some(CallableSignature {
        label,
        parameters: rendered_params,
    })
}

pub(super) fn format_source_type_signature(
    def_id: Option<DefId>,
    typed_module: Option<&Module>,
    def_table: &DefTable,
) -> Option<String> {
    let def_id = def_id?;
    let typed_module = typed_module?;
    let def = def_table.lookup_def(def_id)?;
    if !matches!(def.kind, DefKind::TypeDef { .. }) {
        return None;
    }
    let type_def = typed_module.type_def_by_id(def_table, def_id)?;
    let rendered_tparams = if type_def.type_params.is_empty() {
        String::new()
    } else {
        format!(
            "<{}>",
            type_def
                .type_params
                .iter()
                .map(format_type_param_for_signature)
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    let rendered_body = match &type_def.kind {
        TypeDefKind::Alias { aliased_ty } => format_type_expr_for_signature(aliased_ty)?,
        TypeDefKind::Struct { fields } => format_struct_body(fields),
        TypeDefKind::Enum { variants } => format_enum_variants(variants),
        TypeDefKind::Linear { .. } => {
            return Some(format!("type {}{}", type_def.name, rendered_tparams));
        }
    };
    Some(format!(
        "type {}{} = {}",
        type_def.name, rendered_tparams, rendered_body
    ))
}

pub(super) fn format_source_binding_signature(
    node_id: NodeId,
    typed_module: Option<&Module>,
) -> Option<String> {
    let typed_module = typed_module?;

    struct Finder {
        node_id: NodeId,
        found: Option<String>,
    }

    impl Visitor for Finder {
        fn visit_stmt_expr(&mut self, stmt: &crate::core::ast::StmtExpr) {
            if let StmtExprKind::LetBind {
                pattern,
                decl_ty: Some(decl_ty),
                ..
            }
            | StmtExprKind::VarBind {
                pattern,
                decl_ty: Some(decl_ty),
                ..
            } = &stmt.kind
                && pattern.id == self.node_id
                && let BindPatternKind::Name { ident } = &pattern.kind
                && let Some(rendered_ty) = format_type_expr_for_signature(decl_ty)
            {
                self.found = Some(format!("{ident}: {rendered_ty}"));
                return;
            }
            crate::core::ast::visit::walk_stmt_expr(self, stmt);
        }

        fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
            if let MatchPattern::TypedBinding {
                id, ident, ty_expr, ..
            } = pattern
                && *id == self.node_id
                && let Some(rendered_ty) = format_type_expr_for_signature(ty_expr)
            {
                self.found = Some(format!("{ident}: {rendered_ty}"));
                return;
            }
            crate::core::ast::visit::walk_match_pattern(self, pattern);
        }
    }

    let mut finder = Finder {
        node_id,
        found: None,
    };
    walk_module(&mut finder, typed_module);
    finder.found
}

pub(super) fn format_source_struct_field_signature(
    typed_module: Option<&Module>,
    owner_ty: &Type,
    field_name: &str,
) -> Option<String> {
    let typed_module = typed_module?;
    let Type::Struct { name, .. } = owner_ty else {
        return None;
    };
    let type_def = typed_module
        .type_defs()
        .into_iter()
        .find(|type_def| type_def.name == *name)?;
    let fields = match &type_def.kind {
        TypeDefKind::Struct { fields } => fields,
        TypeDefKind::Linear { linear } => &linear.fields,
        TypeDefKind::Alias { .. } | TypeDefKind::Enum { .. } => return None,
    };
    let field = fields.iter().find(|field| field.name == field_name)?;
    let rendered_ty = format_type_expr_for_signature(&field.ty).unwrap_or_else(|| "_".to_string());
    Some(format!("{field_name}: {rendered_ty}"))
}

fn format_type_param_for_signature(param: &TypeParam) -> String {
    if let Some(bound) = &param.bound {
        format!("{}: {}", param.ident, bound.name)
    } else {
        param.ident.clone()
    }
}

fn format_struct_body(fields: &[StructDefField]) -> String {
    if fields.is_empty() {
        "{}".to_string()
    } else {
        format!("{{ {} }}", format_struct_fields(fields))
    }
}

fn format_struct_fields(fields: &[StructDefField]) -> String {
    fields
        .iter()
        .map(|field| {
            let ty = format_type_expr_for_signature(&field.ty).unwrap_or_else(|| "_".to_string());
            format!("{}: {}", field.name, ty)
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_enum_variants(variants: &[EnumDefVariant]) -> String {
    variants
        .iter()
        .map(|variant| {
            if variant.payload.is_empty() {
                variant.name.clone()
            } else {
                let payload = variant
                    .payload
                    .iter()
                    .filter_map(format_type_expr_for_signature)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", variant.name, payload)
            }
        })
        .collect::<Vec<_>>()
        .join(" | ")
}
fn ret_ty_expr_for_callable(callable: CallableRef<'_>) -> &TypeExpr {
    match callable {
        CallableRef::FuncDecl(func_decl) => &func_decl.sig.ret_ty_expr,
        CallableRef::FuncDef(func_def) => &func_def.sig.ret_ty_expr,
        CallableRef::MethodDecl { method_decl, .. } => &method_decl.sig.ret_ty_expr,
        CallableRef::MethodDef { method_def, .. } => &method_def.sig.ret_ty_expr,
        CallableRef::ClosureDef(closure_def) => &closure_def.sig.return_ty,
    }
}

pub(super) fn format_type_expr_for_signature(ty_expr: &TypeExpr) -> Option<String> {
    use TypeExprKind;
    Some(match &ty_expr.kind {
        TypeExprKind::Infer => "_".to_string(),
        TypeExprKind::Named {
            ident, type_args, ..
        } => {
            if type_args.is_empty() {
                ident.clone()
            } else {
                let args: Vec<_> = type_args
                    .iter()
                    .filter_map(format_type_expr_for_signature)
                    .collect();
                format!("{ident}<{}>", args.join(", "))
            }
        }
        TypeExprKind::Array { elem_ty_expr, dims } => {
            let mut elem = format_type_expr_for_signature(elem_ty_expr)?;
            for dim in dims {
                elem = format!("{elem}[{dim}]");
            }
            elem
        }
        TypeExprKind::DynArray { elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr)?;
            format!("{elem}[*]")
        }
        TypeExprKind::Slice { elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr)?;
            format!("{elem}[]")
        }
        TypeExprKind::Heap { elem_ty_expr } => {
            let elem = format_type_expr_for_signature(elem_ty_expr)?;
            format!("{elem}^")
        }
        TypeExprKind::Tuple { field_ty_exprs } => {
            let fields: Vec<_> = field_ty_exprs
                .iter()
                .filter_map(format_type_expr_for_signature)
                .collect();
            format!("({})", fields.join(", "))
        }
        TypeExprKind::Union { variants } => {
            let fields: Vec<_> = variants
                .iter()
                .filter_map(format_type_expr_for_signature)
                .collect();
            fields.join(" | ")
        }
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            let params: Vec<_> = params
                .iter()
                .filter_map(|param| {
                    let ty = format_type_expr_for_signature(&param.ty_expr)?;
                    let mode = match param.mode {
                        ParamMode::In => "",
                        ParamMode::InOut => "inout ",
                        ParamMode::Out => "out ",
                        ParamMode::Sink => "sink ",
                    };
                    Some(format!("{mode}{ty}"))
                })
                .collect();
            let ret = format_type_expr_for_signature(ret_ty_expr)?;
            format!("fn({}) -> {ret}", params.join(", "))
        }
        TypeExprKind::Refined {
            base_ty_expr,
            refinements: _,
        } => format_type_expr_for_signature(base_ty_expr)?,
        TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => {
            let elem = format_type_expr_for_signature(elem_ty_expr)?;
            if *mutable {
                format!("mut {elem}")
            } else {
                elem
            }
        }
    })
}
