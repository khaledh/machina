use super::{FnParamMode, Type};
use std::collections::BTreeMap;

#[derive(Default)]
pub struct TypeRenderConfig<'a> {
    pub show_in_mode: bool,
    pub type_var_names: Option<&'a BTreeMap<u32, String>>,
    pub nominal_name_map: Option<&'a dyn Fn(&str) -> String>,
}

pub fn render_type(ty: &Type, cfg: &TypeRenderConfig<'_>) -> String {
    let mut seen_vars = Vec::new();
    render_type_inner(ty, cfg, &mut seen_vars)
}

fn render_type_inner(ty: &Type, cfg: &TypeRenderConfig<'_>, seen_vars: &mut Vec<u32>) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => {
            render_nominal_name(name, cfg, seen_vars)
        }
        Type::Array { elem_ty, dims } => {
            let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
            format!(
                "{}[{}]",
                render_type_inner(elem_ty, cfg, seen_vars),
                dims_str.join(", ")
            )
        }
        Type::DynArray { elem_ty } => {
            format!("{}[*]", render_type_inner(elem_ty, cfg, seen_vars))
        }
        Type::Set { elem_ty } => {
            format!("set<{}>", render_type_inner(elem_ty, cfg, seen_vars))
        }
        Type::Map { key_ty, value_ty } => format!(
            "map<{}, {}>",
            render_type_inner(key_ty, cfg, seen_vars),
            render_type_inner(value_ty, cfg, seen_vars)
        ),
        Type::Tuple { field_tys } => {
            let fields = field_tys
                .iter()
                .map(|field| render_type_inner(field, cfg, seen_vars))
                .collect::<Vec<_>>();
            format!("({})", fields.join(", "))
        }
        Type::Slice { elem_ty } => {
            format!("{}[]", render_type_inner(elem_ty, cfg, seen_vars))
        }
        Type::Heap { elem_ty } => format!("{}^", render_type_inner(elem_ty, cfg, seen_vars)),
        Type::Ref { mutable, elem_ty } => {
            if *mutable {
                format!("ref mut {}", render_type_inner(elem_ty, cfg, seen_vars))
            } else {
                format!("ref {}", render_type_inner(elem_ty, cfg, seen_vars))
            }
        }
        Type::Range { elem_ty } => {
            format!("range<{}>", render_type_inner(elem_ty, cfg, seen_vars))
        }
        Type::Pending { response_tys } => format!(
            "Pending<{}>",
            response_tys
                .iter()
                .map(|ty| render_type_inner(ty, cfg, seen_vars))
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        Type::ReplyCap { response_tys } => format!(
            "ReplyCap<{}>",
            response_tys
                .iter()
                .map(|ty| render_type_inner(ty, cfg, seen_vars))
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        Type::ErrorUnion { ok_ty, err_tys } => {
            let mut parts = Vec::with_capacity(err_tys.len() + 1);
            parts.push(render_type_inner(ok_ty, cfg, seen_vars));
            parts.extend(
                err_tys
                    .iter()
                    .map(|err_ty| render_type_inner(err_ty, cfg, seen_vars)),
            );
            parts.join(" | ")
        }
        Type::Fn { params, ret_ty } => {
            let params_str = params
                .iter()
                .map(|param| {
                    let mode = match param.mode {
                        FnParamMode::In if !cfg.show_in_mode => "",
                        FnParamMode::In => "in ",
                        FnParamMode::InOut => "inout ",
                        FnParamMode::Out => "out ",
                        FnParamMode::Sink => "sink ",
                    };
                    format!("{}{}", mode, render_type_inner(&param.ty, cfg, seen_vars))
                })
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = render_type_inner(ret_ty, cfg, seen_vars);
            if seen_vars.is_empty() {
                format!("fn({params_str}) -> {ret_str}")
            } else {
                let tparams = seen_vars
                    .iter()
                    .enumerate()
                    .map(|(idx, var_id)| {
                        cfg.type_var_names
                            .and_then(|names| {
                                names.get(var_id).or_else(|| names.get(&(idx as u32)))
                            })
                            .cloned()
                            .unwrap_or_else(|| format!("T{idx}"))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn<{tparams}>({params_str}) -> {ret_str}")
            }
        }
        Type::Var(var) => render_type_var(var.index(), cfg, seen_vars),
        _ => ty.to_string(),
    }
}

fn render_type_var(var_id: u32, cfg: &TypeRenderConfig<'_>, seen_vars: &mut Vec<u32>) -> String {
    if let Some(pos) = seen_vars.iter().position(|id| *id == var_id) {
        return cfg
            .type_var_names
            .and_then(|names| names.get(&var_id).or_else(|| names.get(&(pos as u32))))
            .cloned()
            .unwrap_or_else(|| format!("T{pos}"));
    }
    seen_vars.push(var_id);
    let pos = seen_vars.len() - 1;
    cfg.type_var_names
        .and_then(|names| names.get(&var_id).or_else(|| names.get(&(pos as u32))))
        .cloned()
        .unwrap_or_else(|| format!("T{pos}"))
}

fn render_nominal_name(name: &str, cfg: &TypeRenderConfig<'_>, seen_vars: &mut Vec<u32>) -> String {
    let replaced = if name.contains('<') {
        replace_tyvars_in_text(name, cfg, seen_vars)
    } else {
        name.split('<').next().unwrap_or(name).trim().to_string()
    };
    if let Some(mapper) = cfg.nominal_name_map {
        mapper(&replaced)
    } else {
        replaced
    }
}

fn replace_tyvars_in_text(
    text: &str,
    cfg: &TypeRenderConfig<'_>,
    seen_vars: &mut Vec<u32>,
) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == 'T' {
            let mut digits = String::new();
            while let Some(next) = chars.peek() {
                if next.is_ascii_digit() {
                    digits.push(*next);
                    chars.next();
                } else {
                    break;
                }
            }
            if !digits.is_empty()
                && let Ok(id) = digits.parse::<u32>()
            {
                out.push_str(&render_type_var(id, cfg, seen_vars));
                continue;
            }
            out.push('T');
            out.push_str(&digits);
            continue;
        }
        out.push(ch);
    }
    out
}
