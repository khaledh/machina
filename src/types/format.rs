use super::*;

use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "()"),
            Type::Int {
                signed,
                bits,
                bounds,
                nonzero,
            } => {
                let prefix = if *signed { "i" } else { "u" };
                let base = format!("{}{}", prefix, bits);
                let mut refinements = Vec::new();
                if let Some(bounds) = bounds {
                    refinements.push(format!("bounds({}, {})", bounds.min, bounds.max_excl));
                }
                if *nonzero {
                    refinements.push("nonzero".to_string());
                }
                if refinements.is_empty() {
                    write!(f, "{base}")
                } else {
                    write!(f, "{}: {}", base, refinements.join(" & "))
                }
            }
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Range { elem_ty } => write!(f, "range<{}>", elem_ty),
            Type::Fn { params, ret_ty } => {
                let params_str = params
                    .iter()
                    .map(|param| {
                        let mode = match param.mode {
                            FnParamMode::In => "",
                            FnParamMode::InOut => "inout ",
                            FnParamMode::Out => "out ",
                            FnParamMode::Sink => "sink ",
                        };
                        format!("{}{}", mode, param.ty)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({}) -> {}", params_str, ret_ty)
            }
            Type::ErrorUnion { ok_ty, err_tys } => {
                let mut variants = Vec::with_capacity(err_tys.len() + 1);
                variants.push(ok_ty.to_string());
                variants.extend(err_tys.iter().map(|ty| ty.to_string()));
                write!(f, "{}", variants.join(" | "))
            }
            Type::String => write!(f, "string"),
            Type::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "{}[{}]", elem_ty, dims_str.join(", "))
            }
            Type::DynArray { elem_ty } => {
                write!(f, "{}[*]", elem_ty)
            }
            Type::Set { elem_ty } => {
                write!(f, "set<{}>", elem_ty)
            }
            Type::Tuple { field_tys } => {
                let fields_str = field_tys.iter().map(|f| f.to_string()).collect::<Vec<_>>();
                write!(f, "({})", fields_str.join(", "))
            }
            Type::Struct { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, f.ty))
                    .collect::<Vec<_>>();
                write!(f, "{} {{ {} }}", name, fields_str.join(", "))
            }
            Type::Enum { name, variants } => {
                let variant_names = variants
                    .iter()
                    .map(|v| {
                        if v.payload.is_empty() {
                            v.name.clone()
                        } else {
                            let payload_str = v
                                .payload
                                .iter()
                                .map(|p| p.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("{}({})", v.name, payload_str)
                        }
                    })
                    .collect::<Vec<_>>();
                write!(f, "{} = {}", name, variant_names.join(" | "))
            }
            Type::Slice { elem_ty } => {
                write!(f, "{}[]", elem_ty)
            }
            Type::Heap { elem_ty } => {
                write!(f, "{}^", elem_ty)
            }
            Type::Ref { mutable, elem_ty } => {
                if *mutable {
                    write!(f, "ref mut {}", elem_ty)
                } else {
                    write!(f, "ref {}", elem_ty)
                }
            }
            Type::Var(var) => write!(f, "T{}", var.index()),
        }
    }
}
