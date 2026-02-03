use super::*;

use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "()"),
            Type::Int { signed, bits } => {
                let prefix = if *signed { "i" } else { "u" };
                write!(f, "{}{}", prefix, bits)
            }
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Range { min, max } => match (min, max) {
                (Some(min), Some(max)) => write!(f, "range({}, {})", min, max),
                _ => write!(f, "range"),
            },
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
            Type::String => write!(f, "string"),
            Type::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "{}[{}]", elem_ty, dims_str.join(", "))
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
                write!(f, "^{}", elem_ty)
            }
            Type::Ref { mutable, elem_ty } => {
                if *mutable {
                    write!(f, "ref mut {}", elem_ty)
                } else {
                    write!(f, "ref {}", elem_ty)
                }
            }
        }
    }
}
