use super::*;

use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "()"),
            Type::UInt64 => write!(f, "u64"),
            Type::UInt32 => write!(f, "u32"),
            Type::UInt8 => write!(f, "u8"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Range { min, max } => write!(f, "range({}, {})", min, max),
            Type::String => write!(f, "string"),
            Type::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "{}[{}]", elem_ty, dims_str.join(", "))
            }
            Type::Tuple { fields } => {
                let fields_str = fields.iter().map(|f| f.to_string()).collect::<Vec<_>>();
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
        }
    }
}
