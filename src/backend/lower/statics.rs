//! Lowering support for source-level `static let` / `static var`.

use std::collections::HashMap;

use crate::backend::lower::LowerToIrError;
use crate::backend::lower::globals::GlobalArena;
use crate::core::ast::{ArrayLitInit, Expr, ExprKind, Module, StructLitField, UnaryOp};
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::Type;
use crate::ir::GlobalId;

pub(super) fn lower_static_defs(
    module: &Module,
    def_table: &DefTable,
    type_map: &TypeMap,
    globals: &mut GlobalArena,
) -> Result<HashMap<DefId, GlobalId>, LowerToIrError> {
    let mut out = HashMap::new();

    for static_def in module.static_defs() {
        let def_id = def_table.def_id(static_def.id);
        let def = def_table.lookup_def(def_id).ok_or(LowerToIrError)?;
        let ty = type_map.lookup_def_type(def).ok_or(LowerToIrError)?;
        let bytes = serialize_expr_as_static_bytes(&static_def.init, &ty)?;
        let align = global_align_for_type(&ty, def_table);
        let section = match &def.kind {
            DefKind::Static { attrs, .. } => attrs.section.clone(),
            _ => None,
        };
        let global_id = globals.add_data(bytes, align, section, false);
        out.insert(def_id, global_id);
    }

    Ok(out)
}

fn global_align_for_type(ty: &Type, def_table: &DefTable) -> u32 {
    let natural = ty.align_of() as u32;
    let nominal_align = match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => def_table
            .lookup_type_def_id(name)
            .and_then(|def_id| def_table.lookup_def(def_id))
            .and_then(|def| match &def.kind {
                DefKind::TypeDef { attrs } => attrs.fixed_align.map(|align| align as u32),
                _ => None,
            }),
        _ => None,
    };
    natural.max(nominal_align.unwrap_or(1))
}

fn serialize_expr_as_static_bytes(expr: &Expr, ty: &Type) -> Result<Vec<u8>, LowerToIrError> {
    match (&expr.kind, ty) {
        (ExprKind::Coerce { expr: inner, .. }, _) => serialize_expr_as_static_bytes(inner, ty),
        (ExprKind::UnitLit, Type::Unit) => Ok(Vec::new()),
        (
            ExprKind::NoneLit,
            Type::NullablePAddr | Type::NullableVAddr | Type::NullableView { .. },
        ) => Ok(0u64.to_le_bytes().to_vec()),
        (ExprKind::BoolLit(value), Type::Bool) => Ok(vec![u8::from(*value)]),
        (ExprKind::CharLit(value), Type::Char) => Ok((*value as u32).to_le_bytes().to_vec()),
        (ExprKind::IntLit(value), Type::Int { signed, bits, .. }) => {
            serialize_int_bytes(*value as i128, *signed, *bits)
        }
        (ExprKind::IntLit(value), Type::PAddr)
        | (ExprKind::IntLit(value), Type::NullablePAddr)
        | (ExprKind::IntLit(value), Type::VAddr)
        | (ExprKind::IntLit(value), Type::NullableVAddr) => Ok(value.to_le_bytes().to_vec()),
        (
            ExprKind::UnaryOp {
                op: UnaryOp::Neg,
                expr: inner,
            },
            Type::Int { signed, bits, .. },
        ) => match &inner.kind {
            ExprKind::IntLit(value) => serialize_int_bytes(-(*value as i128), *signed, *bits),
            _ => Err(LowerToIrError),
        },
        (ExprKind::TupleLit(items), Type::Tuple { field_tys })
            if items.len() == field_tys.len() =>
        {
            serialize_sequence(items, field_tys)
        }
        (
            ExprKind::ArrayLit {
                init: ArrayLitInit::Elems(elems),
                ..
            },
            Type::Array { elem_ty, dims },
        ) if dims.len() == 1 && elems.len() == dims[0] => {
            let elem_tys = vec![(**elem_ty).clone(); elems.len()];
            serialize_sequence(elems, &elem_tys)
        }
        (
            ExprKind::ArrayLit {
                init: ArrayLitInit::Repeat(elem, count),
                ..
            },
            Type::Array { elem_ty, dims },
        ) if dims.len() == 1 && (*count as usize) == dims[0] => {
            let mut out = Vec::new();
            for _ in 0..*count {
                out.extend(serialize_expr_as_static_bytes(elem, elem_ty)?);
            }
            Ok(out)
        }
        (ExprKind::StructLit { fields, .. }, Type::Struct { fields: tys, .. }) => {
            let mut out = Vec::new();
            for field_ty in tys {
                let field_expr = find_struct_field(fields, &field_ty.name).ok_or(LowerToIrError)?;
                out.extend(serialize_expr_as_static_bytes(field_expr, &field_ty.ty)?);
            }
            Ok(out)
        }
        _ => Err(LowerToIrError),
    }
}

fn serialize_sequence(items: &[Expr], tys: &[Type]) -> Result<Vec<u8>, LowerToIrError> {
    let mut out = Vec::new();
    for (item, ty) in items.iter().zip(tys) {
        out.extend(serialize_expr_as_static_bytes(item, ty)?);
    }
    Ok(out)
}

fn find_struct_field<'a>(fields: &'a [StructLitField], name: &str) -> Option<&'a Expr> {
    fields
        .iter()
        .find(|field| field.name == name)
        .map(|field| &field.value)
}

fn serialize_int_bytes(value: i128, signed: bool, bits: u8) -> Result<Vec<u8>, LowerToIrError> {
    let width = (bits / 8) as usize;
    if width == 0 || width > 16 {
        return Err(LowerToIrError);
    }

    let all = if signed {
        value.to_le_bytes().to_vec()
    } else {
        (value as u128).to_le_bytes().to_vec()
    };
    Ok(all[..width].to_vec())
}
