use crate::ast::{NodeId, ParamMode};
use crate::hir::builder::ToHir;
use crate::hir::model::*;
use crate::resolve::def_map::{Def, DefId, DefKind, DefMap};
use crate::typeck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::types::{EnumVariant, FnParam, FnParamMode, StructField, Type};
use std::collections::{HashMap, HashSet};
use std::fmt;

fn builtin_type(name: &str) -> Option<Type> {
    match name {
        "()" => Some(Type::Unit),
        "u8" => Some(Type::uint(8)),
        "u16" => Some(Type::uint(16)),
        "u32" => Some(Type::uint(32)),
        "u64" => Some(Type::uint(64)),
        "i8" => Some(Type::sint(8)),
        "i16" => Some(Type::sint(16)),
        "i32" => Some(Type::sint(32)),
        "i64" => Some(Type::sint(64)),
        "bool" => Some(Type::Bool),
        "char" => Some(Type::Char),
        "string" => Some(Type::String),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResolveDepth {
    Full,
    Shallow,
}

pub(crate) fn resolve_type_expr(
    def_map: &DefMap,
    type_expr: &TypeExpr,
) -> Result<Type, TypeCheckError> {
    let mut in_progress = HashSet::new();
    resolve_type_expr_impl(def_map, type_expr, &mut in_progress, ResolveDepth::Full)
}

fn resolve_type_expr_impl(
    def_map: &DefMap,
    type_expr: &TypeExpr,
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    match &type_expr.kind {
        TypeExprKind::Named(def_id) => {
            resolve_named_type(def_map, type_expr, def_id, in_progress, depth)
        }
        TypeExprKind::Array { elem_ty_expr, dims } => {
            let elem_ty = resolve_type_expr_impl(def_map, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.clone(),
            })
        }
        TypeExprKind::Tuple { field_ty_exprs } => {
            let field_tys = field_ty_exprs
                .iter()
                .map(|f| resolve_type_expr_impl(def_map, f, in_progress, depth))
                .collect::<Result<Vec<Type>, _>>()?;
            Ok(Type::Tuple { field_tys })
        }
        TypeExprKind::Range { min, max } => Ok(Type::Range {
            min: *min,
            max: *max,
        }),
        TypeExprKind::Slice { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(def_map, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Slice {
                elem_ty: Box::new(elem_ty),
            })
        }
        TypeExprKind::Heap { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(def_map, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Heap {
                elem_ty: Box::new(elem_ty),
            })
        }
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            let param_tys = params
                .iter()
                .map(|param| {
                    let ty = resolve_type_expr_impl(def_map, &param.ty_expr, in_progress, depth)?;
                    Ok(FnParam {
                        mode: fn_param_mode(param.mode.clone()),
                        ty,
                    })
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()?;
            let ret_ty = resolve_type_expr_impl(def_map, ret_ty_expr, in_progress, depth)?;
            Ok(Type::Fn {
                params: param_tys,
                ret_ty: Box::new(ret_ty),
            })
        }
    }
}

fn fn_param_mode(mode: ParamMode) -> FnParamMode {
    match mode {
        ParamMode::In => FnParamMode::In,
        ParamMode::InOut => FnParamMode::InOut,
        ParamMode::Out => FnParamMode::Out,
        ParamMode::Sink => FnParamMode::Sink,
    }
}

fn resolve_named_type(
    def_map: &DefMap,
    type_expr: &TypeExpr,
    def_id: &DefId,
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    let def = def_map
        .lookup_def(*def_id)
        .ok_or(TypeCheckErrorKind::UnknownType(type_expr.span))?;

    if let Some(ty) = builtin_type(&def.name) {
        return Ok(ty);
    }

    match &def.kind {
        DefKind::TypeAlias { ty_expr } => {
            let hir_ty_expr = ty_expr.to_hir(def_map);
            resolve_type_alias(def_map, def, &hir_ty_expr, in_progress)
        }
        DefKind::StructDef { fields } => {
            let hir_fields = fields
                .iter()
                .map(|field| field.to_hir(def_map))
                .collect::<Vec<_>>();
            resolve_struct_type(def_map, def, &hir_fields, in_progress, depth)
        }
        DefKind::EnumDef { variants } => {
            let hir_variants = variants
                .iter()
                .map(|variant| variant.to_hir(def_map))
                .collect::<Vec<_>>();
            resolve_enum_type(def_map, def, &hir_variants, in_progress, depth)
        }
        _ => Err(TypeCheckErrorKind::UnknownType(type_expr.span).into()),
    }
}

fn resolve_type_alias(
    def_map: &DefMap,
    def: &Def,
    ty_expr: &TypeExpr,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        return Ok(Type::Unknown);
    }
    in_progress.insert(def.id);
    let ty = resolve_type_expr_impl(def_map, ty_expr, in_progress, ResolveDepth::Full);
    in_progress.remove(&def.id);
    ty
}

fn resolve_struct_type(
    def_map: &DefMap,
    def: &Def,
    fields: &[StructDefField],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        let struct_fields = match depth {
            ResolveDepth::Full => {
                resolve_struct_fields(def_map, fields, in_progress, ResolveDepth::Shallow)?
            }
            ResolveDepth::Shallow => Vec::new(),
        };
        return Ok(Type::Struct {
            name: def.name.clone(),
            fields: struct_fields,
        });
    }
    in_progress.insert(def.id);
    let struct_fields = resolve_struct_fields(def_map, fields, in_progress, depth)?;
    in_progress.remove(&def.id);
    Ok(Type::Struct {
        name: def.name.clone(),
        fields: struct_fields,
    })
}

fn resolve_struct_fields(
    def_map: &DefMap,
    fields: &[StructDefField],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Vec<StructField>, TypeCheckError> {
    fields
        .iter()
        .map(|field| {
            let field_ty = resolve_type_expr_impl(def_map, &field.ty, in_progress, depth)?;
            Ok(StructField {
                name: field.name.clone(),
                ty: field_ty,
            })
        })
        .collect()
}

fn resolve_enum_type(
    def_map: &DefMap,
    def: &Def,
    variants: &[EnumDefVariant],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        let enum_variants = match depth {
            ResolveDepth::Full => {
                resolve_enum_variants(def_map, variants, in_progress, ResolveDepth::Shallow)?
            }
            ResolveDepth::Shallow => Vec::new(),
        };
        return Ok(Type::Enum {
            name: def.name.clone(),
            variants: enum_variants,
        });
    }
    in_progress.insert(def.id);
    let enum_variants = resolve_enum_variants(def_map, variants, in_progress, depth)?;
    in_progress.remove(&def.id);
    Ok(Type::Enum {
        name: def.name.clone(),
        variants: enum_variants,
    })
}

fn resolve_enum_variants(
    def_map: &DefMap,
    variants: &[EnumDefVariant],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Vec<EnumVariant>, TypeCheckError> {
    let mut enum_variants = Vec::new();
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|payload_ty| resolve_type_expr_impl(def_map, payload_ty, in_progress, depth))
            .collect::<Result<Vec<Type>, _>>()?;
        enum_variants.push(EnumVariant {
            name: variant.name.clone(),
            payload,
        });
    }
    Ok(enum_variants)
}

pub struct TypeMapBuilder {
    node_type: HashMap<NodeId, Type>, // maps node to its type
    def_type: HashMap<Def, Type>,     // maps def to its type
    call_def: HashMap<NodeId, DefId>, // maps call expr node id to func def id (overload-resolved)
    call_sig: HashMap<NodeId, CallSig>,
}

impl Default for TypeMapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeMapBuilder {
    pub fn new() -> Self {
        Self {
            node_type: HashMap::new(),
            def_type: HashMap::new(),
            call_def: HashMap::new(),
            call_sig: HashMap::new(),
        }
    }

    pub fn record_def_type(&mut self, def: Def, typ: Type) {
        self.def_type.insert(def, typ);
    }

    pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
        self.node_type.insert(node_id, typ);
    }

    pub fn record_call_def(&mut self, node_id: NodeId, def_id: DefId) {
        self.call_def.insert(node_id, def_id);
    }

    pub fn record_call_sig(&mut self, node_id: NodeId, sig: CallSig) {
        self.call_sig.insert(node_id, sig);
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type.get(def).cloned()
    }

    pub fn finish(self) -> TypeMap {
        TypeMap {
            def_type: self.def_type,
            node_type: self.node_type,
            call_def: self.call_def,
            call_sig: self.call_sig,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallSig {
    pub receiver: Option<CallParam>,
    pub params: Vec<CallParam>,
}

#[derive(Debug, Clone)]
pub struct CallParam {
    pub mode: ParamMode,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TypeMap {
    def_type: HashMap<Def, Type>,
    node_type: HashMap<NodeId, Type>,
    call_def: HashMap<NodeId, DefId>,
    call_sig: HashMap<NodeId, CallSig>,
}

impl TypeMap {
    pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
        self.node_type.get(&node).cloned()
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type.get(def).cloned()
    }

    pub fn lookup_call_def(&self, node: NodeId) -> Option<DefId> {
        self.call_def.get(&node).cloned()
    }

    pub fn lookup_call_sig(&self, node: NodeId) -> Option<CallSig> {
        self.call_sig.get(&node).cloned()
    }
}

impl<'a> IntoIterator for &'a TypeMap {
    type Item = (&'a Def, &'a Type);
    type IntoIter = std::vec::IntoIter<(&'a Def, &'a Type)>;

    fn into_iter(self) -> Self::IntoIter {
        let mut items: Vec<_> = self.def_type.iter().collect();
        items.sort_by_key(|(def, _)| def.id);
        items.into_iter()
    }
}

impl fmt::Display for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // sort by node id
        let mut node_type = self.node_type.iter().collect::<Vec<(&NodeId, &Type)>>();
        node_type.sort_by_key(|(node, _)| node.0);
        for (node, typ) in node_type {
            writeln!(f, "Node [{}] -> Type [{}]", node, typ)?;
        }
        Ok(())
    }
}
