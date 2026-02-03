use crate::resolve::{Def, DefId, DefKind, DefTable};
use crate::tree::normalized as norm;
use crate::tree::resolved as res;
use crate::tree::semantic as sem;
use crate::tree::semantic::{CallPlan, IndexPlan, MatchPlan, SlicePlan};
use crate::tree::{NodeId, ParamMode, RefinementKind};
use crate::typeck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::types::{EnumVariant, FnParam, FnParamMode, StructField, Type, TypeCache, TypeId};
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
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
) -> Result<Type, TypeCheckError> {
    let mut in_progress = HashSet::new();
    resolve_type_expr_impl(
        def_table,
        module,
        type_expr,
        &mut in_progress,
        ResolveDepth::Full,
    )
}

pub(crate) trait TypeDefLookup {
    fn type_def_by_id(&self, def_id: DefId) -> Option<&res::TypeDef>;
}

impl TypeDefLookup for res::Module {
    fn type_def_by_id(&self, def_id: DefId) -> Option<&res::TypeDef> {
        res::Module::type_def_by_id(self, def_id)
    }
}

impl TypeDefLookup for norm::Module {
    fn type_def_by_id(&self, def_id: DefId) -> Option<&res::TypeDef> {
        norm::Module::type_def_by_id(self, def_id)
    }
}

impl TypeDefLookup for sem::Module {
    fn type_def_by_id(&self, def_id: DefId) -> Option<&res::TypeDef> {
        sem::Module::type_def_by_id(self, def_id)
    }
}

fn resolve_type_expr_impl(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    match &type_expr.kind {
        res::TypeExprKind::Named { def_id, .. } => {
            resolve_named_type(def_table, module, type_expr, def_id, in_progress, depth)
        }
        res::TypeExprKind::Array { elem_ty_expr, dims } => {
            let elem_ty =
                resolve_type_expr_impl(def_table, module, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.clone(),
            })
        }
        res::TypeExprKind::Tuple { field_ty_exprs } => {
            let field_tys = field_ty_exprs
                .iter()
                .map(|f| resolve_type_expr_impl(def_table, module, f, in_progress, depth))
                .collect::<Result<Vec<Type>, _>>()?;
            Ok(Type::Tuple { field_tys })
        }
        res::TypeExprKind::Refined {
            base_ty_expr,
            refinement: RefinementKind::Bounds { min, max },
        } => {
            let base_ty =
                resolve_type_expr_impl(def_table, module, base_ty_expr, in_progress, depth)?;
            match base_ty {
                Type::Int {
                    signed,
                    bits,
                    bounds,
                } => {
                    if signed {
                        return Err(TypeCheckErrorKind::BoundsBaseNotUnsignedInt(
                            Type::Int {
                                signed,
                                bits,
                                bounds,
                            },
                            type_expr.span,
                        )
                        .into());
                    }
                    let (min_bound, max_bound) = if let Some(bounds) = bounds {
                        (bounds.min, bounds.max_excl)
                    } else {
                        let min = 0;
                        let max = 1i128 << (bits as u32);
                        (min, max)
                    };
                    let min_val = *min as i128;
                    let max_val = *max as i128;
                    if min_val < min_bound || max_val > max_bound {
                        return Err(TypeCheckErrorKind::BoundsOutOfRange(
                            min_val,
                            max_val,
                            min_bound,
                            max_bound,
                            type_expr.span,
                        )
                        .into());
                    }
                    Ok(Type::Int {
                        signed,
                        bits,
                        bounds: Some(crate::types::IntBounds {
                            min: min_val,
                            max_excl: max_val,
                        }),
                    })
                }
                other => {
                    Err(TypeCheckErrorKind::BoundsBaseNotUnsignedInt(other, type_expr.span).into())
                }
            }
        }
        res::TypeExprKind::Slice { elem_ty_expr } => {
            let elem_ty =
                resolve_type_expr_impl(def_table, module, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Slice {
                elem_ty: Box::new(elem_ty),
            })
        }
        res::TypeExprKind::Heap { elem_ty_expr } => {
            let elem_ty =
                resolve_type_expr_impl(def_table, module, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Heap {
                elem_ty: Box::new(elem_ty),
            })
        }
        res::TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => {
            let elem_ty =
                resolve_type_expr_impl(def_table, module, elem_ty_expr, in_progress, depth)?;
            Ok(Type::Ref {
                mutable: *mutable,
                elem_ty: Box::new(elem_ty),
            })
        }
        res::TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            let param_tys = params
                .iter()
                .map(|param| {
                    let ty = resolve_type_expr_impl(
                        def_table,
                        module,
                        &param.ty_expr,
                        in_progress,
                        depth,
                    )?;
                    Ok(FnParam {
                        mode: fn_param_mode(param.mode.clone()),
                        ty,
                    })
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()?;
            let ret_ty =
                resolve_type_expr_impl(def_table, module, ret_ty_expr, in_progress, depth)?;
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
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
    def_id: &DefId,
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    let def = def_table
        .lookup_def(*def_id)
        .ok_or(TypeCheckErrorKind::UnknownType(type_expr.span))?;

    if let Some(ty) = builtin_type(&def.name) {
        return Ok(ty);
    }

    match &def.kind {
        DefKind::TypeDef { .. } => {
            let type_def = module
                .type_def_by_id(*def_id)
                .ok_or(TypeCheckErrorKind::UnknownType(type_expr.span))?;
            match &type_def.kind {
                res::TypeDefKind::Alias { aliased_ty } => {
                    resolve_type_alias(def_table, module, def, aliased_ty, in_progress)
                }
                res::TypeDefKind::Struct { fields } => {
                    resolve_struct_type(def_table, module, def, fields, in_progress, depth)
                }
                res::TypeDefKind::Enum { variants } => {
                    resolve_enum_type(def_table, module, def, variants, in_progress, depth)
                }
            }
        }
        _ => Err(TypeCheckErrorKind::UnknownType(type_expr.span).into()),
    }
}

fn resolve_type_alias(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def: &Def,
    ty_expr: &res::TypeExpr,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        return Ok(Type::Unknown);
    }
    in_progress.insert(def.id);
    let ty = resolve_type_expr_impl(def_table, module, ty_expr, in_progress, ResolveDepth::Full);
    in_progress.remove(&def.id);
    ty
}

fn resolve_struct_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def: &Def,
    fields: &[res::StructDefField],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        let struct_fields = match depth {
            ResolveDepth::Full => resolve_struct_fields(
                def_table,
                module,
                fields,
                in_progress,
                ResolveDepth::Shallow,
            )?,
            ResolveDepth::Shallow => Vec::new(),
        };
        return Ok(Type::Struct {
            name: def.name.clone(),
            fields: struct_fields,
        });
    }
    in_progress.insert(def.id);
    let struct_fields = resolve_struct_fields(def_table, module, fields, in_progress, depth)?;
    in_progress.remove(&def.id);
    Ok(Type::Struct {
        name: def.name.clone(),
        fields: struct_fields,
    })
}

fn resolve_struct_fields(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    fields: &[res::StructDefField],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Vec<StructField>, TypeCheckError> {
    fields
        .iter()
        .map(|field| {
            let field_ty =
                resolve_type_expr_impl(def_table, module, &field.ty, in_progress, depth)?;
            Ok(StructField {
                name: field.name.clone(),
                ty: field_ty,
            })
        })
        .collect()
}

fn resolve_enum_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def: &Def,
    variants: &[res::EnumDefVariant],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        let enum_variants = match depth {
            ResolveDepth::Full => resolve_enum_variants(
                def_table,
                module,
                variants,
                in_progress,
                ResolveDepth::Shallow,
            )?,
            ResolveDepth::Shallow => Vec::new(),
        };
        return Ok(Type::Enum {
            name: def.name.clone(),
            variants: enum_variants,
        });
    }
    in_progress.insert(def.id);
    let enum_variants = resolve_enum_variants(def_table, module, variants, in_progress, depth)?;
    in_progress.remove(&def.id);
    Ok(Type::Enum {
        name: def.name.clone(),
        variants: enum_variants,
    })
}

fn resolve_enum_variants(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    variants: &[res::EnumDefVariant],
    in_progress: &mut HashSet<DefId>,
    depth: ResolveDepth,
) -> Result<Vec<EnumVariant>, TypeCheckError> {
    let mut enum_variants = Vec::new();
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|payload_ty| {
                resolve_type_expr_impl(def_table, module, payload_ty, in_progress, depth)
            })
            .collect::<Result<Vec<Type>, _>>()?;
        enum_variants.push(EnumVariant {
            name: variant.name.clone(),
            payload,
        });
    }
    Ok(enum_variants)
}

pub struct TypeMapBuilder {
    type_table: TypeCache,
    node_type: HashMap<NodeId, TypeId>, // maps node to its type
    def_type: HashMap<Def, TypeId>,     // maps def to its type
    call_sigs: HashMap<NodeId, CallSig>,
}

impl Default for TypeMapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeMapBuilder {
    pub fn new() -> Self {
        Self {
            type_table: TypeCache::new(),
            node_type: HashMap::new(),
            def_type: HashMap::new(),
            call_sigs: HashMap::new(),
        }
    }

    pub fn record_def_type(&mut self, def: Def, typ: Type) {
        let id = self.type_table.intern(typ);
        self.def_type.insert(def, id);
    }

    pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
        let id = self.type_table.intern(typ);
        self.node_type.insert(node_id, id);
    }

    pub fn record_call_sig(&mut self, node_id: NodeId, sig: CallSig) {
        self.call_sigs.insert(node_id, sig);
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type
            .get(def)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_def_type_id(&self, def: &Def) -> Option<TypeId> {
        self.def_type.get(def).copied()
    }

    pub fn finish(self) -> (TypeMap, CallSigMap) {
        let call_sigs = self.call_sigs;
        (
            TypeMap {
                type_table: self.type_table,
                def_type: self.def_type,
                node_type: self.node_type,
                call_plan: HashMap::new(),
                index_plan: HashMap::new(),
                match_plan: HashMap::new(),
                slice_plan: HashMap::new(),
            },
            call_sigs,
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallSig {
    pub def_id: Option<DefId>,
    pub receiver: Option<CallParam>,
    pub params: Vec<CallParam>,
}

pub type CallSigMap = HashMap<NodeId, CallSig>;

#[derive(Debug, Clone)]
pub struct CallParam {
    pub mode: ParamMode,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TypeMap {
    type_table: TypeCache,
    def_type: HashMap<Def, TypeId>,
    node_type: HashMap<NodeId, TypeId>,
    call_plan: HashMap<NodeId, CallPlan>,
    index_plan: HashMap<NodeId, IndexPlan>,
    match_plan: HashMap<NodeId, MatchPlan>,
    slice_plan: HashMap<NodeId, SlicePlan>,
}

impl TypeMap {
    pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
        self.node_type
            .get(&node)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_node_type_id(&self, node: NodeId) -> Option<TypeId> {
        self.node_type.get(&node).copied()
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type
            .get(def)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_def_type_id(&self, def: &Def) -> Option<TypeId> {
        self.def_type.get(def).copied()
    }

    pub fn lookup_call_plan(&self, node: NodeId) -> Option<CallPlan> {
        self.call_plan.get(&node).cloned()
    }

    pub fn lookup_index_plan(&self, node: NodeId) -> Option<IndexPlan> {
        self.index_plan.get(&node).cloned()
    }

    pub fn lookup_match_plan(&self, node: NodeId) -> Option<MatchPlan> {
        self.match_plan.get(&node).cloned()
    }

    pub fn lookup_slice_plan(&self, node: NodeId) -> Option<SlicePlan> {
        self.slice_plan.get(&node).cloned()
    }

    pub fn insert_def_type(&mut self, def: Def, typ: Type) -> TypeId {
        let id = self.type_table.intern(typ);
        self.def_type.insert(def, id);
        id
    }

    pub fn insert_node_type(&mut self, node: NodeId, typ: Type) -> TypeId {
        let id = self.type_table.intern(typ);
        self.node_type.insert(node, id);
        id
    }

    pub fn insert_call_plan(&mut self, node: NodeId, plan: CallPlan) {
        self.call_plan.insert(node, plan);
    }

    pub fn insert_index_plan(&mut self, node: NodeId, plan: IndexPlan) {
        self.index_plan.insert(node, plan);
    }

    pub fn insert_match_plan(&mut self, node: NodeId, plan: MatchPlan) {
        self.match_plan.insert(node, plan);
    }

    pub fn insert_slice_plan(&mut self, node: NodeId, plan: SlicePlan) {
        self.slice_plan.insert(node, plan);
    }

    pub fn type_table(&self) -> &TypeCache {
        &self.type_table
    }
}

impl<'a> IntoIterator for &'a TypeMap {
    type Item = (&'a Def, &'a Type);
    type IntoIter = std::vec::IntoIter<(&'a Def, &'a Type)>;

    fn into_iter(self) -> Self::IntoIter {
        let mut items: Vec<_> = self
            .def_type
            .iter()
            .map(|(def, ty_id)| (def, self.type_table.get(*ty_id)))
            .collect();
        items.sort_by_key(|(def, _)| def.id);
        items.into_iter()
    }
}

impl fmt::Display for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // sort by node id
        let mut node_type = self.node_type.iter().collect::<Vec<(&NodeId, &TypeId)>>();
        node_type.sort_by_key(|(node, _)| node.0);
        for (node, ty_id) in node_type {
            let typ = self.type_table.get(*ty_id);
            writeln!(f, "Node [{}] -> Type [{}]", node, typ)?;
        }
        Ok(())
    }
}
