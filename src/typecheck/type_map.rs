//! Type-map construction and type-expression resolution helpers.
//!
//! This module is shared by type-check phases and downstream stages. It
//! resolves type expressions, records node/def/call typing side tables, and
//! materializes final `TypeMap` outputs.

use crate::diag::Span;
use crate::resolve::{Def, DefId, DefKind, DefTable};
use crate::tree::normalized as norm;
use crate::tree::resolved as res;
use crate::tree::semantic as sem;
use crate::tree::semantic::{CallPlan, IndexPlan, MatchPlan, SlicePlan};
use crate::tree::{NodeId, ParamMode, RefinementKind};
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::nominal::NominalKey;
use crate::types::{
    EnumVariant, FnParam, FnParamMode, StructField, TyVarId, Type, TypeCache, TypeId,
};
use std::collections::{HashMap, HashSet};
use std::fmt;

type TypeParamMap = HashMap<DefId, TyVarId>;
type TypeArgMap = HashMap<DefId, Type>;

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

pub(crate) fn resolve_type_expr(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
) -> Result<Type, TypeCheckError> {
    resolve_type_expr_with_params_and_args(def_table, module, type_expr, None, None)
}

pub(crate) fn resolve_type_expr_with_params(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
    type_params: Option<&TypeParamMap>,
) -> Result<Type, TypeCheckError> {
    resolve_type_expr_with_params_and_args(def_table, module, type_expr, type_params, None)
}

#[allow(dead_code)]
pub(crate) fn resolve_type_def_with_args(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def_id: DefId,
    type_args: &[Type],
) -> Result<Type, TypeCheckError> {
    let def = def_table
        .lookup_def(def_id)
        .ok_or(TypeCheckErrorKind::UnknownType(Span::default()))?;
    let type_def = module
        .type_def_by_id(def_id)
        .ok_or(TypeCheckErrorKind::UnknownType(Span::default()))?;

    if type_def.type_params.len() != type_args.len() {
        return Err(TypeCheckErrorKind::TypeArgCountMismatch(
            def.name.clone(),
            type_def.type_params.len(),
            type_args.len(),
            type_def.span,
        )
        .into());
    }

    let mut arg_map = TypeArgMap::new();
    for (param, arg_ty) in type_def.type_params.iter().zip(type_args.iter()) {
        arg_map.insert(param.def_id, arg_ty.clone());
    }

    let type_name = mangle_type_name(&def.name, type_args);
    let mut in_progress = HashSet::new();
    match &type_def.kind {
        res::TypeDefKind::Alias { aliased_ty } => resolve_type_alias(
            def_table,
            module,
            def,
            aliased_ty,
            None,
            Some(&arg_map),
            &mut in_progress,
        ),
        res::TypeDefKind::Struct { fields } => resolve_struct_type(
            def_table,
            module,
            def.id,
            &type_name,
            fields,
            None,
            Some(&arg_map),
            &mut in_progress,
        ),
        res::TypeDefKind::Enum { variants } => resolve_enum_type(
            def_table,
            module,
            def.id,
            &type_name,
            variants,
            None,
            Some(&arg_map),
            &mut in_progress,
        ),
    }
}

fn resolve_type_expr_with_params_and_args(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
) -> Result<Type, TypeCheckError> {
    let mut in_progress = HashSet::new();
    resolve_type_expr_impl(
        def_table,
        module,
        type_expr,
        type_params,
        type_args,
        &mut in_progress,
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
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    match &type_expr.kind {
        res::TypeExprKind::Named {
            def_id,
            type_args: type_arg_exprs,
            ..
        } => resolve_named_type(
            def_table,
            module,
            type_expr,
            def_id,
            type_arg_exprs,
            type_params,
            type_args,
            in_progress,
        ),
        res::TypeExprKind::Array { elem_ty_expr, dims } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
            )?;
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.clone(),
            })
        }
        res::TypeExprKind::Tuple { field_ty_exprs } => {
            let field_tys = field_ty_exprs
                .iter()
                .map(|f| {
                    resolve_type_expr_impl(
                        def_table,
                        module,
                        f,
                        type_params,
                        type_args,
                        in_progress,
                    )
                })
                .collect::<Result<Vec<Type>, _>>()?;
            Ok(Type::Tuple { field_tys })
        }
        res::TypeExprKind::Refined {
            base_ty_expr,
            refinements,
        } => {
            let base_ty = resolve_type_expr_impl(
                def_table,
                module,
                base_ty_expr,
                type_params,
                type_args,
                in_progress,
            )?;
            apply_refinements(base_ty, refinements, type_expr.span)
        }
        res::TypeExprKind::Slice { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
            )?;
            Ok(Type::Slice {
                elem_ty: Box::new(elem_ty),
            })
        }
        res::TypeExprKind::Heap { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
            )?;
            Ok(Type::Heap {
                elem_ty: Box::new(elem_ty),
            })
        }
        res::TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
            )?;
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
                        type_params,
                        type_args,
                        in_progress,
                    )?;
                    Ok(FnParam {
                        mode: fn_param_mode(param.mode.clone()),
                        ty,
                    })
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()?;
            let ret_ty = resolve_type_expr_impl(
                def_table,
                module,
                ret_ty_expr,
                type_params,
                type_args,
                in_progress,
            )?;
            Ok(Type::Fn {
                params: param_tys,
                ret_ty: Box::new(ret_ty),
            })
        }
    }
}

fn int_full_range(signed: bool, bits: u8) -> (i128, i128) {
    if signed {
        let max = 1i128 << (bits as u32 - 1);
        (-max, max)
    } else {
        (0, 1i128 << (bits as u32))
    }
}

fn apply_refinements(
    base_ty: Type,
    refinements: &[RefinementKind],
    span: Span,
) -> Result<Type, TypeCheckError> {
    let mut ty = base_ty;
    for refinement in refinements {
        match refinement {
            RefinementKind::Bounds { min, max } => {
                let Type::Int {
                    signed,
                    bits,
                    bounds,
                    nonzero,
                } = ty
                else {
                    return Err(TypeCheckErrorKind::RefinementBaseNotInt(ty, span).into());
                };
                let (min_bound, max_bound) = if let Some(bounds) = bounds {
                    (bounds.min, bounds.max_excl)
                } else {
                    int_full_range(signed, bits)
                };
                if *min < min_bound || *max > max_bound {
                    return Err(TypeCheckErrorKind::BoundsOutOfRange(
                        *min, *max, min_bound, max_bound, span,
                    )
                    .into());
                }
                ty = Type::Int {
                    signed,
                    bits,
                    bounds: Some(crate::types::IntBounds {
                        min: *min,
                        max_excl: *max,
                    }),
                    nonzero,
                };
            }
            RefinementKind::NonZero => {
                let Type::Int {
                    signed,
                    bits,
                    bounds,
                    ..
                } = ty
                else {
                    return Err(TypeCheckErrorKind::RefinementBaseNotInt(ty, span).into());
                };
                ty = Type::Int {
                    signed,
                    bits,
                    bounds,
                    nonzero: true,
                };
            }
        }
    }
    if let Type::Int {
        bounds: Some(bounds),
        nonzero: true,
        ..
    } = ty
        && (bounds.min > 0 || bounds.max_excl <= 0)
    {
        return Err(TypeCheckErrorKind::RedundantNonZero(bounds.min, bounds.max_excl, span).into());
    }

    Ok(ty)
}

fn fn_param_mode(mode: ParamMode) -> FnParamMode {
    match mode {
        ParamMode::In => FnParamMode::In,
        ParamMode::InOut => FnParamMode::InOut,
        ParamMode::Out => FnParamMode::Out,
        ParamMode::Sink => FnParamMode::Sink,
    }
}

fn type_arg_name(ty: &Type) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::Var(var) => format!("T{}", var.index()),
        _ => ty.to_string(),
    }
}

fn mangle_type_name(base: &str, type_args: &[Type]) -> String {
    if type_args.is_empty() {
        base.to_string()
    } else {
        let args = type_args
            .iter()
            .map(type_arg_name)
            .collect::<Vec<_>>()
            .join(", ");
        format!("{base}<{args}>")
    }
}

fn resolve_named_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &res::TypeExpr,
    def_id: &DefId,
    type_arg_exprs: &[res::TypeExpr],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    let def = def_table
        .lookup_def(*def_id)
        .ok_or(TypeCheckErrorKind::UnknownType(type_expr.span))?;

    if let Some(ty) = builtin_type(&def.name) {
        if !type_arg_exprs.is_empty() {
            return Err(TypeCheckErrorKind::TypeArgCountMismatch(
                def.name.clone(),
                0,
                type_arg_exprs.len(),
                type_expr.span,
            )
            .into());
        }
        return Ok(ty);
    }

    match &def.kind {
        DefKind::TypeParam => {
            if let Some(map) = type_args
                && let Some(ty) = map.get(def_id)
            {
                return Ok(ty.clone());
            }
            if let Some(map) = type_params
                && let Some(var) = map.get(def_id)
            {
                return Ok(Type::Var(*var));
            }
            Err(TypeCheckErrorKind::UnknownType(type_expr.span).into())
        }
        DefKind::TypeDef { .. } => {
            let type_def = module
                .type_def_by_id(*def_id)
                .ok_or(TypeCheckErrorKind::UnknownType(type_expr.span))?;
            if type_def.type_params.is_empty() {
                if !type_arg_exprs.is_empty() {
                    return Err(TypeCheckErrorKind::TypeArgCountMismatch(
                        def.name.clone(),
                        0,
                        type_arg_exprs.len(),
                        type_expr.span,
                    )
                    .into());
                }
                let type_name = def.name.as_str();
                return match &type_def.kind {
                    res::TypeDefKind::Alias { aliased_ty } => resolve_type_alias(
                        def_table,
                        module,
                        def,
                        aliased_ty,
                        type_params,
                        type_args,
                        in_progress,
                    ),
                    res::TypeDefKind::Struct { fields } => resolve_struct_type(
                        def_table,
                        module,
                        def.id,
                        type_name,
                        fields,
                        type_params,
                        type_args,
                        in_progress,
                    ),
                    res::TypeDefKind::Enum { variants } => resolve_enum_type(
                        def_table,
                        module,
                        def.id,
                        type_name,
                        variants,
                        type_params,
                        type_args,
                        in_progress,
                    ),
                };
            }

            if type_def.type_params.len() != type_arg_exprs.len() {
                return Err(TypeCheckErrorKind::TypeArgCountMismatch(
                    def.name.clone(),
                    type_def.type_params.len(),
                    type_arg_exprs.len(),
                    type_expr.span,
                )
                .into());
            }

            let resolved_args = type_arg_exprs
                .iter()
                .map(|arg| {
                    resolve_type_expr_impl(
                        def_table,
                        module,
                        arg,
                        type_params,
                        type_args,
                        in_progress,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            let mut arg_map = TypeArgMap::new();
            for (param, arg_ty) in type_def.type_params.iter().zip(resolved_args.iter()) {
                arg_map.insert(param.def_id, arg_ty.clone());
            }

            let type_name = mangle_type_name(&def.name, &resolved_args);
            match &type_def.kind {
                res::TypeDefKind::Alias { aliased_ty } => resolve_type_alias(
                    def_table,
                    module,
                    def,
                    aliased_ty,
                    type_params,
                    Some(&arg_map),
                    in_progress,
                ),
                res::TypeDefKind::Struct { fields } => resolve_struct_type(
                    def_table,
                    module,
                    def.id,
                    &type_name,
                    fields,
                    type_params,
                    Some(&arg_map),
                    in_progress,
                ),
                res::TypeDefKind::Enum { variants } => resolve_enum_type(
                    def_table,
                    module,
                    def.id,
                    &type_name,
                    variants,
                    type_params,
                    Some(&arg_map),
                    in_progress,
                ),
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
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        return Ok(Type::Unknown);
    }
    in_progress.insert(def.id);
    let ty = resolve_type_expr_impl(
        def_table,
        module,
        ty_expr,
        type_params,
        type_args,
        in_progress,
    );
    in_progress.remove(&def.id);
    ty
}

fn resolve_struct_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def_id: DefId,
    type_name: &str,
    fields: &[res::StructDefField],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def_id) {
        return Ok(Type::Struct {
            name: type_name.to_string(),
            // Recursive nominal references must stay shallow so later passes
            // can route recursion through nominal glue instead of truncating
            // structural drop/visit depth.
            fields: Vec::new(),
        });
    }
    in_progress.insert(def_id);
    let struct_fields = resolve_struct_fields(
        def_table,
        module,
        fields,
        type_params,
        type_args,
        in_progress,
    )?;
    in_progress.remove(&def_id);
    Ok(Type::Struct {
        name: type_name.to_string(),
        fields: struct_fields,
    })
}

fn resolve_struct_fields(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    fields: &[res::StructDefField],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Vec<StructField>, TypeCheckError> {
    fields
        .iter()
        .map(|field| {
            let field_ty = resolve_type_expr_impl(
                def_table,
                module,
                &field.ty,
                type_params,
                type_args,
                in_progress,
            )?;
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
    def_id: DefId,
    type_name: &str,
    variants: &[res::EnumDefVariant],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def_id) {
        return Ok(Type::Enum {
            name: type_name.to_string(),
            // Recursive nominal references must stay shallow so later passes
            // can route recursion through nominal glue instead of truncating
            // structural drop/visit depth.
            variants: Vec::new(),
        });
    }
    in_progress.insert(def_id);
    let enum_variants = resolve_enum_variants(
        def_table,
        module,
        variants,
        type_params,
        type_args,
        in_progress,
    )?;
    in_progress.remove(&def_id);
    Ok(Type::Enum {
        name: type_name.to_string(),
        variants: enum_variants,
    })
}

fn resolve_enum_variants(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    variants: &[res::EnumDefVariant],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
) -> Result<Vec<EnumVariant>, TypeCheckError> {
    let mut enum_variants = Vec::new();
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|payload_ty| {
                resolve_type_expr_impl(
                    def_table,
                    module,
                    payload_ty,
                    type_params,
                    type_args,
                    in_progress,
                )
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
    nominal_keys: HashMap<TypeId, NominalKey>,
    call_sigs: HashMap<NodeId, CallSig>,
    generic_insts: HashMap<NodeId, GenericInst>,
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
            nominal_keys: HashMap::new(),
            call_sigs: HashMap::new(),
            generic_insts: HashMap::new(),
        }
    }

    pub fn record_def_type(&mut self, def: Def, typ: Type) {
        self.record_def_type_with_nominal(def, typ, None);
    }

    pub(crate) fn record_def_type_with_nominal(
        &mut self,
        def: Def,
        typ: Type,
        nominal_key: Option<NominalKey>,
    ) {
        let id = self.type_table.intern(typ);
        self.def_type.insert(def, id);
        self.record_nominal_key(id, nominal_key);
    }

    pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
        self.record_node_type_with_nominal(node_id, typ, None);
    }

    pub(crate) fn record_node_type_with_nominal(
        &mut self,
        node_id: NodeId,
        typ: Type,
        nominal_key: Option<NominalKey>,
    ) {
        let id = self.type_table.intern(typ);
        self.node_type.insert(node_id, id);
        self.record_nominal_key(id, nominal_key);
    }

    pub fn record_call_sig(&mut self, node_id: NodeId, sig: CallSig) {
        self.call_sigs.insert(node_id, sig);
    }

    pub fn record_generic_inst(&mut self, node_id: NodeId, inst: GenericInst) {
        self.generic_insts.insert(node_id, inst);
    }

    pub fn apply_inference<F>(&mut self, mut apply: F)
    where
        F: FnMut(&Type) -> Type,
    {
        for type_id in self.node_type.values_mut() {
            let ty = self.type_table.get(*type_id).clone();
            let new_ty = apply(&ty);
            *type_id = self.type_table.intern(new_ty);
        }

        for type_id in self.def_type.values_mut() {
            let ty = self.type_table.get(*type_id).clone();
            let new_ty = apply(&ty);
            *type_id = self.type_table.intern(new_ty);
        }

        for sig in self.call_sigs.values_mut() {
            if let Some(receiver) = &mut sig.receiver {
                receiver.ty = apply(&receiver.ty);
            }
            for param in sig.params.iter_mut() {
                param.ty = apply(&param.ty);
            }
        }

        for inst in self.generic_insts.values_mut() {
            inst.type_args = inst.type_args.iter().map(|ty| apply(ty)).collect();
        }
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type
            .get(def)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_def_type_id(&self, def: &Def) -> Option<TypeId> {
        self.def_type.get(def).copied()
    }

    pub fn finish(self) -> (TypeMap, CallSigMap, GenericInstMap) {
        let call_sigs = self.call_sigs;
        let generic_insts = self.generic_insts;
        (
            TypeMap {
                type_table: self.type_table,
                def_type: self.def_type,
                node_type: self.node_type,
                nominal_keys: self.nominal_keys,
                call_plan: HashMap::new(),
                index_plan: HashMap::new(),
                match_plan: HashMap::new(),
                slice_plan: HashMap::new(),
            },
            call_sigs,
            generic_insts,
        )
    }

    fn record_nominal_key(&mut self, type_id: TypeId, nominal_key: Option<NominalKey>) {
        let Some(key) = nominal_key else {
            return;
        };
        if let Some(existing) = self.nominal_keys.get_mut(&type_id) {
            if let Some(selected) = select_more_concrete_nominal_key(existing, &key) {
                *existing = selected;
            }
            return;
        }
        self.nominal_keys.insert(type_id, key);
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
pub struct GenericInst {
    pub def_id: DefId,
    pub type_args: Vec<Type>,
    pub call_span: Span,
}

pub type GenericInstMap = HashMap<NodeId, GenericInst>;

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
    nominal_keys: HashMap<TypeId, NominalKey>,
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

    pub(crate) fn lookup_nominal_key_for_type_id(&self, type_id: TypeId) -> Option<&NominalKey> {
        self.nominal_keys.get(&type_id)
    }

    pub(crate) fn record_nominal_key_for_type_id(&mut self, type_id: TypeId, key: NominalKey) {
        if let Some(existing) = self.nominal_keys.get_mut(&type_id) {
            if let Some(selected) = select_more_concrete_nominal_key(existing, &key) {
                *existing = selected;
            }
            return;
        }
        self.nominal_keys.insert(type_id, key);
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

fn select_more_concrete_nominal_key(
    existing: &NominalKey,
    candidate: &NominalKey,
) -> Option<NominalKey> {
    if existing == candidate {
        return None;
    }
    if existing.def_id != candidate.def_id || existing.type_args.len() != candidate.type_args.len()
    {
        // Conflicting identities should be rare; keep first-wins to preserve
        // deterministic behavior.
        return None;
    }
    let existing_score = nominal_key_concreteness(existing);
    let candidate_score = nominal_key_concreteness(candidate);
    (candidate_score > existing_score).then(|| candidate.clone())
}

fn nominal_key_concreteness(key: &NominalKey) -> usize {
    key.type_args
        .iter()
        .filter(|arg| !matches!(arg, Type::Var(_)))
        .count()
}
