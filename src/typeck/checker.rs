use std::collections::{HashMap, HashSet};

use crate::context::ResolvedContext;
use crate::diag::Span;
use crate::resolve::{DefId, DefKind};
use crate::tree::fold::{TreeFolder, walk_expr, walk_if};
use crate::tree::resolved::TypeParam;
use crate::tree::resolved::*;
use crate::tree::visit::{
    Visitor, walk_expr as walk_visit_expr, walk_stmt_expr as walk_visit_stmt_expr,
};
use crate::tree::{BinaryOp, CallArgMode, ParamMode, UnaryOp};
use crate::types::{
    EnumVariant, FnParam, FnParamMode, StructField, TyVarId, Type, array_to_slice_assignable,
};
use crate::types::{TypeAssignability, type_assignable};

use super::errors::{TypeCheckError, TypeCheckErrorKind};
use super::overloads::{OverloadSig, ParamSig};
use super::type_map::{
    CallParam, CallSig, CallSigMap, GenericInstMap, TypeMap, TypeMapBuilder,
    resolve_type_def_with_args, resolve_type_expr, resolve_type_expr_with_params,
};
use super::unify::Unifier;

const INFER_VAR_BASE: u32 = 1_000_000;

mod calls;
mod diagnostics;
mod expr;
mod literals;
mod patterns;
mod resolve;
mod resolve_expr;
mod stmt;

#[derive(Debug, Clone)]
struct PropertySig {
    ty: Type,
    getter: Option<DefId>,
    setter: Option<DefId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PropertyAccessorKind {
    Get,
    Set,
}

#[derive(Debug, Default)]
struct ConstraintSet {
    constraints: Vec<(Type, Type)>,
    applied: usize,
}

impl ConstraintSet {
    fn add(
        &mut self,
        left: &Type,
        right: &Type,
        unifier: &mut Unifier,
        is_infer: fn(TyVarId) -> bool,
    ) -> bool {
        self.constraints.push((left.clone(), right.clone()));
        self.apply_pending(unifier, is_infer)
    }

    fn apply_pending(&mut self, unifier: &mut Unifier, is_infer: fn(TyVarId) -> bool) -> bool {
        let mut ok = true;
        while self.applied < self.constraints.len() {
            let (left, right) = &self.constraints[self.applied];
            if unifier.unify_infer(left, right, is_infer).is_err() {
                ok = false;
            }
            self.applied += 1;
        }
        ok
    }
}

#[derive(Debug, Default)]
struct InferCtx {
    unifier: Unifier,
    constraints: ConstraintSet,
    bindings: HashMap<DefId, Span>,
}

pub struct TypeChecker {
    ctx: ResolvedContext,
    type_map_builder: TypeMapBuilder,
    func_sigs: HashMap<String, Vec<OverloadSig>>,
    method_sigs: HashMap<String, HashMap<String, Vec<OverloadSig>>>,
    // Property accessors are tracked separately so `obj.x` only resolves when a
    // `prop x { ... }` exists (not for ordinary methods).
    property_sigs: HashMap<String, HashMap<String, PropertySig>>,
    // Used to avoid duplicate property/field conflict diagnostics.
    property_conflicts: HashSet<(String, String)>,
    type_defs: HashMap<String, Type>,
    errors: Vec<TypeCheckError>,
    halted: bool,
    return_stack: Vec<Type>,
    loop_depth_stack: Vec<usize>,
    type_param_stack: Vec<HashMap<DefId, TyVarId>>,
    infer_ctx: Option<InferCtx>,
    next_infer_var: u32,
}

struct InferScopeGuard {
    checker: *mut TypeChecker,
    finished: bool,
}

impl InferScopeGuard {
    fn new(checker: &mut TypeChecker) -> Self {
        checker.begin_inference();
        Self {
            checker,
            finished: false,
        }
    }

    fn finish(mut self) -> Vec<TypeCheckError> {
        self.finished = true;
        // Safety: the guard is scoped to the borrow used to create it.
        unsafe { (*self.checker).finish_inference() }
    }
}

impl Drop for InferScopeGuard {
    fn drop(&mut self) {
        if self.finished {
            return;
        }
        // Safety: the guard is scoped to the borrow used to create it.
        unsafe {
            let _ = (*self.checker).finish_inference();
        }
    }
}

impl TypeChecker {
    pub fn new(context: ResolvedContext) -> Self {
        Self {
            ctx: context,
            type_map_builder: TypeMapBuilder::new(),
            func_sigs: HashMap::new(),
            method_sigs: HashMap::new(),
            property_sigs: HashMap::new(),
            property_conflicts: HashSet::new(),
            type_defs: HashMap::new(),
            errors: Vec::new(),
            halted: false,
            return_stack: Vec::new(),
            loop_depth_stack: Vec::new(),
            type_param_stack: Vec::new(),
            infer_ctx: None,
            next_infer_var: INFER_VAR_BASE,
        }
    }

    pub fn check(&mut self) -> Result<(TypeMap, CallSigMap, GenericInstMap), Vec<TypeCheckError>> {
        self.populate_type_symbols()?;
        self.populate_function_symbols()?;
        self.populate_method_symbols()?;

        self.halted = false;
        let module = self.ctx.module.clone();
        let _ = self.visit_module(&module);

        if self.errors.is_empty() {
            let builder = std::mem::take(&mut self.type_map_builder);
            Ok(builder.finish())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn push_control_context(&mut self, return_ty: Type) {
        self.return_stack.push(return_ty);
        self.loop_depth_stack.push(0);
    }

    fn pop_control_context(&mut self) {
        self.return_stack.pop();
        self.loop_depth_stack.pop();
    }

    fn current_return_ty(&self) -> Option<&Type> {
        self.return_stack.last()
    }

    fn loop_depth(&self) -> usize {
        self.loop_depth_stack.last().copied().unwrap_or(0)
    }

    fn type_param_map(type_params: &[TypeParam]) -> HashMap<DefId, TyVarId> {
        type_params
            .iter()
            .enumerate()
            .map(|(index, param)| (param.def_id, TyVarId::new(index as u32)))
            .collect()
    }

    fn infer_type_args(&mut self, type_params: &[TypeParam]) -> Vec<Type> {
        type_params
            .iter()
            .map(|_| Type::Var(self.new_infer_var()))
            .collect()
    }

    fn current_type_params(&self) -> Option<&HashMap<DefId, TyVarId>> {
        self.type_param_stack.last()
    }

    fn with_type_params<F, R>(&mut self, type_params: &[TypeParam], f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        if type_params.is_empty() {
            return f(self);
        }

        self.type_param_stack
            .push(Self::type_param_map(type_params));
        let result = f(self);
        self.type_param_stack.pop();
        result
    }

    fn infer_scope(&mut self) -> InferScopeGuard {
        InferScopeGuard::new(self)
    }

    fn begin_inference(&mut self) {
        self.infer_ctx = Some(InferCtx::default());
    }

    fn finish_inference(&mut self) -> Vec<TypeCheckError> {
        let Some(infer) = self.infer_ctx.take() else {
            return Vec::new();
        };

        let mut infer = infer;
        infer
            .constraints
            .apply_pending(&mut infer.unifier, Self::is_infer_var);

        self.type_map_builder
            .apply_inference(|ty| infer.unifier.apply(ty));

        let mut errors = Vec::new();
        for (def_id, span) in infer.bindings {
            let Some(def) = self.ctx.def_table.lookup_def(def_id) else {
                continue;
            };
            let Some(ty) = self.type_map_builder.lookup_def_type(def) else {
                continue;
            };
            if Self::type_has_infer_vars(&ty) {
                errors.push(TypeCheckErrorKind::UnknownType(span).into());
            }
        }

        errors
    }

    fn new_infer_var(&mut self) -> TyVarId {
        let id = TyVarId::new(self.next_infer_var);
        self.next_infer_var = self.next_infer_var.saturating_add(1);
        id
    }

    fn is_infer_var(var: TyVarId) -> bool {
        var.index() >= INFER_VAR_BASE
    }

    fn type_has_infer_vars(ty: &Type) -> bool {
        match ty {
            Type::Var(var) => Self::is_infer_var(*var),
            Type::Array { elem_ty, .. }
            | Type::Slice { elem_ty }
            | Type::Heap { elem_ty }
            | Type::Ref { elem_ty, .. }
            | Type::Range { elem_ty } => Self::type_has_infer_vars(elem_ty),
            Type::Tuple { field_tys } => field_tys.iter().any(Self::type_has_infer_vars),
            Type::Struct { fields, .. } => fields.iter().any(|f| Self::type_has_infer_vars(&f.ty)),
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|v| v.payload.iter().any(Self::type_has_infer_vars)),
            Type::Fn { params, ret_ty } => {
                params.iter().any(|p| Self::type_has_infer_vars(&p.ty))
                    || Self::type_has_infer_vars(ret_ty)
            }
            _ => false,
        }
    }

    fn apply_infer(&self, ty: &Type) -> Type {
        match &self.infer_ctx {
            Some(infer) => infer.unifier.apply(ty),
            None => ty.clone(),
        }
    }

    fn try_unify_infer(&mut self, left: &Type, right: &Type) -> bool {
        let Some(infer) = &mut self.infer_ctx else {
            return false;
        };
        let left_has = Self::type_has_infer_vars(left);
        let right_has = Self::type_has_infer_vars(right);
        if !left_has && !right_has {
            return false;
        }
        let (left, right) = if left_has && !right_has {
            (left, right)
        } else if right_has && !left_has {
            (right, left)
        } else {
            (left, right)
        };
        infer
            .constraints
            .add(left, right, &mut infer.unifier, Self::is_infer_var)
    }

    fn unify_expected_actual(&mut self, expected: &Type, actual: &Type) -> (Type, Type, bool) {
        let mut expected_ty = self.apply_infer(expected);
        let mut actual_ty = self.apply_infer(actual);
        let had_infer =
            Self::type_has_infer_vars(&expected_ty) || Self::type_has_infer_vars(&actual_ty);
        let unified = if had_infer {
            self.try_unify_infer(&expected_ty, &actual_ty)
        } else {
            expected_ty == actual_ty
        };
        expected_ty = self.apply_infer(&expected_ty);
        actual_ty = self.apply_infer(&actual_ty);
        let ok = if had_infer {
            unified
        } else {
            expected_ty == actual_ty
        };
        (expected_ty, actual_ty, ok)
    }

    fn unify_infer_types(&mut self, left: &Type, right: &Type) -> (Type, Type, bool) {
        let mut left_ty = self.apply_infer(left);
        let mut right_ty = self.apply_infer(right);
        let unified = self.try_unify_infer(&left_ty, &right_ty);
        left_ty = self.apply_infer(&left_ty);
        right_ty = self.apply_infer(&right_ty);
        (left_ty, right_ty, unified)
    }

    fn enter_loop(&mut self) {
        if let Some(depth) = self.loop_depth_stack.last_mut() {
            *depth += 1;
        }
    }

    fn exit_loop(&mut self) {
        if let Some(depth) = self.loop_depth_stack.last_mut() {
            *depth = depth.saturating_sub(1);
        }
    }

    fn populate_type_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        for type_def in self.ctx.module.type_defs() {
            if !type_def.type_params.is_empty() {
                // Generic type defs are instantiated on demand.
                continue;
            }
            match &type_def.kind {
                TypeDefKind::Alias { aliased_ty } => {
                    // Resolve the aliased type
                    let ty = resolve_type_expr(&self.ctx.def_table, &self.ctx.module, aliased_ty)
                        .map_err(|e| vec![e])?;

                    self.type_defs.insert(type_def.name.clone(), ty);
                }

                TypeDefKind::Struct { fields } => {
                    // Resolve each struct field type
                    let struct_fields = fields
                        .iter()
                        .map(|f| {
                            let field_ty =
                                resolve_type_expr(&self.ctx.def_table, &self.ctx.module, &f.ty)
                                    .map_err(|e| vec![e])?;
                            Ok(StructField {
                                name: f.name.clone(),
                                ty: field_ty,
                            })
                        })
                        .collect::<Result<_, Vec<TypeCheckError>>>()?;

                    // Create the struct type
                    let ty = Type::Struct {
                        name: type_def.name.clone(),
                        fields: struct_fields,
                    };

                    self.type_defs.insert(type_def.name.clone(), ty);
                }

                TypeDefKind::Enum { variants } => {
                    // Collect the enum variant names + payload types
                    let mut enum_variants = Vec::new();
                    for variant in variants {
                        let payload = variant
                            .payload
                            .iter()
                            .map(|ty_expr| {
                                resolve_type_expr(&self.ctx.def_table, &self.ctx.module, ty_expr)
                            })
                            .collect::<Result<Vec<Type>, _>>()
                            .map_err(|e| vec![e])?;

                        enum_variants.push(EnumVariant {
                            name: variant.name.clone(),
                            payload,
                        });
                    }

                    // Create the enum type
                    let ty = Type::Enum {
                        name: type_def.name.clone(),
                        variants: enum_variants,
                    };

                    self.type_defs.insert(type_def.name.clone(), ty);
                }
            }
        }
        Ok(())
    }

    fn populate_function_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        let mut overloads = Vec::new();

        // Func decls
        for func_decl in self.ctx.module.func_decls() {
            let def_id = func_decl.def_id;
            overloads.push((def_id, func_decl.sig.clone()));
        }

        // Func defs
        for func_def in self.ctx.module.func_defs() {
            let def_id = func_def.def_id;
            overloads.push((def_id, func_def.sig.clone()));
        }

        for (def_id, sig) in overloads {
            self.insert_func_overload(def_id, sig)?;
        }

        Ok(())
    }

    fn populate_method_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        let mut items = Vec::new();

        for method_block in self.ctx.module.method_blocks() {
            let type_name = method_block.type_name.clone();
            for method_item in &method_block.method_items {
                let (def_id, sig, attrs, span) = match method_item {
                    MethodItem::Decl(method_decl) => (
                        method_decl.def_id,
                        &method_decl.sig,
                        &method_decl.attrs,
                        method_decl.span,
                    ),
                    MethodItem::Def(method_def) => (
                        method_def.def_id,
                        &method_def.sig,
                        &method_def.attrs,
                        method_def.span,
                    ),
                };
                items.push((type_name.clone(), def_id, sig.clone(), attrs.clone(), span));
            }
        }

        for (type_name, def_id, sig, attrs, span) in items {
            let type_param_map = if sig.type_params.is_empty() {
                None
            } else {
                Some(Self::type_param_map(&sig.type_params))
            };
            let params = self.build_param_sigs(&sig.params, type_param_map.as_ref())?;
            let ret_type = self.resolve_ret_type(&sig.ret_ty_expr, type_param_map.as_ref())?;

            if let Some(kind) = Self::property_accessor_kind(&attrs) {
                // Record property accessors so field access/assignment can be
                // rewritten into calls later in normalization.
                let prop_span = attrs
                    .iter()
                    .find(|attr| attr.name == "__property_get" || attr.name == "__property_set")
                    .map(|attr| attr.span)
                    .unwrap_or(span);
                self.record_property_sig(
                    &type_name, &sig.name, kind, def_id, &params, &ret_type, prop_span,
                );
            }

            self.method_sigs
                .entry(type_name)
                .or_default()
                .entry(sig.name)
                .or_default()
                .push(OverloadSig {
                    def_id,
                    params,
                    ret_ty: ret_type,
                    type_param_count: sig.type_params.len(),
                });
        }

        Ok(())
    }

    fn property_accessor_kind(attrs: &[Attribute]) -> Option<PropertyAccessorKind> {
        attrs.iter().find_map(|attr| match attr.name.as_str() {
            "__property_get" => Some(PropertyAccessorKind::Get),
            "__property_set" => Some(PropertyAccessorKind::Set),
            _ => None,
        })
    }

    /// Register a property accessor signature and validate its shape.
    ///
    /// Properties are stored separately from normal methods so that `obj.x`
    /// only resolves to explicit `prop x { ... }` definitions.
    fn record_property_sig(
        &mut self,
        type_name: &str,
        prop_name: &str,
        kind: PropertyAccessorKind,
        def_id: DefId,
        params: &[ParamSig],
        ret_type: &Type,
        span: Span,
    ) {
        // Validate accessor shape and determine the property type.
        let prop_ty = match kind {
            PropertyAccessorKind::Get => {
                if !params.is_empty() {
                    self.errors.push(
                        TypeCheckErrorKind::PropertyGetterHasParams(prop_name.to_string(), span)
                            .into(),
                    );
                    return;
                }
                ret_type.clone()
            }
            PropertyAccessorKind::Set => {
                if params.len() != 1 {
                    self.errors.push(
                        TypeCheckErrorKind::PropertySetterParamCount(
                            prop_name.to_string(),
                            params.len(),
                            span,
                        )
                        .into(),
                    );
                    return;
                }
                if *ret_type != Type::Unit {
                    self.errors.push(
                        TypeCheckErrorKind::PropertySetterReturnType(
                            prop_name.to_string(),
                            ret_type.clone(),
                            span,
                        )
                        .into(),
                    );
                    return;
                }
                params[0].ty.clone()
            }
        };

        // We only want to report field conflicts once per property name.
        let props = self.property_sigs.entry(type_name.to_string()).or_default();

        if !props.contains_key(prop_name) {
            if let Some(Type::Struct { fields, .. }) = self.type_defs.get(type_name) {
                if let Some(field) = fields.iter().find(|field| field.name == prop_name) {
                    if self
                        .property_conflicts
                        .insert((type_name.to_string(), prop_name.to_string()))
                    {
                        self.errors.push(
                            TypeCheckErrorKind::PropertyConflictsWithField(
                                prop_name.to_string(),
                                field.name.clone(),
                                span,
                            )
                            .into(),
                        );
                    }
                    return;
                }
            }
        }

        // Merge accessors into a single property signature.
        let entry = props
            .entry(prop_name.to_string())
            .or_insert_with(|| PropertySig {
                ty: prop_ty.clone(),
                getter: None,
                setter: None,
            });

        if entry.ty != prop_ty {
            self.errors.push(
                TypeCheckErrorKind::PropertyAccessorTypeMismatch(
                    prop_name.to_string(),
                    entry.ty.clone(),
                    prop_ty,
                    span,
                )
                .into(),
            );
            return;
        }

        match kind {
            PropertyAccessorKind::Get => {
                if entry.getter.is_some() {
                    self.errors.push(
                        TypeCheckErrorKind::PropertyAccessorDuplicate(prop_name.to_string(), span)
                            .into(),
                    );
                } else {
                    entry.getter = Some(def_id);
                }
            }
            PropertyAccessorKind::Set => {
                if entry.setter.is_some() {
                    self.errors.push(
                        TypeCheckErrorKind::PropertyAccessorDuplicate(prop_name.to_string(), span)
                            .into(),
                    );
                } else {
                    entry.setter = Some(def_id);
                }
            }
        }
    }

    fn insert_func_overload(
        &mut self,
        def_id: DefId,
        sig: FunctionSig,
    ) -> Result<(), Vec<TypeCheckError>> {
        let type_param_map = if sig.type_params.is_empty() {
            None
        } else {
            Some(Self::type_param_map(&sig.type_params))
        };
        let params = self.build_param_sigs(&sig.params, type_param_map.as_ref())?;
        let ret_ty = self.resolve_ret_type(&sig.ret_ty_expr, type_param_map.as_ref())?;

        // Record the function type.
        if let Some(def) = self.ctx.def_table.lookup_def(def_id) {
            let fn_params = params
                .iter()
                .map(|param| FnParam {
                    mode: fn_param_mode(param.mode.clone()),
                    ty: param.ty.clone(),
                })
                .collect::<Vec<_>>();
            self.type_map_builder.record_def_type(
                def.clone(),
                Type::Fn {
                    params: fn_params,
                    ret_ty: Box::new(ret_ty.clone()),
                },
            );
        }

        // Insert an overload entry.
        let func_name = self.def_name(def_id).to_string();
        self.func_sigs
            .entry(func_name)
            .or_default()
            .push(OverloadSig {
                def_id,
                params,
                ret_ty,
                type_param_count: sig.type_params.len(),
            });

        Ok(())
    }

    fn def_name(&self, def_id: DefId) -> &str {
        self.ctx
            .def_table
            .lookup_def(def_id)
            .map(|def| def.name.as_str())
            .unwrap_or_else(|| panic!("def {def_id} not found in def_table"))
    }

    fn expand_shallow_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Struct { name, fields } if fields.is_empty() => self
                .type_defs
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Type::Enum { name, variants } if variants.is_empty() => self
                .type_defs
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            _ => ty.clone(),
        }
    }

    fn view_type(&self, ty: &Type) -> TypeView {
        TypeView {
            ty: self.expand_shallow_type(&ty.peel_heap()),
        }
    }

    fn property_owner_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.clone()),
            Type::String => Some("string".to_string()),
            _ => None,
        }
    }

    fn lookup_property_sig<'a>(&'a self, owner_ty: &Type, field: &str) -> Option<&'a PropertySig> {
        let type_name = self.property_owner_name(owner_ty)?;
        self.property_sigs
            .get(&type_name)
            .and_then(|props| props.get(field))
    }

    fn enum_name_matches(expected: &str, name: &str) -> bool {
        expected == name
            || expected
                .split_once('<')
                .map_or(false, |(base, _)| base == name)
    }

    fn check_unqualified_enum_variant(
        &mut self,
        variant_name: &str,
        payload: &[&Expr],
        expected: Expected<'_>,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        let Some(expected_enum) = expected.as_option() else {
            for expr in payload {
                let _ = self.check_expr(expr, Expected::Unknown)?;
            }
            return Err(TypeCheckErrorKind::UnknownType(span).into());
        };

        let Type::Enum { name, variants } = expected_enum else {
            for expr in payload {
                let _ = self.check_expr(expr, Expected::Unknown)?;
            }
            return Err(TypeCheckErrorKind::UnknownType(span).into());
        };

        let Some(variant) = self.resolve_enum_variant_in(name.as_str(), variants, variant_name)
        else {
            for expr in payload {
                let _ = self.check_expr(expr, Expected::Unknown)?;
            }
            return Err(TypeCheckErrorKind::UnknownEnumVariant(
                name.clone(),
                variant_name.to_string(),
                span,
            )
            .into());
        };

        if payload.len() != variant.payload().len() {
            for expr in payload {
                let _ = self.check_expr(expr, Expected::Unknown)?;
            }
            return Err(TypeCheckErrorKind::EnumVariantPayloadArityMismatch(
                variant_name.to_string(),
                variant.payload().len(),
                payload.len(),
                span,
            )
            .into());
        }

        for (i, (payload_expr, payload_ty)) in
            payload.iter().zip(variant.payload().iter()).enumerate()
        {
            let actual_ty = self.check_expr(payload_expr, Expected::Exact(payload_ty))?;
            if actual_ty != *payload_ty {
                return Err(TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(
                    variant_name.to_string(),
                    i,
                    payload_ty.clone(),
                    actual_ty,
                    payload_expr.span,
                )
                .into());
            }
        }

        Ok(expected_enum.clone())
    }

    fn record_property_call_sig(
        &mut self,
        node_id: NodeId,
        def_id: DefId,
        receiver_ty: Type,
        receiver_mode: ParamMode,
        params: Vec<Type>,
    ) {
        let call_params = params
            .into_iter()
            .map(|ty| CallParam {
                mode: ParamMode::In,
                ty,
            })
            .collect();
        self.type_map_builder.record_call_sig(
            node_id,
            CallSig {
                def_id: Some(def_id),
                receiver: Some(CallParam {
                    mode: receiver_mode,
                    ty: receiver_ty,
                }),
                params: call_params,
            },
        );
    }

    fn check_func_def(&mut self, func_def: &FuncDef) -> Result<Type, Vec<TypeCheckError>> {
        // Lookup the function by def id and find the matching overload.
        let func_def_id = func_def.def_id;
        let func_name = func_def.sig.name.as_str();
        let (param_types, ret_type) = {
            let overloads = self.func_sigs.get(func_name).unwrap_or_else(|| {
                panic!(
                    "compiler bug: function {} not found in func_sigs",
                    func_name
                )
            });
            let func_sig = overloads
                .iter()
                .find(|sig| sig.def_id == func_def_id)
                .unwrap_or_else(|| {
                    panic!(
                        "compiler bug: overload for function {} not found",
                        func_name
                    )
                });
            (
                func_sig
                    .params
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect::<Vec<Type>>(),
                func_sig.ret_ty.clone(),
            )
        };

        self.with_type_params(&func_def.sig.type_params, |this| {
            // Record param types
            for (param, param_ty) in func_def.sig.params.iter().zip(param_types.iter()) {
                match this.ctx.def_table.lookup_def(param.def_id) {
                    Some(def) => {
                        this.type_map_builder
                            .record_def_type(def.clone(), param_ty.clone());
                        this.type_map_builder
                            .record_node_type(param.id, param_ty.clone());
                    }
                    None => panic!("Parameter {} not found in def_table", param.ident),
                }
            }

            let infer_scope = this.infer_scope();
            this.push_control_context(ret_type.clone());
            let body_ty = match this.check_expr(&func_def.body, Expected::Exact(&ret_type)) {
                Ok(ty) => ty,
                Err(e) => {
                    this.errors.push(e);
                    let infer_errors = infer_scope.finish();
                    this.errors.extend(infer_errors);
                    this.pop_control_context();
                    return Err(this.errors.clone());
                }
            };
            this.pop_control_context();
            let body_ty = this.apply_infer(&body_ty);
            let infer_errors = infer_scope.finish();
            if !infer_errors.is_empty() {
                this.errors.extend(infer_errors);
            }

            let return_span = this.function_return_span(&func_def.body);
            let has_return = this.body_has_return_stmt(&func_def.body);
            let has_tail = matches!(func_def.body.kind, ExprKind::Block { tail: Some(_), .. });
            if matches!(
                type_assignable(&body_ty, &ret_type),
                TypeAssignability::Incompatible
            ) && (!has_return || has_tail)
            {
                this.errors.push(
                    TypeCheckErrorKind::DeclTypeMismatch(
                        ret_type.clone(),
                        body_ty.clone(),
                        return_span,
                    )
                    .into(),
                );
                return Err(this.errors.clone());
            }

            // record return type
            this.type_map_builder
                .record_node_type(func_def.id, ret_type.clone());
            if this.errors.is_empty() {
                Ok(body_ty)
            } else {
                Err(this.errors.clone())
            }
        })
    }

    fn check_method(
        &mut self,
        method_block: &MethodBlock,
        method_def: &MethodDef,
    ) -> Result<Type, Vec<TypeCheckError>> {
        let self_ty = match self.type_defs.get(&method_block.type_name) {
            Some(ty) => ty.clone(),
            None => {
                self.errors
                    .push(TypeCheckErrorKind::UnknownType(method_block.span).into());
                return Err(self.errors.clone());
            }
        };

        let type_param_map = if method_def.sig.type_params.is_empty() {
            None
        } else {
            Some(Self::type_param_map(&method_def.sig.type_params))
        };
        let ret_type =
            match self.resolve_ret_type(&method_def.sig.ret_ty_expr, type_param_map.as_ref()) {
                Ok(ty) => ty,
                Err(errs) => {
                    self.errors.extend(errs);
                    return Err(self.errors.clone());
                }
            };

        let param_sigs =
            match self.build_param_sigs(&method_def.sig.params, type_param_map.as_ref()) {
                Ok(params) => params,
                Err(errs) => {
                    self.errors.extend(errs);
                    return Err(self.errors.clone());
                }
            };
        let param_types = param_sigs
            .iter()
            .map(|param| param.ty.clone())
            .collect::<Vec<_>>();

        let self_def_id = method_def.sig.self_param.def_id;
        if let Some(def) = self.ctx.def_table.lookup_def(self_def_id) {
            self.type_map_builder
                .record_def_type(def.clone(), self_ty.clone());
            self.type_map_builder
                .record_node_type(method_def.sig.self_param.id, self_ty.clone());
        } else {
            panic!("self parameter not found in def_table");
        }

        for (param, param_ty) in method_def.sig.params.iter().zip(param_types.iter()) {
            match self.ctx.def_table.lookup_def(param.def_id) {
                Some(def) => {
                    self.type_map_builder
                        .record_def_type(def.clone(), param_ty.clone());
                    self.type_map_builder
                        .record_node_type(param.id, param_ty.clone());
                }
                None => panic!("Parameter {} not found in def_table", param.ident),
            }
        }

        self.with_type_params(&method_def.sig.type_params, |this| {
            let infer_scope = this.infer_scope();
            this.push_control_context(ret_type.clone());
            let body_ty = match this.check_expr(&method_def.body, Expected::Exact(&ret_type)) {
                Ok(ty) => ty,
                Err(e) => {
                    this.errors.push(e);
                    let infer_errors = infer_scope.finish();
                    this.errors.extend(infer_errors);
                    this.pop_control_context();
                    return Err(this.errors.clone());
                }
            };
            this.pop_control_context();
            let body_ty = this.apply_infer(&body_ty);
            let infer_errors = infer_scope.finish();
            if !infer_errors.is_empty() {
                this.errors.extend(infer_errors);
            }

            let return_span = this.function_return_span(&method_def.body);
            let has_return = this.body_has_return_stmt(&method_def.body);
            let has_tail = matches!(method_def.body.kind, ExprKind::Block { tail: Some(_), .. });
            if matches!(
                type_assignable(&body_ty, &ret_type),
                TypeAssignability::Incompatible
            ) && (!has_return || has_tail)
            {
                this.errors.push(
                    TypeCheckErrorKind::DeclTypeMismatch(
                        ret_type.clone(),
                        body_ty.clone(),
                        return_span,
                    )
                    .into(),
                );
                return Err(this.errors.clone());
            }

            this.type_map_builder
                .record_node_type(method_def.id, ret_type.clone());

            if this.errors.is_empty() {
                Ok(body_ty)
            } else {
                Err(this.errors.clone())
            }
        })
    }

    fn check_closure(
        &mut self,
        params: &[Param],
        return_ty: &TypeExpr,
        body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let mut param_types = Vec::with_capacity(params.len());
        for param in params {
            let ty = self.resolve_type_expr_in_scope(&param.typ)?;
            self.type_map_builder.record_node_type(param.id, ty.clone());
            if let Some(def) = self.ctx.def_table.lookup_def(param.def_id) {
                self.type_map_builder
                    .record_def_type(def.clone(), ty.clone());
            }
            param_types.push(FnParam {
                mode: fn_param_mode(param.mode.clone()),
                ty,
            });
        }

        let return_ty = self.resolve_type_expr_in_scope(return_ty)?;

        let infer_scope = self.infer_scope();
        self.push_control_context(return_ty.clone());
        let body_ty = match self.check_expr(body, Expected::Exact(&return_ty)) {
            Ok(ty) => ty,
            Err(err) => {
                let _ = infer_scope.finish();
                self.pop_control_context();
                return Err(err);
            }
        };
        self.pop_control_context();
        let body_ty = self.apply_infer(&body_ty);
        let infer_errors = infer_scope.finish();
        if let Some(err) = infer_errors.into_iter().next() {
            return Err(err);
        }

        let return_span = self.function_return_span(body);
        let has_return = self.body_has_return_stmt(body);
        let has_tail = matches!(body.kind, ExprKind::Block { tail: Some(_), .. });
        if matches!(
            type_assignable(&body_ty, &return_ty),
            TypeAssignability::Incompatible
        ) && (!has_return || has_tail)
        {
            return Err(
                TypeCheckErrorKind::DeclTypeMismatch(return_ty, body_ty, return_span).into(),
            );
        }

        Ok(Type::Fn {
            params: param_types,
            ret_ty: Box::new(return_ty),
        })
    }

    fn function_return_span(&self, body: &Expr) -> Span {
        match &body.kind {
            ExprKind::Block { items, tail } => {
                if let Some(tail) = tail {
                    return tail.span;
                }
                if let Some(last) = items.last() {
                    return match last {
                        BlockItem::Stmt(stmt) => stmt.span,
                        BlockItem::Expr(expr) => expr.span,
                    };
                }
                body.span
            }
            _ => body.span,
        }
    }

    fn body_has_return_stmt(&self, body: &Expr) -> bool {
        let mut finder = ReturnStmtFinder { found: false };
        finder.visit_expr(body);
        finder.found
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

struct TypeView {
    ty: Type,
}

impl TypeView {
    fn ty(&self) -> &Type {
        &self.ty
    }

    fn as_struct(&self) -> Option<(&str, &[StructField])> {
        match &self.ty {
            Type::Struct { name, fields } => Some((name.as_str(), fields.as_slice())),
            _ => None,
        }
    }

    fn as_enum(&self) -> Option<(&str, &[EnumVariant])> {
        match &self.ty {
            Type::Enum { name, variants } => Some((name.as_str(), variants.as_slice())),
            _ => None,
        }
    }

    fn as_tuple(&self) -> Option<&[Type]> {
        match &self.ty {
            Type::Tuple { field_tys } => Some(field_tys.as_slice()),
            _ => None,
        }
    }

    fn as_array(&self) -> Option<(&Type, &[usize])> {
        match &self.ty {
            Type::Array { elem_ty, dims } => Some((elem_ty.as_ref(), dims.as_slice())),
            _ => None,
        }
    }

    fn as_slice(&self) -> Option<&Type> {
        match &self.ty {
            Type::Slice { elem_ty } => Some(elem_ty.as_ref()),
            _ => None,
        }
    }

    fn is_string(&self) -> bool {
        matches!(self.ty, Type::String)
    }
}

#[derive(Clone, Copy)]
pub(super) enum Expected<'a> {
    Exact(&'a Type),
    Unknown,
}

impl<'a> Expected<'a> {
    pub(super) fn from_option(opt: Option<&'a Type>) -> Self {
        match opt {
            Some(ty) => Expected::Exact(ty),
            None => Expected::Unknown,
        }
    }

    pub(super) fn as_option(self) -> Option<&'a Type> {
        match self {
            Expected::Exact(ty) => Some(ty),
            Expected::Unknown => None,
        }
    }
}

enum ExpectedOwned {
    Exact(Type),
    Unknown,
}

impl ExpectedOwned {
    fn as_option(&self) -> Option<&Type> {
        match self {
            ExpectedOwned::Exact(ty) => Some(ty),
            ExpectedOwned::Unknown => None,
        }
    }

    fn as_expected(&self) -> Expected<'_> {
        match self {
            ExpectedOwned::Exact(ty) => Expected::Exact(ty),
            ExpectedOwned::Unknown => Expected::Unknown,
        }
    }
}

impl TypeChecker {
    pub(super) fn check_expr(
        &mut self,
        expr: &Expr,
        expected: Expected<'_>,
    ) -> Result<Type, TypeCheckError> {
        self.visit_expr(expr, expected.as_option())
    }
}

struct ReturnStmtFinder {
    found: bool,
}

impl Visitor<DefId, ()> for ReturnStmtFinder {
    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        if self.found {
            return;
        }
        if matches!(stmt.kind, StmtExprKind::Return { .. }) {
            self.found = true;
            return;
        }
        walk_visit_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if self.found {
            return;
        }
        if matches!(expr.kind, ExprKind::Closure { .. }) {
            return;
        }
        walk_visit_expr(self, expr);
    }
}

impl TreeFolder<DefId> for TypeChecker {
    type Error = TypeCheckError;
    type Output = Type;
    type Input = Type;

    fn visit_func_def(&mut self, func_def: &FuncDef) -> Result<Type, TypeCheckError> {
        if self.halted {
            return Ok(Type::Unit);
        }

        if self.check_func_def(func_def).is_err() {
            self.halted = true;
        }

        Ok(Type::Unit)
    }

    fn visit_method_block(
        &mut self,
        method_block: &MethodBlock,
    ) -> Result<Vec<Type>, TypeCheckError> {
        if self.halted {
            return Ok(Vec::new());
        }

        let mut outputs = Vec::new();
        for method_item in &method_block.method_items {
            let method_def = match method_item {
                MethodItem::Decl(_) => continue,
                MethodItem::Def(method_def) => method_def,
            };
            if self.check_method(method_block, method_def).is_err() {
                self.halted = true;
                break;
            }
            outputs.push(Type::Unit);
        }

        Ok(outputs)
    }

    fn visit_if(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<(Type, Type, Type), TypeCheckError> {
        let (cond_type, then_type, else_type) = walk_if(self, cond, then_body, else_body)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckErrorKind::CondNotBoolean(cond_type, cond.span).into());
        }

        let (then_type, else_type, _unified) = self.unify_infer_types(&then_type, &else_type);

        if then_type != else_type {
            // create a span that covers both the then and else bodies so the
            // diagnostic highlights the whole region
            let span = Span::merge_all(vec![then_body.span, else_body.span]);
            return Err(
                TypeCheckErrorKind::ThenElseTypeMismatch(then_type, else_type, span).into(),
            );
        }

        Ok((cond_type, then_type, else_type))
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) -> Result<Type, TypeCheckError> {
        let ty = match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => self.check_binding(pattern, decl_ty, value)?,

            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => self.check_binding(pattern, decl_ty, value)?,

            StmtExprKind::VarDecl {
                decl_ty,
                ident: _,
                def_id,
            } => {
                let ty = self.resolve_type_expr_in_scope(decl_ty)?;
                if let Some(def) = self.ctx.def_table.lookup_def(*def_id) {
                    self.type_map_builder.record_def_type(def.clone(), ty);
                }
                Type::Unit
            }

            StmtExprKind::Assign {
                assignee, value, ..
            } => self.check_assign(assignee, value)?,

            StmtExprKind::While { cond, body } => self.check_while(cond, body)?,

            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => self.check_for(pattern, iter, body)?,
            StmtExprKind::Break => {
                if self.loop_depth() == 0 {
                    return Err(TypeCheckErrorKind::BreakOutsideLoop(stmt.span).into());
                }
                Type::Unit
            }
            StmtExprKind::Continue => {
                if self.loop_depth() == 0 {
                    return Err(TypeCheckErrorKind::ContinueOutsideLoop(stmt.span).into());
                }
                Type::Unit
            }
            StmtExprKind::Return { value } => {
                let Some(return_ty) = self.current_return_ty().cloned() else {
                    if let Some(value) = value {
                        let _ = self.check_expr(value, Expected::Unknown)?;
                    }
                    return Err(TypeCheckErrorKind::ReturnOutsideFunction(stmt.span).into());
                };

                match value {
                    Some(value) => {
                        if return_ty == Type::Unit {
                            let _ = self.check_expr(value, Expected::Unknown)?;
                            return Err(
                                TypeCheckErrorKind::ReturnValueUnexpected(value.span).into()
                            );
                        }

                        let value_ty = self.check_expr(value, Expected::Exact(&return_ty))?;
                        let (value_ty, return_ty, _unified) =
                            self.unify_infer_types(&value_ty, &return_ty);
                        if matches!(
                            type_assignable(&value_ty, &return_ty),
                            TypeAssignability::Incompatible
                        ) {
                            return Err(TypeCheckErrorKind::ReturnTypeMismatch(
                                return_ty, value_ty, value.span,
                            )
                            .into());
                        }
                    }
                    None => {
                        if return_ty != Type::Unit {
                            return Err(TypeCheckErrorKind::ReturnValueMissing(
                                return_ty, stmt.span,
                            )
                            .into());
                        }
                    }
                }

                Type::Unit
            }
        };

        self.type_map_builder.record_node_type(stmt.id, ty.clone());
        Ok(ty)
    }

    fn visit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<Type, TypeCheckError> {
        let expected = match Expected::from_option(expected) {
            Expected::Exact(ty) => ExpectedOwned::Exact(self.apply_infer(ty)),
            Expected::Unknown => ExpectedOwned::Unknown,
        };
        let result = match (&expr.kind, expected.as_option()) {
            (ExprKind::IntLit(_), Some(expected_ty)) if expected_ty.is_int() => {
                Ok(expected_ty.clone())
            }

            (
                ExprKind::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: operand,
                },
                Some(expected_ty @ Type::Int { signed: true, .. }),
            ) if matches!(operand.kind, ExprKind::IntLit(_)) => {
                let _ = self.check_expr(operand, Expected::Exact(expected_ty))?;
                Ok(expected_ty.clone())
            }

            _ => match &expr.kind {
                ExprKind::IntLit(_) => Ok(Type::uint(64)),

                ExprKind::BoolLit(_) => Ok(Type::Bool),

                ExprKind::CharLit(_) => Ok(Type::Char),

                ExprKind::StringLit { .. } => Ok(Type::String),

                ExprKind::UnitLit => Ok(Type::Unit),

                ExprKind::HeapAlloc { expr } => {
                    let inner_expected = match expected.as_option() {
                        Some(Type::Heap { elem_ty }) => Some(elem_ty.as_ref()),
                        _ => None,
                    };
                    let elem_ty = self.check_expr(expr, Expected::from_option(inner_expected))?;
                    Ok(Type::Heap {
                        elem_ty: Box::new(elem_ty),
                    })
                }

                ExprKind::StringFmt { segments } => {
                    for segment in segments {
                        if let StringFmtSegment::Expr { expr, span } = segment {
                            let ty = walk_expr(self, expr)?;
                            if !ty.is_int() && ty != Type::String {
                                return Err(TypeCheckErrorKind::StringFmtExprUnsupportedType(
                                    ty, *span,
                                )
                                .into());
                            }
                        }
                    }
                    Ok(Type::String)
                }

                ExprKind::ArrayLit { elem_ty, init } => {
                    self.check_array_lit(elem_ty.as_ref(), init, expected.as_expected(), expr.span)
                }

                ExprKind::ArrayIndex { target, indices } => {
                    let target_ty = self.check_expr(target, Expected::Unknown)?;
                    let peeled_ty = target_ty.peel_heap();

                    match peeled_ty {
                        Type::Array { elem_ty, dims } => {
                            self.check_array_index(elem_ty.as_ref(), &dims, indices, target.span)
                        }
                        Type::Slice { elem_ty } => {
                            self.check_slice_index(elem_ty.as_ref(), indices, target.span)
                        }
                        Type::String => self.check_string_index(indices, target.span),
                        _ => {
                            return Err(TypeCheckErrorKind::InvalidIndexTargetType(
                                target_ty,
                                target.span,
                            )
                            .into());
                        }
                    }
                }

                ExprKind::TupleLit(fields) => self.check_tuple_lit(fields),

                ExprKind::TupleField { target, index } => {
                    self.check_tuple_field_access(target, *index)
                }

                ExprKind::StructLit {
                    name,
                    type_args,
                    fields,
                } => self.check_struct_lit(name, type_args, fields, expr.id, expr.span),

                ExprKind::StructField { target, field } => {
                    self.check_field_access(expr.id, target, field)
                }

                ExprKind::StructUpdate { target, fields } => {
                    self.check_struct_update(target, fields)
                }

                ExprKind::EnumVariant {
                    enum_name,
                    type_args,
                    variant,
                    payload,
                } => {
                    if type_args.is_empty()
                        && let Some(Type::Enum { name, .. }) = expected.as_option()
                        && Self::enum_name_matches(name, enum_name)
                    {
                        let payload = payload.iter().collect::<Vec<_>>();
                        self.check_unqualified_enum_variant(
                            variant,
                            &payload,
                            expected.as_expected(),
                            expr.span,
                        )
                    } else {
                        self.check_enum_variant(
                            enum_name, type_args, variant, payload, expr.id, expr.span,
                        )
                    }
                }

                ExprKind::BinOp { left, op, right } => {
                    let (left_type, right_type) = self.visit_binary_expr(left, right)?;
                    match op {
                        // Arithmetic operators
                        BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mul
                        | BinaryOp::Div
                        | BinaryOp::Mod
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor
                        | BinaryOp::BitAnd
                        | BinaryOp::Shl
                        | BinaryOp::Shr => {
                            if !left_type.is_int() {
                                return Err(TypeCheckErrorKind::ArithOperandNotInt(
                                    left_type, left.span,
                                )
                                .into());
                            }
                            if !right_type.is_int() {
                                return Err(TypeCheckErrorKind::ArithOperandNotInt(
                                    right_type, right.span,
                                )
                                .into());
                            }
                            if left_type != right_type {
                                let span = Span::merge_all(vec![left.span, right.span]);
                                return Err(TypeCheckErrorKind::ArithTypeMismatch(
                                    left_type, right_type, span,
                                )
                                .into());
                            }
                            Ok(left_type)
                        }

                        // Comparison operators
                        BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Lt
                        | BinaryOp::Gt
                        | BinaryOp::LtEq
                        | BinaryOp::GtEq => {
                            if !left_type.is_int() {
                                return Err(TypeCheckErrorKind::CmpOperandNotInt(
                                    left_type, left.span,
                                )
                                .into());
                            }
                            if !right_type.is_int() {
                                return Err(TypeCheckErrorKind::CmpOperandNotInt(
                                    right_type, right.span,
                                )
                                .into());
                            }
                            Ok(Type::Bool)
                        }

                        // Logical operators
                        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                            if left_type != Type::Bool {
                                return Err(TypeCheckErrorKind::LogicalOperandNotBoolean(
                                    left_type, left.span,
                                )
                                .into());
                            }
                            if right_type != Type::Bool {
                                return Err(TypeCheckErrorKind::LogicalOperandNotBoolean(
                                    right_type, right.span,
                                )
                                .into());
                            }
                            Ok(Type::Bool)
                        }
                    }
                }

                ExprKind::UnaryOp { op, expr } => {
                    let ty = walk_expr(self, expr)?;
                    match op {
                        UnaryOp::Neg => {
                            if !ty.is_int() {
                                return Err(TypeCheckErrorKind::NegationOperandNotInt(
                                    ty, expr.span,
                                )
                                .into());
                            }
                            Ok(ty)
                        }
                        UnaryOp::LogicalNot => {
                            if ty != Type::Bool {
                                return Err(TypeCheckErrorKind::LogicalOperandNotBoolean(
                                    ty, expr.span,
                                )
                                .into());
                            }
                            Ok(Type::Bool)
                        }
                        UnaryOp::BitNot => {
                            if !ty.is_int() {
                                return Err(
                                    TypeCheckErrorKind::ArithOperandNotInt(ty, expr.span).into()
                                );
                            }
                            Ok(ty)
                        }
                    }
                }

                ExprKind::Move { expr } => walk_expr(self, expr),

                ExprKind::Coerce { expr, .. } => walk_expr(self, expr),

                ExprKind::ImplicitMove { expr } => walk_expr(self, expr),

                ExprKind::AddrOf { expr } => walk_expr(self, expr),

                ExprKind::Deref { expr } => walk_expr(self, expr),

                ExprKind::Block { items, tail } => {
                    for item in items {
                        let _ = self.visit_block_item(item)?;
                    }

                    let tail_ty = self.visit_block_tail(tail.as_deref(), expected.as_option())?;

                    Ok(tail_ty.unwrap_or(Type::Unit))
                }

                ExprKind::Var { ident, def_id } => {
                    if let Some(def) = self.ctx.def_table.lookup_def(*def_id)
                        && matches!(def.kind, DefKind::EnumVariantName)
                    {
                        self.check_unqualified_enum_variant(
                            ident,
                            &[],
                            expected.as_expected(),
                            expr.span,
                        )
                    } else {
                        let mut ty = self.check_var_ref(*def_id, expr.span)?;
                        if let Some(expected_ty) = expected.as_option() {
                            let (new_ty, _expected_ty, _unified) =
                                self.unify_infer_types(&ty, expected_ty);
                            ty = new_ty;
                        }
                        Ok(ty)
                    }
                }

                ExprKind::Call { callee, args } => {
                    self.check_call(expr, callee, args, expected.as_expected())
                }

                ExprKind::MethodCall {
                    callee,
                    method_name,
                    args,
                } => {
                    self.check_method_call(method_name, expr, callee, args, expected.as_expected())
                }

                ExprKind::If {
                    cond,
                    then_body,
                    else_body,
                } => {
                    let (_cond, then_type, _else_type) =
                        self.visit_if(cond, then_body, else_body)?;
                    Ok(then_type)
                }

                ExprKind::Range { start, end } => {
                    let start_ty = self.check_expr(start, Expected::Unknown)?;
                    if start_ty != Type::uint(64) {
                        return Err(
                            TypeCheckErrorKind::IndexTypeNotInt(start_ty, start.span).into()
                        );
                    }
                    let end_ty = self.check_expr(end, Expected::Unknown)?;
                    if end_ty != Type::uint(64) {
                        return Err(TypeCheckErrorKind::IndexTypeNotInt(end_ty, end.span).into());
                    }
                    Ok(Type::Range {
                        elem_ty: Box::new(Type::uint(64)),
                    })
                }

                ExprKind::Slice { target, start, end } => self.check_slice(target, start, end),

                ExprKind::Match { scrutinee, arms } => self.check_match(scrutinee, arms),

                ExprKind::Closure {
                    params,
                    return_ty,
                    body,
                    ..
                } => self.check_closure(params, return_ty, body),
            },
        };

        result.map(|ty| {
            let ty = self.apply_infer(&ty);
            self.type_map_builder.record_node_type(expr.id, ty.clone());
            ty
        })
    }
}
