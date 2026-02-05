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
    value_assignable,
};
use crate::types::{TypeAssignability, ValueAssignability, type_assignable};

use super::errors::{TypeCheckError, TypeCheckErrorKind};
use super::overloads::{OverloadResolver, OverloadSig, ParamSig};
use super::type_map::{
    CallParam, CallSig, CallSigMap, GenericInst, GenericInstMap, TypeMap, TypeMapBuilder,
    resolve_type_expr, resolve_type_expr_with_params,
};
use super::unify::Unifier;

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

    fn resolve_type_expr_in_scope(&self, ty_expr: &TypeExpr) -> Result<Type, TypeCheckError> {
        resolve_type_expr_with_params(
            &self.ctx.def_table,
            &self.ctx.module,
            ty_expr,
            self.current_type_params(),
        )
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

    fn build_param_sigs(
        &self,
        params: &[Param],
        type_params: Option<&HashMap<DefId, TyVarId>>,
    ) -> Result<Vec<ParamSig>, Vec<TypeCheckError>> {
        params
            .iter()
            .map(|param| {
                let ty = resolve_type_expr_with_params(
                    &self.ctx.def_table,
                    &self.ctx.module,
                    &param.typ,
                    type_params,
                )?;
                Ok(ParamSig {
                    name: self.def_name(param.def_id).to_string(),
                    ty,
                    mode: param.mode.clone(),
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| vec![e])
    }

    fn resolve_ret_type(
        &self,
        ret_type: &TypeExpr,
        type_params: Option<&HashMap<DefId, TyVarId>>,
    ) -> Result<Type, Vec<TypeCheckError>> {
        resolve_type_expr_with_params(&self.ctx.def_table, &self.ctx.module, ret_type, type_params)
            .map_err(|e| vec![e])
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

    fn property_owner_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.clone()),
            Type::String => Some("string".to_string()),
            _ => None,
        }
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

            this.push_control_context(ret_type.clone());
            let body_ty = match this.visit_expr(&func_def.body, Some(&ret_type)) {
                Ok(ty) => ty,
                Err(e) => {
                    this.errors.push(e);
                    this.pop_control_context();
                    return Err(this.errors.clone());
                }
            };
            this.pop_control_context();

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
            this.push_control_context(ret_type.clone());
            let body_ty = match this.visit_expr(&method_def.body, Some(&ret_type)) {
                Ok(ty) => ty,
                Err(e) => {
                    this.errors.push(e);
                    this.pop_control_context();
                    return Err(this.errors.clone());
                }
            };
            this.pop_control_context();

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

        self.push_control_context(return_ty.clone());
        let body_ty = match self.visit_expr(body, Some(&return_ty)) {
            Ok(ty) => ty,
            Err(err) => {
                self.pop_control_context();
                return Err(err);
            }
        };
        self.pop_control_context();

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

    fn check_array_lit(
        &mut self,
        elem_ty_expr: Option<&TypeExpr>,
        init: &ArrayLitInit,
        expected: Option<&Type>,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        let len = self.array_lit_len(init, span)?;

        // Resolve the element type
        let elem_ty = if let Some(elem_ty_expr) = elem_ty_expr {
            self.resolve_type_expr_in_scope(elem_ty_expr)?
        } else if let Some(Type::Array {
            elem_ty: expected_elem_ty,
            dims: expected_dims,
        }) = expected
        {
            // When the expected array has multiple dimensions, each element is itself
            // an array of the remaining dimensions.
            if expected_dims.len() > 1 {
                Type::Array {
                    elem_ty: expected_elem_ty.clone(),
                    dims: expected_dims[1..].to_vec(),
                }
            } else {
                expected_elem_ty.as_ref().clone()
            }
        } else {
            match init {
                ArrayLitInit::Elems(elems) => self.visit_expr(&elems[0], None)?,
                ArrayLitInit::Repeat(expr, _) => self.visit_expr(expr, None)?,
            }
        };

        // Type check the elements
        self.check_array_lit_init(init, &elem_ty)?;

        // Build the array type
        Ok(self.build_array_type_from_elem(len, elem_ty))
    }

    fn array_lit_len(&self, init: &ArrayLitInit, span: Span) -> Result<usize, TypeCheckError> {
        match init {
            ArrayLitInit::Elems(elems) if elems.is_empty() => {
                Err(TypeCheckErrorKind::EmptyArrayLiteral(span).into())
            }
            ArrayLitInit::Repeat(_, 0) => Err(TypeCheckErrorKind::EmptyArrayLiteral(span).into()),
            ArrayLitInit::Elems(elems) => Ok(elems.len()),
            ArrayLitInit::Repeat(_, count) => Ok(*count as usize),
        }
    }

    fn check_array_lit_init(
        &mut self,
        init: &ArrayLitInit,
        elem_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        let exprs: Vec<&Expr> = match init {
            ArrayLitInit::Elems(elems) => elems.iter().collect(),
            ArrayLitInit::Repeat(expr, _) => vec![expr],
        };

        let actual_types = self.visit_array_lit_init(init, Some(elem_ty))?;
        for (expr, this_ty) in exprs.into_iter().zip(actual_types) {
            self.check_assignable_to(expr, &this_ty, elem_ty)?;
        }
        Ok(())
    }

    fn build_array_type_from_elem(&self, len: usize, elem_ty: Type) -> Type {
        // Build the array type, flattening nested array dimensions.
        match elem_ty {
            Type::Array {
                elem_ty: inner_elem_ty,
                dims: inner_dims,
            } => {
                let mut new_dims = vec![len];
                new_dims.extend(inner_dims);
                Type::Array {
                    elem_ty: inner_elem_ty,
                    dims: new_dims,
                }
            }
            _ => Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: vec![len],
            },
        }
    }

    fn check_array_index(
        &mut self,
        elem_ty: &Type,
        dims: &[usize],
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Check we don't have more indices than dimensions
        if indices.len() > dims.len() {
            return Err(TypeCheckErrorKind::TooManyIndices(dims.len(), indices.len(), span).into());
        }

        // type check each index
        for index in indices {
            let index_type = self.visit_expr(index, None)?;
            if index_type != Type::uint(64) {
                return Err(TypeCheckErrorKind::IndexTypeNotInt(index_type, index.span).into());
            }
        }

        // Determine result type
        if indices.len() == dims.len() {
            // Fully indexed, return the element type
            Ok(elem_ty.clone())
        } else {
            // Partially indexed, return array with the remaining dimensions
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty.clone()),
                dims: dims[indices.len()..].to_vec(),
            })
        }
    }

    fn check_slice(
        &mut self,
        target: &Expr,
        start: &Option<Box<Expr>>,
        end: &Option<Box<Expr>>,
    ) -> Result<Type, TypeCheckError> {
        let target_ty = self.visit_expr(target, None)?;

        // Type check start and end (must be u64)
        if let Some(start) = start {
            let ty = self.visit_expr(start, None)?;
            if ty != Type::uint(64) {
                return Err(TypeCheckErrorKind::IndexTypeNotInt(ty, start.span).into());
            }
        }
        if let Some(end) = end {
            let ty = self.visit_expr(end, None)?;
            if ty != Type::uint(64) {
                return Err(TypeCheckErrorKind::IndexTypeNotInt(ty, end.span).into());
            }
        }

        // Slices are allowed only for arrays and strings.
        match target_ty {
            Type::Array { .. } => {
                let Some(slice_elem_ty) = target_ty.array_item_type() else {
                    return Err(TypeCheckErrorKind::SliceTargetZeroDimArray(
                        target_ty,
                        target.span,
                    )
                    .into());
                };

                Ok(Type::Slice {
                    elem_ty: Box::new(slice_elem_ty),
                })
            }
            Type::Slice { elem_ty } => Ok(Type::Slice { elem_ty }),
            Type::String => Ok(Type::Slice {
                elem_ty: Box::new(Type::uint(8)),
            }),
            other => {
                Err(TypeCheckErrorKind::SliceTargetNotArrayOrString(other, target.span).into())
            }
        }
    }

    fn check_string_index(&mut self, indices: &[Expr], span: Span) -> Result<Type, TypeCheckError> {
        // Check we have exactly one index
        if indices.len() > 1 {
            return Err(TypeCheckErrorKind::TooManyIndices(1, indices.len(), span).into());
        }

        let index_ty = self.visit_expr(&indices[0], None)?;
        if index_ty != Type::uint(64) {
            return Err(TypeCheckErrorKind::IndexTypeNotInt(index_ty, indices[0].span).into());
        }

        Ok(Type::uint(8))
    }

    fn check_slice_index(
        &mut self,
        elem_ty: &Type,
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Slices are 1D, so only a single index is allowed.
        if indices.len() != 1 {
            return Err(TypeCheckErrorKind::TooManyIndices(1, indices.len(), span).into());
        }

        let index_ty = self.visit_expr(&indices[0], None)?;
        if index_ty != Type::uint(64) {
            return Err(TypeCheckErrorKind::IndexTypeNotInt(index_ty, indices[0].span).into());
        }

        Ok(elem_ty.clone())
    }

    fn check_tuple_lit(&mut self, fields: &[Expr]) -> Result<Type, TypeCheckError> {
        if fields.is_empty() {
            return Err(TypeCheckErrorKind::EmptyTupleLiteral(fields[0].span).into());
        }

        // Type check each field
        let field_tys = self.visit_exprs(fields)?;

        Ok(Type::Tuple { field_tys })
    }

    fn check_tuple_field_access(
        &mut self,
        target: &Expr,
        index: usize,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.visit_expr(target, None)?;
        let peeled_ty = self.expand_shallow_type(&target_ty.peel_heap());

        match peeled_ty {
            Type::Tuple { field_tys } => {
                let index_usize = index;
                if index_usize >= field_tys.len() {
                    return Err(TypeCheckErrorKind::TupleFieldOutOfBounds(
                        field_tys.len(),
                        index,
                        target.span,
                    )
                    .into());
                }

                Ok(field_tys[index_usize].clone())
            }
            _ => Err(TypeCheckErrorKind::InvalidTupleFieldTarget(target_ty, target.span).into()),
        }
    }

    fn check_struct_lit(
        &mut self,
        name: &String,
        fields: &[StructLitField],
    ) -> Result<Type, TypeCheckError> {
        let struct_ty = match self.type_defs.get(name) {
            Some(ty) => ty.clone(),
            None => {
                for field in fields {
                    let _ = self.visit_expr(&field.value, None)?;
                }
                return Ok(Type::Unknown);
            }
        };
        let Type::Struct {
            fields: struct_fields,
            ..
        } = &struct_ty
        else {
            for field in fields {
                let _ = self.visit_expr(&field.value, None)?;
            }
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.visit_expr(&field.value, None)?;
            if let Some(expected) = struct_fields.iter().find(|f| f.name == field.name)
                && actual_ty != expected.ty
            {
                return Err(TypeCheckErrorKind::StructFieldTypeMismatch(
                    field.name.clone(),
                    expected.ty.clone(),
                    actual_ty,
                    field.span,
                )
                .into());
            }
        }

        Ok(struct_ty)
    }

    fn check_field_access(
        &mut self,
        expr_id: NodeId,
        target: &Expr,
        field: &str,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.visit_expr(target, None)?;
        let peeled_ty = self.expand_shallow_type(&target_ty.peel_heap());

        if let Some(type_name) = self.property_owner_name(&peeled_ty) {
            // Property access is treated as a getter call.
            let prop_info = self
                .property_sigs
                .get(&type_name)
                .and_then(|props| props.get(field))
                .map(|prop| (prop.getter, prop.ty.clone()));
            if let Some((getter, prop_ty)) = prop_info {
                let Some(getter) = getter else {
                    return Err(TypeCheckErrorKind::PropertyNotReadable(
                        field.to_string(),
                        target.span,
                    )
                    .into());
                };
                self.record_property_call_sig(
                    expr_id,
                    getter,
                    target_ty,
                    ParamMode::In,
                    Vec::new(),
                );
                return Ok(prop_ty);
            }
        }

        if field == "len" {
            if !self.is_place_expr(target) {
                return Err(TypeCheckErrorKind::LenTargetNotLvalue(target.span).into());
            }
            match peeled_ty {
                Type::Array { .. } | Type::Slice { .. } | Type::String => {
                    self.type_map_builder.record_call_sig(
                        expr_id,
                        CallSig {
                            def_id: None,
                            receiver: None,
                            params: Vec::new(),
                        },
                    );
                    return Ok(Type::uint(64));
                }
                _ => {}
            }
        }

        match peeled_ty {
            Type::Struct { fields, .. } => match fields.iter().find(|f| f.name == field) {
                Some(field) => Ok(field.ty.clone()),
                None => Ok(Type::Unknown),
            },
            _ => Err(TypeCheckErrorKind::InvalidStructFieldTarget(target_ty, target.span).into()),
        }
    }

    fn check_enum_variant(
        &mut self,
        enum_name: &String,
        variant_name: &String,
        payload: &[Expr],
    ) -> Result<Type, TypeCheckError> {
        // Lookup the type
        let enum_ty = match self.type_defs.get(enum_name) {
            Some(ty) => ty.clone(),
            None => {
                for expr in payload {
                    let _ = self.visit_expr(expr, None)?;
                }
                return Ok(Type::Unknown);
            }
        };

        let Type::Enum { variants, .. } = &enum_ty else {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(Type::Unknown);
        };

        // Get the variant
        let Some(variant_ty) = variants.iter().find(|v| v.name == *variant_name) else {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(enum_ty.clone());
        };

        if payload.len() != variant_ty.payload.len() {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(enum_ty.clone());
        }

        // Type check each payload element
        for (i, (payload_expr, payload_ty)) in
            payload.iter().zip(variant_ty.payload.iter()).enumerate()
        {
            let actual_ty = self.visit_expr(payload_expr, None)?;
            if actual_ty != *payload_ty {
                return Err(TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(
                    variant_name.clone(),
                    i,
                    payload_ty.clone(),
                    actual_ty,
                    payload_expr.span,
                )
                .into());
            }
        }

        Ok(enum_ty)
    }

    fn check_struct_update(
        &mut self,
        target: &Expr,
        fields: &[StructUpdateField],
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.visit_expr(target, None)?;
        let struct_ty = match &target_ty {
            Type::Struct { .. } => target_ty.clone(),
            _ => {
                return Err(
                    TypeCheckErrorKind::InvalidStructUpdateTarget(target_ty, target.span).into(),
                );
            }
        };

        let Type::Struct {
            fields: struct_fields,
            ..
        } = &struct_ty
        else {
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.visit_expr(&field.value, None)?;
            if let Some(expected) = struct_fields.iter().find(|f| f.name == field.name)
                && actual_ty != expected.ty
            {
                return Err(TypeCheckErrorKind::StructFieldTypeMismatch(
                    field.name.clone(),
                    expected.ty.clone(),
                    actual_ty,
                    field.span,
                )
                .into());
            }
        }

        Ok(struct_ty)
    }

    fn check_bind_pattern(
        &mut self,
        pattern: &BindPattern,
        value_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                // Record this identifier's type
                if let Some(def) = self.ctx.def_table.lookup_def(*def_id) {
                    self.type_map_builder
                        .record_def_type(def.clone(), value_ty.clone());
                }
                Ok(())
            }
            BindPatternKind::Array { patterns } => {
                // Value must be an array
                match value_ty {
                    Type::Array { dims, .. } => {
                        // Check the pattern has the right number of elements
                        if patterns.len() != dims[0] {
                            return Err(TypeCheckErrorKind::ArrayPatternLengthMismatch(
                                dims[0],
                                patterns.len(),
                                pattern.span,
                            )
                            .into());
                        }

                        // Determine the subtype for each pattern element
                        let sub_ty = value_ty
                            .array_item_type()
                            .unwrap_or_else(|| panic!("compiler bug: empty array dims"));

                        // Recursively type check each sub-pattern
                        for pattern in patterns {
                            self.check_bind_pattern(pattern, &sub_ty)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into()),
                }
            }
            BindPatternKind::Tuple { patterns } => {
                match value_ty {
                    Type::Tuple { field_tys } => {
                        if patterns.len() != field_tys.len() {
                            return Err(TypeCheckErrorKind::TuplePatternLengthMismatch(
                                field_tys.len(),
                                patterns.len(),
                                pattern.span,
                            )
                            .into());
                        }

                        // Recursively type check each sub-pattern
                        for (pattern, field) in patterns.iter().zip(field_tys) {
                            self.check_bind_pattern(pattern, field)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into()),
                }
            }
            BindPatternKind::Struct { name, fields } => {
                let Type::Struct {
                    name: ty_name,
                    fields: struct_fields,
                } = value_ty
                else {
                    return Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into());
                };

                // Check that the struct type name matches
                if ty_name != name {
                    return Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into());
                }

                // Check each field pattern
                for field in fields {
                    // Type check the field
                    if let Some(expected_ty) = struct_fields
                        .iter()
                        .find(|f| f.name == field.name)
                        .map(|f| &f.ty)
                    {
                        self.check_bind_pattern(&field.pattern, expected_ty)?;
                    }
                }

                Ok(())
            }
        }
    }

    fn check_binding(
        &mut self,
        pattern: &BindPattern,
        decl_ty: &Option<TypeExpr>,
        value: &Expr,
    ) -> Result<Type, TypeCheckError> {
        // resolve the declaration type (if present)
        let expected_ty = decl_ty
            .as_ref()
            .map(|ty_expr| self.resolve_type_expr_in_scope(ty_expr))
            .transpose()?;

        let mut value_ty = self.visit_expr(value, expected_ty.as_ref())?;

        if let Some(decl_ty) = &expected_ty {
            self.check_assignable_to(value, &value_ty, decl_ty)?;
            value_ty = decl_ty.clone();
            if matches!(&value.kind, ExprKind::ArrayLit { .. })
                && matches!(value_ty, Type::Array { .. })
            {
                self.type_map_builder
                    .record_node_type(value.id, value_ty.clone());
            }
        }

        self.check_bind_pattern(pattern, &value_ty)?;

        Ok(Type::Unit)
    }

    fn check_assignable_to(
        &mut self,
        from_value: &Expr,
        from_ty: &Type,
        to_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        match type_assignable(from_ty, to_ty) {
            TypeAssignability::Incompatible => Err(TypeCheckErrorKind::DeclTypeMismatch(
                to_ty.clone(),
                from_ty.clone(),
                from_value.span,
            )
            .into()),
            _ => Ok(()),
        }
    }

    fn check_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        // Reject string index assignment (for now)
        if let ExprKind::ArrayIndex { target, .. } = &assignee.kind {
            let target_ty = self.visit_expr(target, None)?;
            if target_ty == Type::String {
                return Err(TypeCheckErrorKind::StringIndexAssign(assignee.span).into());
            }
        }

        if let ExprKind::StructField { target, field } = &assignee.kind {
            let receiver_ty = self.visit_expr(target, None)?;
            let peeled_ty = self.expand_shallow_type(&receiver_ty.peel_heap());
            if let Some(type_name) = self.property_owner_name(&peeled_ty) {
                // Property assignment is treated as a setter call.
                let prop_info = self
                    .property_sigs
                    .get(&type_name)
                    .and_then(|props| props.get(field))
                    .map(|prop| (prop.setter, prop.ty.clone()));
                if let Some((setter, prop_ty)) = prop_info {
                    let Some(setter) = setter else {
                        return Err(TypeCheckErrorKind::PropertyNotWritable(
                            field.clone(),
                            assignee.span,
                        )
                        .into());
                    };
                    let rhs_type = self.visit_expr(value, Some(&prop_ty))?;
                    self.check_assignable_to(value, &rhs_type, &prop_ty)?;
                    self.record_property_call_sig(
                        assignee.id,
                        setter,
                        receiver_ty,
                        ParamMode::InOut,
                        vec![prop_ty.clone()],
                    );
                    self.type_map_builder
                        .record_node_type(assignee.id, Type::Unit);
                    return Ok(Type::Unit);
                }
            }
            if field == "len"
                && matches!(
                    peeled_ty,
                    Type::Array { .. } | Type::Slice { .. } | Type::String
                )
            {
                return Err(
                    TypeCheckErrorKind::PropertyNotWritable(field.clone(), assignee.span).into(),
                );
            }
        }

        let lhs_type = self.visit_expr(assignee, None)?;
        let rhs_type = self.visit_expr(value, Some(&lhs_type))?;

        match self.check_assignable_to(value, &rhs_type, &lhs_type) {
            Ok(()) => Ok(Type::Unit),
            Err(_) => {
                Err(
                    TypeCheckErrorKind::AssignTypeMismatch(lhs_type, rhs_type, assignee.span)
                        .into(),
                )
            }
        }
    }

    fn check_var_ref(&mut self, def_id: DefId, span: Span) -> Result<Type, TypeCheckError> {
        match self
            .ctx
            .def_table
            .lookup_def(def_id)
            .and_then(|def| self.type_map_builder.lookup_def_type(def))
        {
            Some(def_type) => Ok(def_type.clone()),
            None => Err(TypeCheckErrorKind::UnknownType(span).into()),
        }
    }

    fn single_arity_param_types(overloads: &[OverloadSig], arg_count: usize) -> Option<Vec<Type>> {
        let mut matches = overloads.iter().filter(|sig| sig.params.len() == arg_count);
        let sig = matches.next()?;
        if matches.next().is_some() {
            return None;
        }
        Some(sig.params.iter().map(|param| param.ty.clone()).collect())
    }

    fn check_call_arg_types(
        &mut self,
        args: &[CallArg],
        param_types: &[Type],
    ) -> Result<(), TypeCheckError> {
        for (i, arg) in args.iter().enumerate() {
            let param_ty = &param_types[i];
            let arg_ty = self.visit_expr(&arg.expr, Some(param_ty))?;
            match self.check_assignable_to(&arg.expr, &arg_ty, param_ty) {
                Ok(()) => continue,
                Err(_) => {
                    if arg.mode != CallArgMode::Move
                        && arg.mode != CallArgMode::Out
                        && array_to_slice_assignable(&arg_ty, param_ty)
                    {
                        continue;
                    }
                    let span = arg.span;
                    return Err(TypeCheckErrorKind::ArgTypeMismatch(
                        i + 1,
                        param_ty.clone(),
                        arg_ty.clone(),
                        span,
                    )
                    .into());
                }
            }
        }
        Ok(())
    }

    fn check_call(
        &mut self,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        if let ExprKind::Var { def_id, .. } = &callee.kind
            && let Some(def) = self.ctx.def_table.lookup_def(*def_id)
            && matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. })
        {
            let name = def.name.clone();
            return self.check_named_call(&name, call_expr, callee, args, expected);
        }

        self.check_expr_call(call_expr, callee, args)
    }

    fn check_expr_call(
        &mut self,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.visit_expr(callee, None)?;
        let Type::Fn { params, ret_ty } = callee_ty else {
            return Err(TypeCheckErrorKind::InvalidCallee(callee.kind.clone(), callee.span).into());
        };

        if params.len() != args.len() {
            return Err(TypeCheckErrorKind::ArgCountMismatch(
                "<fn>".to_string(),
                params.len(),
                args.len(),
                call_expr.span,
            )
            .into());
        }

        let param_types = params
            .iter()
            .map(|param| param.ty.clone())
            .collect::<Vec<_>>();
        let params = params
            .iter()
            .map(|param| CallParam {
                mode: match param.mode {
                    FnParamMode::In => ParamMode::In,
                    FnParamMode::InOut => ParamMode::InOut,
                    FnParamMode::Out => ParamMode::Out,
                    FnParamMode::Sink => ParamMode::Sink,
                },
                ty: param.ty.clone(),
            })
            .collect::<Vec<_>>();

        self.type_map_builder.record_call_sig(
            call_expr.id,
            CallSig {
                def_id: None,
                receiver: None,
                params,
            },
        );

        self.check_call_arg_types(args, &param_types)?;

        Ok(*ret_ty)
    }

    fn check_named_call(
        &mut self,
        name: &str,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        // Get the function overloads
        let Some(overloads) = self.func_sigs.get(name).cloned() else {
            return Err(TypeCheckErrorKind::UnknownType(callee.span).into());
        };

        self.check_named_call_common(name, callee, call_expr, args, &overloads, false, expected)
    }

    fn check_method_call(
        &mut self,
        method_name: &str,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.visit_expr(callee, None)?;
        let peeled_ty = self.expand_shallow_type(&callee_ty.peel_heap());

        if method_name == "len" {
            if !args.is_empty() {
                return Err(TypeCheckErrorKind::ArgCountMismatch(
                    "len".to_string(),
                    0,
                    args.len(),
                    call_expr.span,
                )
                .into());
            }
            if !self.is_place_expr(callee) {
                return Err(TypeCheckErrorKind::LenTargetNotLvalue(callee.span).into());
            }
            return match peeled_ty {
                Type::Array { .. } | Type::Slice { .. } | Type::String => Ok(Type::uint(64)),
                _ => {
                    Err(TypeCheckErrorKind::InvalidStructFieldTarget(callee_ty, callee.span).into())
                }
            };
        }

        // Check that the callee type is a struct or enum
        let type_name = match peeled_ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name,
            Type::String => "string".to_string(),
            _ => {
                return Err(
                    TypeCheckErrorKind::InvalidStructFieldTarget(callee_ty, callee.span).into(),
                );
            }
        };

        // Get a map of method name to overloads
        let Some(type_methods) = self.method_sigs.get(&type_name) else {
            return Err(TypeCheckErrorKind::UnknownType(call_expr.span).into());
        };

        // Get the overloads for the method
        let Some(overloads) = type_methods.get(method_name).cloned() else {
            return Err(TypeCheckErrorKind::UnknownType(call_expr.span).into());
        };

        // Format the method name as "type::method"
        let name = format!("{}::{}", type_name, method_name);

        self.check_named_call_common(&name, callee, call_expr, args, &overloads, true, expected)
    }

    fn is_place_expr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Var { .. }
            | ExprKind::Deref { .. }
            | ExprKind::ArrayIndex { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. } => true,
            ExprKind::Move { expr } => self.is_place_expr(expr),
            _ => false,
        }
    }

    fn lookup_method_self_mode(&self, def_id: DefId) -> ParamMode {
        for block in self.ctx.module.method_blocks() {
            for method_item in &block.method_items {
                match method_item {
                    MethodItem::Decl(method_decl) => {
                        if method_decl.def_id == def_id {
                            return method_decl.sig.self_param.mode.clone();
                        }
                    }
                    MethodItem::Def(method_def) => {
                        if method_def.def_id == def_id {
                            return method_def.sig.self_param.mode.clone();
                        }
                    }
                }
            }
        }
        panic!("compiler bug: method self mode not found for def {def_id}");
    }

    fn check_named_call_common(
        &mut self,
        name: &str,
        callee: &Expr,
        call_expr: &Expr,
        args: &[CallArg],
        overloads: &[OverloadSig],
        is_method: bool,
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.visit_expr(callee, None)?;
        let arg_types = self.visit_call_args(args)?;

        let (resolved, fallback_param_types) = {
            let fallback_param_types = Self::single_arity_param_types(overloads, arg_types.len());
            // If no overload matches the arity and all overloads share the same arity,
            // report a count mismatch instead of a generic overload error.
            if !overloads
                .iter()
                .any(|sig| sig.params.len() == arg_types.len())
            {
                let mut counts = HashSet::new();
                for sig in overloads {
                    counts.insert(sig.params.len());
                }
                if counts.len() == 1 {
                    let expected = *counts.iter().next().unwrap();
                    return Err(TypeCheckErrorKind::ArgCountMismatch(
                        name.to_string(),
                        expected,
                        arg_types.len(),
                        call_expr.span,
                    )
                    .into());
                }
            }

            let mut generic_overloads = Vec::new();
            let mut concrete_overloads = Vec::new();
            for sig in overloads {
                if Self::sig_has_type_vars(sig) {
                    generic_overloads.push(sig);
                } else {
                    concrete_overloads.push(sig);
                }
            }

            let resolved = if !concrete_overloads.is_empty() {
                let concrete = concrete_overloads.into_iter().cloned().collect::<Vec<_>>();
                match OverloadResolver::new(name, args, &arg_types, call_expr.span)
                    .resolve(&concrete)
                {
                    Ok(resolved) => Ok((
                        resolved.def_id,
                        resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.ty.clone())
                            .collect::<Vec<_>>(),
                        resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.mode.clone())
                            .collect::<Vec<_>>(),
                        resolved.sig.ret_ty.clone(),
                    )),
                    Err(err) => {
                        if matches!(err.kind(), TypeCheckErrorKind::OverloadNoMatch(_, _))
                            && !generic_overloads.is_empty()
                        {
                            let inst = Self::resolve_generic_overload(
                                name,
                                args,
                                &arg_types,
                                &generic_overloads,
                                expected,
                                call_expr.span,
                            )?;
                            let sig = generic_overloads
                                .iter()
                                .find(|sig| sig.def_id == inst.def_id)
                                .ok_or_else(|| {
                                    TypeCheckError::from(TypeCheckErrorKind::OverloadNoMatch(
                                        name.to_string(),
                                        call_expr.span,
                                    ))
                                })?;
                            let (param_types, param_modes, ret_type) =
                                Self::apply_inst_to_sig(sig, &inst);
                            self.type_map_builder
                                .record_generic_inst(call_expr.id, inst);
                            Ok((sig.def_id, param_types, param_modes, ret_type))
                        } else {
                            Err(err)
                        }
                    }
                }
            } else if !generic_overloads.is_empty() {
                let inst = Self::resolve_generic_overload(
                    name,
                    args,
                    &arg_types,
                    &generic_overloads,
                    expected,
                    call_expr.span,
                )?;
                let sig = generic_overloads
                    .iter()
                    .find(|sig| sig.def_id == inst.def_id)
                    .ok_or_else(|| {
                        TypeCheckError::from(TypeCheckErrorKind::OverloadNoMatch(
                            name.to_string(),
                            call_expr.span,
                        ))
                    })?;
                let (param_types, param_modes, ret_type) = Self::apply_inst_to_sig(sig, &inst);
                self.type_map_builder
                    .record_generic_inst(call_expr.id, inst);
                Ok((sig.def_id, param_types, param_modes, ret_type))
            } else {
                OverloadResolver::new(name, args, &arg_types, call_expr.span)
                    .resolve(overloads)
                    .map(|resolved| {
                        let param_types = resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.ty.clone())
                            .collect::<Vec<_>>();
                        let param_modes = resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.mode.clone())
                            .collect::<Vec<_>>();
                        let ret_type = resolved.sig.ret_ty.clone();
                        (resolved.def_id, param_types, param_modes, ret_type)
                    })
            };
            (resolved, fallback_param_types)
        };

        let (def_id, param_types, param_modes, ret_type) = match resolved {
            Ok(resolved) => resolved,
            Err(err) => {
                if matches!(err.kind(), TypeCheckErrorKind::OverloadNoMatch(_, _))
                    && let Some(param_types) = fallback_param_types
                {
                    self.check_call_arg_types(args, &param_types)?
                }
                return Err(err);
            }
        };

        let mut receiver = None;
        if is_method {
            let self_mode = self.lookup_method_self_mode(def_id);
            receiver = Some(CallParam {
                mode: self_mode,
                ty: callee_ty.clone(),
            });
        }

        let params = param_modes
            .iter()
            .cloned()
            .zip(param_types.iter().cloned())
            .map(|(mode, ty)| CallParam { mode, ty })
            .collect::<Vec<_>>();

        self.type_map_builder.record_call_sig(
            call_expr.id,
            CallSig {
                def_id: Some(def_id),
                receiver,
                params,
            },
        );

        self.check_call_arg_types(args, &param_types)?;

        Ok(ret_type)
    }

    fn resolve_generic_overload(
        name: &str,
        args: &[CallArg],
        arg_types: &[Type],
        overloads: &[&OverloadSig],
        expected: Option<&Type>,
        call_span: Span,
    ) -> Result<GenericInst, TypeCheckError> {
        let mut candidates = Vec::new();
        let mut range_err: Option<TypeCheckError> = None;

        let mut unifier_for_sig = |sig: &OverloadSig| -> Result<Option<Unifier>, TypeCheckError> {
            let mut unifier = Unifier::new();
            for ((arg, arg_ty), param) in args.iter().zip(arg_types).zip(sig.params.iter()) {
                let param_ty = &param.ty;
                let mut arg_ty_for_unify = arg_ty.clone();

                if arg.mode != CallArgMode::Move
                    && arg.mode != CallArgMode::Out
                    && matches!(param_ty, Type::Slice { .. })
                    && let Some(item) = arg_ty.array_item_type()
                {
                    arg_ty_for_unify = Type::Slice {
                        elem_ty: Box::new(item),
                    };
                }

                if Self::type_has_vars(param_ty) {
                    if unifier.unify(param_ty, &arg_ty_for_unify).is_err() {
                        return Ok(None);
                    }
                    continue;
                }

                match value_assignable(&arg.expr, arg_ty, param_ty) {
                    ValueAssignability::Assignable(assignability) => {
                        if matches!(assignability, TypeAssignability::Incompatible) {
                            return Ok(None);
                        }
                    }
                    ValueAssignability::ValueOutOfRange { value, min, max } => {
                        range_err.get_or_insert(
                            TypeCheckErrorKind::ValueOutOfRange(value, min, max, arg.span).into(),
                        );
                        return Ok(None);
                    }
                    ValueAssignability::ValueNotNonZero { value } => {
                        return Err(TypeCheckErrorKind::ValueNotNonZero(value, arg.span).into());
                    }
                    ValueAssignability::Incompatible => {
                        if arg.mode != CallArgMode::Move
                            && arg.mode != CallArgMode::Out
                            && array_to_slice_assignable(arg_ty, param_ty)
                        {
                            continue;
                        }
                        return Ok(None);
                    }
                }
            }
            if let Some(expected_ty) = expected {
                if Self::type_has_vars(&sig.ret_ty) {
                    if unifier.unify(&sig.ret_ty, expected_ty).is_err() {
                        return Ok(None);
                    }
                }
            }
            Ok(Some(unifier))
        };

        for sig in overloads {
            if sig.params.len() != arg_types.len() {
                continue;
            }

            let Some(unifier) = unifier_for_sig(sig)? else {
                continue;
            };

            let type_args = (0..sig.type_param_count)
                .map(|index| unifier.apply(&Type::Var(TyVarId::new(index as u32))))
                .collect::<Vec<_>>();

            if type_args.iter().any(|ty| matches!(ty, Type::Var(_))) {
                continue;
            }

            candidates.push(GenericInst {
                def_id: sig.def_id,
                type_args,
                call_span,
            });
        }

        if candidates.is_empty() {
            return Err(range_err.unwrap_or_else(|| {
                TypeCheckErrorKind::OverloadNoMatch(name.to_string(), call_span).into()
            }));
        }

        if candidates.len() != 1 {
            return Err(TypeCheckErrorKind::OverloadAmbiguous(name.to_string(), call_span).into());
        }

        Ok(candidates.pop().unwrap())
    }

    fn sig_has_type_vars(sig: &OverloadSig) -> bool {
        sig.type_param_count > 0
            || sig
                .params
                .iter()
                .any(|param| Self::type_has_vars(&param.ty))
            || Self::type_has_vars(&sig.ret_ty)
    }

    fn apply_inst_to_sig(
        sig: &OverloadSig,
        inst: &GenericInst,
    ) -> (Vec<Type>, Vec<ParamMode>, Type) {
        let mut unifier = Unifier::new();
        for (index, ty) in inst.type_args.iter().enumerate() {
            let var = TyVarId::new(index as u32);
            {
                let _ = unifier.unify(&Type::Var(var), ty);
            }
        }

        let param_types = sig
            .params
            .iter()
            .map(|param| unifier.apply(&param.ty))
            .collect::<Vec<_>>();
        let param_modes = sig
            .params
            .iter()
            .map(|param| param.mode.clone())
            .collect::<Vec<_>>();
        let ret_type = unifier.apply(&sig.ret_ty);
        (param_types, param_modes, ret_type)
    }

    fn type_has_vars(ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::Array { elem_ty, .. }
            | Type::Slice { elem_ty }
            | Type::Heap { elem_ty }
            | Type::Ref { elem_ty, .. }
            | Type::Range { elem_ty } => Self::type_has_vars(elem_ty),
            Type::Tuple { field_tys } => field_tys.iter().any(Self::type_has_vars),
            Type::Struct { fields, .. } => {
                fields.iter().any(|field| Self::type_has_vars(&field.ty))
            }
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|variant| variant.payload.iter().any(Self::type_has_vars)),
            Type::Fn { params, ret_ty } => {
                params.iter().any(|param| Self::type_has_vars(&param.ty))
                    || Self::type_has_vars(ret_ty)
            }
            _ => false,
        }
    }

    fn check_while(&mut self, cond: &Expr, body: &Expr) -> Result<Type, TypeCheckError> {
        let cond_type = self.visit_expr(cond, None)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckErrorKind::CondNotBoolean(cond_type, cond.span).into());
        }

        self.enter_loop();
        let result = self.visit_expr(body, None);
        self.exit_loop();
        let _ = result?;
        Ok(Type::Unit)
    }

    fn check_for(
        &mut self,
        pattern: &BindPattern,
        iter: &Expr,
        body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let iter_ty = self.visit_expr(iter, None)?;
        let item_ty = self.iterable_item_type(&iter_ty, iter.span)?;

        // Loop variable's type is the item type of the iterable
        self.check_bind_pattern(pattern, &item_ty)?;

        // Type check body
        self.enter_loop();
        let result = self.visit_expr(body, None);
        self.exit_loop();
        let _ = result?;

        Ok(Type::Unit)
    }

    fn iterable_item_type(&self, iter_ty: &Type, span: Span) -> Result<Type, TypeCheckError> {
        match iter_ty {
            Type::Range { elem_ty } => Ok((**elem_ty).clone()),
            Type::Array { dims, .. } => {
                if dims.is_empty() {
                    return Err(
                        TypeCheckErrorKind::ForIterNotIterable(iter_ty.clone(), span).into(),
                    );
                }
                Ok(iter_ty
                    .array_item_type()
                    .unwrap_or_else(|| panic!("compiler bug: empty array dims")))
            }
            Type::Slice { elem_ty } => Ok((**elem_ty).clone()),
            _ => Err(TypeCheckErrorKind::ForIterNotIterable(iter_ty.clone(), span).into()),
        }
    }

    fn check_match(&mut self, scrutinee: &Expr, arms: &[MatchArm]) -> Result<Type, TypeCheckError> {
        let scrutinee_ty = self.visit_expr(scrutinee, None)?;
        let peeled_ty = self.expand_shallow_type(&scrutinee_ty.peel_heap());
        let mut arm_ty: Option<Type> = None;

        match &peeled_ty {
            Type::Enum { name, variants } => {
                let enum_name = name.clone();
                self.visit_match_arms(arms, |this, arm| {
                    this.check_enum_match_pattern(&enum_name, variants, &arm.pattern)?;
                    this.check_match_arm_body(arm, &mut arm_ty)
                })?;
            }
            Type::Tuple { field_tys } => {
                self.visit_match_arms(arms, |this, arm| {
                    this.check_tuple_match_pattern(field_tys, &arm.pattern);
                    this.check_match_arm_body(arm, &mut arm_ty)
                })?;
            }
            _ => {
                self.visit_match_arms(arms, |this, arm| {
                    this.check_match_arm_body(arm, &mut arm_ty)
                })?;
            }
        }

        Ok(arm_ty.unwrap_or(Type::Unit))
    }

    fn check_match_arm_body(
        &mut self,
        arm: &MatchArm,
        arm_ty: &mut Option<Type>,
    ) -> Result<(), TypeCheckError> {
        let body_ty = self.visit_match_arm(arm)?;
        if let Some(expected_ty) = arm_ty {
            if body_ty != *expected_ty {
                return Err(TypeCheckErrorKind::MatchArmTypeMismatch(
                    expected_ty.clone(),
                    body_ty,
                    arm.span,
                )
                .into());
            }
        } else {
            *arm_ty = Some(body_ty);
        }

        Ok(())
    }

    fn check_enum_match_pattern(
        &mut self,
        enum_name: &String,
        variants: &[EnumVariant],
        pattern: &MatchPattern,
    ) -> Result<(), TypeCheckError> {
        match pattern {
            MatchPattern::EnumVariant {
                enum_name: pat_enum_name,
                variant_name,
                bindings,
                ..
            } => {
                if let Some(pat_enum_name) = pat_enum_name
                    && pat_enum_name != enum_name
                {
                    return Ok(());
                }

                if let Some(variant) = variants.iter().find(|v| v.name == *variant_name) {
                    if bindings.len() == variant.payload.len() {
                        for (binding, ty) in bindings.iter().zip(variant.payload.iter()) {
                            self.record_match_binding(binding, ty);
                        }
                    } else {
                        for binding in bindings {
                            self.record_match_binding(binding, &Type::Unknown);
                        }
                    }
                } else {
                    for binding in bindings {
                        self.record_match_binding(binding, &Type::Unknown);
                    }
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn record_match_binding(&mut self, binding: &MatchPatternBinding, ty: &Type) {
        let MatchPatternBinding::Named { def_id, .. } = binding else {
            return;
        };
        self.record_binding_def(*def_id, ty);
    }

    fn record_binding_def(&mut self, def_id: DefId, ty: &Type) {
        match self.ctx.def_table.lookup_def(def_id) {
            Some(def) => {
                self.type_map_builder
                    .record_def_type(def.clone(), ty.clone());
            }
            None => panic!(
                "compiler bug: binding def [{}] not found in def_table",
                def_id
            ),
        }
    }

    fn check_tuple_match_pattern(&mut self, fields: &[Type], pattern: &MatchPattern) {
        let MatchPattern::Tuple { patterns, .. } = pattern else {
            return;
        };

        self.record_tuple_pattern_bindings(fields, patterns);
    }

    fn record_tuple_pattern_bindings(&mut self, fields: &[Type], patterns: &[MatchPattern]) {
        if patterns.len() == fields.len() {
            for (pattern, ty) in patterns.iter().zip(fields.iter()) {
                self.record_pattern_bindings(pattern, Some(ty));
            }
        } else {
            for pattern in patterns {
                self.record_pattern_bindings(pattern, None);
            }
        }
    }

    fn record_pattern_bindings(&mut self, pattern: &MatchPattern, ty: Option<&Type>) {
        match pattern {
            MatchPattern::Binding { def_id, .. } => {
                self.record_binding_def(*def_id, ty.unwrap_or(&Type::Unknown));
            }
            MatchPattern::EnumVariant { bindings, .. } => match ty {
                Some(Type::Enum { name, variants }) => {
                    let _ = self.check_enum_match_pattern(name, variants, pattern);
                }
                _ => {
                    for binding in bindings {
                        self.record_match_binding(binding, &Type::Unknown);
                    }
                }
            },
            MatchPattern::Tuple { patterns, .. } => {
                let fields = match ty {
                    Some(Type::Tuple { field_tys }) => Some(field_tys.as_slice()),
                    _ => None,
                };
                if let Some(fields) = fields {
                    self.record_tuple_pattern_bindings(fields, patterns);
                } else {
                    for pattern in patterns {
                        self.record_pattern_bindings(pattern, None);
                    }
                }
            }
            _ => {}
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
                        let _ = self.visit_expr(value, None)?;
                    }
                    return Err(TypeCheckErrorKind::ReturnOutsideFunction(stmt.span).into());
                };

                match value {
                    Some(value) => {
                        if return_ty == Type::Unit {
                            let _ = self.visit_expr(value, None)?;
                            return Err(
                                TypeCheckErrorKind::ReturnValueUnexpected(value.span).into()
                            );
                        }

                        let value_ty = self.visit_expr(value, Some(&return_ty))?;
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
        let result = match (&expr.kind, expected) {
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
                let _ = self.visit_expr(operand, Some(expected_ty))?;
                Ok(expected_ty.clone())
            }

            _ => match &expr.kind {
                ExprKind::IntLit(_) => Ok(Type::uint(64)),

                ExprKind::BoolLit(_) => Ok(Type::Bool),

                ExprKind::CharLit(_) => Ok(Type::Char),

                ExprKind::StringLit { .. } => Ok(Type::String),

                ExprKind::UnitLit => Ok(Type::Unit),

                ExprKind::HeapAlloc { expr } => {
                    let inner_expected = match expected {
                        Some(Type::Heap { elem_ty }) => Some(elem_ty.as_ref()),
                        _ => None,
                    };
                    let elem_ty = self.visit_expr(expr, inner_expected)?;
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
                    self.check_array_lit(elem_ty.as_ref(), init, expected, expr.span)
                }

                ExprKind::ArrayIndex { target, indices } => {
                    let target_ty = self.visit_expr(target, None)?;
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

                ExprKind::StructLit { name, fields } => self.check_struct_lit(name, fields),

                ExprKind::StructField { target, field } => {
                    self.check_field_access(expr.id, target, field)
                }

                ExprKind::StructUpdate { target, fields } => {
                    self.check_struct_update(target, fields)
                }

                ExprKind::EnumVariant {
                    enum_name,
                    variant,
                    payload,
                } => self.check_enum_variant(enum_name, variant, payload),

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

                    let tail_ty = self.visit_block_tail(tail.as_deref(), expected)?;

                    Ok(tail_ty.unwrap_or(Type::Unit))
                }

                ExprKind::Var { def_id, .. } => self.check_var_ref(*def_id, expr.span),

                ExprKind::Call { callee, args } => self.check_call(expr, callee, args, expected),

                ExprKind::MethodCall {
                    callee,
                    method_name,
                    args,
                } => self.check_method_call(method_name, expr, callee, args, expected),

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
                    let start_ty = self.visit_expr(start, None)?;
                    if start_ty != Type::uint(64) {
                        return Err(
                            TypeCheckErrorKind::IndexTypeNotInt(start_ty, start.span).into()
                        );
                    }
                    let end_ty = self.visit_expr(end, None)?;
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
            self.type_map_builder.record_node_type(expr.id, ty.clone());
            ty.clone()
        })
    }
}
