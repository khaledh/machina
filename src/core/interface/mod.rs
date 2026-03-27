//! In-memory module interface model.
//!
//! Phase 2 starts by defining the semantic contract for compiled module
//! interfaces before choosing an on-disk encoding. The types in this module are
//! the compiler-owned shape that later `.mci` readers/writers will encode.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::core::ast::{
    BindPattern, BindPatternKind, BlockItem, CallArgMode, DocComment, Expr, ExprKind, MethodItem,
    MethodSig, Param, SelfParam, StmtExpr, StmtExprKind, TopLevelItem, TraitDef, TraitProperty,
    TypeDef, TypeDefKind,
};
use crate::core::capsule::ModulePath;
use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::resolve::{Def, DefId};
use crate::core::symbol_id::{SymbolId, TypeKey, type_key_for_type_expr};

mod json_codec;
mod stdlib;
mod template;

pub use json_codec::JsonModuleInterfaceCodec;
pub use stdlib::{
    StdlibInterfaceError, ensure_stdlib_module_interface_with_codec,
    load_stdlib_module_interface_with_codec,
};
pub use template::{
    GenericClosureTemplate, GenericFunctionTemplate, GenericMethodTemplate, GenericTemplateGraph,
    GenericTraitTemplate, GenericTypeTemplate, LinkTimeCallableTemplate, TemplateBinding,
    TemplateBindingId, TemplateBindingKind, TemplateBody, TemplateCallArg, TemplateCallArgMode,
    TemplateCallSite, TemplateCallTarget, TemplateDef, TemplateDefId, TemplateDefKind,
    TemplateExpr, TemplateIterableParamSlot, TemplateNestedClosure, TemplateReference,
    TemplateReferenceKind, TemplateReferenceTarget, TemplateSiteId, TemplateStmt,
    TemplateStructField, TemplateTypeParam, TemplateTypeParamId, TemplateTypeSite,
    TemplateTypeSiteRole,
};

pub const MODULE_INTERFACE_FORMAT_VERSION: u32 = 2;

struct SourceToolingContext {
    path: PathBuf,
    source: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModuleInterface {
    #[serde(default)]
    pub format_version: u32,
    pub module_path: ModulePath,
    pub compiler_version: String,
    pub exports: Vec<ExportedDef>,
    pub closure_defs: Vec<ClosureDef>,
    pub tooling: Option<ModuleToolingMetadata>,
}

impl ModuleInterface {
    pub fn from_resolved_context(context: &ResolvedContext) -> Option<Self> {
        Self::from_module_and_resolved(&context.module, context)
    }

    pub fn from_module_and_resolved(
        module: &crate::core::ast::Module,
        context: &ResolvedContext,
    ) -> Option<Self> {
        let module_path = context.module_path.clone()?;
        let tooling_source = load_source_tooling_context(&context.def_table);
        let mut exports = Vec::new();

        for item in &module.top_level_items {
            match item {
                TopLevelItem::FuncDecl(func_decl) => push_callable_export(
                    &mut exports,
                    &module_path,
                    context,
                    func_decl.id,
                    func_decl.doc.as_ref(),
                    CallableOwner::Function,
                    &func_decl.sig.type_params,
                    &func_decl.sig.params,
                    None,
                    &func_decl.sig.ret_ty_expr,
                    None,
                    tooling_source.as_ref(),
                ),
                TopLevelItem::FuncDef(func_def) => push_callable_export(
                    &mut exports,
                    &module_path,
                    context,
                    func_def.id,
                    func_def.doc.as_ref(),
                    CallableOwner::Function,
                    &func_def.sig.type_params,
                    &func_def.sig.params,
                    None,
                    &func_def.sig.ret_ty_expr,
                    Some(&func_def.body),
                    tooling_source.as_ref(),
                ),
                TopLevelItem::MethodBlock(block) => {
                    for method in &block.method_items {
                        match method {
                            MethodItem::Decl(method_decl) => push_callable_export(
                                &mut exports,
                                &module_path,
                                context,
                                method_decl.id,
                                method_decl.doc.as_ref(),
                                CallableOwner::Method {
                                    owner_type: block.type_name.clone(),
                                    owner_type_args: !block.type_args.is_empty(),
                                },
                                &method_decl.sig.type_params,
                                &method_decl.sig.params,
                                Some(&method_decl.sig.self_param),
                                &method_decl.sig.ret_ty_expr,
                                None,
                                tooling_source.as_ref(),
                            ),
                            MethodItem::Def(method_def) => push_callable_export(
                                &mut exports,
                                &module_path,
                                context,
                                method_def.id,
                                method_def.doc.as_ref(),
                                CallableOwner::Method {
                                    owner_type: block.type_name.clone(),
                                    owner_type_args: !block.type_args.is_empty(),
                                },
                                &method_def.sig.type_params,
                                &method_def.sig.params,
                                Some(&method_def.sig.self_param),
                                &method_def.sig.ret_ty_expr,
                                Some(&method_def.body),
                                tooling_source.as_ref(),
                            ),
                        }
                    }
                }
                TopLevelItem::TypeDef(type_def) => {
                    if let Some(export) = type_export_from_def(
                        module_path.clone(),
                        context,
                        type_def,
                        type_def.doc.as_ref(),
                        tooling_source.as_ref(),
                    ) {
                        exports.push(export);
                    }
                }
                TopLevelItem::TraitDef(trait_def) => {
                    if let Some(export) = trait_export_from_def(
                        module_path.clone(),
                        context,
                        trait_def,
                        trait_def.doc.as_ref(),
                        tooling_source.as_ref(),
                    ) {
                        exports.push(export);
                    }
                }
                _ => {}
            }
        }

        exports.sort_by(|left, right| left.symbol_id.to_string().cmp(&right.symbol_id.to_string()));

        Some(Self {
            format_version: MODULE_INTERFACE_FORMAT_VERSION,
            module_path,
            compiler_version: env!("CARGO_PKG_VERSION").to_string(),
            exports,
            closure_defs: Vec::new(),
            tooling: context
                .def_table
                .source_path()
                .map(|path| ModuleToolingMetadata {
                    source_file: Some(path.to_path_buf()),
                }),
        })
    }

    pub fn has_current_format_version(&self) -> bool {
        self.format_version == MODULE_INTERFACE_FORMAT_VERSION
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ModuleToolingMetadata {
    pub source_file: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExportedDef {
    pub symbol_id: SymbolId,
    pub visibility: ExportVisibility,
    pub kind: ExportedDefKind,
    pub tooling: Option<ExportToolingMetadata>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExportVisibility {
    Public,
    Opaque,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExportedDefKind {
    Func(CallableExport),
    Method(MethodExport),
    Type(TypeDefExport),
    Trait(TraitDefExport),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CallableExport {
    pub signature: CallableSignature,
    pub implementation: CallableImplementation,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MethodExport {
    pub owner_type: String,
    pub signature: MethodSignature,
    pub implementation: CallableImplementation,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CallableImplementation {
    LinkSymbol(String),
    GenericTemplate(GenericTemplateGraph),
    GenericBodyPending,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CallableSignature {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<InterfaceParam>,
    pub ret_ty: TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MethodSignature {
    pub name: String,
    pub type_params: Vec<String>,
    pub self_param: InterfaceSelfParam,
    pub params: Vec<InterfaceParam>,
    pub ret_ty: TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InterfaceSelfParam {
    pub mode: crate::core::ast::ParamMode,
    pub receiver_ty: Option<TypeKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InterfaceParam {
    pub name: String,
    pub mode: crate::core::ast::ParamMode,
    pub ty: TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeDefExport {
    pub name: String,
    pub type_params: Vec<String>,
    pub kind: TypeDefExportKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeDefExportKind {
    Alias {
        aliased_ty: TypeKey,
    },
    Struct {
        fields: Vec<NamedField>,
    },
    Enum {
        variants: Vec<EnumVariantExport>,
    },
    Linear {
        fields: Vec<NamedField>,
        states: Vec<LinearStateExport>,
        actions: Vec<LinearTransitionExport>,
        triggers: Vec<LinearTransitionExport>,
        roles: Vec<LinearRoleExport>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NamedField {
    pub name: String,
    pub ty: TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumVariantExport {
    pub name: String,
    pub payload: Vec<TypeKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinearStateExport {
    pub name: String,
    pub payload: Vec<TypeKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinearTransitionExport {
    pub name: String,
    pub params: Vec<InterfaceParam>,
    pub source_state: String,
    pub target_state: String,
    pub error_ty: Option<TypeKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinearRoleExport {
    pub name: String,
    pub allowed_actions: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraitDefExport {
    pub name: String,
    pub methods: Vec<TraitMethodExport>,
    pub properties: Vec<TraitPropertyExport>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraitMethodExport {
    pub signature: MethodSignature,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraitPropertyExport {
    pub name: String,
    pub ty: TypeKey,
    pub has_get: bool,
    pub has_set: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ClosureDef {
    pub symbol_id: SymbolId,
    pub dependency_kind: ClosureDependencyKind,
    pub kind: ClosureDefKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ClosureDependencyKind {
    CompileTime,
    LinkTime,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ClosureDefKind {
    Func(CallableExport),
    Method(MethodExport),
    Type(TypeDefExport),
    Trait(TraitDefExport),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExportToolingMetadata {
    pub doc: Option<String>,
    pub source_location: Option<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: PathBuf,
    pub name_span: Span,
    pub decl_span: Span,
}

pub trait ModuleInterfaceCodec {
    type Error;

    fn encode(interface: &ModuleInterface) -> Result<Vec<u8>, Self::Error>;
    fn decode(bytes: &[u8]) -> Result<ModuleInterface, Self::Error>;
}

#[derive(Debug, thiserror::Error)]
pub enum ModuleInterfaceIoError<E> {
    #[error("module interface codec error: {0}")]
    Codec(E),
    #[error("module interface io error: {0}")]
    Io(#[from] std::io::Error),
}

pub fn write_module_interface_with_codec<C: ModuleInterfaceCodec>(
    interface: &ModuleInterface,
    path: &Path,
) -> Result<(), ModuleInterfaceIoError<C::Error>> {
    let bytes = C::encode(interface).map_err(ModuleInterfaceIoError::Codec)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, bytes)?;
    Ok(())
}

pub fn read_module_interface_with_codec<C: ModuleInterfaceCodec>(
    path: &Path,
) -> Result<ModuleInterface, ModuleInterfaceIoError<C::Error>> {
    let bytes = std::fs::read(path)?;
    C::decode(&bytes).map_err(ModuleInterfaceIoError::Codec)
}

pub fn emit_module_interface_with_codec<C: ModuleInterfaceCodec>(
    artifact_root: &Path,
    context: &ResolvedContext,
) -> Result<PathBuf, ModuleInterfaceIoError<C::Error>> {
    emit_module_interface_from_module_with_codec::<C>(artifact_root, &context.module, context)
}

pub fn emit_module_interface_from_module_with_codec<C: ModuleInterfaceCodec>(
    artifact_root: &Path,
    module: &crate::core::ast::Module,
    context: &ResolvedContext,
) -> Result<PathBuf, ModuleInterfaceIoError<C::Error>> {
    let interface =
        ModuleInterface::from_module_and_resolved(module, context).ok_or_else(|| {
            ModuleInterfaceIoError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "resolved context is missing module_path for interface emission",
            ))
        })?;
    let paths = ModuleArtifactPaths::for_module(artifact_root, &interface.module_path);
    write_module_interface_with_codec::<C>(&interface, &paths.interface_path)?;
    Ok(paths.interface_path)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleArtifactPaths {
    pub interface_path: PathBuf,
    pub object_path: PathBuf,
}

impl ModuleArtifactPaths {
    pub fn for_module(root: &Path, module_path: &ModulePath) -> Self {
        Self {
            interface_path: root.join(interface_rel_path(module_path)),
            object_path: root.join(object_rel_path(module_path)),
        }
    }
}

pub fn interface_rel_path(module_path: &ModulePath) -> PathBuf {
    module_rel_path_with_extension(module_path, "mci")
}

pub fn object_rel_path(module_path: &ModulePath) -> PathBuf {
    module_rel_path_with_extension(module_path, "o")
}

fn module_rel_path_with_extension(module_path: &ModulePath, extension: &str) -> PathBuf {
    let mut path = PathBuf::new();
    let mut segments = module_path.segments().iter();
    if let Some(first) = segments.next() {
        path.push(first);
    }
    for segment in segments {
        path.push(segment);
    }
    path.set_extension(extension);
    path
}

enum CallableOwner {
    Function,
    Method {
        owner_type: String,
        owner_type_args: bool,
    },
}

fn push_callable_export(
    exports: &mut Vec<ExportedDef>,
    module_path: &ModulePath,
    context: &ResolvedContext,
    node_id: crate::core::ast::NodeId,
    doc: Option<&DocComment>,
    owner: CallableOwner,
    type_params: &[crate::core::ast::TypeParam],
    params: &[Param],
    self_param: Option<&SelfParam>,
    ret_ty: &crate::core::ast::TypeExpr,
    body: Option<&Expr>,
    tooling_source: Option<&SourceToolingContext>,
) {
    let Some(def_id) = context.def_table.lookup_node_def_id(node_id) else {
        return;
    };
    let Some(def) = context.def_table.lookup_def(def_id) else {
        return;
    };
    if !def.is_public() {
        return;
    }
    let Some(symbol_id) = context.symbol_ids.lookup_symbol_id(def_id).cloned() else {
        return;
    };
    let type_param_names = type_params
        .iter()
        .map(|param| param.ident.clone())
        .collect::<Vec<_>>();
    let interface_params = params
        .iter()
        .map(|param| InterfaceParam {
            name: param.ident.clone(),
            mode: param.mode.clone(),
            ty: type_key_for_type_expr(&param.typ, type_params, &context.def_table, module_path),
        })
        .collect::<Vec<_>>();
    let visibility = if def.is_opaque() {
        ExportVisibility::Opaque
    } else {
        ExportVisibility::Public
    };

    let kind = match owner {
        CallableOwner::Function => {
            let signature = CallableSignature {
                name: def.name.clone(),
                type_params: type_param_names,
                params: interface_params,
                ret_ty: type_key_for_type_expr(
                    ret_ty,
                    type_params,
                    &context.def_table,
                    module_path,
                ),
            };
            let implementation = callable_implementation(
                def,
                &context.symbols.def_names,
                &context.def_table,
                &context.symbol_ids,
                module_path,
                &owner,
                type_params,
                params,
                &signature,
                body,
            );
            ExportedDefKind::Func(CallableExport {
                signature,
                implementation,
            })
        }
        CallableOwner::Method { ref owner_type, .. } => ExportedDefKind::Method(MethodExport {
            owner_type: owner_type.clone(),
            signature: MethodSignature {
                name: def.name.clone(),
                type_params: type_param_names,
                self_param: InterfaceSelfParam {
                    mode: self_param
                        .map(|param| param.mode.clone())
                        .expect("method exports always have a self param"),
                    receiver_ty: self_param.and_then(|param| {
                        param.receiver_ty_expr.as_ref().map(|expr| {
                            type_key_for_type_expr(
                                expr,
                                type_params,
                                &context.def_table,
                                module_path,
                            )
                        })
                    }),
                },
                params: interface_params,
                ret_ty: type_key_for_type_expr(
                    ret_ty,
                    type_params,
                    &context.def_table,
                    module_path,
                ),
            },
            implementation: callable_implementation(
                def,
                &context.symbols.def_names,
                &context.def_table,
                &context.symbol_ids,
                module_path,
                &owner,
                type_params,
                params,
                &CallableSignature {
                    name: def.name.clone(),
                    type_params: Vec::new(),
                    params: Vec::new(),
                    ret_ty: TypeKey::Unit,
                },
                body,
            ),
        }),
    };

    exports.push(ExportedDef {
        symbol_id,
        visibility,
        kind,
        tooling: export_tooling_metadata(def, &context.def_table, doc, tooling_source),
    });
}

fn callable_implementation(
    def: &Def,
    emitted_names: &std::collections::HashMap<DefId, String>,
    def_table: &crate::core::resolve::DefTable,
    symbol_ids: &crate::core::symbol_id::SymbolIdTable,
    module_path: &ModulePath,
    owner: &CallableOwner,
    type_params: &[crate::core::ast::TypeParam],
    params: &[Param],
    signature: &CallableSignature,
    body: Option<&Expr>,
) -> CallableImplementation {
    let is_generic = !type_params.is_empty()
        || matches!(
            owner,
            CallableOwner::Method {
                owner_type_args: true,
                ..
            }
        );
    if is_generic {
        if matches!(owner, CallableOwner::Function)
            && let Some(body) = body
            && let Some(template) = generic_function_template_graph(
                def,
                signature,
                params,
                type_params,
                body,
                def_table,
                symbol_ids,
                module_path,
            )
        {
            return CallableImplementation::GenericTemplate(template);
        }
        CallableImplementation::GenericBodyPending
    } else {
        let emitted_name = emitted_names
            .get(&def.id)
            .cloned()
            .unwrap_or_else(|| def.name.clone());
        CallableImplementation::LinkSymbol(emitted_name)
    }
}

fn generic_function_template_graph(
    def: &Def,
    signature: &CallableSignature,
    params: &[Param],
    type_params: &[crate::core::ast::TypeParam],
    body: &Expr,
    def_table: &crate::core::resolve::DefTable,
    symbol_ids: &crate::core::symbol_id::SymbolIdTable,
    module_path: &ModulePath,
) -> Option<GenericTemplateGraph> {
    let symbol_id = symbol_ids.lookup_symbol_id(def.id)?.clone();
    let mut builder =
        FunctionTemplateBodyBuilder::new(type_params, def_table, symbol_ids, module_path);
    builder.register_params(params, signature);
    let expr = builder.build_expr(body)?;
    let template = GenericFunctionTemplate {
        signature: signature.clone(),
        type_params: type_params
            .iter()
            .enumerate()
            .map(|(index, param)| TemplateTypeParam {
                id: TemplateTypeParamId(index as u32),
                name: param.ident.clone(),
                bound: param.bound.as_ref().map(|bound| bound.name.clone()),
            })
            .collect(),
        body: builder.finish(expr),
    };
    Some(GenericTemplateGraph {
        root: TemplateDefId(0),
        defs: vec![TemplateDef {
            id: TemplateDefId(0),
            symbol_id: Some(symbol_id),
            kind: TemplateDefKind::Function(template),
        }],
    })
}

struct FunctionTemplateBodyBuilder<'a> {
    type_params: &'a [crate::core::ast::TypeParam],
    def_table: &'a crate::core::resolve::DefTable,
    symbol_ids: &'a crate::core::symbol_id::SymbolIdTable,
    module_path: &'a ModulePath,
    next_binding_id: u32,
    next_site_id: u32,
    binding_ids_by_def: HashMap<DefId, TemplateBindingId>,
    params: Vec<TemplateBinding>,
    locals: Vec<TemplateBinding>,
    call_sites: Vec<TemplateCallSite>,
    type_sites: Vec<TemplateTypeSite>,
    iterable_param_slots: Vec<TemplateIterableParamSlot>,
    references: Vec<TemplateReference>,
    seen_references: HashSet<(TemplateReferenceKind, String)>,
}

impl<'a> FunctionTemplateBodyBuilder<'a> {
    fn new(
        type_params: &'a [crate::core::ast::TypeParam],
        def_table: &'a crate::core::resolve::DefTable,
        symbol_ids: &'a crate::core::symbol_id::SymbolIdTable,
        module_path: &'a ModulePath,
    ) -> Self {
        Self {
            type_params,
            def_table,
            symbol_ids,
            module_path,
            next_binding_id: 0,
            next_site_id: 0,
            binding_ids_by_def: HashMap::new(),
            params: Vec::new(),
            locals: Vec::new(),
            call_sites: Vec::new(),
            type_sites: Vec::new(),
            iterable_param_slots: Vec::new(),
            references: Vec::new(),
            seen_references: HashSet::new(),
        }
    }

    fn register_params(&mut self, params: &[Param], signature: &CallableSignature) {
        for (param_index, param) in params.iter().enumerate() {
            if let Some(def_id) = self.def_table.lookup_node_def_id(param.id) {
                let binding_id = self.alloc_binding_id();
                self.binding_ids_by_def.insert(def_id, binding_id);
                self.params.push(TemplateBinding {
                    id: binding_id,
                    name: param.ident.clone(),
                    kind: TemplateBindingKind::Param,
                    ty: Some(signature.params[param_index].ty.clone()),
                });
                if let Some(item_ty) = iterable_item_type_key(&signature.params[param_index].ty) {
                    self.iterable_param_slots.push(TemplateIterableParamSlot {
                        inst_index: param_index,
                        binding: binding_id,
                        item_ty,
                    });
                }
            }
        }
    }

    fn finish(self, expr: TemplateExpr) -> TemplateBody {
        TemplateBody {
            expr: Some(expr),
            params: self.params,
            locals: self.locals,
            nested_closures: Vec::new(),
            call_sites: self.call_sites,
            type_sites: self.type_sites,
            iterable_param_slots: self.iterable_param_slots,
            references: self.references,
        }
    }

    fn build_expr(&mut self, expr: &Expr) -> Option<TemplateExpr> {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                let items = items
                    .iter()
                    .map(|item| match item {
                        BlockItem::Stmt(stmt) => self.build_stmt(stmt),
                        BlockItem::Expr(expr) => self.build_expr(expr).map(TemplateStmt::Expr),
                    })
                    .collect::<Option<Vec<_>>>()?;
                let tail = match tail.as_ref() {
                    Some(expr) => Some(Box::new(self.build_expr(expr)?)),
                    None => None,
                };
                Some(TemplateExpr::Block { items, tail })
            }
            ExprKind::UnitLit => Some(TemplateExpr::UnitLit),
            ExprKind::IntLit(value) => Some(TemplateExpr::IntLit(*value)),
            ExprKind::BoolLit(value) => Some(TemplateExpr::BoolLit(*value)),
            ExprKind::CharLit(value) => Some(TemplateExpr::CharLit(*value)),
            ExprKind::StringLit { value } => Some(TemplateExpr::StringLit(value.clone())),
            ExprKind::TupleLit(items) => Some(TemplateExpr::TupleLit(
                items
                    .iter()
                    .map(|item| self.build_expr(item))
                    .collect::<Option<Vec<_>>>()?,
            )),
            ExprKind::Var { ident: _ } => self.build_var_expr(expr),
            ExprKind::StructLit {
                name,
                type_args,
                fields,
            } => {
                let type_def_id = self.def_table.lookup_type_def_id(name)?;
                let type_symbol = self.symbol_ids.lookup_symbol_id(type_def_id)?.clone();
                self.record_reference(
                    TemplateReferenceKind::Type,
                    TemplateReferenceTarget::External(type_symbol.clone()),
                );
                let explicit_type_args = type_args
                    .iter()
                    .map(|arg| {
                        let ty = type_key_for_type_expr(
                            arg,
                            self.type_params,
                            self.def_table,
                            self.module_path,
                        );
                        self.record_type_site(TemplateTypeSiteRole::Expr, ty.clone());
                        ty
                    })
                    .collect::<Vec<_>>();
                let fields = fields
                    .iter()
                    .map(|field| {
                        Some(TemplateStructField {
                            name: field.name.clone(),
                            value: self.build_expr(&field.value)?,
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(TemplateExpr::StructLit {
                    type_symbol,
                    explicit_type_args,
                    fields,
                })
            }
            ExprKind::Call { callee, args } => {
                let callee_expr = self.build_expr(callee)?;
                let site = self.alloc_site_id();
                let call_target = match &callee_expr {
                    TemplateExpr::BindingRef(binding) => TemplateCallTarget::Binding(*binding),
                    TemplateExpr::SymbolRef(symbol) => {
                        TemplateCallTarget::Def(TemplateReferenceTarget::External(symbol.clone()))
                    }
                    _ => TemplateCallTarget::Dynamic,
                };
                self.call_sites.push(TemplateCallSite {
                    site,
                    callee: call_target,
                    explicit_type_arg_count: 0,
                    iterable_arg_count: 0,
                });
                let args = args
                    .iter()
                    .map(|arg| {
                        Some(TemplateCallArg {
                            mode: template_call_arg_mode(arg.mode),
                            expr: self.build_expr(&arg.expr)?,
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(TemplateExpr::Call {
                    site,
                    callee: Box::new(callee_expr),
                    args,
                })
            }
            _ => None,
        }
    }

    fn build_stmt(&mut self, stmt: &StmtExpr) -> Option<TemplateStmt> {
        match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => self.build_bind_stmt(pattern, false, decl_ty.as_ref(), value),
            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => self.build_bind_stmt(pattern, true, decl_ty.as_ref(), value),
            StmtExprKind::Return { value } => Some(TemplateStmt::Return(match value.as_ref() {
                Some(expr) => Some(self.build_expr(expr)?),
                None => None,
            })),
            _ => None,
        }
    }

    fn build_bind_stmt(
        &mut self,
        pattern: &BindPattern,
        mutable: bool,
        decl_ty: Option<&crate::core::ast::TypeExpr>,
        value: &Expr,
    ) -> Option<TemplateStmt> {
        let BindPatternKind::Name { ident } = &pattern.kind else {
            return None;
        };
        let def_id = self.def_table.lookup_node_def_id(pattern.id)?;
        let binding_id = self.alloc_binding_id();
        self.binding_ids_by_def.insert(def_id, binding_id);
        let decl_ty = decl_ty.map(|ty_expr| {
            let ty =
                type_key_for_type_expr(ty_expr, self.type_params, self.def_table, self.module_path);
            self.record_type_site(TemplateTypeSiteRole::Local, ty.clone());
            ty
        });
        self.locals.push(TemplateBinding {
            id: binding_id,
            name: ident.clone(),
            kind: TemplateBindingKind::LocalVar,
            ty: decl_ty.clone(),
        });
        Some(TemplateStmt::Let {
            binding: binding_id,
            mutable,
            decl_ty,
            value: Box::new(self.build_expr(value)?),
        })
    }

    fn build_var_expr(&mut self, expr: &Expr) -> Option<TemplateExpr> {
        let def_id = self.def_table.lookup_node_def_id(expr.id)?;
        if let Some(binding_id) = self.binding_ids_by_def.get(&def_id).copied() {
            return Some(TemplateExpr::BindingRef(binding_id));
        }
        let symbol = self.symbol_ids.lookup_symbol_id(def_id)?.clone();
        self.record_reference(
            TemplateReferenceKind::Value,
            TemplateReferenceTarget::External(symbol.clone()),
        );
        Some(TemplateExpr::SymbolRef(symbol))
    }

    fn record_reference(&mut self, kind: TemplateReferenceKind, target: TemplateReferenceTarget) {
        let key = match &target {
            TemplateReferenceTarget::Local(id) => format!("local:{}", id.0),
            TemplateReferenceTarget::External(symbol) => format!("sym:{symbol}"),
        };
        if self.seen_references.insert((kind, key)) {
            self.references.push(TemplateReference { target, kind });
        }
    }

    fn record_type_site(&mut self, role: TemplateTypeSiteRole, ty: TypeKey) {
        let site = self.alloc_site_id();
        self.type_sites.push(TemplateTypeSite { site, role, ty });
    }

    fn alloc_binding_id(&mut self) -> TemplateBindingId {
        let id = TemplateBindingId(self.next_binding_id);
        self.next_binding_id += 1;
        id
    }

    fn alloc_site_id(&mut self) -> TemplateSiteId {
        let id = TemplateSiteId(self.next_site_id);
        self.next_site_id += 1;
        id
    }
}

fn iterable_item_type_key(ty: &TypeKey) -> Option<TypeKey> {
    match ty {
        TypeKey::Named { path, args, .. } if path.to_string() == "Iterable" && args.len() == 1 => {
            Some(args[0].clone())
        }
        _ => None,
    }
}

fn template_call_arg_mode(mode: CallArgMode) -> TemplateCallArgMode {
    match mode {
        CallArgMode::Default => TemplateCallArgMode::Default,
        CallArgMode::InOut => TemplateCallArgMode::InOut,
        CallArgMode::Out => TemplateCallArgMode::Out,
        CallArgMode::Move => TemplateCallArgMode::Move,
    }
}

fn type_export_from_def(
    module_path: ModulePath,
    context: &ResolvedContext,
    type_def: &TypeDef,
    doc: Option<&DocComment>,
    tooling_source: Option<&SourceToolingContext>,
) -> Option<ExportedDef> {
    let def_id = context.def_table.lookup_node_def_id(type_def.id)?;
    let def = context.def_table.lookup_def(def_id)?;
    if !def.is_public() {
        return None;
    }
    let symbol_id = context.symbol_ids.lookup_symbol_id(def_id)?.clone();
    let type_param_names = type_def
        .type_params
        .iter()
        .map(|param| param.ident.clone())
        .collect::<Vec<_>>();
    let kind = match &type_def.kind {
        TypeDefKind::Alias { aliased_ty } => TypeDefExportKind::Alias {
            aliased_ty: type_key_for_type_expr(
                aliased_ty,
                &type_def.type_params,
                &context.def_table,
                &module_path,
            ),
        },
        TypeDefKind::Struct { fields } => TypeDefExportKind::Struct {
            fields: fields
                .iter()
                .map(|field| NamedField {
                    name: field.name.clone(),
                    ty: type_key_for_type_expr(
                        &field.ty,
                        &type_def.type_params,
                        &context.def_table,
                        &module_path,
                    ),
                })
                .collect(),
        },
        TypeDefKind::Enum { variants } => TypeDefExportKind::Enum {
            variants: variants
                .iter()
                .map(|variant| EnumVariantExport {
                    name: variant.name.clone(),
                    payload: variant
                        .payload
                        .iter()
                        .map(|payload_ty| {
                            type_key_for_type_expr(
                                payload_ty,
                                &type_def.type_params,
                                &context.def_table,
                                &module_path,
                            )
                        })
                        .collect(),
                })
                .collect(),
        },
        TypeDefKind::Linear { linear } => TypeDefExportKind::Linear {
            fields: linear
                .fields
                .iter()
                .map(|field| NamedField {
                    name: field.name.clone(),
                    ty: type_key_for_type_expr(
                        &field.ty,
                        &type_def.type_params,
                        &context.def_table,
                        &module_path,
                    ),
                })
                .collect(),
            states: linear
                .states
                .iter()
                .map(|state| LinearStateExport {
                    name: state.name.clone(),
                    payload: state
                        .payload
                        .iter()
                        .map(|payload_ty| {
                            type_key_for_type_expr(
                                payload_ty,
                                &type_def.type_params,
                                &context.def_table,
                                &module_path,
                            )
                        })
                        .collect(),
                })
                .collect(),
            actions: linear
                .actions
                .iter()
                .map(|action| LinearTransitionExport {
                    name: action.name.clone(),
                    params: action
                        .params
                        .iter()
                        .map(|param| InterfaceParam {
                            name: param.name.clone(),
                            mode: crate::core::ast::ParamMode::In,
                            ty: type_key_for_type_expr(
                                &param.ty,
                                &type_def.type_params,
                                &context.def_table,
                                &module_path,
                            ),
                        })
                        .collect(),
                    source_state: action.source_state.clone(),
                    target_state: action.target_state.clone(),
                    error_ty: action.error_ty_expr.as_ref().map(|error_ty| {
                        type_key_for_type_expr(
                            error_ty,
                            &type_def.type_params,
                            &context.def_table,
                            &module_path,
                        )
                    }),
                })
                .collect(),
            triggers: linear
                .triggers
                .iter()
                .map(|trigger| LinearTransitionExport {
                    name: trigger.name.clone(),
                    params: trigger
                        .params
                        .iter()
                        .map(|param| InterfaceParam {
                            name: param.name.clone(),
                            mode: crate::core::ast::ParamMode::In,
                            ty: type_key_for_type_expr(
                                &param.ty,
                                &type_def.type_params,
                                &context.def_table,
                                &module_path,
                            ),
                        })
                        .collect(),
                    source_state: trigger.source_state.clone(),
                    target_state: trigger.target_state.clone(),
                    error_ty: trigger.error_ty_expr.as_ref().map(|error_ty| {
                        type_key_for_type_expr(
                            error_ty,
                            &type_def.type_params,
                            &context.def_table,
                            &module_path,
                        )
                    }),
                })
                .collect(),
            roles: linear
                .roles
                .iter()
                .map(|role| LinearRoleExport {
                    name: role.name.clone(),
                    allowed_actions: role.allowed_actions.clone(),
                })
                .collect(),
        },
    };

    Some(ExportedDef {
        symbol_id,
        visibility: if def.is_opaque() {
            ExportVisibility::Opaque
        } else {
            ExportVisibility::Public
        },
        kind: ExportedDefKind::Type(TypeDefExport {
            name: type_def.name.clone(),
            type_params: type_param_names,
            kind,
        }),
        tooling: export_tooling_metadata(def, &context.def_table, doc, tooling_source),
    })
}

fn trait_export_from_def(
    module_path: ModulePath,
    context: &ResolvedContext,
    trait_def: &TraitDef,
    doc: Option<&DocComment>,
    tooling_source: Option<&SourceToolingContext>,
) -> Option<ExportedDef> {
    let def_id = context.def_table.lookup_node_def_id(trait_def.id)?;
    let def = context.def_table.lookup_def(def_id)?;
    if !def.is_public() {
        return None;
    }
    let symbol_id = context.symbol_ids.lookup_symbol_id(def_id)?.clone();
    let methods = trait_def
        .methods
        .iter()
        .map(|method| TraitMethodExport {
            signature: trait_method_signature(method.sig.clone(), &context.def_table, &module_path),
        })
        .collect();
    let properties = trait_def
        .properties
        .iter()
        .map(|property| trait_property_export(property, &context.def_table, &module_path))
        .collect();

    Some(ExportedDef {
        symbol_id,
        visibility: ExportVisibility::Public,
        kind: ExportedDefKind::Trait(TraitDefExport {
            name: trait_def.name.clone(),
            methods,
            properties,
        }),
        tooling: export_tooling_metadata(def, &context.def_table, doc, tooling_source),
    })
}

fn load_source_tooling_context(
    def_table: &crate::core::resolve::DefTable,
) -> Option<SourceToolingContext> {
    let path = def_table.source_path()?.to_path_buf();
    let source = std::fs::read_to_string(&path).ok()?;
    Some(SourceToolingContext { path, source })
}

fn export_tooling_metadata(
    def: &Def,
    def_table: &crate::core::resolve::DefTable,
    doc: Option<&DocComment>,
    source_ctx: Option<&SourceToolingContext>,
) -> Option<ExportToolingMetadata> {
    let doc = doc.map(|doc| doc.raw.clone());
    let source_location = def_table.lookup_def_location(def.id).and_then(|decl| {
        let file = decl
            .path
            .clone()
            .or_else(|| source_ctx.map(|ctx| ctx.path.clone()))?;
        let name_span = source_ctx
            .and_then(|ctx| token_span_within(decl.span, &def.name, &ctx.source))
            .unwrap_or(decl.span);
        Some(SourceLocation {
            file,
            name_span,
            decl_span: decl.span,
        })
    });

    if doc.is_none() && source_location.is_none() {
        return None;
    }

    Some(ExportToolingMetadata {
        doc,
        source_location,
    })
}

fn token_span_within(container: Span, ident: &str, source: &str) -> Option<Span> {
    let start = container.start.offset.min(source.len());
    let end = container.end.offset.min(source.len());
    let snippet = source.get(start..end)?;
    let rel = snippet.find(ident)?;
    let abs_start = start + rel;
    let abs_end = abs_start + ident.len();
    Some(Span {
        start: position_at_offset(source, abs_start),
        end: position_at_offset(source, abs_end),
    })
}

fn position_at_offset(source: &str, offset: usize) -> crate::core::diag::Position {
    let mut line = 1usize;
    let mut column = 1usize;
    let mut index = 0usize;
    for ch in source.chars() {
        if index >= offset {
            break;
        }
        index += ch.len_utf8();
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    crate::core::diag::Position {
        offset,
        line,
        column,
    }
}

fn trait_method_signature(
    sig: MethodSig,
    def_table: &crate::core::resolve::DefTable,
    module_path: &ModulePath,
) -> MethodSignature {
    MethodSignature {
        name: sig.name,
        type_params: sig
            .type_params
            .iter()
            .map(|param| param.ident.clone())
            .collect(),
        self_param: InterfaceSelfParam {
            mode: sig.self_param.mode.clone(),
            receiver_ty: sig
                .self_param
                .receiver_ty_expr
                .as_ref()
                .map(|expr| type_key_for_type_expr(expr, &sig.type_params, def_table, module_path)),
        },
        params: sig
            .params
            .iter()
            .map(|param| InterfaceParam {
                name: param.ident.clone(),
                mode: param.mode.clone(),
                ty: type_key_for_type_expr(&param.typ, &sig.type_params, def_table, module_path),
            })
            .collect(),
        ret_ty: type_key_for_type_expr(&sig.ret_ty_expr, &sig.type_params, def_table, module_path),
    }
}

fn trait_property_export(
    property: &TraitProperty,
    def_table: &crate::core::resolve::DefTable,
    module_path: &ModulePath,
) -> TraitPropertyExport {
    TraitPropertyExport {
        name: property.name.clone(),
        ty: type_key_for_type_expr(&property.ty, &[], def_table, module_path),
        has_get: property.has_get,
        has_set: property.has_set,
    }
}

#[cfg(test)]
#[path = "../../tests/core/t_module_interface.rs"]
mod tests;
