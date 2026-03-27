//! In-memory module interface model.
//!
//! Phase 2 starts by defining the semantic contract for compiled module
//! interfaces before choosing an on-disk encoding. The types in this module are
//! the compiler-owned shape that later `.mci` readers/writers will encode.

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::core::ast::{
    DocComment, MethodItem, MethodSig, Param, SelfParam, TopLevelItem, TraitDef, TraitProperty,
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
    TemplateBindingId, TemplateBindingKind, TemplateBody, TemplateCallSite, TemplateDef,
    TemplateDefId, TemplateDefKind, TemplateIterableParamSlot, TemplateNestedClosure,
    TemplateReference, TemplateReferenceKind, TemplateReferenceTarget, TemplateSiteId,
    TemplateTypeParam, TemplateTypeParamId, TemplateTypeSite, TemplateTypeSiteRole,
};

pub const MODULE_INTERFACE_FORMAT_VERSION: u32 = 1;

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
    let params = params
        .iter()
        .map(|param| InterfaceParam {
            name: param.ident.clone(),
            mode: param.mode.clone(),
            ty: type_key_for_type_expr(&param.typ, type_params, &context.def_table, module_path),
        })
        .collect::<Vec<_>>();
    let implementation =
        callable_implementation(def, &context.symbols.def_names, &owner, type_params);
    let visibility = if def.is_opaque() {
        ExportVisibility::Opaque
    } else {
        ExportVisibility::Public
    };

    let kind = match owner {
        CallableOwner::Function => ExportedDefKind::Func(CallableExport {
            signature: CallableSignature {
                name: def.name.clone(),
                type_params: type_param_names,
                params,
                ret_ty: type_key_for_type_expr(
                    ret_ty,
                    type_params,
                    &context.def_table,
                    module_path,
                ),
            },
            implementation,
        }),
        CallableOwner::Method { owner_type, .. } => ExportedDefKind::Method(MethodExport {
            owner_type,
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
                params,
                ret_ty: type_key_for_type_expr(
                    ret_ty,
                    type_params,
                    &context.def_table,
                    module_path,
                ),
            },
            implementation,
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
    owner: &CallableOwner,
    type_params: &[crate::core::ast::TypeParam],
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
        CallableImplementation::GenericBodyPending
    } else {
        let emitted_name = emitted_names
            .get(&def.id)
            .cloned()
            .unwrap_or_else(|| def.name.clone());
        CallableImplementation::LinkSymbol(emitted_name)
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
