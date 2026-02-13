//! Program-aware imported symbol/type/trait fact helpers for analysis.

use std::collections::{HashMap, HashSet};

use crate::core::capsule::{ModuleId, RequireKind};
use crate::core::context::{CapsuleParsedContext, ResolvedContext, TypeCheckedContext};
use crate::core::resolve::{
    ImportedCallableSig, ImportedModule, ImportedParamSig, ImportedSymbol, ImportedTraitMethodSig,
    ImportedTraitPropertySig, ImportedTraitSig,
};
use crate::core::tree::ParamMode;
use crate::core::typecheck::type_map::{
    resolve_return_type_expr_with_params, resolve_type_def_with_args, resolve_type_expr_with_params,
};
use crate::core::types::{FnParamMode, TyVarId, Type};

#[derive(Default)]
pub(crate) struct ProgramImportFactsCache {
    public_callables_by_module: HashMap<ModuleId, HashSet<String>>,
    public_types_by_module: HashMap<ModuleId, HashSet<String>>,
    public_traits_by_module: HashMap<ModuleId, HashSet<String>>,
    callable_sigs_by_module: HashMap<ModuleId, HashMap<String, Vec<ImportedCallableSig>>>,
    type_tys_by_module: HashMap<ModuleId, HashMap<String, Type>>,
    trait_sigs_by_module: HashMap<ModuleId, HashMap<String, ImportedTraitSig>>,
}

impl ProgramImportFactsCache {
    pub(crate) fn imported_modules_for(
        &self,
        program_context: &CapsuleParsedContext,
        module_id: ModuleId,
    ) -> HashMap<String, ImportedModule> {
        let mut out = HashMap::new();
        let Some(parsed) = program_context.module(module_id) else {
            return out;
        };

        for req in &parsed.requires {
            if req.kind != RequireKind::Module {
                continue;
            }
            let Some(dep_id) = program_context
                .capsule
                .by_path
                .get(&req.module_path)
                .copied()
            else {
                continue;
            };
            let mut members = HashSet::new();
            members.extend(
                self.public_callables_by_module
                    .get(&dep_id)
                    .cloned()
                    .unwrap_or_default(),
            );
            members.extend(
                self.public_types_by_module
                    .get(&dep_id)
                    .cloned()
                    .unwrap_or_default(),
            );
            members.extend(
                self.public_traits_by_module
                    .get(&dep_id)
                    .cloned()
                    .unwrap_or_default(),
            );
            out.insert(
                req.alias.clone(),
                ImportedModule {
                    path: req.module_path.to_string(),
                    members,
                },
            );
        }

        out
    }

    pub(crate) fn imported_symbols_for(
        &self,
        program_context: &CapsuleParsedContext,
        module_id: ModuleId,
    ) -> HashMap<String, ImportedSymbol> {
        let mut out = HashMap::new();
        let Some(parsed) = program_context.module(module_id) else {
            return out;
        };

        for req in &parsed.requires {
            if req.kind != RequireKind::Symbol {
                continue;
            }
            let Some(member) = &req.member else {
                continue;
            };
            let Some(dep_id) = program_context
                .capsule
                .by_path
                .get(&req.module_path)
                .copied()
            else {
                continue;
            };
            let dep_callable_sigs = self.callable_sigs_by_module.get(&dep_id);
            let dep_type_tys = self.type_tys_by_module.get(&dep_id);
            let dep_trait_sigs = self.trait_sigs_by_module.get(&dep_id);
            let has_callable = self
                .public_callables_by_module
                .get(&dep_id)
                .is_some_and(|names| names.contains(member));
            let has_type = self
                .public_types_by_module
                .get(&dep_id)
                .is_some_and(|names| names.contains(member));
            let has_trait = self
                .public_traits_by_module
                .get(&dep_id)
                .is_some_and(|names| names.contains(member));
            let imported = ImportedSymbol {
                has_callable,
                callable_sigs: if has_callable {
                    dep_callable_sigs
                        .and_then(|module_sigs| module_sigs.get(member))
                        .cloned()
                        .unwrap_or_default()
                } else {
                    Vec::new()
                },
                has_type,
                type_ty: if has_type {
                    dep_type_tys
                        .and_then(|module_types| module_types.get(member))
                        .cloned()
                } else {
                    None
                },
                has_trait,
                trait_sig: if has_trait {
                    dep_trait_sigs
                        .and_then(|module_traits| module_traits.get(member))
                        .cloned()
                } else {
                    None
                },
            };
            if imported.has_callable || imported.has_type || imported.has_trait {
                out.insert(req.alias.clone(), imported);
            }
        }

        out
    }

    pub(crate) fn should_skip_typecheck(
        imported_symbols: &HashMap<String, ImportedSymbol>,
    ) -> bool {
        imported_symbols.values().any(|imported| {
            (imported.has_type && imported.type_ty.is_none())
                || (imported.has_trait && imported.trait_sig.is_none())
                || (imported.has_callable && imported.callable_sigs.is_empty())
        })
    }

    pub(crate) fn ingest_resolved(&mut self, module_id: ModuleId, resolved: &ResolvedContext) {
        self.public_callables_by_module
            .entry(module_id)
            .or_insert_with(|| collect_public_callable_names(resolved));
        self.public_types_by_module
            .entry(module_id)
            .or_insert_with(|| collect_public_type_names(resolved));
        self.public_traits_by_module
            .entry(module_id)
            .or_insert_with(|| collect_public_trait_names(resolved));
        self.callable_sigs_by_module
            .entry(module_id)
            .or_insert_with(|| collect_public_callable_sigs_resolved(resolved));
    }

    pub(crate) fn ingest_typed(&mut self, module_id: ModuleId, typed: &TypeCheckedContext) {
        self.callable_sigs_by_module
            .insert(module_id, collect_public_callable_sigs(typed));
        self.type_tys_by_module
            .insert(module_id, collect_public_type_tys(typed));
        self.trait_sigs_by_module
            .insert(module_id, collect_public_trait_sigs(typed));
    }
}

fn collect_public_callable_sigs(
    typed: &TypeCheckedContext,
) -> HashMap<String, Vec<ImportedCallableSig>> {
    let mut out = HashMap::<String, Vec<ImportedCallableSig>>::new();
    for item in &typed.module.top_level_items {
        let callable = match item {
            crate::core::tree::typed::TopLevelItem::FuncDecl(decl) => {
                Some((&decl.sig.name, decl.def_id))
            }
            crate::core::tree::typed::TopLevelItem::FuncDef(def) => {
                Some((&def.sig.name, def.def_id))
            }
            _ => None,
        };
        let Some((name, def_id)) = callable else {
            continue;
        };
        let Some(def) = typed.def_table.lookup_def(def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }
        let Some(def_ty) = typed.type_map.lookup_def_type(def) else {
            continue;
        };
        let Type::Fn { params, ret_ty } = def_ty else {
            continue;
        };
        out.entry(name.clone())
            .or_default()
            .push(ImportedCallableSig {
                params: params
                    .into_iter()
                    .map(|param| ImportedParamSig {
                        mode: param_mode_from_fn_param(param.mode),
                        ty: param.ty,
                    })
                    .collect(),
                ret_ty: *ret_ty,
            });
    }
    out
}

fn collect_public_callable_names(resolved: &ResolvedContext) -> HashSet<String> {
    let mut out = HashSet::new();
    for item in &resolved.module.top_level_items {
        let callable = match item {
            crate::core::tree::resolved::TopLevelItem::FuncDecl(decl) => {
                Some((&decl.sig.name, decl.def_id))
            }
            crate::core::tree::resolved::TopLevelItem::FuncDef(def) => {
                Some((&def.sig.name, def.def_id))
            }
            _ => None,
        };
        let Some((name, def_id)) = callable else {
            continue;
        };
        let Some(def) = resolved.def_table.lookup_def(def_id) else {
            continue;
        };
        if def.is_public() {
            out.insert(name.clone());
        }
    }
    out
}

fn collect_public_type_names(resolved: &ResolvedContext) -> HashSet<String> {
    let mut out = HashSet::new();
    for type_def in resolved.module.type_defs() {
        let Some(def) = resolved.def_table.lookup_def(type_def.def_id) else {
            continue;
        };
        if def.is_public() {
            out.insert(type_def.name.clone());
        }
    }
    out
}

fn collect_public_trait_names(resolved: &ResolvedContext) -> HashSet<String> {
    let mut out = HashSet::new();
    for trait_def in resolved.module.trait_defs() {
        let Some(def) = resolved.def_table.lookup_def(trait_def.def_id) else {
            continue;
        };
        if def.is_public() {
            out.insert(trait_def.name.clone());
        }
    }
    out
}

fn collect_public_callable_sigs_resolved(
    resolved: &ResolvedContext,
) -> HashMap<String, Vec<ImportedCallableSig>> {
    let mut out = HashMap::<String, Vec<ImportedCallableSig>>::new();
    for item in &resolved.module.top_level_items {
        let callable = match item {
            crate::core::tree::resolved::TopLevelItem::FuncDecl(decl) => {
                Some((&decl.sig.name, decl.def_id, &decl.sig))
            }
            crate::core::tree::resolved::TopLevelItem::FuncDef(def) => {
                Some((&def.sig.name, def.def_id, &def.sig))
            }
            _ => None,
        };
        let Some((name, def_id, sig)) = callable else {
            continue;
        };
        if !sig.type_params.is_empty() {
            continue;
        }
        let Some(def) = resolved.def_table.lookup_def(def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }

        let mut params = Vec::with_capacity(sig.params.len());
        let mut failed = false;
        for param in &sig.params {
            match resolve_type_expr_with_params(&resolved.def_table, resolved, &param.typ, None) {
                Ok(ty) => params.push(ImportedParamSig {
                    mode: param.mode.clone(),
                    ty,
                }),
                Err(_) => {
                    failed = true;
                    break;
                }
            }
        }
        if failed {
            continue;
        }

        let Ok(ret_ty) = resolve_return_type_expr_with_params(
            &resolved.def_table,
            resolved,
            &sig.ret_ty_expr,
            None,
        ) else {
            continue;
        };

        out.entry(name.clone())
            .or_default()
            .push(ImportedCallableSig { params, ret_ty });
    }
    out
}

fn collect_public_type_tys(typed: &TypeCheckedContext) -> HashMap<String, Type> {
    let mut out = HashMap::<String, Type>::new();
    for type_def in typed.module.type_defs() {
        if !type_def.type_params.is_empty() {
            continue;
        }
        let Some(def) = typed.def_table.lookup_def(type_def.def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }
        let Ok(ty) =
            resolve_type_def_with_args(&typed.def_table, &typed.module, type_def.def_id, &[])
        else {
            continue;
        };
        out.insert(type_def.name.clone(), ty);
    }
    out
}

fn collect_public_trait_sigs(typed: &TypeCheckedContext) -> HashMap<String, ImportedTraitSig> {
    let mut out = HashMap::<String, ImportedTraitSig>::new();
    for trait_def in typed.module.trait_defs() {
        let Some(def) = typed.def_table.lookup_def(trait_def.def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }

        let mut methods = HashMap::new();
        for method in &trait_def.methods {
            let Some(sig) = collect_imported_trait_method_sig(typed, &method.sig) else {
                continue;
            };
            methods.insert(method.sig.name.clone(), sig);
        }

        let mut properties = HashMap::new();
        for property in &trait_def.properties {
            let Ok(ty) = resolve_type_expr_with_params(&typed.def_table, typed, &property.ty, None)
            else {
                continue;
            };
            properties.insert(
                property.name.clone(),
                ImportedTraitPropertySig {
                    name: property.name.clone(),
                    ty,
                    has_get: property.has_get,
                    has_set: property.has_set,
                },
            );
        }

        out.insert(
            trait_def.name.clone(),
            ImportedTraitSig {
                methods,
                properties,
            },
        );
    }
    out
}

fn collect_imported_trait_method_sig(
    typed: &TypeCheckedContext,
    sig: &crate::core::tree::typed::MethodSig,
) -> Option<ImportedTraitMethodSig> {
    let type_param_map = if sig.type_params.is_empty() {
        None
    } else {
        Some(
            sig.type_params
                .iter()
                .enumerate()
                .map(|(index, param)| (param.def_id, TyVarId::new(index as u32)))
                .collect::<HashMap<_, _>>(),
        )
    };

    let mut params = Vec::with_capacity(sig.params.len());
    for param in &sig.params {
        let Ok(ty) = resolve_type_expr_with_params(
            &typed.def_table,
            typed,
            &param.typ,
            type_param_map.as_ref(),
        ) else {
            return None;
        };
        params.push(ImportedParamSig {
            mode: param.mode.clone(),
            ty,
        });
    }

    let Ok(ret_ty) = resolve_return_type_expr_with_params(
        &typed.def_table,
        typed,
        &sig.ret_ty_expr,
        type_param_map.as_ref(),
    ) else {
        return None;
    };

    Some(ImportedTraitMethodSig {
        name: sig.name.clone(),
        params,
        ret_ty,
        type_param_count: sig.type_params.len(),
        type_param_bounds: sig
            .type_params
            .iter()
            .map(|param| param.bound.as_ref().map(|bound| bound.name.clone()))
            .collect(),
        self_mode: sig.self_param.mode.clone(),
    })
}

fn param_mode_from_fn_param(mode: FnParamMode) -> ParamMode {
    match mode {
        FnParamMode::In => ParamMode::In,
        FnParamMode::InOut => ParamMode::InOut,
        FnParamMode::Out => ParamMode::Out,
        FnParamMode::Sink => ParamMode::Sink,
    }
}
