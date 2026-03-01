//! Program-aware imported symbol/type/trait fact helpers for analysis.

use std::collections::HashMap;

use crate::core::capsule::{ModuleId, ModulePath};
use crate::core::context::{
    CapsuleParsedContext, ModuleExportFacts, ResolvedContext, TypeCheckedContext,
    import_env_from_requires, module_export_facts_from_def_table,
};
use crate::core::resolve::{
    GlobalDefId, ImportedCallableSig, ImportedModule, ImportedParamSig, ImportedSymbol,
    ImportedTraitMethodSig, ImportedTraitPropertySig, ImportedTraitSig,
};
use crate::core::tree::{MethodSig, ParamMode, TopLevelItem};
use crate::core::typecheck::type_map::{
    resolve_return_type_expr_with_params, resolve_type_def_with_args, resolve_type_expr_with_params,
};
use crate::core::types::{FnParamMode, TyVarId, Type};

#[derive(Default)]
pub(crate) struct ProgramImportFactsCache {
    export_facts_by_module: HashMap<ModuleId, ModuleExportFacts>,
    callable_sigs_by_def: HashMap<GlobalDefId, ImportedCallableSig>,
    type_tys_by_def: HashMap<GlobalDefId, Type>,
    trait_sigs_by_def: HashMap<GlobalDefId, ImportedTraitSig>,
}

impl ProgramImportFactsCache {
    pub(crate) fn imported_modules_for(
        &self,
        program_context: &CapsuleParsedContext,
        module_id: ModuleId,
    ) -> HashMap<String, ImportedModule> {
        let mut out = HashMap::new();
        let import_env =
            import_env_from_requires(program_context, module_id, &self.export_facts_by_module);
        for (alias, binding) in import_env.module_aliases {
            out.insert(
                alias,
                ImportedModule::from_exports(&binding.module_path.to_string(), &binding.exports),
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
        let import_env =
            import_env_from_requires(program_context, module_id, &self.export_facts_by_module);
        for (alias, binding) in import_env.symbol_aliases {
            let callable_sigs = binding
                .callables
                .iter()
                .filter_map(|def_id| self.callable_sigs_by_def.get(def_id))
                .cloned()
                .collect();
            let type_ty = binding
                .type_def
                .and_then(|def_id| self.type_tys_by_def.get(&def_id))
                .cloned();
            let trait_sig = binding
                .trait_def
                .and_then(|def_id| self.trait_sigs_by_def.get(&def_id))
                .cloned();
            if let Some(imported) =
                ImportedSymbol::from_binding(&binding, callable_sigs, type_ty, trait_sig)
            {
                out.insert(alias, imported);
            }
        }

        out
    }

    pub(crate) fn should_skip_typecheck(
        imported_symbols: &HashMap<String, ImportedSymbol>,
    ) -> bool {
        imported_symbols.values().any(|imported| {
            (imported.has_type() && imported.type_ty.is_none())
                || (imported.has_trait() && imported.trait_sig.is_none())
                || (imported.has_callable() && imported.callable_sigs.is_empty())
        })
    }

    pub(crate) fn ingest_resolved(
        &mut self,
        module_id: ModuleId,
        module_path: Option<ModulePath>,
        resolved: &ResolvedContext,
    ) {
        // Export facts are the identity-based summary for cross-module imports.
        // Keep the cache aligned with that core representation instead of
        // rebuilding separate public-name sets.
        self.export_facts_by_module
            .entry(module_id)
            .or_insert_with(|| {
                module_export_facts_from_def_table(module_id, module_path, &resolved.def_table)
            });
        self.callable_sigs_by_def
            .extend(collect_public_callable_sigs_resolved(module_id, resolved));
    }

    pub(crate) fn export_facts(&self, module_id: ModuleId) -> Option<&ModuleExportFacts> {
        self.export_facts_by_module.get(&module_id)
    }

    pub(crate) fn ingest_typed(&mut self, module_id: ModuleId, typed: &TypeCheckedContext) {
        self.callable_sigs_by_def
            .extend(collect_public_callable_sigs(module_id, typed));
        self.type_tys_by_def
            .extend(collect_public_type_tys(module_id, typed));
        self.trait_sigs_by_def
            .extend(collect_public_trait_sigs(module_id, typed));
    }
}

fn collect_public_callable_sigs(
    module_id: ModuleId,
    typed: &TypeCheckedContext,
) -> HashMap<GlobalDefId, ImportedCallableSig> {
    let mut out = HashMap::<GlobalDefId, ImportedCallableSig>::new();
    for item in &typed.module.top_level_items {
        let callable = match item {
            TopLevelItem::FuncDecl(decl) => Some(typed.def_table.def_id(decl.id)),
            TopLevelItem::FuncDef(def) => Some(typed.def_table.def_id(def.id)),
            _ => None,
        };
        let Some(def_id) = callable else {
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
        let global_def = GlobalDefId::new(module_id, def_id);
        out.insert(
            global_def,
            ImportedCallableSig {
                params: params
                    .into_iter()
                    .map(|param| ImportedParamSig {
                        mode: param_mode_from_fn_param(param.mode),
                        ty: param.ty,
                    })
                    .collect(),
                ret_ty: *ret_ty,
            },
        );
    }
    out
}

fn collect_public_callable_sigs_resolved(
    module_id: ModuleId,
    resolved: &ResolvedContext,
) -> HashMap<GlobalDefId, ImportedCallableSig> {
    let mut out = HashMap::<GlobalDefId, ImportedCallableSig>::new();
    for item in &resolved.module.top_level_items {
        let callable = match item {
            TopLevelItem::FuncDecl(decl) => Some((resolved.def_table.def_id(decl.id), &decl.sig)),
            TopLevelItem::FuncDef(def) => Some((resolved.def_table.def_id(def.id), &def.sig)),
            _ => None,
        };
        let Some((def_id, sig)) = callable else {
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

        out.insert(
            GlobalDefId::new(module_id, def_id),
            ImportedCallableSig { params, ret_ty },
        );
    }
    out
}

fn collect_public_type_tys(
    module_id: ModuleId,
    typed: &TypeCheckedContext,
) -> HashMap<GlobalDefId, Type> {
    let mut out = HashMap::<GlobalDefId, Type>::new();
    for type_def in typed.module.type_defs() {
        if !type_def.type_params.is_empty() {
            continue;
        }
        let type_def_id = typed.def_table.def_id(type_def.id);
        let Some(def) = typed.def_table.lookup_def(type_def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }
        let Ok(ty) = resolve_type_def_with_args(&typed.def_table, &typed.module, type_def_id, &[])
        else {
            continue;
        };
        out.insert(GlobalDefId::new(module_id, type_def_id), ty);
    }
    out
}

fn collect_public_trait_sigs(
    module_id: ModuleId,
    typed: &TypeCheckedContext,
) -> HashMap<GlobalDefId, ImportedTraitSig> {
    let mut out = HashMap::<GlobalDefId, ImportedTraitSig>::new();
    for trait_def in typed.module.trait_defs() {
        let def_id = typed.def_table.def_id(trait_def.id);
        let Some(def) = typed.def_table.lookup_def(def_id) else {
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
            GlobalDefId::new(module_id, def_id),
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
    sig: &MethodSig,
) -> Option<ImportedTraitMethodSig> {
    let type_param_map = if sig.type_params.is_empty() {
        None
    } else {
        Some(
            sig.type_params
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    (typed.def_table.def_id(param.id), TyVarId::new(index as u32))
                })
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
