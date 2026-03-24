//! Program-aware imported symbol/type/trait fact helpers for analysis.

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::core::ast::{
    MethodBlock, MethodItem, MethodSig, ParamMode, TopLevelItem, TypeExpr, TypeExprKind, TypeParam,
};
use crate::core::capsule::{ModuleId, ModulePath};
use crate::core::context::{
    CapsuleParsedContext, ModuleExportFacts, ResolvedContext, TypeCheckedContext,
    import_env_from_requires, module_export_facts_from_def_table,
};
use crate::core::resolve::{
    ImportedCallableSig, ImportedMethodSig, ImportedModule, ImportedParamSig, ImportedSymbol,
    ImportedTraitMethodSig, ImportedTraitPropertySig, ImportedTraitSig,
};
use crate::core::symbol_id::SymbolId;
use crate::core::typecheck::type_map::{
    resolve_return_type_expr_with_params, resolve_type_def_with_args, resolve_type_expr_with_params,
};
use crate::core::types::{FnParamMode, TyVarId, Type};

#[derive(Default)]
pub(crate) struct ProgramImportFactsCache {
    export_facts_by_module: HashMap<ModuleId, ModuleExportFacts>,
    callable_sigs_by_symbol: HashMap<SymbolId, ImportedCallableSig>,
    type_tys_by_symbol: HashMap<SymbolId, Type>,
    method_sigs_by_type_symbol: HashMap<SymbolId, HashMap<String, Vec<ImportedMethodSig>>>,
    trait_sigs_by_symbol: HashMap<SymbolId, ImportedTraitSig>,
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
            let mut imported =
                ImportedModule::from_exports(&binding.module_path.to_string(), &binding.exports);
            for member in imported.members.clone() {
                if let Some(materialized) =
                    self.materialize_imported_symbol(&binding.exports, &member)
                {
                    imported.member_symbols.insert(member, materialized);
                }
            }
            out.insert(alias, imported);
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
        let mut referenced_module_ids = HashSet::<ModuleId>::new();
        for binding in import_env.module_aliases.values() {
            referenced_module_ids.insert(binding.module_id);
        }
        for (alias, binding) in import_env.symbol_aliases {
            referenced_module_ids.insert(binding.module_id);
            if binding.is_empty() {
                continue;
            }

            if let Some(imported) = self.materialize_imported_symbol_binding(&binding) {
                out.insert(alias, imported);
            }
        }

        let mut type_counts = HashMap::<String, usize>::new();
        let mut imported_types = Vec::<(String, ImportedSymbol)>::new();
        for dep_module_id in referenced_module_ids {
            let Some(exports) = self.export_facts_by_module.get(&dep_module_id) else {
                continue;
            };
            for type_name in exports.types.keys() {
                *type_counts.entry(type_name.clone()).or_default() += 1;
                let type_ty = exports
                    .types
                    .get(type_name)
                    .and_then(|item| item.symbol_id.as_ref())
                    .and_then(|symbol_id| self.type_tys_by_symbol.get(symbol_id))
                    .cloned();
                let method_sigs = exports
                    .types
                    .get(type_name)
                    .and_then(|item| item.symbol_id.as_ref())
                    .and_then(|symbol_id| self.method_sigs_by_type_symbol.get(symbol_id))
                    .cloned()
                    .unwrap_or_default();
                if (type_ty.is_some() || !method_sigs.is_empty())
                    && let Some(imported) = ImportedSymbol::from_exports(
                        exports,
                        type_name,
                        Vec::new(),
                        type_ty,
                        method_sigs,
                        None,
                    )
                {
                    imported_types.push((type_name.clone(), imported));
                }
            }
        }
        for (type_name, imported) in imported_types {
            if type_counts.get(&type_name).copied() == Some(1) {
                out.entry(type_name).or_insert(imported);
            }
        }

        out
    }

    pub(crate) fn should_skip_typecheck(
        imported_symbols: &HashMap<String, ImportedSymbol>,
    ) -> bool {
        !Self::incomplete_imports(imported_symbols).is_empty()
    }

    pub(crate) fn incomplete_imports(
        imported_symbols: &HashMap<String, ImportedSymbol>,
    ) -> Vec<String> {
        let mut out = Vec::new();
        let mut names: Vec<_> = imported_symbols.keys().cloned().collect();
        names.sort();
        for name in names {
            let Some(imported) = imported_symbols.get(&name) else {
                continue;
            };
            let mut reasons = Vec::new();
            if imported.has_type() && imported.type_ty.is_none() && imported.method_sigs.is_empty()
            {
                reasons.push("missing-type");
            }
            if imported.has_trait() && imported.trait_sig.is_none() {
                reasons.push("missing-trait");
            }
            if imported.has_callable() && imported.callable_sigs.is_empty() {
                reasons.push("missing-callable");
            }
            if !reasons.is_empty() {
                out.push(format!("{name}[{}]", reasons.join(",")));
            }
        }
        out
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
                module_export_facts_from_def_table(
                    module_id,
                    module_path,
                    &resolved.def_table,
                    &resolved.symbol_ids,
                )
            });
        self.callable_sigs_by_symbol
            .extend(collect_public_callable_sigs_resolved(module_id, resolved));
    }

    pub(crate) fn export_facts(&self, module_id: ModuleId) -> Option<&ModuleExportFacts> {
        self.export_facts_by_module.get(&module_id)
    }

    pub(crate) fn ingest_typed(&mut self, module_id: ModuleId, typed: &TypeCheckedContext) {
        self.callable_sigs_by_symbol
            .extend(collect_public_callable_sigs(module_id, typed));
        self.type_tys_by_symbol
            .extend(collect_public_type_tys(module_id, typed));
        self.method_sigs_by_type_symbol
            .extend(collect_public_method_sigs(typed));
        self.trait_sigs_by_symbol
            .extend(collect_public_trait_sigs(module_id, typed));
    }

    fn materialize_imported_symbol_binding(
        &self,
        binding: &crate::core::context::ImportedSymbolBinding,
    ) -> Option<ImportedSymbol> {
        let callable_sigs = binding
            .callables
            .iter()
            .filter_map(|item| item.symbol_id.as_ref())
            .filter_map(|symbol_id| self.callable_sigs_by_symbol.get(symbol_id))
            .cloned()
            .collect();
        let type_ty = binding
            .type_def
            .as_ref()
            .and_then(|item| item.symbol_id.as_ref())
            .and_then(|symbol_id| self.type_tys_by_symbol.get(symbol_id))
            .cloned();
        let method_sigs = binding
            .type_def
            .as_ref()
            .and_then(|item| item.symbol_id.as_ref())
            .and_then(|symbol_id| self.method_sigs_by_type_symbol.get(symbol_id))
            .cloned()
            .unwrap_or_default();
        let trait_sig = binding
            .trait_def
            .as_ref()
            .and_then(|item| item.symbol_id.as_ref())
            .and_then(|symbol_id| self.trait_sigs_by_symbol.get(symbol_id))
            .cloned();
        ImportedSymbol::from_binding(
            binding,
            callable_sigs,
            None,
            type_ty,
            method_sigs,
            trait_sig,
        )
    }

    fn materialize_imported_symbol(
        &self,
        exports: &ModuleExportFacts,
        member: &str,
    ) -> Option<ImportedSymbol> {
        let callable_sigs = exports
            .callables
            .get(member)
            .into_iter()
            .flat_map(|items| items.iter())
            .filter_map(|item| item.symbol_id.as_ref())
            .filter_map(|symbol_id| self.callable_sigs_by_symbol.get(symbol_id))
            .cloned()
            .collect();
        let type_ty = exports
            .types
            .get(member)
            .and_then(|item| item.symbol_id.as_ref())
            .and_then(|symbol_id| self.type_tys_by_symbol.get(symbol_id))
            .cloned();
        let method_sigs = exports
            .types
            .get(member)
            .and_then(|item| item.symbol_id.as_ref())
            .and_then(|symbol_id| self.method_sigs_by_type_symbol.get(symbol_id))
            .cloned()
            .unwrap_or_default();
        let trait_sig = exports
            .traits
            .get(member)
            .and_then(|item| item.symbol_id.as_ref())
            .and_then(|symbol_id| self.trait_sigs_by_symbol.get(symbol_id))
            .cloned();
        ImportedSymbol::from_exports(
            exports,
            member,
            callable_sigs,
            type_ty,
            method_sigs,
            trait_sig,
        )
    }
}

fn collect_public_callable_sigs(
    _module_id: ModuleId,
    typed: &TypeCheckedContext,
) -> HashMap<SymbolId, ImportedCallableSig> {
    let mut out = HashMap::<SymbolId, ImportedCallableSig>::new();
    for item in &typed.module.top_level_items {
        let callable = match item {
            TopLevelItem::FuncDecl(decl) => Some((typed.def_table.def_id(decl.id), &decl.sig)),
            TopLevelItem::FuncDef(def) => Some((typed.def_table.def_id(def.id), &def.sig)),
            _ => None,
        };
        let Some((def_id, sig)) = callable else {
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
        let Some(symbol_id) = typed.symbol_ids.lookup_symbol_id(def_id).cloned() else {
            continue;
        };
        out.insert(
            symbol_id,
            ImportedCallableSig {
                params: params
                    .into_iter()
                    .map(|param| ImportedParamSig {
                        mode: param_mode_from_fn_param(param.mode),
                        ty: param.ty,
                    })
                    .collect(),
                ret_ty: *ret_ty,
                type_param_count: sig.type_params.len(),
                type_param_var_names: sig
                    .type_params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| (index as u32, param.ident.clone()))
                    .collect(),
                type_param_bounds: sig
                    .type_params
                    .iter()
                    .map(|param| param.bound.as_ref().map(|bound| bound.name.clone()))
                    .collect(),
            },
        );
    }
    out
}

fn collect_public_callable_sigs_resolved(
    _module_id: ModuleId,
    resolved: &ResolvedContext,
) -> HashMap<SymbolId, ImportedCallableSig> {
    let mut out = HashMap::<SymbolId, ImportedCallableSig>::new();
    for item in &resolved.module.top_level_items {
        let callable = match item {
            TopLevelItem::FuncDecl(decl) => Some((resolved.def_table.def_id(decl.id), &decl.sig)),
            TopLevelItem::FuncDef(def) => Some((resolved.def_table.def_id(def.id), &def.sig)),
            _ => None,
        };
        let Some((def_id, sig)) = callable else {
            continue;
        };
        let Some(def) = resolved.def_table.lookup_def(def_id) else {
            continue;
        };
        if !def.is_public() {
            continue;
        }

        let type_param_map = if sig.type_params.is_empty() {
            None
        } else {
            Some(
                sig.type_params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| {
                        (
                            resolved.def_table.def_id(param.id),
                            TyVarId::new(index as u32),
                        )
                    })
                    .collect::<HashMap<_, _>>(),
            )
        };

        let mut params = Vec::with_capacity(sig.params.len());
        let mut failed = false;
        for param in &sig.params {
            match resolve_type_expr_with_params(
                &resolved.def_table,
                resolved,
                &param.typ,
                type_param_map.as_ref(),
            ) {
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
            type_param_map.as_ref(),
        ) else {
            continue;
        };

        let Some(symbol_id) = resolved.symbol_ids.lookup_symbol_id(def_id).cloned() else {
            continue;
        };
        out.insert(
            symbol_id,
            ImportedCallableSig {
                params,
                ret_ty,
                type_param_count: sig.type_params.len(),
                type_param_var_names: sig
                    .type_params
                    .iter()
                    .enumerate()
                    .map(|(index, param)| (index as u32, param.ident.clone()))
                    .collect::<BTreeMap<_, _>>(),
                type_param_bounds: sig
                    .type_params
                    .iter()
                    .map(|param| param.bound.as_ref().map(|bound| bound.name.clone()))
                    .collect(),
            },
        );
    }
    out
}

fn collect_public_type_tys(
    _module_id: ModuleId,
    typed: &TypeCheckedContext,
) -> HashMap<SymbolId, Type> {
    let mut out = HashMap::<SymbolId, Type>::new();
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
        let Some(symbol_id) = typed.symbol_ids.lookup_symbol_id(type_def_id).cloned() else {
            continue;
        };
        out.insert(symbol_id, ty);
    }
    out
}

fn collect_public_method_sigs(
    typed: &TypeCheckedContext,
) -> HashMap<SymbolId, HashMap<String, Vec<ImportedMethodSig>>> {
    let mut out = HashMap::<SymbolId, HashMap<String, Vec<ImportedMethodSig>>>::new();
    for method_block in typed.module.method_blocks() {
        let receiver_type_params = method_block_type_params(typed, method_block);
        let Some(owner_symbol_id) = method_block_owner_symbol_id(typed, method_block) else {
            continue;
        };
        let Some(self_ty) =
            resolve_method_block_self_type(typed, method_block, &receiver_type_params)
        else {
            continue;
        };

        for method_item in &method_block.method_items {
            let (sig, def_id) = match method_item {
                MethodItem::Decl(method_decl) => {
                    (&method_decl.sig, typed.def_table.def_id(method_decl.id))
                }
                MethodItem::Def(method_def) => {
                    (&method_def.sig, typed.def_table.def_id(method_def.id))
                }
            };
            let Some(def) = typed.def_table.lookup_def(def_id) else {
                continue;
            };
            if !def.is_public() {
                continue;
            }

            let all_type_params = receiver_type_params
                .iter()
                .cloned()
                .chain(sig.type_params.iter().cloned())
                .collect::<Vec<_>>();
            let type_param_map = if all_type_params.is_empty() {
                None
            } else {
                Some(
                    all_type_params
                        .iter()
                        .enumerate()
                        .map(|(index, param)| {
                            (typed.def_table.def_id(param.id), TyVarId::new(index as u32))
                        })
                        .collect::<HashMap<_, _>>(),
                )
            };

            let mut params = Vec::with_capacity(sig.params.len());
            let mut failed = false;
            for param in &sig.params {
                match resolve_type_expr_with_params(
                    &typed.def_table,
                    typed,
                    &param.typ,
                    type_param_map.as_ref(),
                ) {
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
                &typed.def_table,
                typed,
                &sig.ret_ty_expr,
                type_param_map.as_ref(),
            ) else {
                continue;
            };

            out.entry(owner_symbol_id.clone())
                .or_default()
                .entry(sig.name.clone())
                .or_default()
                .push(ImportedMethodSig {
                    self_ty: self_ty.clone(),
                    self_mode: sig.self_param.mode.clone(),
                    params,
                    ret_ty,
                    type_param_count: all_type_params.len(),
                    type_param_var_names: all_type_params
                        .iter()
                        .enumerate()
                        .map(|(index, param)| (index as u32, param.ident.clone()))
                        .collect(),
                    type_param_bounds: all_type_params
                        .iter()
                        .map(|param| param.bound.as_ref().map(|bound| bound.name.clone()))
                        .collect(),
                });
        }
    }
    out
}

fn method_block_owner_symbol_id(
    typed: &TypeCheckedContext,
    method_block: &MethodBlock,
) -> Option<SymbolId> {
    let type_def = typed
        .module
        .type_defs()
        .into_iter()
        .find(|type_def| type_def.name == method_block.type_name)?;
    let def_id = typed.def_table.def_id(type_def.id);
    typed.symbol_ids.lookup_symbol_id(def_id).cloned()
}

fn method_block_type_params(
    typed: &TypeCheckedContext,
    method_block: &MethodBlock,
) -> Vec<TypeParam> {
    method_block
        .type_args
        .iter()
        .filter_map(|type_arg| {
            let TypeExprKind::Named { ident, type_args } = &type_arg.kind else {
                return None;
            };
            if !type_args.is_empty() {
                return None;
            }
            let def_id = typed.def_table.lookup_node_def_id(type_arg.id)?;
            let def = typed.def_table.lookup_def(def_id)?;
            if !matches!(def.kind, crate::core::resolve::DefKind::TypeParam) {
                return None;
            }
            Some(TypeParam {
                id: type_arg.id,
                ident: ident.clone(),
                bound: None,
                span: type_arg.span,
            })
        })
        .collect()
}

fn resolve_method_block_self_type(
    typed: &TypeCheckedContext,
    method_block: &MethodBlock,
    receiver_type_params: &[TypeParam],
) -> Option<Type> {
    let type_param_map = if receiver_type_params.is_empty() {
        None
    } else {
        Some(
            receiver_type_params
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    (typed.def_table.def_id(param.id), TyVarId::new(index as u32))
                })
                .collect::<HashMap<_, _>>(),
        )
    };
    let self_ty_expr = TypeExpr {
        id: method_block.id,
        kind: TypeExprKind::Named {
            ident: method_block.type_name.clone(),
            type_args: method_block.type_args.clone(),
        },
        span: method_block.span,
    };
    resolve_type_expr_with_params(
        &typed.def_table,
        typed,
        &self_ty_expr,
        type_param_map.as_ref(),
    )
    .ok()
}

fn collect_public_trait_sigs(
    _module_id: ModuleId,
    typed: &TypeCheckedContext,
) -> HashMap<SymbolId, ImportedTraitSig> {
    let mut out = HashMap::<SymbolId, ImportedTraitSig>::new();
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

        let Some(symbol_id) = typed.symbol_ids.lookup_symbol_id(def_id).cloned() else {
            continue;
        };
        out.insert(
            symbol_id,
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
