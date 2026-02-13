use std::collections::HashMap;

use crate::core::capsule::{
    CapsuleParsed, ModuleId, ModulePath, ParsedModule as CapsuleModule, RequireKind,
};
use crate::core::resolve::{DefId, DefKind, DefTable, GlobalDefId};
use crate::core::tree::{NodeId, NodeIdGen};

/// Capsule-level parsed context produced by module discovery/parsing.
///
/// This keeps module-graph data in one place so later resolve/typecheck work
/// can consume a stable capsule abstraction rather than ad-hoc maps.
#[derive(Clone)]
pub struct CapsuleParsedContext {
    pub capsule: CapsuleParsed,
}

/// Stage contract alias: capsule resolve input.
pub type CapsuleResolveStageInput = CapsuleParsedContext;

impl CapsuleParsedContext {
    pub fn new(capsule: CapsuleParsed) -> Self {
        Self { capsule }
    }

    pub fn entry(&self) -> ModuleId {
        self.capsule.entry
    }

    pub fn entry_module(&self) -> &CapsuleModule {
        self.capsule.entry_module()
    }

    pub fn module(&self, id: ModuleId) -> Option<&CapsuleModule> {
        self.capsule.module(id)
    }

    pub fn dependency_order_from_entry(&self) -> Vec<ModuleId> {
        self.capsule.dependency_order_from_entry()
    }

    pub fn next_node_id_gen(&self) -> &NodeIdGen {
        &self.capsule.next_node_id_gen
    }
}

/// Capsule-level resolved context keyed by module id.
#[derive(Clone)]
pub struct CapsuleResolvedContext {
    pub entry: ModuleId,
    pub modules: HashMap<ModuleId, super::ResolvedContext>,
    pub by_path: HashMap<ModulePath, ModuleId>,
    pub edges: HashMap<ModuleId, Vec<ModuleId>>,
    pub top_level_owners: HashMap<NodeId, ModuleId>,
    pub export_facts_by_module: HashMap<ModuleId, ModuleExportFacts>,
    pub import_env_by_module: HashMap<ModuleId, ImportEnv>,
}

/// Stage contract alias: capsule resolve output.
pub type CapsuleResolveStageOutput = CapsuleResolvedContext;

impl CapsuleResolvedContext {
    pub fn entry_module(&self) -> &super::ResolvedContext {
        self.modules
            .get(&self.entry)
            .expect("resolved entry module should exist")
    }

    pub fn module(&self, id: ModuleId) -> Option<&super::ResolvedContext> {
        self.modules.get(&id)
    }

    pub fn global_def_id(&self, module_id: ModuleId, def_id: DefId) -> GlobalDefId {
        GlobalDefId::new(module_id, def_id)
    }

    pub fn export_facts(&self, module_id: ModuleId) -> Option<&ModuleExportFacts> {
        self.export_facts_by_module.get(&module_id)
    }

    pub fn import_env(&self, module_id: ModuleId) -> Option<&ImportEnv> {
        self.import_env_by_module.get(&module_id)
    }

    pub fn imported_symbol_binding(
        &self,
        module_id: ModuleId,
        symbol: &str,
    ) -> Option<&ImportedSymbolBinding> {
        self.import_env(module_id)
            .and_then(|env| env.symbol_aliases.get(symbol))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedSymbolBinding {
    pub module_id: ModuleId,
    pub module_path: ModulePath,
    pub callables: Vec<GlobalDefId>,
    pub type_def: Option<GlobalDefId>,
    pub trait_def: Option<GlobalDefId>,
}

impl ImportedSymbolBinding {
    pub fn is_empty(&self) -> bool {
        self.callables.is_empty() && self.type_def.is_none() && self.trait_def.is_none()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleExportFacts {
    pub module_id: ModuleId,
    pub module_path: Option<ModulePath>,
    pub callables: HashMap<String, Vec<GlobalDefId>>,
    pub types: HashMap<String, GlobalDefId>,
    pub traits: HashMap<String, GlobalDefId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleImportBinding {
    pub module_id: ModuleId,
    pub module_path: ModulePath,
    pub exports: ModuleExportFacts,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ImportEnv {
    pub module_aliases: HashMap<String, ModuleImportBinding>,
    pub symbol_aliases: HashMap<String, ImportedSymbolBinding>,
}

pub fn module_export_facts_from_def_table(
    module_id: ModuleId,
    module_path: Option<ModulePath>,
    def_table: &DefTable,
) -> ModuleExportFacts {
    let mut facts = ModuleExportFacts {
        module_id,
        module_path,
        callables: HashMap::new(),
        types: HashMap::new(),
        traits: HashMap::new(),
    };
    for def in def_table.defs() {
        if !def.is_public() {
            continue;
        }
        match def.kind {
            DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => {
                facts
                    .callables
                    .entry(def.name.clone())
                    .or_default()
                    .push(GlobalDefId::new(module_id, def.id));
            }
            DefKind::TypeDef { .. } => {
                facts
                    .types
                    .entry(def.name.clone())
                    .or_insert_with(|| GlobalDefId::new(module_id, def.id));
            }
            DefKind::TraitDef { .. } => {
                facts
                    .traits
                    .entry(def.name.clone())
                    .or_insert_with(|| GlobalDefId::new(module_id, def.id));
            }
            _ => {}
        }
    }
    for overloads in facts.callables.values_mut() {
        overloads.sort_by_key(|id| id.def_id);
        overloads.dedup();
    }
    facts
}

pub fn imported_symbol_binding_from_exports(
    dep_module_id: ModuleId,
    dep_module_path: &ModulePath,
    dep_exports: &ModuleExportFacts,
    member: &str,
) -> ImportedSymbolBinding {
    ImportedSymbolBinding {
        module_id: dep_module_id,
        module_path: dep_module_path.clone(),
        callables: dep_exports
            .callables
            .get(member)
            .cloned()
            .unwrap_or_default(),
        type_def: dep_exports.types.get(member).copied(),
        trait_def: dep_exports.traits.get(member).copied(),
    }
}

pub fn import_env_from_requires(
    program: &CapsuleParsedContext,
    module_id: ModuleId,
    exports_by_module: &HashMap<ModuleId, ModuleExportFacts>,
) -> ImportEnv {
    let mut out = ImportEnv::default();
    let Some(parsed) = program.module(module_id) else {
        return out;
    };
    for req in &parsed.requires {
        let Some(dep_module_id) = program.capsule.by_path.get(&req.module_path).copied() else {
            continue;
        };
        let Some(dep_exports) = exports_by_module.get(&dep_module_id) else {
            continue;
        };
        match req.kind {
            RequireKind::Module => {
                out.module_aliases.insert(
                    req.alias.clone(),
                    ModuleImportBinding {
                        module_id: dep_module_id,
                        module_path: req.module_path.clone(),
                        exports: dep_exports.clone(),
                    },
                );
            }
            RequireKind::Symbol => {
                let Some(member) = &req.member else {
                    continue;
                };
                let binding = imported_symbol_binding_from_exports(
                    dep_module_id,
                    &req.module_path,
                    dep_exports,
                    member,
                );
                if !binding.is_empty() {
                    out.symbol_aliases.insert(req.alias.clone(), binding);
                }
            }
        }
    }
    out
}
