//! Capsule-level import/export binding helpers.
//!
//! This pass computes, for each module:
//! - its exported symbol surface (with visibility metadata), and
//! - alias-to-module bindings for its `requires` entries.
//!
//! Downstream stages (currently flattening) consume these bindings instead of
//! rebuilding ad-hoc alias maps.

use std::collections::HashMap;

use crate::core::capsule::{ModuleId, ModulePath, RequireKind};
use crate::core::context::CapsuleParsedContext;
use crate::core::tree::parsed;

#[derive(Clone, Copy, Default)]
pub(crate) struct MemberAttrs {
    pub public: bool,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct TypeMemberAttrs {
    pub public: bool,
    pub opaque: bool,
}

#[derive(Clone, Default)]
pub(crate) struct ModuleExports {
    pub callables: HashMap<String, MemberAttrs>,
    pub types: HashMap<String, TypeMemberAttrs>,
    pub traits: HashMap<String, MemberAttrs>,
}

impl ModuleExports {
    fn from_module(module: &parsed::Module) -> Self {
        let mut out = Self::default();
        for item in &module.top_level_items {
            match item {
                parsed::TopLevelItem::FuncDecl(func_decl) => {
                    let member = out.callables.entry(func_decl.sig.name.clone()).or_default();
                    member.public |= has_public_attr(&func_decl.attrs);
                }
                parsed::TopLevelItem::FuncDef(func_def) => {
                    let member = out.callables.entry(func_def.sig.name.clone()).or_default();
                    member.public |= has_public_attr(&func_def.attrs);
                }
                parsed::TopLevelItem::TypeDef(type_def) => {
                    out.types
                        .insert(type_def.name.clone(), type_member_attrs(&type_def.attrs));
                }
                parsed::TopLevelItem::TraitDef(trait_def) => {
                    let member = out.traits.entry(trait_def.name.clone()).or_default();
                    member.public |= has_public_attr(&trait_def.attrs);
                }
                _ => {}
            }
        }
        out
    }
}

#[derive(Clone)]
pub(crate) struct AliasSymbols {
    pub module_path: ModulePath,
    pub callables: HashMap<String, MemberAttrs>,
    pub types: HashMap<String, TypeMemberAttrs>,
    pub traits: HashMap<String, MemberAttrs>,
}

#[derive(Clone, Default)]
pub(crate) struct CapsuleBindings {
    alias_symbols_by_module: HashMap<ModuleId, HashMap<String, AliasSymbols>>,
    exports_by_module: HashMap<ModuleId, ModuleExports>,
}

impl CapsuleBindings {
    pub(crate) fn build(program: &CapsuleParsedContext) -> Self {
        let mut exports_by_module = HashMap::<ModuleId, ModuleExports>::new();
        for module_id in program.dependency_order_from_entry() {
            if let Some(parsed) = program.module(module_id) {
                exports_by_module.insert(module_id, ModuleExports::from_module(&parsed.module));
            }
        }

        let mut alias_symbols_by_module = HashMap::new();
        for module_id in program.dependency_order_from_entry() {
            let Some(parsed) = program.module(module_id) else {
                continue;
            };
            let mut aliases = HashMap::new();
            for req in &parsed.requires {
                if req.kind != RequireKind::Module {
                    continue;
                }
                if let Some(dep_id) = program.capsule.by_path.get(&req.module_path)
                    && let Some(dep_exports) = exports_by_module.get(dep_id)
                {
                    aliases.insert(
                        req.alias.clone(),
                        AliasSymbols {
                            module_path: req.module_path.clone(),
                            callables: dep_exports.callables.clone(),
                            types: dep_exports.types.clone(),
                            traits: dep_exports.traits.clone(),
                        },
                    );
                }
            }
            alias_symbols_by_module.insert(module_id, aliases);
        }

        Self {
            alias_symbols_by_module,
            exports_by_module,
        }
    }

    pub(crate) fn alias_symbols_for(&self, module_id: ModuleId) -> HashMap<String, AliasSymbols> {
        self.alias_symbols_by_module
            .get(&module_id)
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn exports_for(&self, module_id: ModuleId) -> Option<&ModuleExports> {
        self.exports_by_module.get(&module_id)
    }
}

fn has_public_attr(attrs: &[parsed::Attribute]) -> bool {
    attrs
        .iter()
        .any(|attr| attr.name == "public" || attr.name == "opaque")
}

fn type_member_attrs(attrs: &[parsed::Attribute]) -> TypeMemberAttrs {
    TypeMemberAttrs {
        public: has_public_attr(attrs),
        opaque: attrs.iter().any(|attr| attr.name == "opaque"),
    }
}
