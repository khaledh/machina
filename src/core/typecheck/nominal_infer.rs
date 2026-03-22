//! Nominal-key inference helpers used after solver output is materialized.
//!
//! This logic does not participate in constraint solving itself. It looks at
//! already-resolved structural types and recovers the nominal instantiation
//! they correspond to so the finalized type map can retain nominal identity.

use std::collections::HashMap;

use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::*;
use crate::core::context::ResolvedContext;
use crate::core::resolve::{DefId, DefTable, ImportedFacts};
use crate::core::typecheck::nominal::NominalKey;
use crate::core::typecheck::template_bind::bind_template_type_vars;
use crate::core::typecheck::type_map::{
    TypeDefLookup, TypeMap, resolve_type_def_with_args, resolve_type_expr,
};
use crate::core::typecheck::utils::nominal_key_concreteness;
use crate::core::types::{TyVarId, Type};

#[derive(Debug, Clone)]
struct NominalTemplate {
    def_id: DefId,
    param_count: usize,
    ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct NominalKeyResolver {
    explicit_nominal_keys: HashMap<String, NominalKey>,
    nominal_templates: Vec<NominalTemplate>,
}

impl NominalKeyResolver {
    pub(crate) fn new(resolved: &ResolvedContext, imported_facts: &ImportedFacts) -> Self {
        Self {
            explicit_nominal_keys: collect_explicit_nominal_keys(resolved, imported_facts),
            nominal_templates: collect_nominal_templates(resolved),
        }
    }

    pub(crate) fn infer(&self, ty: &Type) -> Option<NominalKey> {
        let nominal_name = match ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name.as_str(),
            _ => return None,
        };

        if let Some(key) = self.explicit_nominal_keys.get(nominal_name) {
            return Some(key.clone());
        }

        infer_nominal_key_from_templates(ty, &self.nominal_templates)
    }

    pub(crate) fn hydrate(&self, type_map: &mut TypeMap) {
        let entries = type_map
            .type_table()
            .entries()
            .map(|(id, ty)| (id, ty.clone()))
            .collect::<Vec<_>>();

        for (type_id, ty) in entries {
            if type_map.lookup_nominal_key_for_type_id(type_id).is_some() {
                continue;
            }
            let Some(key) = self.infer(&ty) else {
                continue;
            };
            type_map.record_nominal_key_for_type_id(type_id, key);
        }
    }
}

pub(crate) fn infer_type_args_from_instance(
    template: &Type,
    concrete: &Type,
    param_count: usize,
) -> Option<Vec<Type>> {
    let bindings = bind_template_type_vars(template, concrete)?;

    let mut args = Vec::with_capacity(param_count);
    for index in 0..param_count {
        let var = TyVarId::new(index as u32);
        let arg = bindings.get(&var)?.clone();
        args.push(arg);
    }
    Some(args)
}

fn collect_nominal_templates(resolved: &ResolvedContext) -> Vec<NominalTemplate> {
    let mut templates = Vec::new();
    for type_def in resolved.module.type_defs() {
        if !matches!(
            type_def.kind,
            TypeDefKind::Struct { .. } | TypeDefKind::Enum { .. }
        ) {
            continue;
        }
        let param_count = type_def.type_params.len();
        let type_args = (0..param_count)
            .map(|i| Type::Var(TyVarId::new(i as u32)))
            .collect::<Vec<_>>();
        let Ok(ty) = resolve_type_def_with_args(
            &resolved.def_table,
            &resolved.module,
            resolved.def_table.def_id(type_def.id),
            &type_args,
        ) else {
            continue;
        };
        if !matches!(ty, Type::Struct { .. } | Type::Enum { .. }) {
            continue;
        }
        templates.push(NominalTemplate {
            def_id: resolved.def_table.def_id(type_def.id),
            param_count,
            ty,
        });
    }
    templates
}

fn infer_nominal_key_from_templates(
    ty: &Type,
    nominal_templates: &[NominalTemplate],
) -> Option<NominalKey> {
    let mut candidates = Vec::new();
    for template in nominal_templates {
        let Some(type_args) = infer_type_args_from_instance(&template.ty, ty, template.param_count)
        else {
            continue;
        };
        candidates.push(NominalKey::new(template.def_id, type_args));
    }

    if candidates.is_empty() {
        return None;
    }

    candidates.sort_by_key(nominal_key_concreteness);
    let best = candidates.pop().expect("checked non-empty");
    if candidates
        .iter()
        .any(|other| nominal_key_concreteness(other) == nominal_key_concreteness(&best))
    {
        // Ambiguous structural match across multiple nominal defs.
        return None;
    }
    Some(best)
}

#[derive(Debug, Clone)]
struct ExplicitNominalUse {
    def_id: DefId,
    type_args: Vec<TypeExpr>,
}

struct ExplicitNominalCollector<'a> {
    def_table: &'a DefTable,
    uses: Vec<ExplicitNominalUse>,
}

impl<'a> ExplicitNominalCollector<'a> {
    fn collect(def_table: &'a DefTable, module: &Module) -> Vec<ExplicitNominalUse> {
        let mut collector = Self {
            def_table,
            uses: Vec::new(),
        };
        collector.visit_module(module);
        collector.uses
    }

    fn push_use(&mut self, def_id: DefId, type_args: &[TypeExpr]) {
        self.uses.push(ExplicitNominalUse {
            def_id,
            type_args: type_args.to_vec(),
        });
    }
}

impl Visitor for ExplicitNominalCollector<'_> {
    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        if let TypeExprKind::Named { type_args, .. } = &type_expr.kind
            && let Some(def_id) = self.def_table.lookup_node_def_id(type_expr.id)
        {
            self.push_use(def_id, type_args);
        }
        visit::walk_type_expr(self, type_expr);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::StructLit { type_args, .. } | ExprKind::EnumVariant { type_args, .. } => {
                if let Some(def_id) = self.def_table.lookup_node_def_id(expr.id) {
                    self.push_use(def_id, type_args);
                }
            }
            _ => {}
        }
        visit::walk_expr(self, expr);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        if let MatchPattern::EnumVariant {
            id,
            enum_name: Some(_),
            type_args,
            ..
        } = pattern
            && let Some(def_id) = self.def_table.lookup_node_def_id(*id)
        {
            self.push_use(def_id, type_args);
        }
        visit::walk_match_pattern(self, pattern);
    }
}

struct ResolvedTypeLookup<'a> {
    context: &'a ResolvedContext,
    imported_facts: &'a ImportedFacts,
}

impl TypeDefLookup for ResolvedTypeLookup<'_> {
    fn type_def_by_id(&self, _def_table: &DefTable, def_id: DefId) -> Option<&TypeDef> {
        self.context
            .module
            .type_def_by_id(&self.context.def_table, def_id)
    }

    fn imported_type_by_id(&self, def_id: DefId) -> Option<&Type> {
        self.imported_facts.imported_type(def_id)
    }
}

fn collect_explicit_nominal_keys(
    resolved: &ResolvedContext,
    imported_facts: &ImportedFacts,
) -> HashMap<String, NominalKey> {
    let mut out = HashMap::new();
    let uses = ExplicitNominalCollector::collect(&resolved.def_table, &resolved.module);
    let type_lookup = ResolvedTypeLookup {
        context: resolved,
        imported_facts,
    };

    for usage in uses {
        let Some(type_def) = resolved
            .module
            .type_def_by_id(&resolved.def_table, usage.def_id)
        else {
            continue;
        };
        let is_nominal = matches!(
            type_def.kind,
            TypeDefKind::Struct { .. } | TypeDefKind::Enum { .. }
        );
        if !is_nominal {
            continue;
        }

        let resolved_args = if usage.type_args.is_empty() {
            if type_def.type_params.is_empty() {
                Vec::new()
            } else {
                continue;
            }
        } else {
            let mut args = Vec::with_capacity(usage.type_args.len());
            let mut ok = true;
            for arg in &usage.type_args {
                match resolve_type_expr(&resolved.def_table, &type_lookup, arg) {
                    Ok(ty) => args.push(ty),
                    Err(_) => {
                        ok = false;
                        break;
                    }
                }
            }
            if !ok || args.len() != type_def.type_params.len() {
                continue;
            }
            args
        };

        let Ok(inst_ty) = resolve_type_def_with_args(
            &resolved.def_table,
            &type_lookup,
            usage.def_id,
            &resolved_args,
        ) else {
            continue;
        };
        let inst_name = match inst_ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name,
            _ => continue,
        };
        let key = NominalKey::new(usage.def_id, resolved_args);
        if let Some(existing) = out.get(&inst_name) {
            debug_assert_eq!(existing, &key);
            continue;
        }
        out.insert(inst_name, key);
    }

    out
}
