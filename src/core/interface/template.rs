//! Generic template model for imported monomorphization.
//!
//! This module defines the first compiler-owned representation for generic body
//! transport across module boundaries. It is intentionally narrower than the
//! final `.mci` payload: this slice establishes the identity/topology contract
//! (root template, interface-local def ids, local vs external references,
//! owner/type-param structure) before we commit to a concrete serialized body
//! format.
//!
//! The key boundary is:
//! - external references are always addressed by [`SymbolId`]
//! - internal references within one imported template graph are always
//!   addressed by [`TemplateDefId`]
//!
//! That split lets the consumer monomorphize without name resolution while
//! keeping producer-local `DefId`/`NodeId` details out of the interface
//! contract.

use serde::{Deserialize, Serialize};

use crate::core::symbol_id::SymbolId;

use super::{CallableSignature, MethodSignature, TraitDefExport, TypeDefExport};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TemplateDefId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TemplateTypeParamId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TemplateBindingId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TemplateSiteId(pub u32);

/// A self-contained generic template graph rooted at one exported generic
/// callable. Compile-time closure defs live alongside the root in `defs`, while
/// concrete helper dependencies appear as `LinkTimeCallable` defs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericTemplateGraph {
    pub root: TemplateDefId,
    pub defs: Vec<TemplateDef>,
}

impl GenericTemplateGraph {
    pub fn root_def(&self) -> Option<&TemplateDef> {
        self.def(self.root)
    }

    pub fn def(&self, id: TemplateDefId) -> Option<&TemplateDef> {
        self.defs.iter().find(|def| def.id == id)
    }

    pub fn local_dependencies(&self) -> impl Iterator<Item = TemplateDefId> + '_ {
        self.defs.iter().flat_map(|def| {
            def.references()
                .filter_map(|reference| match reference.target {
                    TemplateReferenceTarget::Local(id) => Some(id),
                    TemplateReferenceTarget::External(_) => None,
                })
        })
    }

    pub fn external_dependencies(&self) -> impl Iterator<Item = &SymbolId> + '_ {
        self.defs.iter().flat_map(|def| {
            def.references()
                .filter_map(|reference| match &reference.target {
                    TemplateReferenceTarget::External(symbol_id) => Some(symbol_id),
                    TemplateReferenceTarget::Local(_) => None,
                })
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateDef {
    pub id: TemplateDefId,
    /// Present when this template def corresponds to a globally addressable
    /// source definition; absent for purely internal/nested template defs.
    pub symbol_id: Option<SymbolId>,
    pub kind: TemplateDefKind,
}

impl TemplateDef {
    pub fn references(&self) -> impl Iterator<Item = &TemplateReference> {
        match &self.kind {
            TemplateDefKind::Function(template) => template.body.references.iter(),
            TemplateDefKind::Method(template) => template.body.references.iter(),
            TemplateDefKind::Closure(template) => template.body.references.iter(),
            TemplateDefKind::Type(template) => template.references.iter(),
            TemplateDefKind::Trait(template) => template.references.iter(),
            TemplateDefKind::LinkTimeCallable(template) => template.references.iter(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateDefKind {
    Function(GenericFunctionTemplate),
    Method(GenericMethodTemplate),
    Closure(GenericClosureTemplate),
    Type(GenericTypeTemplate),
    Trait(GenericTraitTemplate),
    LinkTimeCallable(LinkTimeCallableTemplate),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateTypeParam {
    pub id: TemplateTypeParamId,
    pub name: String,
    pub bound: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericFunctionTemplate {
    pub signature: CallableSignature,
    pub type_params: Vec<TemplateTypeParam>,
    pub body: TemplateBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericMethodTemplate {
    pub owner_type: String,
    /// Type params contributed by the generic owner (`Type<T> :: { ... }`).
    pub owner_type_params: Vec<TemplateTypeParam>,
    /// Type params declared directly on the method itself.
    pub method_type_params: Vec<TemplateTypeParam>,
    pub signature: MethodSignature,
    pub body: TemplateBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericClosureTemplate {
    pub captures: Vec<TemplateBindingId>,
    pub body: TemplateBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericTypeTemplate {
    pub export: TypeDefExport,
    pub references: Vec<TemplateReference>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenericTraitTemplate {
    pub export: TraitDefExport,
    pub references: Vec<TemplateReference>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LinkTimeCallableTemplate {
    pub signature: CallableSignature,
    pub link_symbol: String,
    pub references: Vec<TemplateReference>,
}

/// Transitional body representation for the template model.
///
/// This deliberately captures the minimal cloneable payload shape the current
/// monomorphizer depends on:
/// - parameter/local binding identities
/// - nested closure ownership
/// - call/type substitution anchor sites
/// - iterable-parameter instantiation slots
/// - dependency references
///
/// A later step will extend this with the actual typed statement/expression
/// payload that specialization clones and substitutes, but keeping these
/// structural anchors explicit now gives the interface format a stable contract
/// to grow into.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct TemplateBody {
    pub params: Vec<TemplateBinding>,
    pub locals: Vec<TemplateBinding>,
    pub nested_closures: Vec<TemplateNestedClosure>,
    pub call_sites: Vec<TemplateCallSite>,
    pub type_sites: Vec<TemplateTypeSite>,
    pub iterable_param_slots: Vec<TemplateIterableParamSlot>,
    pub references: Vec<TemplateReference>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateBinding {
    pub id: TemplateBindingId,
    pub name: String,
    pub kind: TemplateBindingKind,
    pub ty: Option<super::TypeKey>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateBindingKind {
    Param,
    LocalVar,
    Capture,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateNestedClosure {
    pub def: TemplateDefId,
    pub captures: Vec<TemplateBindingId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateCallSite {
    pub site: TemplateSiteId,
    pub callee: TemplateReferenceTarget,
    pub explicit_type_arg_count: usize,
    pub iterable_arg_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateTypeSite {
    pub site: TemplateSiteId,
    pub role: TemplateTypeSiteRole,
    pub ty: super::TypeKey,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateTypeSiteRole {
    Param,
    Return,
    Local,
    Expr,
    Capture,
    OwnerTypeArg,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateIterableParamSlot {
    /// Position in `GenericInst.iterable_param_tys`.
    pub inst_index: usize,
    /// Parameter binding that receives the concrete iterable witness type.
    pub binding: TemplateBindingId,
    /// Exposed item type recorded at the source surface.
    pub item_ty: super::TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TemplateReference {
    pub target: TemplateReferenceTarget,
    pub kind: TemplateReferenceKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateReferenceTarget {
    Local(TemplateDefId),
    External(SymbolId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateReferenceKind {
    Callable,
    Type,
    Trait,
    Value,
}
