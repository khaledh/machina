//! Shared analysis result schemas and lookup APIs.
//!
//! These result objects are query-friendly immutable products that mirror
//! parser/resolve/typecheck outputs. Lookup traits are implemented for both
//! result objects and existing batch contexts to keep one API surface for
//! CLI and IDE consumers.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::core::capsule::{ModuleId, ParsedModule};
use crate::core::context::{ResolvedContext, ResolvedTables, TypeCheckedContext, TypedTables};
use crate::core::diag::Span;
use crate::core::resolve::{Def, DefId, DefTable};
use crate::core::symtab::SymbolTable;
use crate::core::tree::NodeId;
use crate::core::tree::NodeIdGen;
use crate::core::tree::resolved::Module as ResolvedModule;
use crate::core::tree::typed::Module as TypedModule;
use crate::core::typecheck::type_map::{CallSig, CallSigMap, GenericInstMap, TypeMap};
use crate::core::types::Type;
use crate::services::analysis::snapshot::FileId;

#[derive(Clone)]
pub struct ParsedModuleResult {
    pub parsed: ParsedModule,
}

impl ParsedModuleResult {
    pub fn new(parsed: ParsedModule) -> Self {
        Self { parsed }
    }

    pub fn module_id(&self) -> ModuleId {
        self.parsed.source.id
    }
}

#[derive(Clone)]
pub struct ResolvedModuleResult {
    pub module_id: ModuleId,
    pub module: ResolvedModule,
    pub def_table: DefTable,
    pub def_owners: HashMap<DefId, ModuleId>,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub typestate_role_impls: Vec<crate::core::context::TypestateRoleImplBinding>,
    pub protocol_index: crate::core::protocol::ProtocolIndex,
}

impl ResolvedModuleResult {
    pub fn from_context(module_id: ModuleId, context: ResolvedContext) -> Self {
        let ResolvedContext {
            module,
            payload: tables,
        } = context;
        let ResolvedTables {
            def_table,
            def_owners,
            symbols,
            node_id_gen,
            typestate_role_impls,
            protocol_index,
        } = tables;
        Self {
            module_id,
            module,
            def_table,
            def_owners,
            symbols,
            node_id_gen,
            typestate_role_impls,
            protocol_index,
        }
    }

    pub fn into_context(self) -> ResolvedContext {
        ResolvedContext {
            module: self.module,
            payload: ResolvedTables {
                def_table: self.def_table,
                def_owners: self.def_owners,
                symbols: self.symbols,
                node_id_gen: self.node_id_gen,
                typestate_role_impls: self.typestate_role_impls,
                protocol_index: self.protocol_index,
            },
        }
    }
}

#[derive(Clone)]
pub struct TypedModuleResult {
    pub module_id: ModuleId,
    pub module: TypedModule,
    pub def_table: DefTable,
    pub def_owners: HashMap<DefId, ModuleId>,
    pub type_map: TypeMap,
    pub call_sigs: CallSigMap,
    pub generic_insts: GenericInstMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub typestate_role_impls: Vec<crate::core::context::TypestateRoleImplBinding>,
    pub protocol_index: crate::core::protocol::ProtocolIndex,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HoverInfo {
    pub node_id: NodeId,
    pub span: Span,
    pub def_id: Option<DefId>,
    pub def_name: Option<String>,
    pub ty: Option<Type>,
    pub display: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompletionKind {
    Function,
    Type,
    Trait,
    Variable,
    Parameter,
    TypeParameter,
    EnumVariant,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionKind,
    pub def_id: DefId,
    pub detail: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignatureHelp {
    pub label: String,
    pub def_id: Option<DefId>,
    pub active_parameter: usize,
    pub parameters: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DocumentSymbolKind {
    Type,
    Trait,
    Function,
    Method,
    Property,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentSymbol {
    pub name: String,
    pub kind: DocumentSymbolKind,
    pub def_id: DefId,
    pub span: Span,
    pub detail: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticTokenKind {
    Type,
    Trait,
    Function,
    Method,
    Property,
    Variable,
    Parameter,
    TypeParameter,
    EnumVariant,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemanticToken {
    pub span: Span,
    pub kind: SemanticTokenKind,
    pub def_id: DefId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CodeActionKind {
    QuickFix,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextEdit {
    pub span: Span,
    pub new_text: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodeAction {
    pub title: String,
    pub kind: CodeActionKind,
    pub diagnostic_code: String,
    pub edits: Vec<TextEdit>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub file_id: FileId,
    pub path: Option<PathBuf>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RenameEdit {
    pub location: Location,
    pub replacement: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RenameConflict {
    pub message: String,
    pub existing_def: Option<DefId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RenamePlan {
    pub def_id: DefId,
    pub old_name: Option<String>,
    pub new_name: String,
    pub edits: Vec<RenameEdit>,
    pub conflicts: Vec<RenameConflict>,
}

impl RenamePlan {
    pub fn can_apply(&self) -> bool {
        !self.edits.is_empty() && self.conflicts.is_empty()
    }
}

impl TypedModuleResult {
    pub fn from_context(module_id: ModuleId, context: TypeCheckedContext) -> Self {
        let TypeCheckedContext {
            module,
            payload: tables,
        } = context;
        let TypedTables {
            resolved,
            type_map,
            call_sigs,
            generic_insts,
        } = tables;
        let ResolvedTables {
            def_table,
            def_owners,
            symbols,
            node_id_gen,
            typestate_role_impls,
            protocol_index,
        } = resolved;
        Self {
            module_id,
            module,
            def_table,
            def_owners,
            type_map,
            call_sigs,
            generic_insts,
            symbols,
            node_id_gen,
            typestate_role_impls,
            protocol_index,
        }
    }

    pub fn into_context(self) -> TypeCheckedContext {
        TypeCheckedContext {
            module: self.module,
            payload: TypedTables {
                resolved: ResolvedTables {
                    def_table: self.def_table,
                    def_owners: self.def_owners,
                    symbols: self.symbols,
                    node_id_gen: self.node_id_gen,
                    typestate_role_impls: self.typestate_role_impls,
                    protocol_index: self.protocol_index,
                },
                type_map: self.type_map,
                call_sigs: self.call_sigs,
                generic_insts: self.generic_insts,
            },
        }
    }
}

/// Common symbol/definition lookup surface shared by contexts and query results.
pub trait SymbolLookup {
    fn def_table(&self) -> &DefTable;

    fn lookup_def(&self, def_id: DefId) -> Option<&Def> {
        self.def_table().lookup_def(def_id)
    }

    fn lookup_def_id_by_node(&self, node_id: NodeId) -> Option<DefId> {
        self.def_table().lookup_node_def_id(node_id)
    }

    fn lookup_def_by_node(&self, node_id: NodeId) -> Option<&Def> {
        let def_id = self.lookup_def_id_by_node(node_id)?;
        self.lookup_def(def_id)
    }
}

/// Common type/call lookup surface shared by typed contexts and typed results.
pub trait TypeLookup: SymbolLookup {
    fn type_map(&self) -> &TypeMap;
    fn call_sig_map(&self) -> &CallSigMap;
    fn generic_inst_map(&self) -> &GenericInstMap;

    fn lookup_node_type(&self, node_id: NodeId) -> Option<Type> {
        self.type_map().lookup_node_type(node_id)
    }

    fn lookup_def_type(&self, def_id: DefId) -> Option<Type> {
        let def = self.lookup_def(def_id)?;
        self.type_map().lookup_def_type(def)
    }

    fn lookup_call_sig(&self, node_id: NodeId) -> Option<&CallSig> {
        self.call_sig_map().get(&node_id)
    }
}

impl SymbolLookup for ResolvedContext {
    fn def_table(&self) -> &DefTable {
        &self.def_table
    }
}

impl SymbolLookup for TypeCheckedContext {
    fn def_table(&self) -> &DefTable {
        &self.def_table
    }
}

impl SymbolLookup for ResolvedModuleResult {
    fn def_table(&self) -> &DefTable {
        &self.def_table
    }
}

impl SymbolLookup for TypedModuleResult {
    fn def_table(&self) -> &DefTable {
        &self.def_table
    }
}

impl TypeLookup for TypeCheckedContext {
    fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    fn call_sig_map(&self) -> &CallSigMap {
        &self.call_sigs
    }

    fn generic_inst_map(&self) -> &GenericInstMap {
        &self.generic_insts
    }
}

impl TypeLookup for TypedModuleResult {
    fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    fn call_sig_map(&self) -> &CallSigMap {
        &self.call_sigs
    }

    fn generic_inst_map(&self) -> &GenericInstMap {
        &self.generic_insts
    }
}

#[cfg(test)]
#[path = "../../tests/analysis/t_results.rs"]
mod tests;
