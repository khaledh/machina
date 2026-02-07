//! The core elaboration driver and shared state.
//!
//! The `Elaborator` struct holds all context needed during elaboration:
//! references to the definition table, type map, semantic analysis results,
//! and accumulated closure transformations. It provides the entry point
//! for module elaboration and shared helper methods used across the
//! elaboration submodules.

use std::collections::{HashMap, HashSet};

use crate::diag::Span;
use crate::resolve::DefId;
use crate::resolve::DefTable;
use crate::semck::closure::capture::CaptureMode;
use crate::semck::closure::capture::ClosureCapture;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{InitInfo, NodeId, NodeIdGen, ParamMode};
use crate::typecheck::type_map::{CallSigMap, TypeMap};
use crate::types::{Type, TypeId};

/// Information about a single captured variable in a closure's environment.
/// Used to build the closure struct fields and rewrite captured variable
/// accesses inside closure bodies.
#[derive(Clone, Debug)]
pub(super) struct CaptureField {
    pub(super) def_id: DefId,
    pub(super) name: String,
    pub(super) mode: CaptureMode,
    pub(super) base_ty: Type,
    pub(super) base_ty_id: TypeId,
    pub(super) field_ty: Type,
    pub(super) field_ty_id: TypeId,
    pub(super) field_ty_expr: sem::TypeExpr,
}

/// Complete metadata for a lifted closure, including its generated struct
/// type and the `invoke` method signature. Created once per closure definition
/// and reused for all references to that closure.
#[derive(Clone, Debug)]
pub(super) struct ClosureInfo {
    pub(super) type_name: String,
    pub(super) type_id: TypeId,
    pub(super) param_modes: Vec<ParamMode>,
    pub(super) ty: Type,
    pub(super) self_def_id: DefId,
    pub(super) captures: Vec<CaptureField>,
}

/// Active context when elaborating inside a closure body. Pushed onto
/// `closure_stack` when entering a closure and popped when exiting.
/// Used to rewrite references to captured variables as `env.<field>`.
#[derive(Clone, Debug)]
pub(super) struct ClosureContext {
    pub(super) self_def_id: DefId,
    pub(super) type_id: TypeId,
    pub(super) ty: Type,
    pub(super) captures: HashMap<DefId, CaptureField>,
}

impl ClosureContext {
    pub(super) fn new(info: &ClosureInfo) -> Self {
        let captures = info
            .captures
            .iter()
            .cloned()
            .map(|capture| (capture.def_id, capture))
            .collect();
        Self {
            self_def_id: info.self_def_id,
            type_id: info.type_id,
            ty: info.ty.clone(),
            captures,
        }
    }

    pub(super) fn capture_field(&self, def_id: DefId) -> Option<&CaptureField> {
        self.captures.get(&def_id)
    }
}

/// The main driver for the elaboration pass.
///
/// Holds references to shared compiler state (def table, type map, semantic
/// analysis results) plus mutable state accumulated during elaboration
/// (lifted closure types, method blocks, binding mappings).
///
/// The elaborator traverses the normalized tree recursively, transforming
/// each node into its semantic equivalent while applying the elaboration
/// rules defined in the submodules.
pub struct Elaborator<'a> {
    // Shared compiler state (borrowed)
    pub(super) def_table: &'a mut DefTable,
    pub(super) type_map: &'a mut TypeMap,
    pub(super) call_sigs: &'a CallSigMap,
    pub(super) node_id_gen: &'a mut NodeIdGen,

    // Semantic analysis results from semck
    pub(super) implicit_moves: &'a HashSet<NodeId>,
    pub(super) init_assigns: &'a HashSet<NodeId>,
    pub(super) full_init_assigns: &'a HashSet<NodeId>,
    pub(super) closure_captures: &'a HashMap<DefId, Vec<ClosureCapture>>,

    // Accumulated closure lifting results
    pub(super) closure_types: Vec<sem::TypeDef>,
    pub(super) closure_methods: Vec<sem::MethodBlock>,
    /// Captureless closures lowered as top-level functions.
    pub(super) closure_funcs: Vec<sem::FuncDef>,
    pub(super) closure_info: HashMap<DefId, ClosureInfo>,
    pub(super) closure_bindings: HashMap<DefId, DefId>,
    pub(super) closure_stack: Vec<ClosureContext>,
    /// Track captureless closures already emitted as functions.
    pub(super) closure_func_ids: HashSet<DefId>,
}

impl<'a> Elaborator<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        def_table: &'a mut DefTable,
        type_map: &'a mut TypeMap,
        call_sigs: &'a CallSigMap,
        node_id_gen: &'a mut NodeIdGen,
        implicit_moves: &'a HashSet<NodeId>,
        init_assigns: &'a HashSet<NodeId>,
        full_init_assigns: &'a HashSet<NodeId>,
        closure_captures: &'a HashMap<DefId, Vec<ClosureCapture>>,
    ) -> Self {
        Self {
            def_table,
            type_map,
            call_sigs,
            node_id_gen,
            implicit_moves,
            init_assigns,
            full_init_assigns,
            closure_captures,
            closure_types: Vec::new(),
            closure_methods: Vec::new(),
            closure_funcs: Vec::new(),
            closure_info: HashMap::new(),
            closure_bindings: HashMap::new(),
            closure_stack: Vec::new(),
            closure_func_ids: HashSet::new(),
        }
    }

    /// Elaborate an entire module, returning the transformed semantic tree.
    ///
    /// Processes all top-level items and appends any closure struct types
    /// and method blocks that were generated during elaboration.
    pub fn elaborate_module(&mut self, module: &norm::Module) -> sem::Module {
        self.closure_types.clear();
        self.closure_methods.clear();
        self.closure_funcs.clear();
        self.closure_info.clear();
        self.closure_bindings.clear();
        self.closure_stack.clear();
        self.closure_func_ids.clear();

        let mut top_level_items: Vec<_> = module
            .top_level_items
            .iter()
            .map(|item| self.elab_top_level_item(item))
            .collect();

        // Append lifted closure types, invoke methods, and captureless functions.
        top_level_items.extend(self.closure_types.drain(..).map(sem::TopLevelItem::TypeDef));
        top_level_items.extend(
            self.closure_methods
                .drain(..)
                .map(sem::TopLevelItem::MethodBlock),
        );
        top_level_items.extend(self.closure_funcs.drain(..).map(sem::TopLevelItem::FuncDef));
        sem::Module { top_level_items }
    }

    fn elab_top_level_item(&mut self, item: &norm::TopLevelItem) -> sem::TopLevelItem {
        match item {
            norm::TopLevelItem::TraitDef(def) => sem::TopLevelItem::TraitDef(def.clone()),
            norm::TopLevelItem::TypeDef(def) => sem::TopLevelItem::TypeDef(def.clone()),
            norm::TopLevelItem::FuncDecl(decl) => sem::TopLevelItem::FuncDecl(sem::FuncDecl {
                id: decl.id,
                def_id: decl.def_id,
                attrs: decl.attrs.clone(),
                sig: decl.sig.clone(),
                span: decl.span,
            }),
            norm::TopLevelItem::FuncDef(def) => sem::TopLevelItem::FuncDef(sem::FuncDef {
                id: def.id,
                def_id: def.def_id,
                attrs: def.attrs.clone(),
                sig: def.sig.clone(),
                body: self.elab_value(&def.body),
                span: def.span,
            }),
            norm::TopLevelItem::MethodBlock(block) => {
                sem::TopLevelItem::MethodBlock(sem::MethodBlock {
                    id: block.id,
                    type_name: block.type_name.clone(),
                    trait_name: block.trait_name.clone(),
                    method_items: block
                        .method_items
                        .iter()
                        .map(|method_item| match method_item {
                            norm::MethodItem::Decl(method_decl) => {
                                sem::MethodItem::Decl(self.elab_method_decl(method_decl))
                            }
                            norm::MethodItem::Def(method_def) => {
                                sem::MethodItem::Def(self.elab_method_def(method_def))
                            }
                        })
                        .collect(),
                    span: block.span,
                })
            }
            norm::TopLevelItem::ClosureDef(_) => {
                panic!("compiler bug: closure defs should not exist before elaborate")
            }
        }
    }

    fn elab_method_def(&mut self, def: &norm::MethodDef) -> sem::MethodDef {
        sem::MethodDef {
            id: def.id,
            def_id: def.def_id,
            attrs: def.attrs.clone(),
            sig: def.sig.clone(),
            body: self.elab_value(&def.body),
            span: def.span,
        }
    }

    fn elab_method_decl(&mut self, decl: &norm::MethodDecl) -> sem::MethodDecl {
        sem::MethodDecl {
            id: decl.id,
            def_id: decl.def_id,
            attrs: decl.attrs.clone(),
            sig: decl.sig.clone(),
            span: decl.span,
        }
    }

    /// Retrieve initialization status for an assignment target from semck.
    /// Used by lowering to determine whether an assignment is an initial
    /// write (for out-params) or a full initialization (for partial init).
    pub(super) fn init_info_for_id(&self, id: NodeId) -> InitInfo {
        InitInfo {
            is_init: self.init_assigns.contains(&id),
            promotes_full: self.full_init_assigns.contains(&id),
        }
    }

    /// Create a new value expression with a fresh node ID.
    /// Used when elaboration synthesizes new nodes (e.g., implicit moves,
    /// closure captures, desugared loops).
    pub(super) fn new_value(
        &mut self,
        kind: sem::ValueExprKind,
        ty: TypeId,
        span: Span,
    ) -> sem::ValueExpr {
        let id = self.node_id_gen.new_id();
        sem::ValueExpr { id, kind, ty, span }
    }
}
