use std::collections::{HashMap, HashSet};

use crate::diag::Span;
use crate::resolve::DefId;
use crate::resolve::DefTable;
use crate::semck::closure::capture::ClosureCapture;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::tree::{InitInfo, NodeId, NodeIdGen};
use crate::typeck::type_map::TypeMap;
use crate::types::{Type, TypeId};

#[derive(Clone, Debug)]
pub(super) struct CaptureField {
    pub(super) def_id: DefId,
    pub(super) name: String,
    pub(super) base_ty: Type,
    pub(super) base_ty_id: TypeId,
    pub(super) field_ty: Type,
    pub(super) field_ty_id: TypeId,
    pub(super) field_ty_expr: sem::TypeExpr,
}

#[derive(Clone, Debug)]
pub(super) struct ClosureInfo {
    pub(super) type_name: String,
    pub(super) type_id: TypeId,
    pub(super) param_modes: Vec<crate::tree::ParamMode>,
    pub(super) ty: Type,
    pub(super) self_def_id: DefId,
    pub(super) captures: Vec<CaptureField>,
}

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

pub struct Elaborator<'a> {
    pub(super) def_table: &'a mut DefTable,
    pub(super) type_map: &'a mut TypeMap,
    pub(super) node_id_gen: &'a mut NodeIdGen,
    pub(super) implicit_moves: &'a HashSet<NodeId>,
    pub(super) init_assigns: &'a HashSet<NodeId>,
    pub(super) full_init_assigns: &'a HashSet<NodeId>,
    pub(super) closure_captures: &'a HashMap<DefId, Vec<ClosureCapture>>,
    pub(super) closure_types: Vec<sem::TypeDef>,
    pub(super) closure_methods: Vec<sem::MethodBlock>,
    pub(super) closure_info: HashMap<DefId, ClosureInfo>,
    pub(super) closure_bindings: HashMap<DefId, DefId>,
    pub(super) closure_stack: Vec<ClosureContext>,
}

impl<'a> Elaborator<'a> {
    pub fn new(
        def_table: &'a mut DefTable,
        type_map: &'a mut TypeMap,
        node_id_gen: &'a mut NodeIdGen,
        implicit_moves: &'a HashSet<NodeId>,
        init_assigns: &'a HashSet<NodeId>,
        full_init_assigns: &'a HashSet<NodeId>,
        closure_captures: &'a HashMap<DefId, Vec<ClosureCapture>>,
    ) -> Self {
        Self {
            def_table,
            type_map,
            node_id_gen,
            implicit_moves,
            init_assigns,
            full_init_assigns,
            closure_captures,
            closure_types: Vec::new(),
            closure_methods: Vec::new(),
            closure_info: HashMap::new(),
            closure_bindings: HashMap::new(),
            closure_stack: Vec::new(),
        }
    }

    pub fn elaborate_module(&mut self, module: &norm::Module) -> sem::Module {
        // Lift closures to top level
        self.closure_types.clear();
        self.closure_methods.clear();
        self.closure_info.clear();
        self.closure_bindings.clear();
        self.closure_stack.clear();
        let mut top_level_items: Vec<_> = module
            .top_level_items
            .iter()
            .map(|item| self.elab_top_level_item(item))
            .collect();
        top_level_items.extend(self.closure_types.drain(..).map(sem::TopLevelItem::TypeDef));
        top_level_items.extend(
            self.closure_methods
                .drain(..)
                .map(sem::TopLevelItem::MethodBlock),
        );
        sem::Module { top_level_items }
    }

    fn elab_top_level_item(&mut self, item: &norm::TopLevelItem) -> sem::TopLevelItem {
        match item {
            norm::TopLevelItem::TypeDef(def) => sem::TopLevelItem::TypeDef(def.clone()),
            norm::TopLevelItem::FuncDecl(decl) => sem::TopLevelItem::FuncDecl(sem::FuncDecl {
                id: decl.id,
                def_id: decl.def_id,
                sig: decl.sig.clone(),
                span: decl.span,
            }),
            norm::TopLevelItem::FuncDef(def) => sem::TopLevelItem::FuncDef(sem::FuncDef {
                id: def.id,
                def_id: def.def_id,
                sig: def.sig.clone(),
                body: self.elab_value(&def.body),
                span: def.span,
            }),
            norm::TopLevelItem::MethodBlock(block) => {
                sem::TopLevelItem::MethodBlock(sem::MethodBlock {
                    id: block.id,
                    type_name: block.type_name.clone(),
                    method_defs: block
                        .method_defs
                        .iter()
                        .map(|method| self.elab_method_def(method))
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
            sig: def.sig.clone(),
            body: self.elab_value(&def.body),
            span: def.span,
        }
    }

    pub(super) fn init_info_for_id(&self, id: NodeId) -> InitInfo {
        // Pull init tracking from semck for out-param and partial-init lowering.
        InitInfo {
            is_init: self.init_assigns.contains(&id),
            promotes_full: self.full_init_assigns.contains(&id),
        }
    }

    pub(super) fn new_value(
        &mut self,
        kind: sem::ValueExprKind,
        ty: TypeId,
        span: Span,
    ) -> sem::ValueExpr {
        // Elaborate often synthesizes new nodes; always allocate fresh ids.
        let id = self.node_id_gen.new_id();
        sem::ValueExpr { id, kind, ty, span }
    }
}
