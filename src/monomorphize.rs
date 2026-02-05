use std::collections::{HashMap, HashSet};

use crate::context::ResolvedContext;
use crate::diag::Span;
use crate::resolve::{DefId, DefKind, DefTable};
use crate::tree::map::TreeMapper;
use crate::tree::resolved as res;
use crate::tree::visit_mut::{VisitorMut, walk_expr, walk_type_expr};
use crate::tree::{NodeId, NodeIdGen, ParamMode, RefinementKind};
use crate::typeck::type_map::{GenericInst, GenericInstMap};
use crate::types::{FnParamMode, Type};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InstKey {
    def_id: DefId,
    type_args: Vec<Type>,
}

#[derive(Debug, Error)]
pub enum MonomorphizeError {
    #[error("generic function `{name}` is used with multiple type arguments")]
    MultipleInstantiations { name: String, span: Span },

    #[error("generic function `{name}` expects {expected} type arguments, got {got}")]
    ArityMismatch {
        name: String,
        expected: usize,
        got: usize,
        span: Span,
    },

    #[error("unknown type `{name}`")]
    UnknownType { name: String, span: Span },

    #[error("unsupported type in monomorphization")]
    UnsupportedType { span: Span },
}

impl MonomorphizeError {
    pub fn span(&self) -> Span {
        match self {
            MonomorphizeError::MultipleInstantiations { span, .. } => *span,
            MonomorphizeError::ArityMismatch { span, .. } => *span,
            MonomorphizeError::UnknownType { span, .. } => *span,
            MonomorphizeError::UnsupportedType { span, .. } => *span,
        }
    }
}

pub fn monomorphize(
    mut ctx: ResolvedContext,
    generic_insts: &GenericInstMap,
) -> Result<ResolvedContext, MonomorphizeError> {
    if generic_insts.is_empty() {
        return Ok(ctx);
    }

    let mut insts_by_def: HashMap<DefId, Vec<GenericInst>> = HashMap::new();
    for inst in generic_insts.values() {
        let entry = insts_by_def.entry(inst.def_id).or_default();
        if !entry
            .iter()
            .any(|existing| existing.type_args == inst.type_args)
        {
            entry.push(inst.clone());
        }
    }

    let mut inst_to_def: HashMap<InstKey, DefId> = HashMap::new();
    for (def_id, insts) in insts_by_def.iter() {
        let def = ctx
            .def_table
            .lookup_def(*def_id)
            .ok_or_else(|| MonomorphizeError::UnknownType {
                name: def_name(&ctx.def_table, *def_id),
                span: insts.first().map(|inst| inst.call_span).unwrap_or_default(),
            })?
            .clone();
        for inst in insts {
            let new_def_id = ctx.def_table.add_def(def.name.clone(), def.kind.clone());
            inst_to_def.insert(
                InstKey {
                    def_id: *def_id,
                    type_args: inst.type_args.clone(),
                },
                new_def_id,
            );
        }
    }

    let mut call_inst_map: HashMap<NodeId, DefId> = HashMap::new();
    for (call_id, inst) in generic_insts {
        if let Some(def_id) = inst_to_def.get(&InstKey {
            def_id: inst.def_id,
            type_args: inst.type_args.clone(),
        }) {
            call_inst_map.insert(*call_id, *def_id);
        }
    }

    let mut new_items = Vec::with_capacity(ctx.module.top_level_items.len());
    let mut node_id_gen = ctx.node_id_gen;

    for mut item in ctx.module.top_level_items.into_iter() {
        match &mut item {
            res::TopLevelItem::FuncDef(func_def) => {
                if func_def.sig.type_params.is_empty() {
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                    continue;
                }

                if let Some(insts) = insts_by_def.get(&func_def.def_id) {
                    for inst in insts {
                        let mut cloned = func_def.clone();
                        if let Some(new_def_id) = inst_to_def.get(&InstKey {
                            def_id: func_def.def_id,
                            type_args: inst.type_args.clone(),
                        }) {
                            cloned.def_id = *new_def_id;
                        }
                        apply_inst_to_func_def(
                            &mut cloned,
                            inst,
                            &ctx.def_table,
                            &mut node_id_gen,
                        )?;
                        let mut cloned_item = res::TopLevelItem::FuncDef(cloned);
                        cloned_item = remap_local_defs_in_item(cloned_item, &mut ctx.def_table);
                        rewrite_calls_in_item(&mut cloned_item, &call_inst_map);
                        reseed_ids_in_item(&mut cloned_item, &mut node_id_gen);
                        new_items.push(cloned_item);
                    }
                }
            }
            res::TopLevelItem::FuncDecl(func_decl) => {
                if func_decl.sig.type_params.is_empty() {
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    new_items.push(item);
                    continue;
                }

                if let Some(insts) = insts_by_def.get(&func_decl.def_id) {
                    for inst in insts {
                        let mut cloned = func_decl.clone();
                        if let Some(new_def_id) = inst_to_def.get(&InstKey {
                            def_id: func_decl.def_id,
                            type_args: inst.type_args.clone(),
                        }) {
                            cloned.def_id = *new_def_id;
                        }
                        apply_inst_to_func_decl(
                            &mut cloned,
                            inst,
                            &ctx.def_table,
                            &mut node_id_gen,
                        )?;
                        let mut cloned_item = res::TopLevelItem::FuncDecl(cloned);
                        cloned_item = remap_local_defs_in_item(cloned_item, &mut ctx.def_table);
                        rewrite_calls_in_item(&mut cloned_item, &call_inst_map);
                        reseed_ids_in_item(&mut cloned_item, &mut node_id_gen);
                        new_items.push(cloned_item);
                    }
                }
            }
            res::TopLevelItem::MethodBlock(method_block) => {
                let mut kept_items = Vec::with_capacity(method_block.method_items.len());
                for mut method_item in method_block.method_items.drain(..) {
                    match &mut method_item {
                        res::MethodItem::Def(method_def) => {
                            if method_def.sig.type_params.is_empty() {
                                rewrite_calls_in_method_item(&mut method_item, &call_inst_map);
                                kept_items.push(method_item);
                                continue;
                            }
                            if let Some(insts) = insts_by_def.get(&method_def.def_id) {
                                for inst in insts {
                                    let mut cloned = method_def.clone();
                                    if let Some(new_def_id) = inst_to_def.get(&InstKey {
                                        def_id: method_def.def_id,
                                        type_args: inst.type_args.clone(),
                                    }) {
                                        cloned.def_id = *new_def_id;
                                    }
                                    apply_inst_to_method_def(
                                        &mut cloned,
                                        inst,
                                        &ctx.def_table,
                                        &mut node_id_gen,
                                    )?;
                                    let mut cloned_item = res::MethodItem::Def(cloned);
                                    cloned_item = remap_local_defs_in_method_item(
                                        cloned_item,
                                        &mut ctx.def_table,
                                    );
                                    rewrite_calls_in_method_item(&mut cloned_item, &call_inst_map);
                                    reseed_ids_in_method_item(&mut cloned_item, &mut node_id_gen);
                                    kept_items.push(cloned_item);
                                }
                            }
                        }
                        res::MethodItem::Decl(method_decl) => {
                            if method_decl.sig.type_params.is_empty() {
                                rewrite_calls_in_method_item(&mut method_item, &call_inst_map);
                                kept_items.push(method_item);
                                continue;
                            }
                            if let Some(insts) = insts_by_def.get(&method_decl.def_id) {
                                for inst in insts {
                                    let mut cloned = method_decl.clone();
                                    if let Some(new_def_id) = inst_to_def.get(&InstKey {
                                        def_id: method_decl.def_id,
                                        type_args: inst.type_args.clone(),
                                    }) {
                                        cloned.def_id = *new_def_id;
                                    }
                                    apply_inst_to_method_decl(
                                        &mut cloned,
                                        inst,
                                        &ctx.def_table,
                                        &mut node_id_gen,
                                    )?;
                                    let mut cloned_item = res::MethodItem::Decl(cloned);
                                    cloned_item = remap_local_defs_in_method_item(
                                        cloned_item,
                                        &mut ctx.def_table,
                                    );
                                    rewrite_calls_in_method_item(&mut cloned_item, &call_inst_map);
                                    reseed_ids_in_method_item(&mut cloned_item, &mut node_id_gen);
                                    kept_items.push(cloned_item);
                                }
                            }
                        }
                    }
                }

                if !kept_items.is_empty() {
                    method_block.method_items = kept_items;
                    rewrite_calls_in_item(&mut item, &call_inst_map);
                    reseed_ids_in_item(&mut item, &mut node_id_gen);
                    new_items.push(item);
                }
            }
            _ => {
                rewrite_calls_in_item(&mut item, &call_inst_map);
                new_items.push(item);
            }
        }
    }

    ctx.module.top_level_items = new_items;
    ctx.node_id_gen = node_id_gen;
    ctx.symbols = crate::symtab::SymbolTable::new(&ctx.module, &ctx.def_table);
    Ok(ctx)
}

struct CallInstRewriter<'a> {
    call_inst_map: &'a HashMap<NodeId, DefId>,
}

impl<'a> VisitorMut<DefId, ()> for CallInstRewriter<'a> {
    fn visit_expr(&mut self, expr: &mut res::Expr) {
        walk_expr(self, expr);
        if let res::ExprKind::Call { callee, .. } = &mut expr.kind {
            if let Some(new_def_id) = self.call_inst_map.get(&expr.id) {
                if let res::ExprKind::Var { def_id, .. } = &mut callee.kind {
                    *def_id = *new_def_id;
                }
            }
        }
    }
}

fn rewrite_calls_in_item(item: &mut res::TopLevelItem, call_inst_map: &HashMap<NodeId, DefId>) {
    let mut rewriter = CallInstRewriter { call_inst_map };
    match item {
        res::TopLevelItem::FuncDef(func_def) => rewriter.visit_func_def(func_def),
        res::TopLevelItem::FuncDecl(func_decl) => rewriter.visit_func_decl(func_decl),
        res::TopLevelItem::MethodBlock(method_block) => rewriter.visit_method_block(method_block),
        res::TopLevelItem::ClosureDef(closure_def) => rewriter.visit_closure_def(closure_def),
        res::TopLevelItem::TypeDef(_) => {}
    }
}

fn rewrite_calls_in_method_item(
    item: &mut res::MethodItem,
    call_inst_map: &HashMap<NodeId, DefId>,
) {
    let mut rewriter = CallInstRewriter { call_inst_map };
    match item {
        res::MethodItem::Def(method_def) => rewriter.visit_method_def(method_def),
        res::MethodItem::Decl(method_decl) => rewriter.visit_method_decl(method_decl),
    }
}

struct ClosureDefCollector<'a> {
    defs: &'a mut HashSet<DefId>,
}

impl<'a> VisitorMut<DefId, ()> for ClosureDefCollector<'a> {
    fn visit_expr(&mut self, expr: &mut res::Expr) {
        walk_expr(self, expr);
        if let res::ExprKind::Closure { def_id, .. } = &expr.kind {
            self.defs.insert(*def_id);
        }
    }
}

struct LocalDefRemapper<'a> {
    def_table: &'a mut DefTable,
    remap: HashMap<DefId, DefId>,
    closure_defs: HashSet<DefId>,
}

impl<'a> LocalDefRemapper<'a> {
    fn new(def_table: &'a mut DefTable, closure_defs: HashSet<DefId>) -> Self {
        Self {
            def_table,
            remap: HashMap::new(),
            closure_defs,
        }
    }

    fn remap_def_id(&mut self, def_id: DefId) -> DefId {
        if let Some(mapped) = self.remap.get(&def_id) {
            return *mapped;
        }

        let Some(def) = self.def_table.lookup_def(def_id) else {
            return def_id;
        };

        let should_remap = match def.kind {
            DefKind::Param { .. } | DefKind::LocalVar { .. } | DefKind::TypeParam => true,
            DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => {
                self.closure_defs.contains(&def_id)
            }
            _ => false,
        };

        if !should_remap {
            return def_id;
        }

        let new_def_id = self.def_table.add_def(def.name.clone(), def.kind.clone());
        self.remap.insert(def_id, new_def_id);
        new_def_id
    }
}

impl<'a> TreeMapper for LocalDefRemapper<'a> {
    type Context = ();
    type InD = DefId;
    type InT = ();
    type OutD = DefId;
    type OutT = ();

    fn map_def_id(
        &mut self,
        _node_id: NodeId,
        def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        self.remap_def_id(*def_id)
    }

    fn map_type_payload(
        &mut self,
        _node_id: NodeId,
        _payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
        ()
    }
}

fn remap_local_defs_in_item(
    mut item: res::TopLevelItem,
    def_table: &mut DefTable,
) -> res::TopLevelItem {
    let mut closure_defs = HashSet::new();
    {
        let mut collector = ClosureDefCollector {
            defs: &mut closure_defs,
        };
        match &mut item {
            res::TopLevelItem::FuncDef(func_def) => collector.visit_func_def(func_def),
            res::TopLevelItem::FuncDecl(func_decl) => collector.visit_func_decl(func_decl),
            res::TopLevelItem::MethodBlock(method_block) => {
                collector.visit_method_block(method_block)
            }
            res::TopLevelItem::ClosureDef(closure_def) => collector.visit_closure_def(closure_def),
            res::TopLevelItem::TypeDef(_) => {}
        }
    }
    let mut remapper = LocalDefRemapper::new(def_table, closure_defs);
    remapper.map_top_level_item(&item, &mut ())
}

fn remap_local_defs_in_method_item(
    mut item: res::MethodItem,
    def_table: &mut DefTable,
) -> res::MethodItem {
    let mut closure_defs = HashSet::new();
    {
        let mut collector = ClosureDefCollector {
            defs: &mut closure_defs,
        };
        collector.visit_method_item(&mut item);
    }
    let mut remapper = LocalDefRemapper::new(def_table, closure_defs);
    remapper.map_method_item(&item, &mut ())
}

fn reseed_ids_in_item(item: &mut res::TopLevelItem, node_id_gen: &mut NodeIdGen) {
    match item {
        res::TopLevelItem::FuncDef(func_def) => reseed_func_def(func_def, node_id_gen),
        res::TopLevelItem::FuncDecl(func_decl) => reseed_func_decl(func_decl, node_id_gen),
        res::TopLevelItem::MethodBlock(method_block) => {
            reseed_method_block(method_block, node_id_gen)
        }
        res::TopLevelItem::ClosureDef(closure_def) => reseed_closure_def(closure_def, node_id_gen),
        res::TopLevelItem::TypeDef(type_def) => {
            type_def.id = node_id_gen.new_id();
            reseed_type_def(type_def, node_id_gen);
        }
    }
}

fn reseed_ids_in_method_item(item: &mut res::MethodItem, node_id_gen: &mut NodeIdGen) {
    match item {
        res::MethodItem::Def(method_def) => reseed_method_def(method_def, node_id_gen),
        res::MethodItem::Decl(method_decl) => reseed_method_decl(method_decl, node_id_gen),
    }
}

fn reseed_func_def(func_def: &mut res::FuncDef, node_id_gen: &mut NodeIdGen) {
    func_def.id = node_id_gen.new_id();
    reseed_func_sig(&mut func_def.sig, node_id_gen);
    reseed_expr(&mut func_def.body, node_id_gen);
}

fn reseed_func_decl(func_decl: &mut res::FuncDecl, node_id_gen: &mut NodeIdGen) {
    func_decl.id = node_id_gen.new_id();
    reseed_func_sig(&mut func_decl.sig, node_id_gen);
}

fn reseed_method_block(method_block: &mut res::MethodBlock, node_id_gen: &mut NodeIdGen) {
    method_block.id = node_id_gen.new_id();
    for item in &mut method_block.method_items {
        reseed_ids_in_method_item(item, node_id_gen);
    }
}

fn reseed_method_def(method_def: &mut res::MethodDef, node_id_gen: &mut NodeIdGen) {
    method_def.id = node_id_gen.new_id();
    reseed_method_sig(&mut method_def.sig, node_id_gen);
    reseed_expr(&mut method_def.body, node_id_gen);
}

fn reseed_method_decl(method_decl: &mut res::MethodDecl, node_id_gen: &mut NodeIdGen) {
    method_decl.id = node_id_gen.new_id();
    reseed_method_sig(&mut method_decl.sig, node_id_gen);
}

fn reseed_closure_def(closure_def: &mut res::ClosureDef, node_id_gen: &mut NodeIdGen) {
    closure_def.id = node_id_gen.new_id();
    reseed_closure_sig(&mut closure_def.sig, node_id_gen);
    reseed_expr(&mut closure_def.body, node_id_gen);
}

fn reseed_func_sig(func_sig: &mut res::FunctionSig, node_id_gen: &mut NodeIdGen) {
    for param in &mut func_sig.type_params {
        reseed_type_param(param, node_id_gen);
    }
    for param in &mut func_sig.params {
        reseed_param(param, node_id_gen);
    }
    reseed_type_expr(&mut func_sig.ret_ty_expr, node_id_gen);
}

fn reseed_method_sig(method_sig: &mut res::MethodSig, node_id_gen: &mut NodeIdGen) {
    method_sig.self_param.id = node_id_gen.new_id();
    for param in &mut method_sig.type_params {
        reseed_type_param(param, node_id_gen);
    }
    for param in &mut method_sig.params {
        reseed_param(param, node_id_gen);
    }
    reseed_type_expr(&mut method_sig.ret_ty_expr, node_id_gen);
}

fn reseed_closure_sig(closure_sig: &mut res::ClosureSig, node_id_gen: &mut NodeIdGen) {
    for param in &mut closure_sig.params {
        reseed_param(param, node_id_gen);
    }
    reseed_type_expr(&mut closure_sig.return_ty, node_id_gen);
}

fn reseed_param(param: &mut res::Param, node_id_gen: &mut NodeIdGen) {
    param.id = node_id_gen.new_id();
    reseed_type_expr(&mut param.typ, node_id_gen);
}

fn reseed_type_param(param: &mut res::TypeParam, node_id_gen: &mut NodeIdGen) {
    param.id = node_id_gen.new_id();
}

fn reseed_type_def(type_def: &mut res::TypeDef, node_id_gen: &mut NodeIdGen) {
    match &mut type_def.kind {
        res::TypeDefKind::Alias { aliased_ty } => reseed_type_expr(aliased_ty, node_id_gen),
        res::TypeDefKind::Struct { fields } => {
            for field in fields {
                field.id = node_id_gen.new_id();
                reseed_type_expr(&mut field.ty, node_id_gen);
            }
        }
        res::TypeDefKind::Enum { variants } => {
            for variant in variants {
                variant.id = node_id_gen.new_id();
                for payload in &mut variant.payload {
                    reseed_type_expr(payload, node_id_gen);
                }
            }
        }
    }
}

fn reseed_type_expr(type_expr: &mut res::TypeExpr, node_id_gen: &mut NodeIdGen) {
    type_expr.id = node_id_gen.new_id();
    match &mut type_expr.kind {
        res::TypeExprKind::Named { type_args, .. } => {
            for arg in type_args {
                reseed_type_expr(arg, node_id_gen);
            }
        }
        res::TypeExprKind::Refined { base_ty_expr, .. } => {
            reseed_type_expr(base_ty_expr, node_id_gen);
        }
        res::TypeExprKind::Array { elem_ty_expr, .. } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        res::TypeExprKind::Tuple { field_ty_exprs } => {
            for field in field_ty_exprs {
                reseed_type_expr(field, node_id_gen);
            }
        }
        res::TypeExprKind::Slice { elem_ty_expr } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        res::TypeExprKind::Heap { elem_ty_expr } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        res::TypeExprKind::Ref { elem_ty_expr, .. } => {
            reseed_type_expr(elem_ty_expr, node_id_gen);
        }
        res::TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            for param in params {
                reseed_type_expr(&mut param.ty_expr, node_id_gen);
            }
            reseed_type_expr(ret_ty_expr, node_id_gen);
        }
    }
}

fn reseed_stmt_expr(stmt: &mut res::StmtExpr, node_id_gen: &mut NodeIdGen) {
    stmt.id = node_id_gen.new_id();
    match &mut stmt.kind {
        res::StmtExprKind::LetBind {
            pattern,
            decl_ty,
            value,
        }
        | res::StmtExprKind::VarBind {
            pattern,
            decl_ty,
            value,
        } => {
            reseed_bind_pattern(pattern, node_id_gen);
            if let Some(ty) = decl_ty {
                reseed_type_expr(ty, node_id_gen);
            }
            reseed_expr(value, node_id_gen);
        }
        res::StmtExprKind::VarDecl { decl_ty, .. } => {
            reseed_type_expr(decl_ty, node_id_gen);
        }
        res::StmtExprKind::Assign {
            assignee, value, ..
        } => {
            reseed_expr(assignee, node_id_gen);
            reseed_expr(value, node_id_gen);
        }
        res::StmtExprKind::While { cond, body } => {
            reseed_expr(cond, node_id_gen);
            reseed_expr(body, node_id_gen);
        }
        res::StmtExprKind::For {
            pattern,
            iter,
            body,
        } => {
            reseed_bind_pattern(pattern, node_id_gen);
            reseed_expr(iter, node_id_gen);
            reseed_expr(body, node_id_gen);
        }
        res::StmtExprKind::Break | res::StmtExprKind::Continue => {}
        res::StmtExprKind::Return { value } => {
            if let Some(value) = value {
                reseed_expr(value, node_id_gen);
            }
        }
    }
}

fn reseed_bind_pattern(pattern: &mut res::BindPattern, node_id_gen: &mut NodeIdGen) {
    pattern.id = node_id_gen.new_id();
    match &mut pattern.kind {
        res::BindPatternKind::Name { .. } => {}
        res::BindPatternKind::Array { patterns } | res::BindPatternKind::Tuple { patterns } => {
            for pattern in patterns {
                reseed_bind_pattern(pattern, node_id_gen);
            }
        }
        res::BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                reseed_bind_pattern(&mut field.pattern, node_id_gen);
            }
        }
    }
}

fn reseed_match_arm(arm: &mut res::MatchArm, node_id_gen: &mut NodeIdGen) {
    arm.id = node_id_gen.new_id();
    reseed_match_pattern(&mut arm.pattern, node_id_gen);
    reseed_expr(&mut arm.body, node_id_gen);
}

fn reseed_match_pattern(pattern: &mut res::MatchPattern, node_id_gen: &mut NodeIdGen) {
    match pattern {
        res::MatchPattern::Binding { id, .. } => *id = node_id_gen.new_id(),
        res::MatchPattern::Tuple { patterns, .. } => {
            for pattern in patterns {
                reseed_match_pattern(pattern, node_id_gen);
            }
        }
        res::MatchPattern::EnumVariant { bindings, .. } => {
            for binding in bindings {
                reseed_match_pattern_binding(binding, node_id_gen);
            }
        }
        res::MatchPattern::Wildcard { .. }
        | res::MatchPattern::BoolLit { .. }
        | res::MatchPattern::IntLit { .. } => {}
    }
}

fn reseed_match_pattern_binding(
    binding: &mut res::MatchPatternBinding,
    node_id_gen: &mut NodeIdGen,
) {
    if let res::MatchPatternBinding::Named { id, .. } = binding {
        *id = node_id_gen.new_id();
    }
}

fn reseed_struct_lit_field(field: &mut res::StructLitField, node_id_gen: &mut NodeIdGen) {
    field.id = node_id_gen.new_id();
    reseed_expr(&mut field.value, node_id_gen);
}

fn reseed_struct_update_field(field: &mut res::StructUpdateField, node_id_gen: &mut NodeIdGen) {
    field.id = node_id_gen.new_id();
    reseed_expr(&mut field.value, node_id_gen);
}

fn reseed_capture_spec(spec: &mut res::CaptureSpec, node_id_gen: &mut NodeIdGen) {
    match spec {
        res::CaptureSpec::Move { id, .. } => *id = node_id_gen.new_id(),
    }
}

fn reseed_expr(expr: &mut res::Expr, node_id_gen: &mut NodeIdGen) {
    expr.id = node_id_gen.new_id();
    match &mut expr.kind {
        res::ExprKind::Block { items, tail } => {
            for item in items {
                match item {
                    res::BlockItem::Stmt(stmt) => reseed_stmt_expr(stmt, node_id_gen),
                    res::BlockItem::Expr(expr) => reseed_expr(expr, node_id_gen),
                }
            }
            if let Some(tail) = tail {
                reseed_expr(tail, node_id_gen);
            }
        }
        res::ExprKind::StringFmt { segments } => {
            for segment in segments {
                if let res::StringFmtSegment::Expr { expr, .. } = segment {
                    reseed_expr(expr, node_id_gen);
                }
            }
        }
        res::ExprKind::ArrayLit { init, elem_ty } => {
            if let Some(elem_ty) = elem_ty {
                reseed_type_expr(elem_ty, node_id_gen);
            }
            match init {
                res::ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        reseed_expr(elem, node_id_gen);
                    }
                }
                res::ArrayLitInit::Repeat(expr, _) => reseed_expr(expr, node_id_gen),
            }
        }
        res::ExprKind::TupleLit(fields) => {
            for field in fields {
                reseed_expr(field, node_id_gen);
            }
        }
        res::ExprKind::StructLit { fields, .. } => {
            for field in fields {
                reseed_struct_lit_field(field, node_id_gen);
            }
        }
        res::ExprKind::EnumVariant { payload, .. } => {
            for expr in payload {
                reseed_expr(expr, node_id_gen);
            }
        }
        res::ExprKind::StructUpdate { target, fields } => {
            reseed_expr(target, node_id_gen);
            for field in fields {
                reseed_struct_update_field(field, node_id_gen);
            }
        }
        res::ExprKind::BinOp { left, right, .. } => {
            reseed_expr(left, node_id_gen);
            reseed_expr(right, node_id_gen);
        }
        res::ExprKind::UnaryOp { expr, .. } => {
            reseed_expr(expr, node_id_gen);
        }
        res::ExprKind::HeapAlloc { expr }
        | res::ExprKind::Move { expr }
        | res::ExprKind::AddrOf { expr }
        | res::ExprKind::Deref { expr }
        | res::ExprKind::ImplicitMove { expr }
        | res::ExprKind::Coerce { expr, .. } => {
            reseed_expr(expr, node_id_gen);
        }
        res::ExprKind::ArrayIndex { target, indices } => {
            reseed_expr(target, node_id_gen);
            for index in indices {
                reseed_expr(index, node_id_gen);
            }
        }
        res::ExprKind::TupleField { target, .. } | res::ExprKind::StructField { target, .. } => {
            reseed_expr(target, node_id_gen);
        }
        res::ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            reseed_expr(cond, node_id_gen);
            reseed_expr(then_body, node_id_gen);
            reseed_expr(else_body, node_id_gen);
        }
        res::ExprKind::Range { start, end } => {
            reseed_expr(start, node_id_gen);
            reseed_expr(end, node_id_gen);
        }
        res::ExprKind::Slice { target, start, end } => {
            reseed_expr(target, node_id_gen);
            if let Some(start) = start {
                reseed_expr(start, node_id_gen);
            }
            if let Some(end) = end {
                reseed_expr(end, node_id_gen);
            }
        }
        res::ExprKind::Match { scrutinee, arms } => {
            reseed_expr(scrutinee, node_id_gen);
            for arm in arms {
                reseed_match_arm(arm, node_id_gen);
            }
        }
        res::ExprKind::Call { callee, args } => {
            reseed_expr(callee, node_id_gen);
            for arg in args {
                reseed_expr(&mut arg.expr, node_id_gen);
            }
        }
        res::ExprKind::MethodCall { callee, args, .. } => {
            reseed_expr(callee, node_id_gen);
            for arg in args {
                reseed_expr(&mut arg.expr, node_id_gen);
            }
        }
        res::ExprKind::Closure {
            params,
            return_ty,
            body,
            captures,
            ..
        } => {
            for param in params {
                reseed_param(param, node_id_gen);
            }
            for capture in captures {
                reseed_capture_spec(capture, node_id_gen);
            }
            reseed_type_expr(return_ty, node_id_gen);
            reseed_expr(body, node_id_gen);
        }
        res::ExprKind::UnitLit
        | res::ExprKind::IntLit(_)
        | res::ExprKind::BoolLit(_)
        | res::ExprKind::CharLit(_)
        | res::ExprKind::StringLit { .. }
        | res::ExprKind::Var { .. } => {}
    }
}

fn apply_inst_to_func_def(
    func_def: &mut res::FuncDef,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&func_def.sig.type_params, inst, def_table)?;
    func_def.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_func_def(func_def);
    substituter.finish()
}

fn apply_inst_to_func_decl(
    func_decl: &mut res::FuncDecl,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&func_decl.sig.type_params, inst, def_table)?;
    func_decl.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_func_decl(func_decl);
    substituter.finish()
}

fn apply_inst_to_method_def(
    method_def: &mut res::MethodDef,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&method_def.sig.type_params, inst, def_table)?;
    method_def.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_method_def(method_def);
    substituter.finish()
}

fn apply_inst_to_method_decl(
    method_decl: &mut res::MethodDecl,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&method_decl.sig.type_params, inst, def_table)?;
    method_decl.sig.type_params.clear();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    substituter.visit_method_decl(method_decl);
    substituter.finish()
}

fn build_subst(
    type_params: &[res::TypeParam],
    inst: &GenericInst,
    def_table: &DefTable,
) -> Result<HashMap<DefId, Type>, MonomorphizeError> {
    if type_params.len() != inst.type_args.len() {
        let name = def_name(def_table, inst.def_id);
        return Err(MonomorphizeError::ArityMismatch {
            name,
            expected: type_params.len(),
            got: inst.type_args.len(),
            span: inst.call_span,
        });
    }

    Ok(type_params
        .iter()
        .zip(inst.type_args.iter().cloned())
        .map(|(param, ty)| (param.def_id, ty))
        .collect())
}

struct TypeExprSubstitutor<'a> {
    subst: &'a HashMap<DefId, Type>,
    def_table: &'a DefTable,
    node_id_gen: &'a mut NodeIdGen,
    error: Option<MonomorphizeError>,
}

impl<'a> TypeExprSubstitutor<'a> {
    fn new(
        subst: &'a HashMap<DefId, Type>,
        def_table: &'a DefTable,
        node_id_gen: &'a mut NodeIdGen,
    ) -> Self {
        Self {
            subst,
            def_table,
            node_id_gen,
            error: None,
        }
    }

    fn finish(self) -> Result<(), MonomorphizeError> {
        if let Some(err) = self.error {
            Err(err)
        } else {
            Ok(())
        }
    }
}

impl<'a> VisitorMut<DefId, ()> for TypeExprSubstitutor<'a> {
    fn visit_type_expr(&mut self, type_expr: &mut res::TypeExpr) {
        if self.error.is_some() {
            return;
        }

        if let res::TypeExprKind::Named { def_id, .. } = &type_expr.kind {
            if let Some(ty) = self.subst.get(def_id) {
                match type_expr_from_type(ty, self.def_table, self.node_id_gen, type_expr.span) {
                    Ok(new_expr) => {
                        *type_expr = new_expr;
                        return;
                    }
                    Err(err) => {
                        self.error = Some(err);
                        return;
                    }
                }
            }
        }

        walk_type_expr(self, type_expr);
    }
}

fn type_expr_from_type(
    ty: &Type,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<res::TypeExpr, MonomorphizeError> {
    let id = node_id_gen.new_id();
    let kind = match ty {
        Type::Unit => {
            return named_type_expr("()", def_table, node_id_gen, span);
        }
        Type::Int {
            signed,
            bits,
            bounds,
            nonzero,
        } => {
            let name = match (*signed, *bits) {
                (false, 8) => "u8",
                (false, 16) => "u16",
                (false, 32) => "u32",
                (false, 64) => "u64",
                (true, 8) => "i8",
                (true, 16) => "i16",
                (true, 32) => "i32",
                (true, 64) => "i64",
                _ => return Err(MonomorphizeError::UnsupportedType { span }),
            };
            let mut refinements = Vec::new();
            if let Some(bounds) = bounds {
                refinements.push(RefinementKind::Bounds {
                    min: bounds.min,
                    max: bounds.max_excl,
                });
            }
            if *nonzero {
                refinements.push(RefinementKind::NonZero);
            }
            if refinements.is_empty() {
                return named_type_expr(name, def_table, node_id_gen, span);
            }
            let base_expr = named_type_expr(name, def_table, node_id_gen, span)?;
            res::TypeExprKind::Refined {
                base_ty_expr: Box::new(base_expr),
                refinements,
            }
        }
        Type::Bool => {
            return named_type_expr("bool", def_table, node_id_gen, span);
        }
        Type::Char => {
            return named_type_expr("char", def_table, node_id_gen, span);
        }
        Type::String => {
            return named_type_expr("string", def_table, node_id_gen, span);
        }
        Type::Array { elem_ty, dims } => res::TypeExprKind::Array {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
            dims: dims.clone(),
        },
        Type::Tuple { field_tys } => res::TypeExprKind::Tuple {
            field_ty_exprs: field_tys
                .iter()
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        Type::Slice { elem_ty } => res::TypeExprKind::Slice {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Heap { elem_ty } => res::TypeExprKind::Heap {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Ref { mutable, elem_ty } => res::TypeExprKind::Ref {
            mutable: *mutable,
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Fn { params, ret_ty } => {
            let params = params
                .iter()
                .map(|param| {
                    let mode = match param.mode {
                        FnParamMode::In => ParamMode::In,
                        FnParamMode::InOut => ParamMode::InOut,
                        FnParamMode::Out => ParamMode::Out,
                        FnParamMode::Sink => ParamMode::Sink,
                    };
                    Ok(res::FnTypeParam {
                        mode,
                        ty_expr: type_expr_from_type(&param.ty, def_table, node_id_gen, span)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            res::TypeExprKind::Fn {
                params,
                ret_ty_expr: Box::new(type_expr_from_type(ret_ty, def_table, node_id_gen, span)?),
            }
        }
        Type::Struct { name, .. } | Type::Enum { name, .. } => {
            return named_type_expr(name, def_table, node_id_gen, span);
        }
        Type::Range { .. } => return Err(MonomorphizeError::UnsupportedType { span }),
        Type::Unknown | Type::Var(_) => return Err(MonomorphizeError::UnsupportedType { span }),
    };

    Ok(res::TypeExpr { id, kind, span })
}

fn named_type_expr(
    name: &str,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<res::TypeExpr, MonomorphizeError> {
    let def_id =
        def_table
            .lookup_type_def_id(name)
            .ok_or_else(|| MonomorphizeError::UnknownType {
                name: name.to_string(),
                span,
            })?;
    Ok(res::TypeExpr {
        id: node_id_gen.new_id(),
        kind: res::TypeExprKind::Named {
            ident: name.to_string(),
            def_id,
            type_args: Vec::new(),
        },
        span,
    })
}

fn def_name(def_table: &DefTable, def_id: DefId) -> String {
    def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone())
        .unwrap_or_else(|| format!("def_{def_id:?}"))
}

#[cfg(test)]
#[path = "tests/monomorphize/t_monomorphize.rs"]
mod tests;
