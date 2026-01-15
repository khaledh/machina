//! Build the resolved tree from the parsed tree.
//!
//! This is the first resolved step: identifiers are replaced with DefIds.

use crate::resolve::def_table::NodeDefLookup;
use crate::resolve::{Def, DefId, DefTable};
use crate::tree::NodeId;
use crate::tree::parsed as par;
use crate::tree::resolved as res;

pub struct DefLookup<'a> {
    def_table: &'a DefTable,
    node_def_lookup: &'a NodeDefLookup,
}

impl<'a> DefLookup<'a> {
    pub fn new(def_table: &'a DefTable, node_def_lookup: &'a NodeDefLookup) -> Self {
        Self {
            def_table,
            node_def_lookup,
        }
    }

    pub fn lookup_node_def(&self, node_id: NodeId) -> Option<&Def> {
        self.node_def_lookup
            .lookup_node_def_id(node_id)
            .and_then(|def_id| self.def_table.lookup_def(def_id))
    }
}

pub trait ToResolved {
    type Output;
    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output;
}

pub struct ResolvedTreeBuilder<'a> {
    def_lookup: DefLookup<'a>,
}

impl<'a> ResolvedTreeBuilder<'a> {
    pub fn new(def_table: &'a DefTable, node_def_lookup: &'a NodeDefLookup) -> Self {
        let def_lookup = DefLookup::new(def_table, node_def_lookup);
        Self { def_lookup }
    }

    pub fn build_module(&self, module: par::Module) -> res::Module {
        module.to_resolved(&self.def_lookup)
    }
}

fn def_id(def_lookup: &DefLookup, node_id: NodeId) -> DefId {
    let def = def_lookup
        .lookup_node_def(node_id)
        .unwrap_or_else(|| panic!("Missing def for NodeId({})", node_id.0));
    def.id
}

impl ToResolved for par::Module {
    type Output = res::Module;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::Module {
            top_level_items: self
                .top_level_items
                .into_iter()
                .map(|item| item.to_resolved(def_lookup))
                .collect(),
        }
    }
}

impl ToResolved for par::TopLevelItem {
    type Output = res::TopLevelItem;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            par::TopLevelItem::TypeDef(type_def) => {
                res::TopLevelItem::TypeDef(type_def.to_resolved(def_lookup))
            }
            par::TopLevelItem::FuncDecl(func_decl) => {
                res::TopLevelItem::FuncDecl(func_decl.to_resolved(def_lookup))
            }
            par::TopLevelItem::FuncDef(func_def) => {
                res::TopLevelItem::FuncDef(func_def.to_resolved(def_lookup))
            }
            par::TopLevelItem::MethodBlock(method_block) => {
                res::TopLevelItem::MethodBlock(method_block.to_resolved(def_lookup))
            }
            par::TopLevelItem::ClosureDef(closure_decl) => {
                res::TopLevelItem::ClosureDef(closure_decl.to_resolved(def_lookup))
            }
        }
    }
}

impl ToResolved for par::TypeDef {
    type Output = res::TypeDef;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::TypeDef {
            id: self.id,
            def_id: def_id(def_lookup, self.id),
            name: self.name,
            kind: match self.kind {
                par::TypeDefKind::Alias { aliased_ty } => res::TypeDefKind::Alias {
                    aliased_ty: aliased_ty.to_resolved(def_lookup),
                },
                par::TypeDefKind::Struct { fields } => res::TypeDefKind::Struct {
                    fields: fields
                        .into_iter()
                        .map(|field| field.to_resolved(def_lookup))
                        .collect(),
                },
                par::TypeDefKind::Enum { variants } => res::TypeDefKind::Enum {
                    variants: variants
                        .into_iter()
                        .map(|variant| variant.to_resolved(def_lookup))
                        .collect(),
                },
            },
            span: self.span,
        }
    }
}

impl ToResolved for par::StructDefField {
    type Output = res::StructDefField;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        struct_def_field_to_resolved(def_lookup, &self)
    }
}

impl ToResolved for &par::StructDefField {
    type Output = res::StructDefField;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        struct_def_field_to_resolved(def_lookup, self)
    }
}

fn struct_def_field_to_resolved(
    def_lookup: &DefLookup,
    field: &par::StructDefField,
) -> res::StructDefField {
    res::StructDefField {
        id: field.id,
        name: field.name.clone(),
        ty: (&field.ty).to_resolved(def_lookup),
        span: field.span,
    }
}

impl ToResolved for par::EnumDefVariant {
    type Output = res::EnumDefVariant;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        enum_def_variant_to_resolved(def_lookup, &self)
    }
}

impl ToResolved for &par::EnumDefVariant {
    type Output = res::EnumDefVariant;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        enum_def_variant_to_resolved(def_lookup, self)
    }
}

fn enum_def_variant_to_resolved(
    def_lookup: &DefLookup,
    variant: &par::EnumDefVariant,
) -> res::EnumDefVariant {
    res::EnumDefVariant {
        id: variant.id,
        name: variant.name.clone(),
        payload: variant
            .payload
            .iter()
            .map(|ty| ty.to_resolved(def_lookup))
            .collect(),
        span: variant.span,
    }
}

impl ToResolved for par::TypeExpr {
    type Output = res::TypeExpr;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        type_expr_to_resolved(def_lookup, &self)
    }
}

impl ToResolved for &par::TypeExpr {
    type Output = res::TypeExpr;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        type_expr_to_resolved(def_lookup, self)
    }
}

fn type_expr_to_resolved(def_lookup: &DefLookup, type_expr: &par::TypeExpr) -> res::TypeExpr {
    res::TypeExpr {
        id: type_expr.id,
        span: type_expr.span,
        kind: match &type_expr.kind {
            par::TypeExprKind::Named { ident, .. } => res::TypeExprKind::Named {
                ident: ident.clone(),
                def_id: def_id(def_lookup, type_expr.id),
            },
            par::TypeExprKind::Array { elem_ty_expr, dims } => res::TypeExprKind::Array {
                elem_ty_expr: Box::new(elem_ty_expr.as_ref().to_resolved(def_lookup)),
                dims: dims.clone(),
            },
            par::TypeExprKind::Tuple { field_ty_exprs } => res::TypeExprKind::Tuple {
                field_ty_exprs: field_ty_exprs
                    .iter()
                    .map(|t| t.to_resolved(def_lookup))
                    .collect(),
            },
            par::TypeExprKind::Range { min, max } => res::TypeExprKind::Range {
                min: *min,
                max: *max,
            },
            par::TypeExprKind::Slice { elem_ty_expr } => res::TypeExprKind::Slice {
                elem_ty_expr: Box::new(elem_ty_expr.as_ref().to_resolved(def_lookup)),
            },
            par::TypeExprKind::Heap { elem_ty_expr } => res::TypeExprKind::Heap {
                elem_ty_expr: Box::new(elem_ty_expr.as_ref().to_resolved(def_lookup)),
            },
            par::TypeExprKind::Fn {
                params,
                ret_ty_expr,
            } => res::TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| param.to_resolved(def_lookup))
                    .collect(),
                ret_ty_expr: Box::new(ret_ty_expr.as_ref().to_resolved(def_lookup)),
            },
        },
    }
}

impl ToResolved for par::FnTypeParam {
    type Output = res::FnTypeParam;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        fn_type_param_to_resolved(def_lookup, &self)
    }
}

impl ToResolved for &par::FnTypeParam {
    type Output = res::FnTypeParam;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        fn_type_param_to_resolved(def_lookup, self)
    }
}

fn fn_type_param_to_resolved(def_lookup: &DefLookup, param: &par::FnTypeParam) -> res::FnTypeParam {
    res::FnTypeParam {
        mode: param.mode.clone(),
        ty_expr: (&param.ty_expr).to_resolved(def_lookup),
    }
}

impl ToResolved for par::FuncDecl {
    type Output = res::FuncDecl;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        res::FuncDecl {
            id: self.id,
            def_id,
            sig: self.sig.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::FuncDef {
    type Output = res::FuncDef;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        res::FuncDef {
            id: self.id,
            def_id,
            sig: self.sig.to_resolved(def_lookup),
            body: self.body.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::FunctionSig {
    type Output = res::FunctionSig;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::FunctionSig {
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.to_resolved(def_lookup))
                .collect(),
            ret_ty_expr: self.ret_ty_expr.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::MethodBlock {
    type Output = res::MethodBlock;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::MethodBlock {
            id: self.id,
            type_name: self.type_name,
            method_defs: self
                .method_defs
                .into_iter()
                .map(|method| method.to_resolved(def_lookup))
                .collect(),
            span: self.span,
        }
    }
}

impl ToResolved for par::MethodDef {
    type Output = res::MethodDef;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        res::MethodDef {
            id: self.id,
            def_id,
            sig: self.sig.to_resolved(def_lookup),
            body: self.body.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::MethodSig {
    type Output = res::MethodSig;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let self_def_id = def_id(def_lookup, self.self_param.id);
        res::MethodSig {
            name: self.name,
            self_param: res::SelfParam {
                id: self.self_param.id,
                def_id: self_def_id,
                mode: self.self_param.mode,
                span: self.self_param.span,
            },
            params: self
                .params
                .into_iter()
                .map(|param| param.to_resolved(def_lookup))
                .collect(),
            ret_ty_expr: self.ret_ty_expr.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::ClosureDef {
    type Output = res::ClosureDef;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        res::ClosureDef {
            id: self.id,
            def_id,
            sig: self.sig.to_resolved(def_lookup),
            body: self.body.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::ClosureSig {
    type Output = res::ClosureSig;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::ClosureSig {
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.to_resolved(def_lookup))
                .collect(),
            return_ty: self.return_ty.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::Param {
    type Output = res::Param;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::Param {
            id: self.id,
            ident: self.ident,
            def_id: def_id(def_lookup, self.id),
            typ: self.typ.to_resolved(def_lookup),
            mode: self.mode,
            span: self.span,
        }
    }
}

impl ToResolved for par::Expr {
    type Output = res::Expr;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let id = self.id;
        let span = self.span;
        let kind = match self.kind {
            par::ExprKind::Block { items, tail } => res::ExprKind::Block {
                items: items
                    .into_iter()
                    .map(|item| item.to_resolved(def_lookup))
                    .collect(),
                tail: tail.map(|expr| Box::new(expr.to_resolved(def_lookup))),
            },

            par::ExprKind::UnitLit => res::ExprKind::UnitLit,
            par::ExprKind::IntLit(value) => res::ExprKind::IntLit(value),
            par::ExprKind::BoolLit(value) => res::ExprKind::BoolLit(value),
            par::ExprKind::CharLit(value) => res::ExprKind::CharLit(value),
            par::ExprKind::StringLit { value } => res::ExprKind::StringLit { value },
            par::ExprKind::StringFmt { segments } => res::ExprKind::StringFmt {
                segments: segments
                    .into_iter()
                    .map(|seg| seg.to_resolved(def_lookup))
                    .collect(),
            },

            par::ExprKind::ArrayLit { elem_ty, init } => res::ExprKind::ArrayLit {
                elem_ty: elem_ty.map(|ty| ty.to_resolved(def_lookup)),
                init: init.to_resolved(def_lookup),
            },
            par::ExprKind::TupleLit(items) => res::ExprKind::TupleLit(
                items
                    .into_iter()
                    .map(|e| e.to_resolved(def_lookup))
                    .collect(),
            ),
            par::ExprKind::StructLit { name, fields } => res::ExprKind::StructLit {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| field.to_resolved(def_lookup))
                    .collect(),
            },
            par::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => res::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload: payload
                    .into_iter()
                    .map(|expr| expr.to_resolved(def_lookup))
                    .collect(),
            },
            par::ExprKind::StructUpdate { target, fields } => res::ExprKind::StructUpdate {
                target: Box::new(target.to_resolved(def_lookup)),
                fields: fields
                    .into_iter()
                    .map(|field| field.to_resolved(def_lookup))
                    .collect(),
            },

            par::ExprKind::BinOp { left, op, right } => res::ExprKind::BinOp {
                left: Box::new(left.to_resolved(def_lookup)),
                op,
                right: Box::new(right.to_resolved(def_lookup)),
            },
            par::ExprKind::UnaryOp { op, expr } => res::ExprKind::UnaryOp {
                op,
                expr: Box::new(expr.to_resolved(def_lookup)),
            },
            par::ExprKind::HeapAlloc { expr } => res::ExprKind::HeapAlloc {
                expr: Box::new(expr.to_resolved(def_lookup)),
            },
            par::ExprKind::Move { expr } => res::ExprKind::Move {
                expr: Box::new(expr.to_resolved(def_lookup)),
            },
            par::ExprKind::Coerce { kind, expr } => res::ExprKind::Coerce {
                kind,
                expr: Box::new(expr.to_resolved(def_lookup)),
            },
            par::ExprKind::ImplicitMove { expr } => res::ExprKind::ImplicitMove {
                expr: Box::new(expr.to_resolved(def_lookup)),
            },
            par::ExprKind::Var { ident, .. } => res::ExprKind::Var {
                ident,
                def_id: def_id(def_lookup, id),
            },
            par::ExprKind::ArrayIndex { target, indices } => res::ExprKind::ArrayIndex {
                target: Box::new(target.to_resolved(def_lookup)),
                indices: indices
                    .into_iter()
                    .map(|index| index.to_resolved(def_lookup))
                    .collect(),
            },
            par::ExprKind::TupleField { target, index } => res::ExprKind::TupleField {
                target: Box::new(target.to_resolved(def_lookup)),
                index,
            },
            par::ExprKind::StructField { target, field } => res::ExprKind::StructField {
                target: Box::new(target.to_resolved(def_lookup)),
                field,
            },

            par::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => res::ExprKind::If {
                cond: Box::new(cond.to_resolved(def_lookup)),
                then_body: Box::new(then_body.to_resolved(def_lookup)),
                else_body: Box::new(else_body.to_resolved(def_lookup)),
            },

            par::ExprKind::Range { start, end } => res::ExprKind::Range { start, end },
            par::ExprKind::Slice { target, start, end } => res::ExprKind::Slice {
                target: Box::new(target.to_resolved(def_lookup)),
                start: start.map(|expr| Box::new(expr.to_resolved(def_lookup))),
                end: end.map(|expr| Box::new(expr.to_resolved(def_lookup))),
            },
            par::ExprKind::Match { scrutinee, arms } => res::ExprKind::Match {
                scrutinee: Box::new(scrutinee.to_resolved(def_lookup)),
                arms: arms
                    .into_iter()
                    .map(|arm| arm.to_resolved(def_lookup))
                    .collect(),
            },

            par::ExprKind::Call { callee, args } => res::ExprKind::Call {
                callee: Box::new(callee.to_resolved(def_lookup)),
                args: args
                    .into_iter()
                    .map(|arg| arg.to_resolved(def_lookup))
                    .collect(),
            },
            par::ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => res::ExprKind::MethodCall {
                callee: Box::new(callee.to_resolved(def_lookup)),
                method_name,
                args: args
                    .into_iter()
                    .map(|arg| arg.to_resolved(def_lookup))
                    .collect(),
            },

            par::ExprKind::Closure {
                ident,
                params,
                return_ty,
                body,
                ..
            } => res::ExprKind::Closure {
                ident,
                def_id: def_id(def_lookup, id),
                params: params
                    .into_iter()
                    .map(|param| param.to_resolved(def_lookup))
                    .collect(),
                return_ty: return_ty.to_resolved(def_lookup),
                body: Box::new(body.to_resolved(def_lookup)),
            },
        };

        res::Expr {
            id,
            kind,
            ty: (),
            span,
        }
    }
}

impl ToResolved for par::BlockItem {
    type Output = res::BlockItem;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            par::BlockItem::Stmt(stmt) => res::BlockItem::Stmt(stmt.to_resolved(def_lookup)),
            par::BlockItem::Expr(expr) => res::BlockItem::Expr(expr.to_resolved(def_lookup)),
        }
    }
}

impl ToResolved for par::StmtExpr {
    type Output = res::StmtExpr;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let id = self.id;
        let span = self.span;
        let kind = match self.kind {
            par::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => res::StmtExprKind::LetBind {
                pattern: pattern.to_resolved(def_lookup),
                decl_ty: decl_ty.map(|ty| ty.to_resolved(def_lookup)),
                value: Box::new(value.to_resolved(def_lookup)),
            },
            par::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => res::StmtExprKind::VarBind {
                pattern: pattern.to_resolved(def_lookup),
                decl_ty: decl_ty.map(|ty| ty.to_resolved(def_lookup)),
                value: Box::new(value.to_resolved(def_lookup)),
            },
            par::StmtExprKind::VarDecl { ident, decl_ty, .. } => res::StmtExprKind::VarDecl {
                ident,
                def_id: def_id(def_lookup, id),
                decl_ty: decl_ty.to_resolved(def_lookup),
            },
            par::StmtExprKind::Assign {
                assignee,
                value,
                init,
            } => res::StmtExprKind::Assign {
                assignee: Box::new(assignee.to_resolved(def_lookup)),
                value: Box::new(value.to_resolved(def_lookup)),
                init,
            },
            par::StmtExprKind::While { cond, body } => res::StmtExprKind::While {
                cond: Box::new(cond.to_resolved(def_lookup)),
                body: Box::new(body.to_resolved(def_lookup)),
            },
            par::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => res::StmtExprKind::For {
                pattern: pattern.to_resolved(def_lookup),
                iter: Box::new(iter.to_resolved(def_lookup)),
                body: Box::new(body.to_resolved(def_lookup)),
            },
        };

        res::StmtExpr {
            id,
            kind,
            ty: (),
            span,
        }
    }
}

impl ToResolved for par::BindPattern {
    type Output = res::BindPattern;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        let id = self.id;
        let span = self.span;
        let kind = match self.kind {
            par::BindPatternKind::Name { ident, .. } => res::BindPatternKind::Name {
                ident,
                def_id: def_id(def_lookup, id),
            },
            par::BindPatternKind::Array { patterns } => res::BindPatternKind::Array {
                patterns: patterns
                    .into_iter()
                    .map(|pat| pat.to_resolved(def_lookup))
                    .collect(),
            },
            par::BindPatternKind::Tuple { patterns } => res::BindPatternKind::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|pat| pat.to_resolved(def_lookup))
                    .collect(),
            },
            par::BindPatternKind::Struct { name, fields } => res::BindPatternKind::Struct {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| field.to_resolved(def_lookup))
                    .collect(),
            },
        };

        res::BindPattern { id, span, kind }
    }
}

impl ToResolved for par::StructFieldBindPattern {
    type Output = res::StructPatternField;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::StructPatternField {
            name: self.name,
            pattern: self.pattern.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::MatchArm {
    type Output = res::MatchArm;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::MatchArm {
            id: self.id,
            pattern: self.pattern.to_resolved(def_lookup),
            body: self.body.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::MatchPattern {
    type Output = res::MatchPattern;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            par::MatchPattern::Wildcard { span } => res::MatchPattern::Wildcard { span },
            par::MatchPattern::BoolLit { value, span } => {
                res::MatchPattern::BoolLit { value, span }
            }
            par::MatchPattern::IntLit { value, span } => res::MatchPattern::IntLit { value, span },
            par::MatchPattern::Binding {
                id, ident, span, ..
            } => res::MatchPattern::Binding {
                id,
                ident,
                def_id: def_id(def_lookup, id),
                span,
            },
            par::MatchPattern::Tuple { patterns, span } => res::MatchPattern::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|pat| pat.to_resolved(def_lookup))
                    .collect(),
                span,
            },
            par::MatchPattern::EnumVariant {
                enum_name,
                variant_name,
                bindings,
                span,
            } => res::MatchPattern::EnumVariant {
                enum_name,
                variant_name,
                bindings: bindings
                    .into_iter()
                    .map(|binding| binding.to_resolved(def_lookup))
                    .collect(),
                span,
            },
        }
    }
}

impl ToResolved for par::MatchPatternBinding {
    type Output = res::MatchPatternBinding;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            par::MatchPatternBinding::Named {
                id, ident, span, ..
            } => res::MatchPatternBinding::Named {
                id,
                ident,
                def_id: def_id(def_lookup, id),
                span,
            },
            par::MatchPatternBinding::Wildcard { span } => {
                res::MatchPatternBinding::Wildcard { span }
            }
        }
    }
}

impl ToResolved for par::CallArg {
    type Output = res::CallArg;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::CallArg {
            mode: self.mode,
            expr: self.expr.to_resolved(def_lookup),
            init: self.init,
            span: self.span,
        }
    }
}

impl ToResolved for par::ArrayLitInit {
    type Output = res::ArrayLitInit;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            par::ArrayLitInit::Elems(elems) => res::ArrayLitInit::Elems(
                elems
                    .into_iter()
                    .map(|expr| expr.to_resolved(def_lookup))
                    .collect(),
            ),
            par::ArrayLitInit::Repeat(expr, count) => {
                res::ArrayLitInit::Repeat(Box::new(expr.to_resolved(def_lookup)), count)
            }
        }
    }
}

impl ToResolved for par::StringFmtSegment {
    type Output = res::StringFmtSegment;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            par::StringFmtSegment::Literal { value, span } => {
                res::StringFmtSegment::Literal { value, span }
            }
            par::StringFmtSegment::Expr { expr, span } => res::StringFmtSegment::Expr {
                expr: Box::new(expr.to_resolved(def_lookup)),
                span,
            },
        }
    }
}

impl ToResolved for par::StructLitField {
    type Output = res::StructLitField;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::StructLitField {
            id: self.id,
            name: self.name,
            value: self.value.to_resolved(def_lookup),
            span: self.span,
        }
    }
}

impl ToResolved for par::StructUpdateField {
    type Output = res::StructUpdateField;

    fn to_resolved(self, def_lookup: &DefLookup) -> Self::Output {
        res::StructUpdateField {
            id: self.id,
            name: self.name,
            value: self.value.to_resolved(def_lookup),
            span: self.span,
        }
    }
}
