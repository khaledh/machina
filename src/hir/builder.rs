//! Build HIR from resolved AST.
//!
//! This is the first resolved HIR step: identifiers are replaced with DefIds.

use crate::ast;
use crate::hir::model as hir;
use crate::resolve::def_table::NodeDefLookup;
use crate::resolve::{Def, DefId, DefTable};

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

    pub fn lookup_node_def(&self, node_id: ast::NodeId) -> Option<&Def> {
        self.node_def_lookup
            .lookup_node_def_id(node_id)
            .and_then(|def_id| self.def_table.lookup_def(def_id))
    }
}

pub trait ToHir {
    type Output;
    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output;
}

pub struct HirBuilder<'a> {
    def_lookup: DefLookup<'a>,
}

impl<'a> HirBuilder<'a> {
    pub fn new(def_table: &'a DefTable, node_def_lookup: &'a NodeDefLookup) -> Self {
        let def_lookup = DefLookup::new(def_table, node_def_lookup);
        Self { def_lookup }
    }

    pub fn build_module(&self, module: ast::Module) -> hir::Module {
        module.to_hir(&self.def_lookup)
    }
}

fn def_id(def_lookup: &DefLookup, node_id: ast::NodeId) -> DefId {
    let def = def_lookup
        .lookup_node_def(node_id)
        .unwrap_or_else(|| panic!("Missing def for NodeId({})", node_id.0));
    def.id
}

impl ToHir for ast::Module {
    type Output = hir::Module;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::Module {
            top_level_items: self
                .top_level_items
                .into_iter()
                .map(|item| item.to_hir(def_lookup))
                .collect(),
        }
    }
}

impl ToHir for ast::TopLevelItem {
    type Output = hir::Decl;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            ast::TopLevelItem::TypeDef(type_def) => hir::Decl::TypeDef(type_def.to_hir(def_lookup)),
            ast::TopLevelItem::FuncDecl(func_decl) => {
                hir::Decl::FuncDecl(func_decl.to_hir(def_lookup))
            }
            ast::TopLevelItem::FuncDef(func_def) => hir::Decl::FuncDef(func_def.to_hir(def_lookup)),
            ast::TopLevelItem::MethodBlock(method_block) => {
                hir::Decl::MethodBlock(method_block.to_hir(def_lookup))
            }
            ast::TopLevelItem::ClosureDecl(closure_decl) => {
                hir::Decl::ClosureDecl(closure_decl.to_hir(def_lookup))
            }
        }
    }
}

impl ToHir for ast::TypeDef {
    type Output = hir::TypeDef;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::TypeDef {
            id: self.id,
            def_id: def_id(def_lookup, self.id),
            name: self.name,
            kind: match self.kind {
                ast::TypeDefKind::Alias { aliased_ty } => hir::TypeDefKind::Alias {
                    aliased_ty: aliased_ty.to_hir(def_lookup),
                },
                ast::TypeDefKind::Struct { fields } => hir::TypeDefKind::Struct {
                    fields: fields
                        .into_iter()
                        .map(|field| field.to_hir(def_lookup))
                        .collect(),
                },
                ast::TypeDefKind::Enum { variants } => hir::TypeDefKind::Enum {
                    variants: variants
                        .into_iter()
                        .map(|variant| variant.to_hir(def_lookup))
                        .collect(),
                },
            },
            span: self.span,
        }
    }
}

impl ToHir for ast::StructDefField {
    type Output = hir::StructDefField;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        struct_def_field_to_hir(def_lookup, &self)
    }
}

impl ToHir for &ast::StructDefField {
    type Output = hir::StructDefField;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        struct_def_field_to_hir(def_lookup, self)
    }
}

fn struct_def_field_to_hir(
    def_lookup: &DefLookup,
    field: &ast::StructDefField,
) -> hir::StructDefField {
    hir::StructDefField {
        id: field.id,
        name: field.name.clone(),
        ty: (&field.ty).to_hir(def_lookup),
        span: field.span,
    }
}

impl ToHir for ast::EnumDefVariant {
    type Output = hir::EnumDefVariant;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        enum_def_variant_to_hir(def_lookup, &self)
    }
}

impl ToHir for &ast::EnumDefVariant {
    type Output = hir::EnumDefVariant;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        enum_def_variant_to_hir(def_lookup, self)
    }
}

fn enum_def_variant_to_hir(
    def_lookup: &DefLookup,
    variant: &ast::EnumDefVariant,
) -> hir::EnumDefVariant {
    hir::EnumDefVariant {
        id: variant.id,
        name: variant.name.clone(),
        payload: variant
            .payload
            .iter()
            .map(|ty| ty.to_hir(def_lookup))
            .collect(),
        span: variant.span,
    }
}

impl ToHir for ast::TypeExpr {
    type Output = hir::TypeExpr;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        type_expr_to_hir(def_lookup, &self)
    }
}

impl ToHir for &ast::TypeExpr {
    type Output = hir::TypeExpr;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        type_expr_to_hir(def_lookup, self)
    }
}

fn type_expr_to_hir(def_lookup: &DefLookup, type_expr: &ast::TypeExpr) -> hir::TypeExpr {
    hir::TypeExpr {
        id: type_expr.id,
        span: type_expr.span,
        kind: match &type_expr.kind {
            ast::TypeExprKind::Named(_name) => {
                hir::TypeExprKind::Named(def_id(def_lookup, type_expr.id))
            }
            ast::TypeExprKind::Array { elem_ty_expr, dims } => hir::TypeExprKind::Array {
                elem_ty_expr: Box::new(elem_ty_expr.as_ref().to_hir(def_lookup)),
                dims: dims.clone(),
            },
            ast::TypeExprKind::Tuple { field_ty_exprs } => hir::TypeExprKind::Tuple {
                field_ty_exprs: field_ty_exprs
                    .iter()
                    .map(|t| t.to_hir(def_lookup))
                    .collect(),
            },
            ast::TypeExprKind::Range { min, max } => hir::TypeExprKind::Range {
                min: *min,
                max: *max,
            },
            ast::TypeExprKind::Slice { elem_ty_expr } => hir::TypeExprKind::Slice {
                elem_ty_expr: Box::new(elem_ty_expr.as_ref().to_hir(def_lookup)),
            },
            ast::TypeExprKind::Heap { elem_ty_expr } => hir::TypeExprKind::Heap {
                elem_ty_expr: Box::new(elem_ty_expr.as_ref().to_hir(def_lookup)),
            },
            ast::TypeExprKind::Fn {
                params,
                ret_ty_expr,
            } => hir::TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| param.to_hir(def_lookup))
                    .collect(),
                ret_ty_expr: Box::new(ret_ty_expr.as_ref().to_hir(def_lookup)),
            },
        },
    }
}

impl ToHir for ast::FnTypeParam {
    type Output = hir::FnTypeParam;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        fn_type_param_to_hir(def_lookup, &self)
    }
}

impl ToHir for &ast::FnTypeParam {
    type Output = hir::FnTypeParam;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        fn_type_param_to_hir(def_lookup, self)
    }
}

fn fn_type_param_to_hir(def_lookup: &DefLookup, param: &ast::FnTypeParam) -> hir::FnTypeParam {
    hir::FnTypeParam {
        mode: param.mode.clone(),
        ty_expr: (&param.ty_expr).to_hir(def_lookup),
    }
}

impl ToHir for ast::FuncDecl {
    type Output = hir::FuncDecl;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        hir::FuncDecl {
            id: self.id,
            def_id,
            sig: self.sig.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::FuncDef {
    type Output = hir::FuncDef;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        hir::FuncDef {
            id: self.id,
            def_id,
            sig: self.sig.to_hir(def_lookup),
            body: self.body.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::FunctionSig {
    type Output = hir::FunctionSig;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::FunctionSig {
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.to_hir(def_lookup))
                .collect(),
            ret_ty_expr: self.ret_ty_expr.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::MethodBlock {
    type Output = hir::MethodBlock;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::MethodBlock {
            id: self.id,
            type_name: self.type_name,
            method_defs: self
                .method_defs
                .into_iter()
                .map(|method| method.to_hir(def_lookup))
                .collect(),
            span: self.span,
        }
    }
}

impl ToHir for ast::MethodDef {
    type Output = hir::MethodDef;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        hir::MethodDef {
            id: self.id,
            def_id,
            sig: self.sig.to_hir(def_lookup),
            body: self.body.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::MethodSig {
    type Output = hir::MethodSig;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let self_def_id = def_id(def_lookup, self.self_param.id);
        hir::MethodSig {
            name: self.name,
            self_param: hir::SelfParam {
                id: self.self_param.id,
                def_id: self_def_id,
                mode: self.self_param.mode,
                span: self.self_param.span,
            },
            params: self
                .params
                .into_iter()
                .map(|param| param.to_hir(def_lookup))
                .collect(),
            ret_ty_expr: self.ret_ty_expr.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::ClosureDecl {
    type Output = hir::ClosureDecl;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let def_id = def_id(def_lookup, self.id);
        hir::ClosureDecl {
            id: self.id,
            def_id,
            sig: self.sig.to_hir(def_lookup),
            body: self.body.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::ClosureSig {
    type Output = hir::ClosureSig;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::ClosureSig {
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.to_hir(def_lookup))
                .collect(),
            return_ty: self.return_ty.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::Param {
    type Output = hir::Param;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::Param {
            id: self.id,
            ident: def_id(def_lookup, self.id),
            typ: self.typ.to_hir(def_lookup),
            mode: self.mode,
            span: self.span,
        }
    }
}

impl ToHir for ast::Expr {
    type Output = hir::Expr;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let id = self.id;
        let span = self.span;
        let kind = match self.kind {
            ast::ExprKind::Block { items, tail } => hir::ExprKind::Block {
                items: items
                    .into_iter()
                    .map(|item| item.to_hir(def_lookup))
                    .collect(),
                tail: tail.map(|expr| Box::new(expr.to_hir(def_lookup))),
            },

            ast::ExprKind::UnitLit => hir::ExprKind::UnitLit,
            ast::ExprKind::IntLit(value) => hir::ExprKind::IntLit(value),
            ast::ExprKind::BoolLit(value) => hir::ExprKind::BoolLit(value),
            ast::ExprKind::CharLit(value) => hir::ExprKind::CharLit(value),
            ast::ExprKind::StringLit { value } => hir::ExprKind::StringLit { value },
            ast::ExprKind::StringFmt { segments } => hir::ExprKind::StringFmt {
                segments: segments
                    .into_iter()
                    .map(|seg| seg.to_hir(def_lookup))
                    .collect(),
            },

            ast::ExprKind::ArrayLit { elem_ty, init } => hir::ExprKind::ArrayLit {
                elem_ty: elem_ty.map(|ty| ty.to_hir(def_lookup)),
                init: init.to_hir(def_lookup),
            },
            ast::ExprKind::TupleLit(items) => {
                hir::ExprKind::TupleLit(items.into_iter().map(|e| e.to_hir(def_lookup)).collect())
            }
            ast::ExprKind::StructLit { name, fields } => hir::ExprKind::StructLit {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| field.to_hir(def_lookup))
                    .collect(),
            },
            ast::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => hir::ExprKind::EnumVariant {
                enum_name,
                variant,
                payload: payload
                    .into_iter()
                    .map(|expr| expr.to_hir(def_lookup))
                    .collect(),
            },
            ast::ExprKind::StructUpdate { target, fields } => hir::ExprKind::StructUpdate {
                target: Box::new(target.to_hir(def_lookup)),
                fields: fields
                    .into_iter()
                    .map(|field| field.to_hir(def_lookup))
                    .collect(),
            },

            ast::ExprKind::BinOp { left, op, right } => hir::ExprKind::BinOp {
                left: Box::new(left.to_hir(def_lookup)),
                op,
                right: Box::new(right.to_hir(def_lookup)),
            },
            ast::ExprKind::UnaryOp { op, expr } => hir::ExprKind::UnaryOp {
                op,
                expr: Box::new(expr.to_hir(def_lookup)),
            },
            ast::ExprKind::HeapAlloc { expr } => hir::ExprKind::HeapAlloc {
                expr: Box::new(expr.to_hir(def_lookup)),
            },
            ast::ExprKind::Move { expr } => hir::ExprKind::Move {
                expr: Box::new(expr.to_hir(def_lookup)),
            },
            ast::ExprKind::Var(_) => hir::ExprKind::Var(def_id(def_lookup, id)),
            ast::ExprKind::ArrayIndex { target, indices } => hir::ExprKind::ArrayIndex {
                target: Box::new(target.to_hir(def_lookup)),
                indices: indices
                    .into_iter()
                    .map(|index| index.to_hir(def_lookup))
                    .collect(),
            },
            ast::ExprKind::TupleField { target, index } => hir::ExprKind::TupleField {
                target: Box::new(target.to_hir(def_lookup)),
                index,
            },
            ast::ExprKind::StructField { target, field } => hir::ExprKind::StructField {
                target: Box::new(target.to_hir(def_lookup)),
                field,
            },

            ast::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => hir::ExprKind::If {
                cond: Box::new(cond.to_hir(def_lookup)),
                then_body: Box::new(then_body.to_hir(def_lookup)),
                else_body: Box::new(else_body.to_hir(def_lookup)),
            },

            ast::ExprKind::Range { start, end } => hir::ExprKind::Range { start, end },
            ast::ExprKind::Slice { target, start, end } => hir::ExprKind::Slice {
                target: Box::new(target.to_hir(def_lookup)),
                start: start.map(|expr| Box::new(expr.to_hir(def_lookup))),
                end: end.map(|expr| Box::new(expr.to_hir(def_lookup))),
            },
            ast::ExprKind::Match { scrutinee, arms } => hir::ExprKind::Match {
                scrutinee: Box::new(scrutinee.to_hir(def_lookup)),
                arms: arms.into_iter().map(|arm| arm.to_hir(def_lookup)).collect(),
            },

            ast::ExprKind::Call { callee, args } => hir::ExprKind::Call {
                callee: Box::new(callee.to_hir(def_lookup)),
                args: args.into_iter().map(|arg| arg.to_hir(def_lookup)).collect(),
            },
            ast::ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => hir::ExprKind::MethodCall {
                callee: Box::new(callee.to_hir(def_lookup)),
                method_name,
                args: args.into_iter().map(|arg| arg.to_hir(def_lookup)).collect(),
            },

            ast::ExprKind::Closure {
                params,
                return_ty,
                body,
                ..
            } => hir::ExprKind::Closure {
                ident: def_id(def_lookup, id),
                params: params
                    .into_iter()
                    .map(|param| param.to_hir(def_lookup))
                    .collect(),
                return_ty: return_ty.to_hir(def_lookup),
                body: Box::new(body.to_hir(def_lookup)),
            },
        };

        hir::Expr { id, span, kind }
    }
}

impl ToHir for ast::BlockItem {
    type Output = hir::BlockItem;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            ast::BlockItem::Stmt(stmt) => hir::BlockItem::Stmt(stmt.to_hir(def_lookup)),
            ast::BlockItem::Expr(expr) => hir::BlockItem::Expr(expr.to_hir(def_lookup)),
        }
    }
}

impl ToHir for ast::StmtExpr {
    type Output = hir::StmtExpr;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let id = self.id;
        let span = self.span;
        let kind = match self.kind {
            ast::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => hir::StmtExprKind::LetBind {
                pattern: pattern.to_hir(def_lookup),
                decl_ty: decl_ty.map(|ty| ty.to_hir(def_lookup)),
                value: Box::new(value.to_hir(def_lookup)),
            },
            ast::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => hir::StmtExprKind::VarBind {
                pattern: pattern.to_hir(def_lookup),
                decl_ty: decl_ty.map(|ty| ty.to_hir(def_lookup)),
                value: Box::new(value.to_hir(def_lookup)),
            },
            ast::StmtExprKind::VarDecl {
                ident: _ident,
                decl_ty,
            } => hir::StmtExprKind::VarDecl {
                ident: def_id(def_lookup, id),
                decl_ty: decl_ty.to_hir(def_lookup),
            },
            ast::StmtExprKind::Assign { assignee, value } => hir::StmtExprKind::Assign {
                assignee: Box::new(assignee.to_hir(def_lookup)),
                value: Box::new(value.to_hir(def_lookup)),
            },
            ast::StmtExprKind::While { cond, body } => hir::StmtExprKind::While {
                cond: Box::new(cond.to_hir(def_lookup)),
                body: Box::new(body.to_hir(def_lookup)),
            },
            ast::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => hir::StmtExprKind::For {
                pattern: pattern.to_hir(def_lookup),
                iter: Box::new(iter.to_hir(def_lookup)),
                body: Box::new(body.to_hir(def_lookup)),
            },
        };

        hir::StmtExpr { id, span, kind }
    }
}

impl ToHir for ast::BindPattern {
    type Output = hir::BindPattern;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        let id = self.id;
        let span = self.span;
        let kind = match self.kind {
            ast::BindPatternKind::Name(_) => hir::BindPatternKind::Name(def_id(def_lookup, id)),
            ast::BindPatternKind::Array { patterns } => hir::BindPatternKind::Array {
                patterns: patterns
                    .into_iter()
                    .map(|pat| pat.to_hir(def_lookup))
                    .collect(),
            },
            ast::BindPatternKind::Tuple { patterns } => hir::BindPatternKind::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|pat| pat.to_hir(def_lookup))
                    .collect(),
            },
            ast::BindPatternKind::Struct { name, fields } => hir::BindPatternKind::Struct {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| field.to_hir(def_lookup))
                    .collect(),
            },
        };

        hir::BindPattern { id, span, kind }
    }
}

impl ToHir for ast::StructFieldBindPattern {
    type Output = hir::StructPatternField;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::StructPatternField {
            name: self.name,
            pattern: self.pattern.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::MatchArm {
    type Output = hir::MatchArm;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::MatchArm {
            id: self.id,
            pattern: self.pattern.to_hir(def_lookup),
            body: self.body.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::MatchPattern {
    type Output = hir::MatchPattern;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            ast::MatchPattern::Wildcard { span } => hir::MatchPattern::Wildcard { span },
            ast::MatchPattern::BoolLit { value, span } => {
                hir::MatchPattern::BoolLit { value, span }
            }
            ast::MatchPattern::IntLit { value, span } => hir::MatchPattern::IntLit { value, span },
            ast::MatchPattern::Binding { id, ident: _, span } => hir::MatchPattern::Binding {
                id,
                ident: def_id(def_lookup, id),
                span,
            },
            ast::MatchPattern::Tuple { patterns, span } => hir::MatchPattern::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|pat| pat.to_hir(def_lookup))
                    .collect(),
                span,
            },
            ast::MatchPattern::EnumVariant {
                enum_name,
                variant_name,
                bindings,
                span,
            } => hir::MatchPattern::EnumVariant {
                enum_name,
                variant_name,
                bindings: bindings
                    .into_iter()
                    .map(|binding| binding.to_hir(def_lookup))
                    .collect(),
                span,
            },
        }
    }
}

impl ToHir for ast::MatchPatternBinding {
    type Output = hir::MatchPatternBinding;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            ast::MatchPatternBinding::Named { id, ident: _, span } => {
                hir::MatchPatternBinding::Named {
                    id,
                    ident: def_id(def_lookup, id),
                    span,
                }
            }
            ast::MatchPatternBinding::Wildcard { span } => {
                hir::MatchPatternBinding::Wildcard { span }
            }
        }
    }
}

impl ToHir for ast::CallArg {
    type Output = hir::CallArg;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::CallArg {
            mode: self.mode,
            expr: self.expr.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::ArrayLitInit {
    type Output = hir::ArrayLitInit;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            ast::ArrayLitInit::Elems(elems) => hir::ArrayLitInit::Elems(
                elems
                    .into_iter()
                    .map(|expr| expr.to_hir(def_lookup))
                    .collect(),
            ),
            ast::ArrayLitInit::Repeat(expr, count) => {
                hir::ArrayLitInit::Repeat(Box::new(expr.to_hir(def_lookup)), count)
            }
        }
    }
}

impl ToHir for ast::StringFmtSegment {
    type Output = hir::StringFmtSegment;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        match self {
            ast::StringFmtSegment::Literal { value, span } => {
                hir::StringFmtSegment::Literal { value, span }
            }
            ast::StringFmtSegment::Expr { expr, span } => hir::StringFmtSegment::Expr {
                expr: Box::new(expr.to_hir(def_lookup)),
                span,
            },
        }
    }
}

impl ToHir for ast::StructLitField {
    type Output = hir::StructLitField;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::StructLitField {
            id: self.id,
            name: self.name,
            value: self.value.to_hir(def_lookup),
            span: self.span,
        }
    }
}

impl ToHir for ast::StructUpdateField {
    type Output = hir::StructUpdateField;

    fn to_hir(self, def_lookup: &DefLookup) -> Self::Output {
        hir::StructUpdateField {
            id: self.id,
            name: self.name,
            value: self.value.to_hir(def_lookup),
            span: self.span,
        }
    }
}
