//! Build HIR from resolved AST.
//!
//! This is the first resolved HIR step: identifiers are replaced with DefIds.

use crate::ast;
use crate::diag::Span;
use crate::hir::model as hir;
use crate::resolve::def_map::DefMap;

pub struct HirBuilder<'a> {
    def_map: &'a DefMap,
}

impl<'a> HirBuilder<'a> {
    pub fn new(def_map: &'a DefMap) -> Self {
        Self { def_map }
    }

    pub fn build_module(&self, module: ast::Module) -> hir::Module {
        hir::Module {
            decls: module
                .decls
                .into_iter()
                .map(|decl| self.build_decl(decl))
                .collect(),
        }
    }

    fn build_decl(&self, decl: ast::Decl) -> hir::Decl {
        match decl {
            ast::Decl::TypeDecl(decl) => hir::Decl::TypeDecl(self.build_type_decl(decl)),
            ast::Decl::FunctionDecl(decl) => {
                hir::Decl::FunctionDecl(self.build_function_decl(decl))
            }
            ast::Decl::Function(func) => hir::Decl::Function(self.build_function(func)),
            ast::Decl::MethodBlock(block) => hir::Decl::MethodBlock(self.build_method_block(block)),
            ast::Decl::Closure(closure) => hir::Decl::Closure(self.build_closure_decl(closure)),
        }
    }

    fn build_ident(&self, node_id: ast::NodeId, span: Span) -> hir::Ident {
        let def = self
            .def_map
            .lookup_def(node_id)
            .unwrap_or_else(|| panic!("Missing def for NodeId({})", node_id.0));
        hir::Ident {
            def_id: def.id,
            span,
        }
    }

    fn build_type_decl(&self, decl: ast::TypeDecl) -> hir::TypeDecl {
        hir::TypeDecl {
            id: decl.id,
            name: decl.name,
            kind: match decl.kind {
                ast::TypeDeclKind::Alias { aliased_ty } => hir::TypeDeclKind::Alias {
                    aliased_ty: self.build_type_expr(aliased_ty),
                },
                ast::TypeDeclKind::Struct { fields } => hir::TypeDeclKind::Struct {
                    fields: fields
                        .into_iter()
                        .map(|field| hir::StructField {
                            id: field.id,
                            name: field.name,
                            ty: self.build_type_expr(field.ty),
                            span: field.span,
                        })
                        .collect(),
                },
                ast::TypeDeclKind::Enum { variants } => hir::TypeDeclKind::Enum {
                    variants: variants
                        .into_iter()
                        .map(|variant| hir::EnumVariant {
                            id: variant.id,
                            name: variant.name,
                            payload: variant
                                .payload
                                .into_iter()
                                .map(|ty| self.build_type_expr(ty))
                                .collect(),
                            span: variant.span,
                        })
                        .collect(),
                },
            },
            span: decl.span,
        }
    }

    fn build_type_expr(&self, expr: ast::TypeExpr) -> hir::TypeExpr {
        hir::TypeExpr {
            id: expr.id,
            span: expr.span,
            kind: match expr.kind {
                ast::TypeExprKind::Named(_name) => {
                    hir::TypeExprKind::Named(self.build_ident(expr.id, expr.span))
                }
                ast::TypeExprKind::Array { elem_ty, dims } => hir::TypeExprKind::Array {
                    elem_ty: Box::new(self.build_type_expr(*elem_ty)),
                    dims,
                },
                ast::TypeExprKind::Tuple { fields } => hir::TypeExprKind::Tuple {
                    fields: fields
                        .into_iter()
                        .map(|field| self.build_type_expr(field))
                        .collect(),
                },
                ast::TypeExprKind::Range { min, max } => hir::TypeExprKind::Range { min, max },
                ast::TypeExprKind::Slice { elem_ty } => hir::TypeExprKind::Slice {
                    elem_ty: Box::new(self.build_type_expr(*elem_ty)),
                },
                ast::TypeExprKind::Heap { elem_ty } => hir::TypeExprKind::Heap {
                    elem_ty: Box::new(self.build_type_expr(*elem_ty)),
                },
                ast::TypeExprKind::Fn { params, return_ty } => hir::TypeExprKind::Fn {
                    params: params
                        .into_iter()
                        .map(|param| hir::FnTypeParam {
                            mode: param.mode,
                            ty: self.build_type_expr(param.ty),
                        })
                        .collect(),
                    return_ty: Box::new(self.build_type_expr(*return_ty)),
                },
            },
        }
    }

    fn build_function_decl(&self, decl: ast::FunctionDecl) -> hir::FunctionDecl {
        hir::FunctionDecl {
            id: decl.id,
            sig: self.build_function_sig(decl.sig, decl.id),
            span: decl.span,
        }
    }

    fn build_function(&self, func: ast::Function) -> hir::Function {
        hir::Function {
            id: func.id,
            sig: self.build_function_sig(func.sig, func.id),
            body: self.build_expr(func.body),
            span: func.span,
        }
    }

    fn build_function_sig(&self, sig: ast::FunctionSig, owner_id: ast::NodeId) -> hir::FunctionSig {
        hir::FunctionSig {
            id: owner_id,
            name: self.build_ident(owner_id, sig.span),
            params: sig
                .params
                .into_iter()
                .map(|param| self.build_param(param))
                .collect(),
            return_type: self.build_type_expr(sig.return_type),
            span: sig.span,
        }
    }

    fn build_method_block(&self, block: ast::MethodBlock) -> hir::MethodBlock {
        hir::MethodBlock {
            id: block.id,
            name: block.type_name,
            methods: block
                .methods
                .into_iter()
                .map(|method| self.build_method(method))
                .collect(),
            span: block.span,
        }
    }

    fn build_method(&self, method: ast::Method) -> hir::Method {
        hir::Method {
            id: method.id,
            sig: self.build_method_sig(method.sig, method.id),
            body: self.build_expr(method.body),
            span: method.span,
        }
    }

    fn build_method_sig(&self, sig: ast::MethodSig, owner_id: ast::NodeId) -> hir::MethodSig {
        hir::MethodSig {
            id: owner_id,
            name: sig.name,
            self_param: hir::SelfParam {
                id: sig.self_param.id,
                mode: sig.self_param.mode,
                span: sig.self_param.span,
            },
            params: sig
                .params
                .into_iter()
                .map(|param| self.build_param(param))
                .collect(),
            return_type: self.build_type_expr(sig.return_type),
            span: sig.span,
        }
    }

    fn build_param(&self, param: ast::Param) -> hir::Param {
        hir::Param {
            id: param.id,
            name: self.build_ident(param.id, param.span),
            typ: self.build_type_expr(param.typ),
            mode: param.mode,
            span: param.span,
        }
    }

    fn build_expr(&self, expr: ast::Expr) -> hir::Expr {
        let id = expr.id;
        let span = expr.span;
        let kind = match expr.kind {
            ast::ExprKind::Block { items, tail } => hir::ExprKind::Block {
                items: items
                    .into_iter()
                    .map(|item| self.build_block_item(item))
                    .collect(),
                tail: tail.map(|expr| Box::new(self.build_expr(*expr))),
            },

            ast::ExprKind::UnitLit => hir::ExprKind::UnitLit,
            ast::ExprKind::IntLit(value) => hir::ExprKind::IntLit(value),
            ast::ExprKind::BoolLit(value) => hir::ExprKind::BoolLit(value),
            ast::ExprKind::CharLit(value) => hir::ExprKind::CharLit(value),
            ast::ExprKind::StringLit { value } => hir::ExprKind::StringLit { value },
            ast::ExprKind::StringFmt { segments } => hir::ExprKind::StringFmt {
                segments: segments
                    .into_iter()
                    .map(|seg| self.build_string_fmt_segment(seg))
                    .collect(),
            },

            ast::ExprKind::ArrayLit { elem_ty, init } => hir::ExprKind::ArrayLit {
                elem_ty: elem_ty.map(|ty| self.build_type_expr(ty)),
                init: self.build_array_lit_init(init),
            },
            ast::ExprKind::TupleLit(items) => {
                hir::ExprKind::TupleLit(items.into_iter().map(|e| self.build_expr(e)).collect())
            }
            ast::ExprKind::StructLit { name, fields } => hir::ExprKind::StructLit {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| hir::StructLitField {
                        id: field.id,
                        name: field.name,
                        value: self.build_expr(field.value),
                        span: field.span,
                    })
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
                    .map(|expr| self.build_expr(expr))
                    .collect(),
            },
            ast::ExprKind::StructUpdate { target, fields } => hir::ExprKind::StructUpdate {
                target: Box::new(self.build_expr(*target)),
                fields: fields
                    .into_iter()
                    .map(|field| hir::StructUpdateField {
                        id: field.id,
                        name: field.name,
                        value: self.build_expr(field.value),
                        span: field.span,
                    })
                    .collect(),
            },
            ast::ExprKind::BinOp { left, op, right } => hir::ExprKind::BinOp {
                left: Box::new(self.build_expr(*left)),
                op,
                right: Box::new(self.build_expr(*right)),
            },
            ast::ExprKind::UnaryOp { op, expr } => hir::ExprKind::UnaryOp {
                op,
                expr: Box::new(self.build_expr(*expr)),
            },
            ast::ExprKind::HeapAlloc { expr } => hir::ExprKind::HeapAlloc {
                expr: Box::new(self.build_expr(*expr)),
            },
            ast::ExprKind::Move { expr } => hir::ExprKind::Move {
                expr: Box::new(self.build_expr(*expr)),
            },
            ast::ExprKind::Var(_) => hir::ExprKind::Var(self.build_ident(id, span)),
            ast::ExprKind::ArrayIndex { target, indices } => hir::ExprKind::ArrayIndex {
                target: Box::new(self.build_expr(*target)),
                indices: indices
                    .into_iter()
                    .map(|expr| self.build_expr(expr))
                    .collect(),
            },
            ast::ExprKind::TupleField { target, index } => hir::ExprKind::TupleField {
                target: Box::new(self.build_expr(*target)),
                index,
            },
            ast::ExprKind::StructField { target, field } => hir::ExprKind::StructField {
                target: Box::new(self.build_expr(*target)),
                field,
            },
            ast::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => hir::ExprKind::If {
                cond: Box::new(self.build_expr(*cond)),
                then_body: Box::new(self.build_expr(*then_body)),
                else_body: Box::new(self.build_expr(*else_body)),
            },
            ast::ExprKind::Range { start, end } => hir::ExprKind::Range { start, end },
            ast::ExprKind::Slice { target, start, end } => hir::ExprKind::Slice {
                target: Box::new(self.build_expr(*target)),
                start: start.map(|expr| Box::new(self.build_expr(*expr))),
                end: end.map(|expr| Box::new(self.build_expr(*expr))),
            },
            ast::ExprKind::Match { scrutinee, arms } => hir::ExprKind::Match {
                scrutinee: Box::new(self.build_expr(*scrutinee)),
                arms: arms
                    .into_iter()
                    .map(|arm| self.build_match_arm(arm))
                    .collect(),
            },
            ast::ExprKind::Call { callee, args } => hir::ExprKind::Call {
                callee: Box::new(self.build_expr(*callee)),
                args: args
                    .into_iter()
                    .map(|arg| self.build_call_arg(arg))
                    .collect(),
            },
            ast::ExprKind::MethodCall {
                callee,
                method,
                args,
            } => hir::ExprKind::MethodCall {
                callee: Box::new(self.build_expr(*callee)),
                method,
                args: args
                    .into_iter()
                    .map(|arg| self.build_call_arg(arg))
                    .collect(),
            },
            ast::ExprKind::Closure {
                params,
                return_ty,
                body,
            } => hir::ExprKind::Closure {
                params: params
                    .into_iter()
                    .map(|param| self.build_param(param))
                    .collect(),
                return_ty: return_ty.map(|ty| self.build_type_expr(ty)),
                body: Box::new(self.build_expr(*body)),
            },
        };

        hir::Expr { id, span, kind }
    }

    fn build_block_item(&self, item: ast::BlockItem) -> hir::BlockItem {
        match item {
            ast::BlockItem::Stmt(stmt) => hir::BlockItem::Stmt(self.build_stmt_expr(stmt)),
            ast::BlockItem::Expr(expr) => hir::BlockItem::Expr(self.build_expr(expr)),
        }
    }

    fn build_stmt_expr(&self, stmt: ast::StmtExpr) -> hir::StmtExpr {
        let id = stmt.id;
        let span = stmt.span;
        let kind = match stmt.kind {
            ast::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => hir::StmtExprKind::LetBind {
                pattern: self.build_pattern(pattern),
                decl_ty: decl_ty.map(|ty| self.build_type_expr(ty)),
                value: Box::new(self.build_expr(*value)),
            },
            ast::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => hir::StmtExprKind::VarBind {
                pattern: self.build_pattern(pattern),
                decl_ty: decl_ty.map(|ty| self.build_type_expr(ty)),
                value: Box::new(self.build_expr(*value)),
            },
            ast::StmtExprKind::VarDecl {
                name: _name,
                decl_ty,
            } => hir::StmtExprKind::VarDecl {
                name: self.build_ident(id, span),
                decl_ty: self.build_type_expr(decl_ty),
            },
            ast::StmtExprKind::Assign { assignee, value } => hir::StmtExprKind::Assign {
                assignee: Box::new(self.build_expr(*assignee)),
                value: Box::new(self.build_expr(*value)),
            },
            ast::StmtExprKind::While { cond, body } => hir::StmtExprKind::While {
                cond: Box::new(self.build_expr(*cond)),
                body: Box::new(self.build_expr(*body)),
            },
            ast::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => hir::StmtExprKind::For {
                pattern: self.build_pattern(pattern),
                iter: Box::new(self.build_expr(*iter)),
                body: Box::new(self.build_expr(*body)),
            },
        };

        hir::StmtExpr { id, span, kind }
    }

    fn build_pattern(&self, pattern: ast::Pattern) -> hir::Pattern {
        let id = pattern.id;
        let span = pattern.span;
        let kind = match pattern.kind {
            ast::PatternKind::Ident { .. } => hir::PatternKind::Ident {
                ident: self.build_ident(id, span),
            },
            ast::PatternKind::Array { patterns } => hir::PatternKind::Array {
                patterns: patterns
                    .into_iter()
                    .map(|pat| self.build_pattern(pat))
                    .collect(),
            },
            ast::PatternKind::Tuple { patterns } => hir::PatternKind::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|pat| self.build_pattern(pat))
                    .collect(),
            },
            ast::PatternKind::Struct { name, fields } => hir::PatternKind::Struct {
                name,
                fields: fields
                    .into_iter()
                    .map(|field| hir::StructPatternField {
                        name: field.name,
                        pattern: self.build_pattern(field.pattern),
                        span: field.span,
                    })
                    .collect(),
            },
        };

        hir::Pattern { id, span, kind }
    }

    fn build_match_arm(&self, arm: ast::MatchArm) -> hir::MatchArm {
        hir::MatchArm {
            id: arm.id,
            pattern: self.build_match_pattern(arm.pattern),
            body: self.build_expr(arm.body),
            span: arm.span,
        }
    }

    fn build_match_pattern(&self, pattern: ast::MatchPattern) -> hir::MatchPattern {
        match pattern {
            ast::MatchPattern::Wildcard { span } => hir::MatchPattern::Wildcard { span },
            ast::MatchPattern::BoolLit { value, span } => {
                hir::MatchPattern::BoolLit { value, span }
            }
            ast::MatchPattern::IntLit { value, span } => hir::MatchPattern::IntLit { value, span },
            ast::MatchPattern::Binding { id, span, .. } => hir::MatchPattern::Binding {
                ident: self.build_ident(id, span),
            },
            ast::MatchPattern::Tuple { patterns, span } => hir::MatchPattern::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|pat| self.build_match_pattern(pat))
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
                    .map(|binding| self.build_match_pattern_binding(binding))
                    .collect(),
                span,
            },
        }
    }

    fn build_match_pattern_binding(
        &self,
        binding: ast::MatchPatternBinding,
    ) -> hir::MatchPatternBinding {
        match binding {
            ast::MatchPatternBinding::Named { id, span, .. } => hir::MatchPatternBinding::Named {
                ident: self.build_ident(id, span),
            },
            ast::MatchPatternBinding::Wildcard { span } => {
                hir::MatchPatternBinding::Wildcard { span }
            }
        }
    }

    fn build_call_arg(&self, arg: ast::CallArg) -> hir::CallArg {
        hir::CallArg {
            mode: arg.mode,
            expr: self.build_expr(arg.expr),
            span: arg.span,
        }
    }

    fn build_array_lit_init(&self, init: ast::ArrayLitInit) -> hir::ArrayLitInit {
        match init {
            ast::ArrayLitInit::Elems(elems) => hir::ArrayLitInit::Elems(
                elems
                    .into_iter()
                    .map(|expr| self.build_expr(expr))
                    .collect(),
            ),
            ast::ArrayLitInit::Repeat(expr, count) => {
                hir::ArrayLitInit::Repeat(Box::new(self.build_expr(*expr)), count)
            }
        }
    }

    fn build_string_fmt_segment(&self, seg: ast::StringFmtSegment) -> hir::StringFmtSegment {
        match seg {
            ast::StringFmtSegment::Literal { value, span } => {
                hir::StringFmtSegment::Literal { value, span }
            }
            ast::StringFmtSegment::Expr { expr, span } => hir::StringFmtSegment::Expr {
                expr: Box::new(self.build_expr(*expr)),
                span,
            },
        }
    }

    fn build_closure_decl(&self, closure: ast::Expr) -> hir::Closure {
        let id = closure.id;
        let span = closure.span;
        let ast::ExprKind::Closure {
            params,
            return_ty,
            body,
        } = closure.kind
        else {
            panic!(
                "Expected closure expression for Decl::Closure (NodeId({}))",
                id.0
            );
        };

        hir::Closure {
            id,
            params: params
                .into_iter()
                .map(|param| self.build_param(param))
                .collect(),
            return_ty: return_ty.map(|ty| self.build_type_expr(ty)),
            body: self.build_expr(*body),
            span,
        }
    }
}
