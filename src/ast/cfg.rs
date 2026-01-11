//! AST-based CFG construction.

use crate::analysis::dataflow::DataflowGraph;
use crate::ast::model::{BindPattern, BlockItem, Expr, ExprKind, StmtExpr, StmtExprKind};
use crate::resolve::DefId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AstBlockId(pub usize);

pub enum CfgItem<'a, T> {
    Stmt(&'a StmtExpr<T>),
    Expr(&'a Expr<T>),
}

pub enum CfgTerminator<'a, T> {
    Goto(AstBlockId),
    If {
        cond: &'a Expr<T>,
        then_bb: AstBlockId,
        else_bb: AstBlockId,
    },
    End,
}

pub struct CfgNode<'a, T> {
    pub items: Vec<CfgItem<'a, T>>,
    pub term: CfgTerminator<'a, T>,
    pub loop_inits: Vec<&'a BindPattern<T>>,
}

pub struct Cfg<'a, T> {
    pub nodes: Vec<CfgNode<'a, T>>,
    preds: Vec<Vec<AstBlockId>>,
    succs: Vec<Vec<AstBlockId>>,
}

pub struct CfgBuilder<'a, T> {
    nodes: Vec<CfgNode<'a, T>>,
    succs: Vec<Vec<AstBlockId>>,
}

impl<'a, T> Default for CfgBuilder<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T> CfgBuilder<'a, T> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            succs: Vec::new(),
        }
    }

    fn new_block(&mut self) -> AstBlockId {
        let id = AstBlockId(self.nodes.len());
        self.nodes.push(CfgNode {
            items: Vec::new(),
            term: CfgTerminator::End,
            loop_inits: Vec::new(),
        });
        self.succs.push(Vec::new());
        id
    }

    fn push_edge(&mut self, from: AstBlockId, to: AstBlockId) {
        self.succs[from.0].push(to);
    }

    fn set_term(&mut self, block: AstBlockId, term: CfgTerminator<'a, T>) {
        self.nodes[block.0].term = term;
    }

    fn push_item(&mut self, block: AstBlockId, item: CfgItem<'a, T>) {
        self.nodes[block.0].items.push(item);
    }

    fn push_loop_init(&mut self, block: AstBlockId, pattern: &'a BindPattern<T>) {
        self.nodes[block.0].loop_inits.push(pattern);
    }

    pub fn build_from_expr(self, expr: &'a Expr<T>) -> Cfg<'a, T> {
        let mut builder = self;
        builder.build_block_expr(expr);
        builder.finish()
    }

    fn build_block_expr(&mut self, expr: &'a Expr<T>) -> AstBlockId {
        let ExprKind::Block { items, tail } = &expr.kind else {
            // For now, require a block expression at entry.
            let b = self.new_block();
            self.push_item(b, CfgItem::Expr(expr));
            return b;
        };

        let mut curr_bb = self.new_block();

        for item in items {
            match item {
                BlockItem::Stmt(stmt) => {
                    curr_bb = self.handle_stmt(curr_bb, stmt);
                }
                BlockItem::Expr(expr) => match &expr.kind {
                    ExprKind::If { .. } => {
                        curr_bb = self.handle_expr(curr_bb, expr);
                    }
                    _ => {
                        self.push_item(curr_bb, CfgItem::Expr(expr));
                        curr_bb = self.handle_expr(curr_bb, expr);
                    }
                },
            }
        }

        if let Some(tail) = tail {
            self.push_item(curr_bb, CfgItem::Expr(tail));
            curr_bb = self.handle_expr(curr_bb, tail);
        }

        curr_bb
    }

    fn handle_stmt(&mut self, curr_bb: AstBlockId, stmt: &'a StmtExpr<T>) -> AstBlockId {
        match &stmt.kind {
            StmtExprKind::While { cond, body } => {
                let cond_bb = self.new_block();
                let body_bb = self.build_block_expr(body);
                let exit_bb = self.new_block();

                // current -> cond
                self.set_term(curr_bb, CfgTerminator::Goto(cond_bb));

                // cond -> body/exit
                self.set_term(
                    cond_bb,
                    CfgTerminator::If {
                        cond: cond.as_ref(),
                        then_bb: body_bb,
                        else_bb: exit_bb,
                    },
                );
                self.push_edge(cond_bb, body_bb);
                self.push_edge(cond_bb, exit_bb);

                // body -> cond
                self.set_term(body_bb, CfgTerminator::Goto(cond_bb));
                self.push_edge(body_bb, cond_bb);

                exit_bb
            }
            StmtExprKind::For { body, .. } => {
                // For now, treat like while over an iterator expression.
                // We can refine to model iter uses or desugaring later.
                let cond_bb = self.new_block();
                let body_bb = self.build_block_expr(body);
                let exit_bb = self.new_block();

                self.set_term(curr_bb, CfgTerminator::Goto(cond_bb));
                self.push_edge(curr_bb, cond_bb);

                // Placeholder: no explicit condition expr yet, so use End with two edges.
                self.set_term(cond_bb, CfgTerminator::End);
                self.push_edge(cond_bb, body_bb);
                self.push_edge(cond_bb, exit_bb);

                // Loop pattern bindings are initialized at the start of each body iteration.
                if let StmtExprKind::For { pattern, .. } = &stmt.kind {
                    self.push_loop_init(body_bb, pattern);
                }

                self.set_term(body_bb, CfgTerminator::Goto(cond_bb));
                self.push_edge(body_bb, cond_bb);

                exit_bb
            }
            _ => {
                self.push_item(curr_bb, CfgItem::Stmt(stmt));
                curr_bb
            }
        }
    }

    fn handle_expr(&mut self, cur: AstBlockId, expr: &'a Expr<T>) -> AstBlockId {
        match &expr.kind {
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let then_bb = self.build_block_expr(then_body);
                let else_bb = self.build_block_expr(else_body);
                let join = self.new_block();

                self.set_term(
                    cur,
                    CfgTerminator::If {
                        cond: cond.as_ref(),
                        then_bb,
                        else_bb,
                    },
                );
                self.push_edge(cur, then_bb);
                self.push_edge(cur, else_bb);

                // Join edges (if blocks don’t already end).
                self.push_edge(then_bb, join);
                self.push_edge(else_bb, join);

                join
            }
            _ => cur,
        }
    }

    fn finish(self) -> Cfg<'a, T> {
        let mut preds = vec![vec![]; self.nodes.len()];
        for (idx, outs) in self.succs.iter().enumerate() {
            let src = AstBlockId(idx);
            for &dst in outs {
                preds[dst.0].push(src);
            }
        }

        Cfg {
            nodes: self.nodes,
            preds,
            succs: self.succs,
        }
    }
}

impl<T> DataflowGraph for Cfg<'_, T> {
    type Node = AstBlockId;

    fn num_nodes(&self) -> usize {
        self.nodes.len()
    }

    fn index(&self, node: Self::Node) -> usize {
        node.0
    }

    fn node_at(&self, idx: usize) -> Self::Node {
        AstBlockId(idx)
    }

    fn preds(&self, node: Self::Node) -> &[Self::Node] {
        &self.preds[node.0]
    }

    fn succs(&self, node: Self::Node) -> &[Self::Node] {
        &self.succs[node.0]
    }
}

pub type AstItem<'a> = CfgItem<'a, String>;
pub type AstTerminator<'a> = CfgTerminator<'a, String>;
pub type AstCfgNode<'a> = CfgNode<'a, String>;
pub type AstCfg<'a> = Cfg<'a, String>;
pub type AstCfgBuilder<'a> = CfgBuilder<'a, String>;

pub type HirItem<'a> = CfgItem<'a, DefId>;
pub type HirTerminator<'a> = CfgTerminator<'a, DefId>;
pub type HirCfgNode<'a> = CfgNode<'a, DefId>;
pub type HirCfg<'a> = Cfg<'a, DefId>;
pub type HirCfgBuilder<'a> = CfgBuilder<'a, DefId>;
