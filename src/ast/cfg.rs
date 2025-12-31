//! AST-based CFG construction.

use crate::analysis::dataflow::DataflowGraph;
use crate::ast::{BlockItem, Expr, ExprKind, StmtExpr, StmtExprKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AstBlockId(pub usize);

pub enum AstItem<'a> {
    Stmt(&'a StmtExpr),
    Expr(&'a Expr),
}

pub enum AstTerminator<'a> {
    Goto(AstBlockId),
    If {
        cond: &'a Expr,
        then_bb: AstBlockId,
        else_bb: AstBlockId,
    },
    End,
}

pub struct AstCfgNode<'a> {
    pub items: Vec<AstItem<'a>>,
    pub term: AstTerminator<'a>,
}

pub struct AstCfg<'a> {
    pub nodes: Vec<AstCfgNode<'a>>,
    preds: Vec<Vec<AstBlockId>>,
    succs: Vec<Vec<AstBlockId>>,
}

pub struct AstCfgBuilder<'a> {
    nodes: Vec<AstCfgNode<'a>>,
    succs: Vec<Vec<AstBlockId>>,
}

impl<'a> Default for AstCfgBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> AstCfgBuilder<'a> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            succs: Vec::new(),
        }
    }

    fn new_block(&mut self) -> AstBlockId {
        let id = AstBlockId(self.nodes.len());
        self.nodes.push(AstCfgNode {
            items: Vec::new(),
            term: AstTerminator::End,
        });
        self.succs.push(Vec::new());
        id
    }

    fn push_edge(&mut self, from: AstBlockId, to: AstBlockId) {
        self.succs[from.0].push(to);
    }

    fn set_term(&mut self, block: AstBlockId, term: AstTerminator<'a>) {
        self.nodes[block.0].term = term;
    }

    fn push_item(&mut self, block: AstBlockId, item: AstItem<'a>) {
        self.nodes[block.0].items.push(item);
    }

    pub fn build_from_expr(self, expr: &'a Expr) -> AstCfg<'a> {
        let mut builder = self;
        builder.build_block_expr(expr);
        builder.finish()
    }

    fn build_block_expr(&mut self, expr: &'a Expr) -> AstBlockId {
        let ExprKind::Block { items, tail } = &expr.kind else {
            // For now, require a block expression at entry.
            let b = self.new_block();
            self.push_item(b, AstItem::Expr(expr));
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
                        self.push_item(curr_bb, AstItem::Expr(expr));
                        curr_bb = self.handle_expr(curr_bb, expr);
                    }
                },
            }
        }

        if let Some(tail) = tail {
            self.push_item(curr_bb, AstItem::Expr(tail));
            curr_bb = self.handle_expr(curr_bb, tail);
        }

        curr_bb
    }

    fn handle_stmt(&mut self, curr_bb: AstBlockId, stmt: &'a StmtExpr) -> AstBlockId {
        match &stmt.kind {
            StmtExprKind::While { cond, body } => {
                let cond_bb = self.new_block();
                let body_bb = self.build_block_expr(body);
                let exit_bb = self.new_block();

                // current -> cond
                self.set_term(curr_bb, AstTerminator::Goto(cond_bb));

                // cond -> body/exit
                self.set_term(
                    cond_bb,
                    AstTerminator::If {
                        cond: cond.as_ref(),
                        then_bb: body_bb,
                        else_bb: exit_bb,
                    },
                );
                self.push_edge(cond_bb, body_bb);
                self.push_edge(cond_bb, exit_bb);

                // body -> cond
                self.set_term(body_bb, AstTerminator::Goto(cond_bb));
                self.push_edge(body_bb, cond_bb);

                exit_bb
            }
            StmtExprKind::For { body, .. } => {
                // For now, treat like while over an iterator expression.
                // We can refine to model iter uses or desugaring later.
                let cond_bb = self.new_block();
                let body_bb = self.build_block_expr(body);
                let exit_bb = self.new_block();

                self.set_term(curr_bb, AstTerminator::Goto(cond_bb));
                self.push_edge(curr_bb, cond_bb);

                // Placeholder: no explicit condition expr yet, so use End with two edges.
                self.set_term(cond_bb, AstTerminator::End);
                self.push_edge(cond_bb, body_bb);
                self.push_edge(cond_bb, exit_bb);

                self.set_term(body_bb, AstTerminator::Goto(cond_bb));
                self.push_edge(body_bb, cond_bb);

                exit_bb
            }
            _ => {
                self.push_item(curr_bb, AstItem::Stmt(stmt));
                curr_bb
            }
        }
    }

    fn handle_expr(&mut self, cur: AstBlockId, expr: &'a Expr) -> AstBlockId {
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
                    AstTerminator::If {
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

    fn finish(self) -> AstCfg<'a> {
        let mut preds = vec![vec![]; self.nodes.len()];
        for (idx, outs) in self.succs.iter().enumerate() {
            let src = AstBlockId(idx);
            for &dst in outs {
                preds[dst.0].push(src);
            }
        }

        AstCfg {
            nodes: self.nodes,
            preds,
            succs: self.succs,
        }
    }
}

impl DataflowGraph for AstCfg<'_> {
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
