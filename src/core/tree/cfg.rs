//! Parsed-tree-based CFG construction.

use crate::core::analysis::dataflow::DataflowGraph;
use crate::resolve::DefId;
use crate::tree::{BindPattern, BlockItem, Expr, ExprKind, StmtExpr, StmtExprKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AstBlockId(pub usize);

pub enum CfgItem<'a, D, T = ()> {
    Stmt(&'a StmtExpr<D, T>),
    Expr(&'a Expr<D, T>),
}

pub enum CfgTerminator<'a, D, T = ()> {
    Goto(AstBlockId),
    If {
        cond: &'a Expr<D, T>,
        then_bb: AstBlockId,
        else_bb: AstBlockId,
    },
    End,
}

pub struct CfgNode<'a, D, T = ()> {
    pub items: Vec<CfgItem<'a, D, T>>,
    pub term: CfgTerminator<'a, D, T>,
    pub loop_inits: Vec<&'a BindPattern<D>>,
}

pub struct Cfg<'a, D, T = ()> {
    pub nodes: Vec<CfgNode<'a, D, T>>,
    preds: Vec<Vec<AstBlockId>>,
    succs: Vec<Vec<AstBlockId>>,
}

pub struct CfgBuilder<'a, D, T = ()> {
    nodes: Vec<CfgNode<'a, D, T>>,
    succs: Vec<Vec<AstBlockId>>,
    loop_stack: Vec<LoopContext>,
}

impl<'a, D, T> Default for CfgBuilder<'a, D, T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug)]
struct LoopContext {
    break_bb: AstBlockId,
    continue_bb: AstBlockId,
}

#[derive(Clone, Copy, Debug)]
struct BlockRange {
    entry: AstBlockId,
    exit: AstBlockId,
}

impl<'a, D, T> CfgBuilder<'a, D, T> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            succs: Vec::new(),
            loop_stack: Vec::new(),
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

    fn set_term(&mut self, block: AstBlockId, term: CfgTerminator<'a, D, T>) {
        self.nodes[block.0].term = term;
    }

    fn push_item(&mut self, block: AstBlockId, item: CfgItem<'a, D, T>) {
        self.nodes[block.0].items.push(item);
    }

    fn push_loop_init(&mut self, block: AstBlockId, pattern: &'a BindPattern<D>) {
        self.nodes[block.0].loop_inits.push(pattern);
    }

    pub fn build_from_expr(self, expr: &'a Expr<D, T>) -> Cfg<'a, D, T> {
        let mut builder = self;
        let _ = builder.build_block_expr(expr);
        builder.finish()
    }

    fn build_block_expr(&mut self, expr: &'a Expr<D, T>) -> BlockRange {
        let ExprKind::Block { items, tail } = &expr.kind else {
            // For now, require a block expression at entry.
            let b = self.new_block();
            self.push_item(b, CfgItem::Expr(expr));
            return BlockRange { entry: b, exit: b };
        };

        let entry_bb = self.new_block();
        let mut curr_bb = entry_bb;

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

        BlockRange {
            entry: entry_bb,
            exit: curr_bb,
        }
    }

    fn handle_stmt(&mut self, curr_bb: AstBlockId, stmt: &'a StmtExpr<D, T>) -> AstBlockId {
        match &stmt.kind {
            StmtExprKind::While { cond, body } => {
                let cond_bb = self.new_block();
                let exit_bb = self.new_block();
                let loop_ctx = LoopContext {
                    break_bb: exit_bb,
                    continue_bb: cond_bb,
                };
                self.loop_stack.push(loop_ctx);
                let body_range = self.build_block_expr(body);
                self.loop_stack.pop();

                // current -> cond
                self.set_term(curr_bb, CfgTerminator::Goto(cond_bb));
                self.push_edge(curr_bb, cond_bb);

                // cond -> body/exit
                self.set_term(
                    cond_bb,
                    CfgTerminator::If {
                        cond: cond.as_ref(),
                        then_bb: body_range.entry,
                        else_bb: exit_bb,
                    },
                );
                self.push_edge(cond_bb, body_range.entry);
                self.push_edge(cond_bb, exit_bb);

                // body -> cond
                self.set_term(body_range.exit, CfgTerminator::Goto(cond_bb));
                self.push_edge(body_range.exit, cond_bb);

                exit_bb
            }
            StmtExprKind::For { body, .. } => {
                // For now, treat like while over an iterator expression.
                // We can refine to ir iter uses or desugaring later.
                let cond_bb = self.new_block();
                let exit_bb = self.new_block();
                let loop_ctx = LoopContext {
                    break_bb: exit_bb,
                    continue_bb: cond_bb,
                };
                self.loop_stack.push(loop_ctx);
                let body_range = self.build_block_expr(body);
                self.loop_stack.pop();

                self.set_term(curr_bb, CfgTerminator::Goto(cond_bb));
                self.push_edge(curr_bb, cond_bb);

                // Placeholder: no explicit condition expr yet, so use End with two edges.
                self.set_term(cond_bb, CfgTerminator::End);
                self.push_edge(cond_bb, body_range.entry);
                self.push_edge(cond_bb, exit_bb);

                // Loop pattern bindings are initialized at the start of each body iteration.
                if let StmtExprKind::For { pattern, .. } = &stmt.kind {
                    self.push_loop_init(body_range.entry, pattern);
                }

                self.set_term(body_range.exit, CfgTerminator::Goto(cond_bb));
                self.push_edge(body_range.exit, cond_bb);

                exit_bb
            }
            StmtExprKind::Break => {
                self.push_item(curr_bb, CfgItem::Stmt(stmt));
                if let Some(loop_ctx) = self.loop_stack.last().copied() {
                    self.set_term(curr_bb, CfgTerminator::Goto(loop_ctx.break_bb));
                    self.push_edge(curr_bb, loop_ctx.break_bb);
                } else {
                    self.set_term(curr_bb, CfgTerminator::End);
                }
                self.new_block()
            }
            StmtExprKind::Continue => {
                self.push_item(curr_bb, CfgItem::Stmt(stmt));
                if let Some(loop_ctx) = self.loop_stack.last().copied() {
                    self.set_term(curr_bb, CfgTerminator::Goto(loop_ctx.continue_bb));
                    self.push_edge(curr_bb, loop_ctx.continue_bb);
                } else {
                    self.set_term(curr_bb, CfgTerminator::End);
                }
                self.new_block()
            }
            StmtExprKind::Return { .. } => {
                self.push_item(curr_bb, CfgItem::Stmt(stmt));
                self.set_term(curr_bb, CfgTerminator::End);
                self.new_block()
            }
            _ => {
                self.push_item(curr_bb, CfgItem::Stmt(stmt));
                curr_bb
            }
        }
    }

    fn handle_expr(&mut self, cur: AstBlockId, expr: &'a Expr<D, T>) -> AstBlockId {
        match &expr.kind {
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let then_range = self.build_block_expr(then_body);
                let else_range = self.build_block_expr(else_body);
                let join = self.new_block();

                self.set_term(
                    cur,
                    CfgTerminator::If {
                        cond: cond.as_ref(),
                        then_bb: then_range.entry,
                        else_bb: else_range.entry,
                    },
                );
                self.push_edge(cur, then_range.entry);
                self.push_edge(cur, else_range.entry);

                // Join edges (if blocks don’t already end).
                self.push_edge(then_range.exit, join);
                self.push_edge(else_range.exit, join);

                join
            }
            _ => cur,
        }
    }

    fn finish(self) -> Cfg<'a, D, T> {
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

impl<D, T> DataflowGraph for Cfg<'_, D, T> {
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

pub type TreeCfgItem<'a, Ty = ()> = CfgItem<'a, DefId, Ty>;
pub type TreeCfgTerminator<'a, Ty = ()> = CfgTerminator<'a, DefId, Ty>;
pub type TreeCfgNode<'a, Ty = ()> = CfgNode<'a, DefId, Ty>;
pub type TreeCfg<'a, Ty = ()> = Cfg<'a, DefId, Ty>;
pub type TreeCfgBuilder<'a, Ty = ()> = CfgBuilder<'a, DefId, Ty>;
