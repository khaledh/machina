use super::*;

/// AST folder with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
/// Example:
/// ```rust
/// use machina::ast::{AstFolder, Expr, ExprKind, StmtExpr};
///
/// struct CountExprs;
/// impl AstFolder for CountExprs {
///     type Error = ();
///     type Output = usize;
///     type Input = ();
///
///     fn visit_expr(&mut self, expr: &Expr, _input: Option<&Self::Input>) -> Result<usize, ()> {
///         match &expr.kind {
///             ExprKind::BinOp { left, right, .. } => {
///                 let (left, right) = self.visit_binary_expr(left, right)?;
///                 Ok(left + right + 1)
///             }
///             _ => Ok(1),
///         }
///     }
///
///     fn visit_stmt_expr(&mut self, _stmt: &StmtExpr) -> Result<usize, ()> {
///         Ok(0)
///     }
/// }
/// ```
pub trait AstFolder {
    type Error;
    type Output;
    type Input;

    // --- Module ---

    fn visit_module(&mut self, module: &Module) -> Result<Vec<Self::Output>, Self::Error> {
        walk_module(self, module)
    }

    // --- Functions ---

    fn visit_func(&mut self, func: &Function) -> Result<Self::Output, Self::Error> {
        walk_func(self, func)
    }

    // --- Methods ---

    fn visit_method_block(
        &mut self,
        method_block: &MethodBlock,
    ) -> Result<Vec<Self::Output>, Self::Error> {
        walk_method_block(self, method_block)
    }

    fn visit_method(&mut self, method: &Method) -> Result<Self::Output, Self::Error> {
        walk_method(self, method)
    }

    // --- Blocks ---

    fn visit_block(
        &mut self,
        items: &[BlockItem],
        tail: Option<&Expr>,
    ) -> Result<(Vec<Self::Output>, Option<Self::Output>), Self::Error> {
        walk_block(self, items, tail)
    }

    fn visit_block_item(&mut self, item: &BlockItem) -> Result<Self::Output, Self::Error> {
        walk_block_item(self, item)
    }

    fn visit_block_tail(
        &mut self,
        tail: Option<&Expr>,
        expected: Option<&Self::Input>,
    ) -> Result<Option<Self::Output>, Self::Error> {
        walk_block_tail(self, tail, expected)
    }

    // --- Statements ---

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) -> Result<Self::Output, Self::Error>;

    // --- Expressions ---

    fn visit_expr(
        &mut self,
        expr: &Expr,
        input: Option<&Self::Input>,
    ) -> Result<Self::Output, Self::Error>;

    fn visit_exprs(&mut self, exprs: &[Expr]) -> Result<Vec<Self::Output>, Self::Error> {
        walk_exprs(self, exprs)
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        right: &Expr,
    ) -> Result<(Self::Output, Self::Output), Self::Error> {
        walk_binary_expr(self, left, right)
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        args: &[CallArg],
    ) -> Result<Vec<Self::Output>, Self::Error> {
        walk_call(self, callee, args)
    }

    fn visit_array_lit_init(
        &mut self,
        init: &ArrayLitInit,
        expected: Option<&Self::Input>,
    ) -> Result<Vec<Self::Output>, Self::Error> {
        walk_array_lit_init(self, init, expected)
    }

    // --- Control Flow ---

    fn visit_if(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<(Self::Output, Self::Output, Self::Output), Self::Error> {
        walk_if(self, cond, then_body, else_body)
    }

    fn visit_match_arms<T>(
        &mut self,
        arms: &[MatchArm],
        visit_arm: impl FnMut(&mut Self, &MatchArm) -> Result<T, Self::Error>,
    ) -> Result<Vec<T>, Self::Error> {
        walk_match_arms(self, arms, visit_arm)
    }

    fn visit_match_arm(&mut self, arm: &MatchArm) -> Result<Self::Output, Self::Error> {
        walk_match_arm(self, arm)
    }

    // --- Calls ---

    fn visit_call_args(&mut self, args: &[CallArg]) -> Result<Vec<Self::Output>, Self::Error> {
        walk_call_args(self, args)
    }
}

// --- Module ---

pub fn walk_module<F: AstFolder + ?Sized>(
    f: &mut F,
    module: &Module,
) -> Result<Vec<F::Output>, F::Error> {
    let mut outputs = Vec::new();
    for decl in &module.decls {
        match decl {
            Decl::Function(func) => outputs.push(f.visit_func(func)?),
            Decl::MethodBlock(method_block) => {
                outputs.extend(f.visit_method_block(method_block)?);
            }
            _ => {}
        }
    }
    Ok(outputs)
}

// --- Functions ---

pub fn walk_func<F: AstFolder + ?Sized>(f: &mut F, func: &Function) -> Result<F::Output, F::Error> {
    walk_expr(f, &func.body)
}

// --- Methods ---

pub fn walk_method_block<F: AstFolder + ?Sized>(
    f: &mut F,
    method_block: &MethodBlock,
) -> Result<Vec<F::Output>, F::Error> {
    method_block
        .methods
        .iter()
        .map(|method| f.visit_method(method))
        .collect()
}

pub fn walk_method<F: AstFolder + ?Sized>(
    f: &mut F,
    method: &Method,
) -> Result<F::Output, F::Error> {
    walk_expr(f, &method.body)
}

// --- Blocks ---

pub fn walk_block_item<F: AstFolder + ?Sized>(
    f: &mut F,
    item: &BlockItem,
) -> Result<F::Output, F::Error> {
    match item {
        BlockItem::Stmt(stmt) => f.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => walk_expr(f, expr),
    }
}

pub fn walk_block_tail<F: AstFolder + ?Sized>(
    f: &mut F,
    tail: Option<&Expr>,
    expected: Option<&F::Input>,
) -> Result<Option<F::Output>, F::Error> {
    match tail {
        Some(tail_expr) => Some(f.visit_expr(tail_expr, expected)).transpose(),
        None => Ok(None),
    }
}

/// Visit a block's items and optional tail expression.
pub fn walk_block<F: AstFolder + ?Sized>(
    f: &mut F,
    items: &[BlockItem],
    tail: Option<&Expr>,
) -> Result<(Vec<F::Output>, Option<F::Output>), F::Error> {
    let item_outputs = items
        .iter()
        .map(|item| f.visit_block_item(item))
        .collect::<Result<Vec<_>, _>>()?;

    let tail_output = tail.map(|expr| walk_expr(f, expr)).transpose()?;

    Ok((item_outputs, tail_output))
}

// --- Expressions ---

/// Visit a list of expressions with no input.
pub fn walk_exprs<F: AstFolder + ?Sized>(
    f: &mut F,
    exprs: &[Expr],
) -> Result<Vec<F::Output>, F::Error> {
    exprs.iter().map(|expr| walk_expr(f, expr)).collect()
}

/// Visit a single expression with no input.
pub fn walk_expr<F: AstFolder + ?Sized>(f: &mut F, expr: &Expr) -> Result<F::Output, F::Error> {
    f.visit_expr(expr, None)
}

/// Visit a binary expression's children and return their outputs.
pub fn walk_binary_expr<F: AstFolder + ?Sized>(
    f: &mut F,
    left: &Expr,
    right: &Expr,
) -> Result<(F::Output, F::Output), F::Error> {
    Ok((walk_expr(f, left)?, walk_expr(f, right)?))
}

pub fn walk_call<F: AstFolder + ?Sized>(
    f: &mut F,
    callee: &Expr,
    args: &[CallArg],
) -> Result<Vec<F::Output>, F::Error> {
    let mut outputs = Vec::with_capacity(args.len() + 1);
    outputs.push(walk_expr(f, callee)?);
    outputs.extend(walk_call_args(f, args)?);
    Ok(outputs)
}

pub fn walk_array_lit_init<F: AstFolder + ?Sized>(
    f: &mut F,
    init: &ArrayLitInit,
    expected: Option<&F::Input>,
) -> Result<Vec<F::Output>, F::Error> {
    match init {
        ArrayLitInit::Elems(elems) => elems
            .iter()
            .map(|elem| f.visit_expr(elem, expected))
            .collect(),
        ArrayLitInit::Repeat(expr, _) => Ok(vec![f.visit_expr(expr, expected)?]),
    }
}

// --- Control Flow ---

/// Visit an if expression's condition and branches.
pub fn walk_if<F: AstFolder + ?Sized>(
    f: &mut F,
    cond: &Expr,
    then_body: &Expr,
    else_body: &Expr,
) -> Result<(F::Output, F::Output, F::Output), F::Error> {
    Ok((
        walk_expr(f, cond)?,
        walk_expr(f, then_body)?,
        walk_expr(f, else_body)?,
    ))
}

/// Visit each match arm using a caller-provided hook.
pub fn walk_match_arms<F: AstFolder + ?Sized, T>(
    f: &mut F,
    arms: &[MatchArm],
    mut visit_arm: impl FnMut(&mut F, &MatchArm) -> Result<T, F::Error>,
) -> Result<Vec<T>, F::Error> {
    let mut outputs = Vec::with_capacity(arms.len());
    for arm in arms {
        outputs.push(visit_arm(f, arm)?);
    }
    Ok(outputs)
}

pub fn walk_match_arm<F: AstFolder + ?Sized>(
    f: &mut F,
    arm: &MatchArm,
) -> Result<F::Output, F::Error> {
    f.visit_expr(&arm.body, None)
}

// --- Calls ---

/// Visit a call's arguments (callee handling is left to the caller).
pub fn walk_call_args<F: AstFolder + ?Sized>(
    f: &mut F,
    args: &[CallArg],
) -> Result<Vec<F::Output>, F::Error> {
    args.iter().map(|arg| walk_expr(f, &arg.expr)).collect()
}

// --- Closures ---

/// Visit a closure body (params/return types are handled by the caller).
pub fn walk_closure<F: AstFolder + ?Sized>(f: &mut F, body: &Expr) -> Result<F::Output, F::Error> {
    walk_expr(f, body)
}
