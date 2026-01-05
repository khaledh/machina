//! Lvalue overlap checking for function call arguments.
//!
//! Prevents aliasing violations like `swap(arr[i], arr[i])` or `f(x, x.field)`
//! when at least one argument is mutated (inout/out/sink mode).
//! Read-only aliasing (`in` mode) is allowed.

use crate::ast::{CallArg, Expr, ExprKind, Function, FunctionParamMode, Visitor, walk_expr};
use crate::context::TypeCheckedContext;
use crate::diag::Span;
use crate::resolve::def_map::DefId;
use crate::semck::SemCheckError;
use crate::semck::util::lookup_call_sig;

/// An argument access: its mode, lvalue path, and source location.
struct ArgAccess {
    mode: FunctionParamMode,
    path: LvaluePath,
    span: Span,
}

/// An lvalue path: base variable + sequence of projections.
/// E.g., `a.x[0].y` = base `a`, projections [Field("x"), Index(0), Field("y")].
#[derive(Clone)]
struct LvaluePath {
    base: DefId,
    projections: Vec<Projection>,
}

/// A single step in an lvalue path.
#[derive(Clone)]
enum Projection {
    Field(String),
    TupleField(usize),
    Index(Index),
    Slice(SliceRange),
}

/// Array index: known constant or unknown (dynamic).
#[derive(Clone)]
enum Index {
    Const(u64),
    Unknown,
}

/// Slice range bounds (for `arr[start..end]`).
#[derive(Clone)]
struct SliceRange {
    start: Bound,
    end: Bound,
}

/// A slice bound: known constant, unknown expression, or unspecified (omitted).
#[derive(Clone)]
enum Bound {
    Const(u64),
    Unknown,     // `arr[x..]` where x is not a literal
    Unspecified, // `arr[..n]` (start omitted) or `arr[n..]` (end omitted)
}

pub(super) fn check(ctx: &TypeCheckedContext) -> Vec<SemCheckError> {
    let mut checker = LvalueOverlapChecker::new(ctx);
    checker.visit_module(&ctx.module);
    checker.errors
}

struct LvalueOverlapChecker<'a> {
    ctx: &'a TypeCheckedContext,
    errors: Vec<SemCheckError>,
}

impl<'a> LvalueOverlapChecker<'a> {
    fn new(ctx: &'a TypeCheckedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
        }
    }

    /// Check a function call for overlapping lvalue arguments.
    fn check_call(&mut self, call: &Expr, args: &[CallArg], receiver: Option<&Expr>) {
        let Some(sig) = lookup_call_sig(call, self.ctx) else {
            return;
        };

        // Extract lvalue paths from arguments (non-lvalues like literals are ignored).
        let mut accesses = Vec::new();
        if let (Some(self_mode), Some(receiver)) = (sig.self_mode(), receiver) {
            if let Some(path) = self.lvalue_path(receiver) {
                accesses.push(ArgAccess {
                    mode: self_mode,
                    path,
                    span: receiver.span,
                });
            }
        }
        for (param, arg) in sig.params().iter().zip(args) {
            if let Some(path) = self.lvalue_path(&arg.expr) {
                accesses.push(ArgAccess {
                    mode: param.mode.clone(),
                    path,
                    span: arg.span,
                });
            }
        }

        // pairwise check: reject overlapping paths if either is write-mode.
        for i in 0..accesses.len() {
            for j in (i + 1)..accesses.len() {
                let left = &accesses[i];
                let right = &accesses[j];
                // Two read-only args can alias safely.
                if !Self::is_write(&left.mode) && !Self::is_write(&right.mode) {
                    continue;
                }
                if Self::paths_maybe_overlap(&left.path, &right.path) {
                    self.errors
                        .push(SemCheckError::OverlappingLvalueArgs(right.span));
                }
            }
        }
    }

    /// Returns true if the param mode can mutate the argument.
    fn is_write(mode: &FunctionParamMode) -> bool {
        matches!(
            mode,
            FunctionParamMode::Inout | FunctionParamMode::Out | FunctionParamMode::Sink
        )
    }

    /// Extract the lvalue path from an expression (base variable + projections).
    /// Returns None for non-lvalue expressions (e.g., literals, calls).
    fn lvalue_path(&self, expr: &Expr) -> Option<LvaluePath> {
        match &expr.kind {
            ExprKind::Var(_) => {
                let def_id = self.ctx.def_map.lookup_def(expr.id)?.id;
                Some(LvaluePath {
                    base: def_id,
                    projections: Vec::new(),
                })
            }
            ExprKind::StructField { target, field } => {
                let mut path = self.lvalue_path(target)?;
                path.projections.push(Projection::Field(field.clone()));
                Some(path)
            }
            ExprKind::TupleField { target, index } => {
                let mut path = self.lvalue_path(target)?;
                path.projections.push(Projection::TupleField(*index));
                Some(path)
            }
            ExprKind::ArrayIndex { target, indices } => {
                let mut path = self.lvalue_path(target)?;
                for index in indices {
                    path.projections
                        .push(Projection::Index(Self::index_of(index)));
                }
                Some(path)
            }
            ExprKind::Slice { target, start, end } => {
                let mut path = self.lvalue_path(target)?;
                let start = Self::bound_of(start.as_deref());
                let end = Self::bound_of(end.as_deref());
                path.projections
                    .push(Projection::Slice(SliceRange { start, end }));
                Some(path)
            }
            // Treat move as an lvalue use for overlap detection.
            ExprKind::Move { expr } => self.lvalue_path(expr),
            _ => None,
        }
    }

    /// Convert index expression to Index: constant if literal, Unknown otherwise.
    fn index_of(expr: &Expr) -> Index {
        if let ExprKind::IntLit(value) = expr.kind {
            Index::Const(value)
        } else {
            Index::Unknown
        }
    }

    /// Convert optional bound expression to Bound.
    fn bound_of(expr: Option<&Expr>) -> Bound {
        match expr {
            Some(Expr {
                kind: ExprKind::IntLit(value),
                ..
            }) => Bound::Const(*value),
            Some(_) => Bound::Unknown,
            None => Bound::Unspecified,
        }
    }

    /// Check if two lvalue paths might alias. Conservative: returns true if uncertain.
    ///
    /// Examples:
    /// - `a` vs `b` -> false (different bases)
    /// - `a.x` vs `a.y` -> false (different fields)
    /// - `a[0]` vs `a[1]` -> false (different constant indices)
    /// - `a[i]` vs `a[j]` -> true (unknown indices, might be same)
    /// - `a` vs `a.x` -> true (prefix = overlap)
    fn paths_maybe_overlap(a: &LvaluePath, b: &LvaluePath) -> bool {
        // Different base variables never overlap.
        if a.base != b.base {
            return false;
        }

        // Walk projections in parallel until they diverge.
        let min_len = a.projections.len().min(b.projections.len());
        for idx in 0..min_len {
            let left = &a.projections[idx];
            let right = &b.projections[idx];
            match (left, right) {
                (Projection::Field(left), Projection::Field(right)) => {
                    if left != right {
                        return false; // Different fields are disjoint.
                    }
                }
                (Projection::TupleField(left), Projection::TupleField(right)) => {
                    if left != right {
                        return false; // Different tuple elements are disjoint.
                    }
                }
                (Projection::Index(left), Projection::Index(right)) => {
                    if let (Index::Const(a), Index::Const(b)) = (left, right) {
                        if a != b {
                            return false; // Different constant indices are disjoint.
                        }
                    }
                    // Unknown (or equal) indices may still be disjoint if later projections
                    // diverge on fields, so keep scanning.
                }
                (Projection::Slice(left), Projection::Slice(right)) => {
                    if Self::slices_disjoint(left, right) {
                        return false;
                    }
                    // Slices might overlap, and we can't track sub-ranges precisely.
                    return true;
                }
                (Projection::Index(index), Projection::Slice(slice))
                | (Projection::Slice(slice), Projection::Index(index)) => {
                    if Self::index_slice_disjoint(index, slice) {
                        return false;
                    }
                    return true;
                }
                _ => {
                    // Mismatched projection kinds -> conservatively assume overlap.
                    return true;
                }
            }
        }

        // One path is a prefix of the other (e.g., `a` vs `a.x`) -> overlap.
        true
    }

    /// Check if two slice ranges are provably disjoint (no common indices).
    /// E.g., `[0..2]` and `[3..5]` are disjoint; `[0..3]` and `[2..5]` are not.
    fn slices_disjoint(left: &SliceRange, right: &SliceRange) -> bool {
        let Some(left_start) = Self::bound_start(&left.start) else {
            return false;
        };
        let Some(right_start) = Self::bound_start(&right.start) else {
            return false;
        };

        // Disjoint if one ends before the other starts.
        match (Self::bound_end(&left.end), Self::bound_end(&right.end)) {
            (Some(left_end), Some(right_end)) => left_end <= right_start || right_end <= left_start,
            (Some(left_end), None) => left_end <= right_start,
            (None, Some(right_end)) => right_end <= left_start,
            (None, None) => false, // Both open-ended, must assume overlap.
        }
    }

    /// Check if a constant index is outside the slice range.
    fn index_slice_disjoint(index: &Index, slice: &SliceRange) -> bool {
        let Index::Const(value) = index else {
            return false; // Unknown index might be in the slice.
        };
        let Some(start) = Self::bound_start(&slice.start) else {
            return false;
        };
        // Index before slice start -> disjoint.
        if *value < start {
            return true;
        }
        // Index >= slice end -> disjoint.
        if let Some(end) = Self::bound_end(&slice.end) {
            return *value >= end;
        }
        false
    }

    /// Get the start bound as a concrete value (Unspecified = 0).
    fn bound_start(bound: &Bound) -> Option<u64> {
        match bound {
            Bound::Const(value) => Some(*value),
            Bound::Unspecified => Some(0), // `[..n]` starts at 0.
            Bound::Unknown => None,
        }
    }

    /// Get the end bound as a concrete value (open-ended = None).
    fn bound_end(bound: &Bound) -> Option<u64> {
        match bound {
            Bound::Const(value) => Some(*value),
            Bound::Unknown | Bound::Unspecified => None, // `[n..]` has no known end.
        }
    }
}

impl Visitor for LvalueOverlapChecker<'_> {
    fn visit_func(&mut self, func: &Function) {
        self.visit_expr(&func.body);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Call { args, .. } => {
                self.check_call(expr, args, None);
            }
            ExprKind::MethodCall { target, args, .. } => {
                self.check_call(expr, args, Some(target));
            }
            _ => {}
        }
        walk_expr(self, expr);
    }
}
