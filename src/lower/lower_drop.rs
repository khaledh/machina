use std::collections::HashSet;

use crate::ast::{Expr, ExprKind};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::mcir::types::{Callee, Place, PlaceAny, Statement};
use crate::resolve::def_map::DefId;
use crate::types::Type;

#[derive(Debug, Clone)]
pub(super) struct DropInfo {
    def_id: DefId,
    ty: Type,
}

#[derive(Debug, Default, Clone)]
pub(super) struct DropScope {
    locals: Vec<DropInfo>,
    moved: HashSet<DefId>,
}

impl DropScope {
    fn contains(&self, def_id: DefId) -> bool {
        self.locals.iter().any(|info| info.def_id == def_id)
    }
}

impl<'a> FuncLowerer<'a> {
    pub(super) fn enter_drop_scope(&mut self) {
        // Track owned locals created within this lexical scope.
        self.drop_scopes.push(DropScope::default());
    }

    pub(super) fn exit_drop_scope(&mut self) -> Result<(), LowerError> {
        let Some(scope) = self.drop_scopes.pop() else {
            return Ok(());
        };

        // Drop remaining owned heap locals in reverse definition order.
        for info in scope.locals.iter().rev() {
            if scope.moved.contains(&info.def_id) {
                continue;
            }
            if info.ty.needs_drop() {
                self.emit_drop_value(info.def_id, &info.ty)?;
            }
        }
        Ok(())
    }

    pub(super) fn register_drop(&mut self, def_id: DefId, ty: &Type) {
        if !ty.needs_drop() {
            return;
        }

        // Only track the first binding for each def in this scope.
        let Some(scope) = self.drop_scopes.last_mut() else {
            return;
        };
        if scope.contains(def_id) {
            return;
        }
        scope.locals.push(DropInfo {
            def_id,
            ty: ty.clone(),
        });
    }

    pub(super) fn record_move(&mut self, expr: &Expr) {
        let ExprKind::Var(_) = expr.kind else {
            return;
        };
        let Ok(ty) = self.ty_for_node(expr.id) else {
            return;
        };
        if !ty.needs_drop() {
            return;
        }
        // Mark the binding as moved so we skip drop at scope exit.
        let Ok(def) = self.def_for_node(expr.id) else {
            return;
        };
        self.mark_moved(def.id);
    }

    fn mark_moved(&mut self, def_id: DefId) {
        for scope in self.drop_scopes.iter_mut().rev() {
            if scope.contains(def_id) {
                scope.moved.insert(def_id);
                break;
            }
        }
    }

    pub(super) fn clear_moved(&mut self, def_id: DefId) {
        // Reassigning clears moved status for the nearest scope binding.
        for scope in self.drop_scopes.iter_mut().rev() {
            if scope.contains(def_id) {
                scope.moved.remove(&def_id);
                break;
            }
        }
    }

    fn emit_drop_value(&mut self, def_id: DefId, ty: &Type) -> Result<(), LowerError> {
        let Some(&local_id) = self.locals.get(&def_id) else {
            return Ok(());
        };

        let ty_id = self.ty_lowerer.lower_ty(ty);
        let place = if ty.is_scalar() {
            PlaceAny::Scalar(Place::new(local_id, ty_id, vec![]))
        } else {
            PlaceAny::Aggregate(Place::new(local_id, ty_id, vec![]))
        };

        self.emit_drop_place(place, ty);

        Ok(())
    }

    pub(super) fn emit_drop_place(&mut self, place: PlaceAny, ty: &Type) {
        if !ty.needs_drop() {
            return;
        }

        let callee_def_id = self.drop_glue.get_or_create(ty);
        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Def(callee_def_id),
                args: vec![place],
            },
        );
    }
}
