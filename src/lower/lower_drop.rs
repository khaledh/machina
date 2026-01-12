use std::collections::HashSet;

use crate::hir::model::{Expr, ExprKind};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::FuncLowerer;
use crate::mcir::types::*;
use crate::resolve::DefId;
use crate::types::Type;

#[derive(Debug, Clone)]
pub(super) struct DropInfo {
    def_id: DefId,
    ty: Type,
    is_initialized: Option<LocalId>,
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

    fn is_initialized_flag(&self, def_id: DefId) -> Option<LocalId> {
        self.locals
            .iter()
            .find(|info| info.def_id == def_id)
            .and_then(|info| info.is_initialized)
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
            if info.is_initialized.is_none() && scope.moved.contains(&info.def_id) {
                continue;
            }
            if info.ty.needs_drop() {
                if let Some(flag) = info.is_initialized {
                    self.emit_drop_value_if_initialized(info.def_id, &info.ty, flag)?;
                } else {
                    self.emit_drop_value(info.def_id, &info.ty)?;
                }
            }
        }
        Ok(())
    }

    pub(super) fn register_drop(
        &mut self,
        def_id: DefId,
        ty: &Type,
        is_initialized: Option<LocalId>,
    ) {
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
            is_initialized,
        });
    }

    pub(super) fn record_move(&mut self, expr: &Expr) {
        let ExprKind::Var { def_id, .. } = expr.kind else {
            return;
        };
        let Ok(ty) = self.ty_for_node(expr.id) else {
            return;
        };
        if !ty.needs_drop() {
            return;
        }
        // Mark the binding as moved so we skip drop at scope exit.
        if let Some(flag) = self.is_initialized_for_def(def_id) {
            // For conditional drops, moving clears the init flag so we don't drop.
            self.set_is_initialized(flag, false);
            return;
        }
        self.mark_moved(def_id);
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
        if self.is_initialized_for_def(def_id).is_some() {
            return;
        }
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

    fn emit_drop_value_if_initialized(
        &mut self,
        def_id: DefId,
        ty: &Type,
        is_initialized: LocalId,
    ) -> Result<(), LowerError> {
        let Some(&local_id) = self.locals.get(&def_id) else {
            return Ok(());
        };

        let ty_id = self.ty_lowerer.lower_ty(ty);
        let place = if ty.is_scalar() {
            PlaceAny::Scalar(Place::new(local_id, ty_id, vec![]))
        } else {
            PlaceAny::Aggregate(Place::new(local_id, ty_id, vec![]))
        };

        self.emit_drop_place_if_initialized(place, ty, is_initialized);

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

    pub(super) fn emit_drop_place_if_initialized(
        &mut self,
        place: PlaceAny,
        ty: &Type,
        is_initialized: LocalId,
    ) {
        if !ty.needs_drop() {
            return;
        }

        // Branch on the init flag to avoid dropping uninitialized values.
        let flag_ty = self.fb.body.locals[is_initialized.0 as usize].ty;
        let flag_place = Place::new(is_initialized, flag_ty, vec![]);
        let cond = Operand::Copy(flag_place);

        let drop_bb = self.fb.new_block();
        let cont_bb = self.fb.new_block();
        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond,
                then_bb: drop_bb,
                else_bb: cont_bb,
            },
        );

        self.curr_block = drop_bb;
        self.emit_drop_place(place, ty);
        self.fb.set_terminator(drop_bb, Terminator::Goto(cont_bb));

        // Continue lowering in the fallthrough block.
        self.curr_block = cont_bb;
    }

    pub(super) fn is_initialized_for_def(&self, def_id: DefId) -> Option<LocalId> {
        for scope in self.drop_scopes.iter().rev() {
            if let Some(flag) = scope.is_initialized_flag(def_id) {
                return Some(flag);
            }
        }
        None
    }

    pub(super) fn set_is_initialized(&mut self, flag: LocalId, value: bool) {
        let flag_ty = self.fb.body.locals[flag.0 as usize].ty;
        let flag_place = Place::new(flag, flag_ty, vec![]);
        self.emit_copy_scalar(flag_place, Rvalue::Use(Operand::Const(Const::Bool(value))));
    }
}
