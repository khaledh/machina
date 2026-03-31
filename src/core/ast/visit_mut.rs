use crate::core::ast::*;

/// Tree mutable visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
pub trait VisitorMut {
    // --- Module ---

    fn visit_module(&mut self, module: &mut Module) {
        walk_module(self, module)
    }

    // --- Type Definitions ---

    fn visit_type_def(&mut self, type_def: &mut TypeDef) {
        walk_type_def(self, type_def)
    }

    fn visit_trait_def(&mut self, trait_def: &mut TraitDef) {
        walk_trait_def(self, trait_def)
    }

    fn visit_trait_method(&mut self, method: &mut TraitMethod) {
        walk_trait_method(self, method)
    }

    fn visit_trait_property(&mut self, property: &mut TraitProperty) {
        walk_trait_property(self, property)
    }

    fn visit_machine_def(&mut self, machine_def: &mut MachineDef) {
        walk_machine_def(self, machine_def)
    }

    fn visit_machine_item(&mut self, item: &mut MachineItem) {
        walk_machine_item(self, item)
    }

    fn visit_machine_fields(&mut self, fields: &mut MachineFields) {
        walk_machine_fields(self, fields)
    }

    fn visit_machine_transition_handler(&mut self, handler: &mut MachineTransitionHandler) {
        walk_machine_transition_handler(self, handler)
    }

    fn visit_machine_on_handler(&mut self, handler: &mut MachineOnHandler) {
        walk_machine_on_handler(self, handler)
    }

    fn visit_struct_def_fields(&mut self, fields: &mut [StructDefField]) {
        walk_struct_def_fields(self, fields)
    }

    fn visit_struct_def_field(&mut self, field: &mut StructDefField) {
        walk_struct_def_field(self, field)
    }

    fn visit_enum_def_variants(&mut self, variants: &mut [EnumDefVariant]) {
        walk_enum_def_variants(self, variants)
    }

    fn visit_enum_def_variant(&mut self, variant: &mut EnumDefVariant) {
        walk_enum_def_variant(self, variant)
    }

    fn visit_linear_type_def(&mut self, linear: &mut LinearTypeDef) {
        walk_linear_type_def(self, linear)
    }

    fn visit_linear_state_variant(&mut self, state: &mut LinearStateVariant) {
        walk_linear_state_variant(self, state)
    }

    fn visit_linear_transition_decl(&mut self, decl: &mut LinearTransitionDecl) {
        walk_linear_transition_decl(self, decl)
    }

    fn visit_linear_transition_param(&mut self, param: &mut LinearTransitionParam) {
        walk_linear_transition_param(self, param)
    }

    // --- Type Expressions ---

    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        walk_type_expr(self, type_expr)
    }

    // --- Function Declarations ---

    fn visit_func_decl(&mut self, func_decl: &mut FuncDecl) {
        walk_func_decl(self, func_decl)
    }

    fn visit_static_def(&mut self, static_def: &mut StaticDef) {
        walk_static_def(self, static_def)
    }

    // --- Functions ---

    fn visit_func_def(&mut self, func_def: &mut FuncDef) {
        walk_func_def(self, func_def)
    }

    // --- Function Signatures ---

    fn visit_func_sig(&mut self, func_sig: &mut FunctionSig) {
        walk_func_sig(self, func_sig)
    }

    // --- Type Parameters ---

    fn visit_type_param(&mut self, param: &mut TypeParam) {
        walk_type_param(self, param)
    }

    // --- Method Signatures ---

    fn visit_method_sig(&mut self, method_sig: &mut MethodSig) {
        walk_method_sig(self, method_sig)
    }

    fn visit_self_param(&mut self, self_param: &mut SelfParam) {
        walk_self_param(self, self_param)
    }

    // --- Parameters (common) ---

    fn visit_param(&mut self, param: &mut Param) {
        walk_param(self, param)
    }

    // --- Method Blocks ---

    fn visit_method_block(&mut self, method_block: &mut MethodBlock) {
        walk_method_block(self, method_block)
    }

    fn visit_method_item(&mut self, method_item: &mut MethodItem) {
        walk_method_item(self, method_item)
    }

    fn visit_method_decl(&mut self, method_decl: &mut MethodDecl) {
        walk_method_decl(self, method_decl)
    }

    fn visit_method_def(&mut self, method_def: &mut MethodDef) {
        walk_method_def(self, method_def)
    }

    // --- Closure Definitions ---

    fn visit_closure_def(&mut self, closure_def: &mut ClosureDef) {
        walk_closure_def(self, closure_def)
    }

    // --- Blocks ---

    fn visit_block_item(&mut self, item: &mut BlockItem) {
        walk_block_item(self, item)
    }

    // --- Bind Patterns ---

    fn visit_bind_pattern(&mut self, pattern: &mut BindPattern) {
        walk_bind_pattern(self, pattern)
    }

    fn visit_using_binding(&mut self, binding: &mut UsingBinding) {
        walk_using_binding(self, binding)
    }

    // --- Match Patterns ---

    fn visit_match_pattern(&mut self, pattern: &mut MatchPattern) {
        walk_match_pattern(self, pattern)
    }

    fn visit_match_arm(&mut self, arm: &mut MatchArm) {
        walk_match_arm(self, arm)
    }

    fn visit_match_pattern_binding(&mut self, binding: &mut MatchPatternBinding) {
        walk_match_pattern_binding(self, binding)
    }

    // --- Expressions ---

    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr)
    }
}

// --- Module ---

pub fn walk_module<V: VisitorMut + ?Sized>(v: &mut V, module: &mut Module) {
    for item in &mut module.top_level_items {
        match item {
            TopLevelItem::TraitDef(trait_def) => v.visit_trait_def(trait_def),
            TopLevelItem::TypeDef(type_def) => v.visit_type_def(type_def),
            TopLevelItem::MachineDef(machine_def) => v.visit_machine_def(machine_def),
            TopLevelItem::StaticDef(static_def) => v.visit_static_def(static_def),
            TopLevelItem::FuncDecl(func_decl) => v.visit_func_decl(func_decl),
            TopLevelItem::FuncDef(func_def) => v.visit_func_def(func_def),
            TopLevelItem::MethodBlock(method_block) => v.visit_method_block(method_block),
            TopLevelItem::ClosureDef(closure_def) => v.visit_closure_def(closure_def),
        }
    }
}

// --- Type Definitions ---

pub fn walk_machine_def<V: VisitorMut + ?Sized>(v: &mut V, machine_def: &mut MachineDef) {
    for item in &mut machine_def.items {
        v.visit_machine_item(item);
    }
}

pub fn walk_machine_item<V: VisitorMut + ?Sized>(v: &mut V, item: &mut MachineItem) {
    match item {
        MachineItem::Fields(fields) => v.visit_machine_fields(fields),
        MachineItem::Constructor(constructor) => v.visit_func_def(constructor),
        MachineItem::Action(handler) | MachineItem::Trigger(handler) => {
            v.visit_machine_transition_handler(handler)
        }
        MachineItem::On(handler) => v.visit_machine_on_handler(handler),
    }
}

pub fn walk_machine_fields<V: VisitorMut + ?Sized>(v: &mut V, fields: &mut MachineFields) {
    v.visit_struct_def_fields(&mut fields.fields);
}

pub fn walk_machine_transition_handler<V: VisitorMut + ?Sized>(
    v: &mut V,
    handler: &mut MachineTransitionHandler,
) {
    for param in &mut handler.params {
        v.visit_param(param);
    }
    if let Some(ret_ty_expr) = &mut handler.ret_ty_expr {
        v.visit_type_expr(ret_ty_expr);
    }
    v.visit_expr(&mut handler.body);
}

pub fn walk_machine_on_handler<V: VisitorMut + ?Sized>(v: &mut V, handler: &mut MachineOnHandler) {
    v.visit_type_expr(&mut handler.selector_ty);
    for param in &mut handler.params {
        v.visit_param(param);
    }
    if let Some(provenance) = &mut handler.provenance {
        v.visit_param(&mut provenance.param);
    }
    v.visit_expr(&mut handler.body);
}

pub fn walk_trait_def<V: VisitorMut + ?Sized>(v: &mut V, trait_def: &mut TraitDef) {
    for method in &mut trait_def.methods {
        v.visit_trait_method(method);
    }
    for property in &mut trait_def.properties {
        v.visit_trait_property(property);
    }
}

pub fn walk_trait_method<V: VisitorMut + ?Sized>(v: &mut V, method: &mut TraitMethod) {
    v.visit_method_sig(&mut method.sig);
}

pub fn walk_trait_property<V: VisitorMut + ?Sized>(v: &mut V, property: &mut TraitProperty) {
    v.visit_type_expr(&mut property.ty);
}

pub fn walk_static_def<V: VisitorMut + ?Sized>(v: &mut V, static_def: &mut StaticDef) {
    if let Some(ty) = &mut static_def.ty {
        v.visit_type_expr(ty);
    }
    v.visit_expr(&mut static_def.init);
}

pub fn walk_type_def<V: VisitorMut + ?Sized>(v: &mut V, type_def: &mut TypeDef) {
    for param in &mut type_def.type_params {
        v.visit_type_param(param);
    }
    match &mut type_def.kind {
        TypeDefKind::Alias { aliased_ty } => v.visit_type_expr(aliased_ty),
        TypeDefKind::Struct { fields } => v.visit_struct_def_fields(fields),
        TypeDefKind::Enum { variants } => v.visit_enum_def_variants(variants),
        TypeDefKind::Linear { linear } => v.visit_linear_type_def(linear),
    }
}

pub fn walk_linear_type_def<V: VisitorMut + ?Sized>(v: &mut V, linear: &mut LinearTypeDef) {
    v.visit_struct_def_fields(&mut linear.fields);
    for state in &mut linear.states {
        v.visit_linear_state_variant(state);
    }
    for action in &mut linear.actions {
        v.visit_linear_transition_decl(action);
    }
    for trigger in &mut linear.triggers {
        v.visit_linear_transition_decl(trigger);
    }
}

pub fn walk_linear_state_variant<V: VisitorMut + ?Sized>(
    v: &mut V,
    state: &mut LinearStateVariant,
) {
    for payload in &mut state.payload {
        v.visit_type_expr(payload);
    }
}

pub fn walk_linear_transition_decl<V: VisitorMut + ?Sized>(
    v: &mut V,
    decl: &mut LinearTransitionDecl,
) {
    for param in &mut decl.params {
        v.visit_linear_transition_param(param);
    }
    if let Some(error_ty_expr) = &mut decl.error_ty_expr {
        v.visit_type_expr(error_ty_expr);
    }
}

pub fn walk_linear_transition_param<V: VisitorMut + ?Sized>(
    v: &mut V,
    param: &mut LinearTransitionParam,
) {
    v.visit_type_expr(&mut param.ty);
}

pub fn walk_struct_def_fields<V: VisitorMut + ?Sized>(v: &mut V, fields: &mut [StructDefField]) {
    for field in fields {
        v.visit_struct_def_field(field);
    }
}

pub fn walk_struct_def_field<V: VisitorMut + ?Sized>(v: &mut V, field: &mut StructDefField) {
    v.visit_type_expr(&mut field.ty);
}

pub fn walk_enum_def_variants<V: VisitorMut + ?Sized>(v: &mut V, variants: &mut [EnumDefVariant]) {
    for variant in variants {
        v.visit_enum_def_variant(variant);
    }
}

pub fn walk_enum_def_variant<V: VisitorMut + ?Sized>(v: &mut V, variant: &mut EnumDefVariant) {
    for payload in &mut variant.payload {
        v.visit_type_expr(payload);
    }
}

// --- Type Expressions ---

pub fn walk_type_expr<V: VisitorMut + ?Sized>(v: &mut V, type_expr: &mut TypeExpr) {
    match &mut type_expr.kind {
        TypeExprKind::Infer => {}
        TypeExprKind::Union { variants } => {
            for variant in variants {
                v.visit_type_expr(variant);
            }
        }
        TypeExprKind::Named { type_args, .. } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
        }
        TypeExprKind::Refined { base_ty_expr, .. } => v.visit_type_expr(base_ty_expr),
        TypeExprKind::Array { elem_ty_expr, .. } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::DynArray { elem_ty_expr } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Tuple { field_ty_exprs } => {
            for field in field_ty_exprs {
                v.visit_type_expr(field);
            }
        }
        TypeExprKind::Slice { elem_ty_expr } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Heap { elem_ty_expr } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::RawPtr { elem_ty_expr } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Ref { elem_ty_expr, .. } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            for param in params {
                v.visit_type_expr(&mut param.ty_expr);
            }
            v.visit_type_expr(ret_ty_expr);
        }
    }
}

// --- Function Declarations ---

pub fn walk_func_decl<V: VisitorMut + ?Sized>(v: &mut V, func_decl: &mut FuncDecl) {
    v.visit_func_sig(&mut func_decl.sig);
}

// --- Functions ---

pub fn walk_func_def<V: VisitorMut + ?Sized>(v: &mut V, func_def: &mut FuncDef) {
    v.visit_func_sig(&mut func_def.sig);
    v.visit_expr(&mut func_def.body);
}

// --- Function Signatures ---

pub fn walk_func_sig<V: VisitorMut + ?Sized>(v: &mut V, func_sig: &mut FunctionSig) {
    for param in &mut func_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &mut func_sig.params {
        v.visit_param(param);
    }
    v.visit_type_expr(&mut func_sig.ret_ty_expr);
}

pub fn walk_type_param<V: VisitorMut + ?Sized>(_v: &mut V, _param: &mut TypeParam) {}

// --- Method Signatures ---

pub fn walk_method_sig<V: VisitorMut + ?Sized>(v: &mut V, method_sig: &mut MethodSig) {
    v.visit_self_param(&mut method_sig.self_param);
    for param in &mut method_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &mut method_sig.params {
        v.visit_param(param);
    }
    v.visit_type_expr(&mut method_sig.ret_ty_expr);
}

pub fn walk_self_param<V: VisitorMut + ?Sized>(_v: &mut V, _self_param: &mut SelfParam) {}

// --- Parameters (common) ---

pub fn walk_param<V: VisitorMut + ?Sized>(v: &mut V, param: &mut Param) {
    v.visit_type_expr(&mut param.typ);
    if let Some(default) = &mut param.default {
        v.visit_expr(default);
    }
}

// --- Method Blocks ---

pub fn walk_method_block<V: VisitorMut + ?Sized>(v: &mut V, method_block: &mut MethodBlock) {
    for type_arg in &mut method_block.type_args {
        v.visit_type_expr(type_arg);
    }
    for method_item in &mut method_block.method_items {
        v.visit_method_item(method_item);
    }
}

pub fn walk_method_item<V: VisitorMut + ?Sized>(v: &mut V, method_item: &mut MethodItem) {
    match method_item {
        MethodItem::Decl(method_decl) => v.visit_method_decl(method_decl),
        MethodItem::Def(method_def) => v.visit_method_def(method_def),
    }
}

pub fn walk_method_decl<V: VisitorMut + ?Sized>(v: &mut V, method_decl: &mut MethodDecl) {
    v.visit_method_sig(&mut method_decl.sig);
}

pub fn walk_method_def<V: VisitorMut + ?Sized>(v: &mut V, method_def: &mut MethodDef) {
    v.visit_method_sig(&mut method_def.sig);
    v.visit_expr(&mut method_def.body);
}

// --- Closure Definitions ---

pub fn walk_closure_def<V: VisitorMut + ?Sized>(_v: &mut V, _closure_def: &mut ClosureDef) {
    // Closures are also visited at their expression sites; avoid walking the lifted body twice.
}

// --- Blocks ---

pub fn walk_block_item<V: VisitorMut + ?Sized>(v: &mut V, item: &mut BlockItem) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

// --- Bind Patterns ---

pub fn walk_bind_pattern<V: VisitorMut + ?Sized>(v: &mut V, pattern: &mut BindPattern) {
    pattern
        .kind
        .for_each_child_pattern_mut(|pattern| v.visit_bind_pattern(pattern));
}

pub fn walk_using_binding<V: VisitorMut + ?Sized>(_v: &mut V, _binding: &mut UsingBinding) {}

// --- Match Patterns ---

pub fn walk_match_pattern<V: VisitorMut + ?Sized>(v: &mut V, pattern: &mut MatchPattern) {
    match pattern {
        MatchPattern::TypedBinding { ty_expr, .. } => {
            v.visit_type_expr(ty_expr);
        }
        MatchPattern::Tuple { patterns, .. } => {
            for pattern in patterns {
                v.visit_match_pattern(pattern);
            }
        }
        MatchPattern::EnumVariant {
            type_args,
            bindings,
            ..
        } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
            for binding in bindings {
                v.visit_match_pattern_binding(binding);
            }
        }
        _ => {}
    }
}

pub fn walk_match_pattern_bindings<V: VisitorMut + ?Sized>(v: &mut V, pattern: &mut MatchPattern) {
    if let MatchPattern::EnumVariant { bindings, .. } = pattern {
        for binding in bindings {
            v.visit_match_pattern_binding(binding);
        }
    }
}

pub fn walk_match_pattern_binding<V: VisitorMut + ?Sized>(
    _v: &mut V,
    _binding: &mut MatchPatternBinding,
) {
}

pub fn walk_match_arm<V: VisitorMut + ?Sized>(v: &mut V, arm: &mut MatchArm) {
    for pattern in &mut arm.patterns {
        v.visit_match_pattern(pattern);
    }
    v.visit_expr(&mut arm.body);
}

// --- Expressions ---

pub fn walk_stmt_expr<V: VisitorMut + ?Sized>(v: &mut V, stmt: &mut StmtExpr) {
    match &mut stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. }
        | StmtExprKind::VarBind { pattern, value, .. } => {
            v.visit_bind_pattern(pattern);
            v.visit_expr(value);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            v.visit_expr(assignee);
            v.visit_expr(value);
        }
        StmtExprKind::CompoundAssign {
            assignee, value, ..
        } => {
            v.visit_expr(assignee);
            v.visit_expr(value);
        }
        StmtExprKind::While { cond, body } => {
            v.visit_expr(cond);
            v.visit_expr(body);
        }
        StmtExprKind::For {
            pattern,
            iter,
            body,
        } => {
            v.visit_bind_pattern(pattern);
            v.visit_expr(iter);
            v.visit_expr(body);
        }
        StmtExprKind::Defer { value } => {
            v.visit_expr(value);
        }
        StmtExprKind::Using {
            binding,
            value,
            body,
        } => {
            v.visit_using_binding(binding);
            v.visit_expr(value);
            v.visit_expr(body);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                v.visit_expr(value);
            }
        }
    }
}

pub fn walk_expr<V: VisitorMut + ?Sized>(v: &mut V, expr: &mut Expr) {
    match &mut expr.kind {
        ExprKind::Block { items, tail } => {
            for item in items {
                v.visit_block_item(item);
            }
            if let Some(tail) = tail {
                v.visit_expr(tail);
            }
        }
        ExprKind::Unsafe { body } => {
            v.visit_expr(body);
        }

        ExprKind::NoneLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::UnitLit
        | ExprKind::Var { .. }
        | ExprKind::RoleProjection { .. } => {}

        ExprKind::Range { start, end } => {
            v.visit_expr(start);
            v.visit_expr(end);
        }

        ExprKind::StringFmt { segments } => {
            for segment in segments {
                if let StringFmtSegment::Expr { expr, .. } = segment {
                    v.visit_expr(expr);
                }
            }
        }

        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => {
                for elem in elems {
                    v.visit_expr(elem);
                }
            }
            ArrayLitInit::Repeat(expr, _) => {
                v.visit_expr(expr);
            }
        },

        ExprKind::SetLit { elem_ty, elems } => {
            if let Some(elem_ty) = elem_ty {
                v.visit_type_expr(elem_ty);
            }
            for elem in elems {
                v.visit_expr(elem);
            }
        }
        ExprKind::MapLit {
            key_ty,
            value_ty,
            entries,
        } => {
            if let Some(key_ty) = key_ty {
                v.visit_type_expr(key_ty);
            }
            if let Some(value_ty) = value_ty {
                v.visit_type_expr(value_ty);
            }
            for entry in entries {
                v.visit_expr(&mut entry.key);
                v.visit_expr(&mut entry.value);
            }
        }

        ExprKind::TupleLit(fields) => {
            for field in fields {
                v.visit_expr(field);
            }
        }

        ExprKind::StructLit {
            type_args, fields, ..
        } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
            for field in fields {
                v.visit_expr(&mut field.value);
            }
        }

        ExprKind::EnumVariant {
            type_args, payload, ..
        } => {
            for arg in type_args {
                v.visit_type_expr(arg);
            }
            for expr in payload {
                v.visit_expr(expr);
            }
        }

        ExprKind::StructUpdate { target, fields } => {
            v.visit_expr(target);
            for field in fields {
                v.visit_expr(&mut field.value);
            }
        }

        ExprKind::BinOp { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        ExprKind::UnaryOp { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            v.visit_expr(fallible_expr);
            if let Some(handler) = on_error {
                v.visit_expr(handler);
            }
        }

        ExprKind::HeapAlloc { expr } => {
            v.visit_expr(expr);
        }

        ExprKind::Move { expr } => {
            v.visit_expr(expr);
        }

        ExprKind::ArrayIndex { target, indices } => {
            v.visit_expr(target);
            for index in indices {
                v.visit_expr(index);
            }
        }

        ExprKind::TupleField { target, .. } => {
            v.visit_expr(target);
        }

        ExprKind::StructField { target, .. } => {
            v.visit_expr(target);
        }

        ExprKind::MethodCall { callee, args, .. } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(&mut arg.expr);
            }
        }
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                v.visit_expr(to);
                v.visit_expr(payload);
            }
        },
        ExprKind::Reply { cap, value } => {
            v.visit_expr(cap);
            v.visit_expr(value);
        }

        ExprKind::Closure {
            params,
            return_ty,
            body,
            ..
        } => {
            for param in params {
                v.visit_param(param);
            }
            v.visit_type_expr(return_ty);
            v.visit_expr(body);
        }

        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            v.visit_expr(cond);
            v.visit_expr(then_body);
            v.visit_expr(else_body);
        }

        ExprKind::Slice { target, start, end } => {
            v.visit_expr(target);
            if let Some(start) = start {
                v.visit_expr(start);
            }
            if let Some(end) = end {
                v.visit_expr(end);
            }
        }

        ExprKind::Match { scrutinee, arms } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                v.visit_match_arm(arm);
            }
        }

        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(&mut arg.expr);
            }
        }

        ExprKind::Coerce { expr, .. } => {
            v.visit_expr(expr);
        }

        ExprKind::ImplicitMove { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::AddrOf { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::Deref { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::Load { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::MapGet { target, key } => {
            v.visit_expr(target);
            v.visit_expr(key);
        }
        ExprKind::Len { expr } => {
            v.visit_expr(expr);
        }
        ExprKind::ClosureRef { .. } => {}
    }
}
