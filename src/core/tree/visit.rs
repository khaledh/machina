use crate::core::tree::*;

/// Tree visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
/// Example:
/// ```rust
/// use machina::core::tree::Expr;
/// use machina::core::tree::visit::{walk_expr, Visitor};
///
/// struct MyVisitor;
/// impl Visitor for MyVisitor {
///     fn visit_expr(&mut self, expr: &Expr) {
///         // pre-visit logic here
///         walk_expr(self, expr);
///         // post-visit logic here
///     }
/// }
/// ```
pub trait Visitor {
    // --- Module ---

    fn visit_module(&mut self, module: &Module) {
        walk_module(self, module)
    }

    // --- Type Definitions ---

    fn visit_type_def(&mut self, type_def: &TypeDef) {
        walk_type_def(self, type_def)
    }

    fn visit_protocol_def(&mut self, protocol_def: &ProtocolDef) {
        walk_protocol_def(self, protocol_def)
    }

    fn visit_protocol_role(&mut self, role: &ProtocolRole) {
        walk_protocol_role(self, role)
    }

    fn visit_protocol_message(&mut self, message: &ProtocolMessage) {
        walk_protocol_message(self, message)
    }

    fn visit_protocol_request_contract(&mut self, contract: &ProtocolRequestContract) {
        walk_protocol_request_contract(self, contract)
    }

    fn visit_protocol_state(&mut self, state: &ProtocolState) {
        walk_protocol_state(self, state)
    }

    fn visit_protocol_transition(&mut self, transition: &ProtocolTransition) {
        walk_protocol_transition(self, transition)
    }

    fn visit_protocol_trigger(&mut self, trigger: &ProtocolTrigger) {
        walk_protocol_trigger(self, trigger)
    }

    fn visit_protocol_effect(&mut self, effect: &ProtocolEffect) {
        walk_protocol_effect(self, effect)
    }

    fn visit_trait_def(&mut self, trait_def: &TraitDef) {
        walk_trait_def(self, trait_def)
    }

    fn visit_trait_method(&mut self, method: &TraitMethod) {
        walk_trait_method(self, method)
    }

    fn visit_trait_property(&mut self, property: &TraitProperty) {
        walk_trait_property(self, property)
    }

    fn visit_typestate_def(&mut self, typestate_def: &TypestateDef) {
        walk_typestate_def(self, typestate_def)
    }

    fn visit_typestate_item(&mut self, item: &TypestateItem) {
        walk_typestate_item(self, item)
    }

    fn visit_typestate_role_impl(&mut self, role_impl: &TypestateRoleImpl) {
        walk_typestate_role_impl(self, role_impl)
    }

    fn visit_typestate_fields(&mut self, fields: &TypestateFields) {
        walk_typestate_fields(self, fields)
    }

    fn visit_typestate_state(&mut self, state: &TypestateState) {
        walk_typestate_state(self, state)
    }

    fn visit_typestate_state_item(&mut self, item: &TypestateStateItem) {
        walk_typestate_state_item(self, item)
    }

    fn visit_typestate_on_handler(&mut self, handler: &TypestateOnHandler) {
        walk_typestate_on_handler(self, handler)
    }

    fn visit_struct_def_fields(&mut self, fields: &[StructDefField]) {
        walk_struct_def_fields(self, fields)
    }

    fn visit_struct_def_field(&mut self, field: &StructDefField) {
        walk_struct_def_field(self, field)
    }

    fn visit_enum_def_variants(&mut self, variants: &[EnumDefVariant]) {
        walk_enum_def_variants(self, variants)
    }

    fn visit_enum_def_variant(&mut self, variant: &EnumDefVariant) {
        walk_enum_def_variant(self, variant)
    }

    // --- Type Expressions ---

    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        walk_type_expr(self, type_expr)
    }

    // --- Function Declarations ---

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        walk_func_decl(self, func_decl)
    }

    // --- Functions ---

    fn visit_func_def(&mut self, func_def: &FuncDef) {
        walk_func_def(self, func_def)
    }

    // --- Function Signatures ---

    fn visit_func_sig(&mut self, func_sig: &FunctionSig) {
        walk_func_sig(self, func_sig)
    }

    // --- Type Parameters ---

    fn visit_type_param(&mut self, param: &TypeParam) {
        walk_type_param(self, param)
    }

    // --- Method Signatures ---

    fn visit_method_sig(&mut self, method_sig: &MethodSig) {
        walk_method_sig(self, method_sig)
    }

    fn visit_self_param(&mut self, self_param: &SelfParam) {
        walk_self_param(self, self_param)
    }

    // --- Parameters (common) ---

    fn visit_param(&mut self, param: &Param) {
        walk_param(self, param)
    }

    // --- Method Blocks ---

    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        walk_method_block(self, method_block)
    }

    fn visit_method_item(&mut self, method_item: &MethodItem) {
        walk_method_item(self, method_item)
    }

    fn visit_method_decl(&mut self, method_decl: &MethodDecl) {
        walk_method_decl(self, method_decl)
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        walk_method_def(self, method_def)
    }

    // --- Closure Definitions ---

    fn visit_closure_def(&mut self, closure_def: &ClosureDef) {
        walk_closure_def(self, closure_def)
    }

    // --- Blocks ---

    fn visit_block_item(&mut self, item: &BlockItem) {
        walk_block_item(self, item)
    }

    // --- Bind Patterns ---

    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        walk_bind_pattern(self, pattern)
    }

    // --- Match Patterns ---

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        walk_match_pattern(self, pattern)
    }

    fn visit_match_arm(&mut self, arm: &MatchArm) {
        walk_match_arm(self, arm)
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        walk_match_pattern_binding(self, binding)
    }

    // --- Expressions ---

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr)
    }
}

// --- Module ---

pub fn walk_module<V: Visitor + ?Sized>(v: &mut V, module: &Module) {
    for item in &module.top_level_items {
        match item {
            TopLevelItem::ProtocolDef(protocol_def) => v.visit_protocol_def(protocol_def),
            TopLevelItem::TraitDef(trait_def) => v.visit_trait_def(trait_def),
            TopLevelItem::TypeDef(type_def) => v.visit_type_def(type_def),
            TopLevelItem::TypestateDef(typestate_def) => v.visit_typestate_def(typestate_def),
            TopLevelItem::FuncDecl(func_decl) => v.visit_func_decl(func_decl),
            TopLevelItem::FuncDef(func_def) => v.visit_func_def(func_def),
            TopLevelItem::MethodBlock(method_block) => v.visit_method_block(method_block),
            TopLevelItem::ClosureDef(closure_def) => v.visit_closure_def(closure_def),
        }
    }
}

// --- Type Definitions ---

pub fn walk_protocol_def<V: Visitor + ?Sized>(v: &mut V, protocol_def: &ProtocolDef) {
    for message in &protocol_def.messages {
        v.visit_protocol_message(message);
    }
    for contract in &protocol_def.request_contracts {
        v.visit_protocol_request_contract(contract);
    }
    for role in &protocol_def.roles {
        v.visit_protocol_role(role);
    }
}

pub fn walk_protocol_role<V: Visitor + ?Sized>(v: &mut V, role: &ProtocolRole) {
    for state in &role.states {
        v.visit_protocol_state(state);
    }
}

pub fn walk_protocol_message<V: Visitor + ?Sized>(v: &mut V, message: &ProtocolMessage) {
    v.visit_type_expr(&message.ty);
}

pub fn walk_protocol_request_contract<V: Visitor + ?Sized>(
    v: &mut V,
    contract: &ProtocolRequestContract,
) {
    v.visit_type_expr(&contract.request_ty);
    for response_ty in &contract.response_tys {
        v.visit_type_expr(response_ty);
    }
}

pub fn walk_protocol_state<V: Visitor + ?Sized>(v: &mut V, state: &ProtocolState) {
    for transition in &state.transitions {
        v.visit_protocol_transition(transition);
    }
}

pub fn walk_protocol_transition<V: Visitor + ?Sized>(v: &mut V, transition: &ProtocolTransition) {
    v.visit_protocol_trigger(&transition.trigger);
    for effect in &transition.effects {
        v.visit_protocol_effect(effect);
    }
}

pub fn walk_protocol_trigger<V: Visitor + ?Sized>(v: &mut V, trigger: &ProtocolTrigger) {
    v.visit_type_expr(&trigger.selector_ty);
}

pub fn walk_protocol_effect<V: Visitor + ?Sized>(v: &mut V, effect: &ProtocolEffect) {
    v.visit_type_expr(&effect.payload_ty);
}

pub fn walk_trait_def<V: Visitor + ?Sized>(v: &mut V, trait_def: &TraitDef) {
    for method in &trait_def.methods {
        v.visit_trait_method(method);
    }
    for property in &trait_def.properties {
        v.visit_trait_property(property);
    }
}

pub fn walk_trait_method<V: Visitor + ?Sized>(v: &mut V, method: &TraitMethod) {
    v.visit_method_sig(&method.sig);
}

pub fn walk_trait_property<V: Visitor + ?Sized>(v: &mut V, property: &TraitProperty) {
    v.visit_type_expr(&property.ty);
}

pub fn walk_typestate_def<V: Visitor + ?Sized>(v: &mut V, typestate_def: &TypestateDef) {
    for role_impl in &typestate_def.role_impls {
        v.visit_typestate_role_impl(role_impl);
    }
    for item in &typestate_def.items {
        v.visit_typestate_item(item);
    }
}

pub fn walk_typestate_role_impl<V: Visitor + ?Sized>(_v: &mut V, _role_impl: &TypestateRoleImpl) {}

pub fn walk_typestate_item<V: Visitor + ?Sized>(v: &mut V, item: &TypestateItem) {
    match item {
        TypestateItem::Fields(fields) => v.visit_typestate_fields(fields),
        TypestateItem::Constructor(constructor) => v.visit_func_def(constructor),
        TypestateItem::Handler(handler) => v.visit_typestate_on_handler(handler),
        TypestateItem::State(state) => v.visit_typestate_state(state),
    }
}

pub fn walk_typestate_fields<V: Visitor + ?Sized>(v: &mut V, fields: &TypestateFields) {
    for field in &fields.fields {
        v.visit_struct_def_field(field);
    }
}

pub fn walk_typestate_state<V: Visitor + ?Sized>(v: &mut V, state: &TypestateState) {
    for item in &state.items {
        v.visit_typestate_state_item(item);
    }
}

pub fn walk_typestate_state_item<V: Visitor + ?Sized>(v: &mut V, item: &TypestateStateItem) {
    match item {
        TypestateStateItem::Fields(fields) => v.visit_typestate_fields(fields),
        TypestateStateItem::Method(method) => v.visit_func_def(method),
        TypestateStateItem::Handler(handler) => v.visit_typestate_on_handler(handler),
    }
}

pub fn walk_typestate_on_handler<V: Visitor + ?Sized>(v: &mut V, handler: &TypestateOnHandler) {
    v.visit_type_expr(&handler.selector_ty);
    for param in &handler.params {
        v.visit_param(param);
    }
    if let Some(provenance) = &handler.provenance {
        v.visit_param(&provenance.param);
    }
    v.visit_type_expr(&handler.ret_ty_expr);
    v.visit_expr(&handler.body);
}

pub fn walk_type_def<V: Visitor + ?Sized>(v: &mut V, type_def: &TypeDef) {
    for param in &type_def.type_params {
        v.visit_type_param(param);
    }
    match &type_def.kind {
        TypeDefKind::Alias { aliased_ty } => v.visit_type_expr(aliased_ty),
        TypeDefKind::Struct { fields } => v.visit_struct_def_fields(fields),
        TypeDefKind::Enum { variants } => v.visit_enum_def_variants(variants),
    }
}

pub fn walk_struct_def_fields<V: Visitor + ?Sized>(v: &mut V, fields: &[StructDefField]) {
    for field in fields {
        v.visit_struct_def_field(field);
    }
}

pub fn walk_struct_def_field<V: Visitor + ?Sized>(v: &mut V, field: &StructDefField) {
    v.visit_type_expr(&field.ty);
}

pub fn walk_enum_def_variants<V: Visitor + ?Sized>(v: &mut V, variants: &[EnumDefVariant]) {
    for variant in variants {
        v.visit_enum_def_variant(variant);
    }
}

pub fn walk_enum_def_variant<V: Visitor + ?Sized>(v: &mut V, variant: &EnumDefVariant) {
    for payload in &variant.payload {
        v.visit_type_expr(payload);
    }
}

// --- Type Expressions ---

pub fn walk_type_expr<V: Visitor + ?Sized>(v: &mut V, type_expr: &TypeExpr) {
    match &type_expr.kind {
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
        TypeExprKind::Ref { elem_ty_expr, .. } => v.visit_type_expr(elem_ty_expr),
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            for param in params {
                v.visit_type_expr(&param.ty_expr);
            }
            v.visit_type_expr(ret_ty_expr);
        }
    }
}

// --- Function Declarations ---

pub fn walk_func_decl<V: Visitor + ?Sized>(v: &mut V, func_decl: &FuncDecl) {
    v.visit_func_sig(&func_decl.sig);
}

// --- Functions ---

pub fn walk_func_def<V: Visitor + ?Sized>(v: &mut V, func_def: &FuncDef) {
    v.visit_func_sig(&func_def.sig);
    v.visit_expr(&func_def.body);
}

// --- Function Signatures ---

pub fn walk_func_sig<V: Visitor + ?Sized>(v: &mut V, func_sig: &FunctionSig) {
    for param in &func_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &func_sig.params {
        v.visit_param(param);
    }
    v.visit_type_expr(&func_sig.ret_ty_expr);
}

pub fn walk_type_param<V: Visitor + ?Sized>(_v: &mut V, _param: &TypeParam) {}

// --- Method Signatures ---

pub fn walk_method_sig<V: Visitor + ?Sized>(v: &mut V, method_sig: &MethodSig) {
    v.visit_self_param(&method_sig.self_param);
    for param in &method_sig.type_params {
        v.visit_type_param(param);
    }
    for param in &method_sig.params {
        v.visit_param(param);
    }
    v.visit_type_expr(&method_sig.ret_ty_expr);
}

pub fn walk_self_param<V: Visitor + ?Sized>(_v: &mut V, _self_param: &SelfParam) {}

// --- Parameters (common) ---

pub fn walk_param<V: Visitor + ?Sized>(v: &mut V, param: &Param) {
    v.visit_type_expr(&param.typ);
}

// --- Method Blocks ---

pub fn walk_method_block<V: Visitor + ?Sized>(v: &mut V, method_block: &MethodBlock) {
    for method_item in &method_block.method_items {
        v.visit_method_item(method_item);
    }
}

pub fn walk_method_item<V: Visitor + ?Sized>(v: &mut V, method_item: &MethodItem) {
    match method_item {
        MethodItem::Decl(method_decl) => v.visit_method_decl(method_decl),
        MethodItem::Def(method_def) => v.visit_method_def(method_def),
    }
}

pub fn walk_method_decl<V: Visitor + ?Sized>(v: &mut V, method_decl: &MethodDecl) {
    v.visit_method_sig(&method_decl.sig);
}

pub fn walk_method_def<V: Visitor + ?Sized>(v: &mut V, method_def: &MethodDef) {
    v.visit_method_sig(&method_def.sig);
    v.visit_expr(&method_def.body);
}

// --- Closure Definitions ---

pub fn walk_closure_def<V: Visitor + ?Sized>(_v: &mut V, _closure_def: &ClosureDef) {
    // Closures are also visited at their expression sites; avoid walking the lifted body twice.
}

// --- Blocks ---

pub fn walk_block_item<V: Visitor + ?Sized>(v: &mut V, item: &BlockItem) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

// --- Bind Patterns ---

pub fn walk_bind_pattern<V: Visitor + ?Sized>(v: &mut V, pattern: &BindPattern) {
    match &pattern.kind {
        BindPatternKind::Name { .. } => {}
        BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
            for pattern in patterns {
                v.visit_bind_pattern(pattern);
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                v.visit_bind_pattern(&field.pattern);
            }
        }
    }
}

// --- Match Patterns ---

pub fn walk_match_pattern<V: Visitor + ?Sized>(v: &mut V, pattern: &MatchPattern) {
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

pub fn walk_match_pattern_bindings<V: Visitor + ?Sized>(v: &mut V, pattern: &MatchPattern) {
    if let MatchPattern::EnumVariant { bindings, .. } = pattern {
        for binding in bindings {
            v.visit_match_pattern_binding(binding);
        }
    }
}

pub fn walk_match_pattern_binding<V: Visitor + ?Sized>(_v: &mut V, _binding: &MatchPatternBinding) {
}

pub fn walk_match_arm<V: Visitor + ?Sized>(v: &mut V, arm: &MatchArm) {
    v.visit_match_pattern(&arm.pattern);
    v.visit_expr(&arm.body);
}

// --- Expressions ---

pub fn walk_stmt_expr<V: Visitor + ?Sized>(v: &mut V, stmt: &StmtExpr) {
    match &stmt.kind {
        StmtExprKind::LetBind {
            pattern,
            decl_ty,
            value,
        }
        | StmtExprKind::VarBind {
            pattern,
            decl_ty,
            value,
        } => {
            v.visit_bind_pattern(pattern);
            if let Some(decl_ty) = decl_ty {
                v.visit_type_expr(decl_ty);
            }
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
        StmtExprKind::Using { value, body, .. } => {
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

pub fn walk_expr<V: Visitor + ?Sized>(v: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Block { items, tail } => {
            for item in items {
                v.visit_block_item(item);
            }
            if let Some(tail) = tail {
                v.visit_expr(tail);
            }
        }

        ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::UnitLit
        | ExprKind::Var { .. } => {}

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
                v.visit_expr(&entry.key);
                v.visit_expr(&entry.value);
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
                v.visit_expr(&field.value);
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
                v.visit_expr(&field.value);
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
                v.visit_expr(&arg.expr);
            }
        }
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload }
            | EmitKind::Request {
                to,
                payload,
                request_site_label: _,
            } => {
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
                v.visit_expr(&arg.expr);
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
    }
}
