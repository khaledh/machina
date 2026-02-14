use crate::core::api::{
    FrontendPolicy, ResolveInputs, elaborate_stage, parse_module_with_id_gen,
    resolve_stage_with_policy, resolve_typecheck_pipeline_with_policy, semcheck_stage,
    typecheck_stage_with_policy,
};
use crate::core::context::ParsedContext;
use crate::core::tree::NodeIdGen;
use crate::core::tree::semantic as sem;

fn parsed_context(source: &str) -> ParsedContext {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) =
        parse_module_with_id_gen(source, id_gen).expect("parse should succeed for test source");
    ParsedContext::new(module, id_gen)
}

#[test]
fn resolve_policy_strict_vs_partial() {
    let source = r#"
fn main() -> u64 {
    missing()
}
"#;
    let parsed = parsed_context(source);

    let strict = resolve_stage_with_policy(
        parsed.clone(),
        ResolveInputs::default(),
        FrontendPolicy::Strict,
    );
    assert!(strict.has_errors());
    assert!(strict.context.is_none());

    let partial =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Partial);
    assert!(partial.has_errors());
    assert!(partial.context.is_some());
}

#[test]
fn typecheck_policy_strict_vs_partial() {
    let source = r#"
fn main() -> u64 {
    true
}
"#;
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for typecheck policy test");

    let strict = typecheck_stage_with_policy(
        resolved_ctx.clone(),
        resolved.imported_facts.clone(),
        FrontendPolicy::Strict,
    );
    assert!(strict.has_errors());
    assert!(strict.context.is_none());

    let partial = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Partial,
    );
    assert!(partial.has_errors());
    assert!(partial.context.is_some());
}

#[test]
fn resolve_typecheck_pipeline_policy_strict_vs_partial() {
    let source = r#"
fn main() -> u64 {
    missing()
}
"#;
    let parsed = parsed_context(source);

    let strict = resolve_typecheck_pipeline_with_policy(
        parsed.clone(),
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(strict.has_errors());
    assert!(strict.resolved_context.is_none());
    assert!(strict.typed_context.is_none());

    let partial = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Partial,
    );
    assert!(partial.has_errors());
    assert!(partial.resolved_context.is_some());
    assert!(partial.typed_context.is_some());
}

#[test]
fn elaborate_output_has_no_for_statements_after_syntax_desugar_pass() {
    let source = r#"
fn main() -> u64 {
    var sum = 0;
    for x in [1, 2, 3] {
        sum = sum + x;
    }
    sum
}
"#;
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for desugar test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for desugar test");
    let sem_checked = semcheck_stage(typed).expect("semcheck should succeed for desugar test");
    let semantic = elaborate_stage(sem_checked);
    assert!(
        !semantic_module_has_for(&semantic.module),
        "semantic module should not contain StmtExprKind::For after elaborate pipeline"
    );
}

fn semantic_module_has_for(module: &sem::Module) -> bool {
    module.top_level_items.iter().any(top_level_item_has_for)
}

fn top_level_item_has_for(item: &sem::TopLevelItem) -> bool {
    match item {
        sem::TopLevelItem::FuncDef(def) => value_has_for(&def.body),
        sem::TopLevelItem::MethodBlock(block) => block
            .method_items
            .iter()
            .filter_map(|item| match item {
                sem::MethodItem::Def(def) => Some(def),
                sem::MethodItem::Decl(_) => None,
            })
            .any(|def| value_has_for(&def.body)),
        sem::TopLevelItem::TraitDef(_)
        | sem::TopLevelItem::TypeDef(_)
        | sem::TopLevelItem::FuncDecl(_) => false,
    }
}

fn value_has_for(value: &sem::ValueExpr) -> bool {
    match &value.kind {
        sem::ValueExprKind::Block { items, tail } => {
            items.iter().any(block_item_has_for)
                || tail.as_ref().is_some_and(|tail| value_has_for(tail))
        }
        sem::ValueExprKind::If {
            cond,
            then_body,
            else_body,
        } => value_has_for(cond) || value_has_for(then_body) || value_has_for(else_body),
        sem::ValueExprKind::Match { scrutinee, arms } => {
            value_has_for(scrutinee) || arms.iter().any(|arm| value_has_for(&arm.body))
        }
        sem::ValueExprKind::Call { callee, args } => {
            value_has_for(callee) || args.iter().any(call_arg_has_for)
        }
        sem::ValueExprKind::MethodCall { receiver, args, .. } => {
            let receiver_has_for = match receiver {
                sem::MethodReceiver::ValueExpr(value) => value_has_for(value),
                sem::MethodReceiver::PlaceExpr(_) => false,
            };
            receiver_has_for || args.iter().any(call_arg_has_for)
        }
        sem::ValueExprKind::StructLit { fields, .. } => {
            fields.iter().any(|field| value_has_for(&field.value))
        }
        sem::ValueExprKind::StructUpdate { target, fields } => {
            value_has_for(target) || fields.iter().any(|field| value_has_for(&field.value))
        }
        sem::ValueExprKind::EnumVariant { payload, .. } | sem::ValueExprKind::TupleLit(payload) => {
            payload.iter().any(value_has_for)
        }
        sem::ValueExprKind::ArrayLit { init, .. } => match init {
            sem::ArrayLitInit::Elems(elems) => elems.iter().any(value_has_for),
            sem::ArrayLitInit::Repeat(value, _) => value_has_for(value),
        },
        sem::ValueExprKind::SetLit { elems, .. } => elems.iter().any(value_has_for),
        sem::ValueExprKind::MapLit { entries, .. } => entries
            .iter()
            .any(|entry| value_has_for(&entry.key) || value_has_for(&entry.value)),
        sem::ValueExprKind::UnaryOp { expr, .. }
        | sem::ValueExprKind::HeapAlloc { expr }
        | sem::ValueExprKind::Coerce { expr, .. } => value_has_for(expr),
        sem::ValueExprKind::BinOp { left, right, .. } => {
            value_has_for(left) || value_has_for(right)
        }
        sem::ValueExprKind::Range { start, end } => value_has_for(start) || value_has_for(end),
        sem::ValueExprKind::Slice { start, end, .. } => {
            start.as_ref().is_some_and(|start| value_has_for(start))
                || end.as_ref().is_some_and(|end| value_has_for(end))
        }
        sem::ValueExprKind::MapGet { target, key } => value_has_for(target) || value_has_for(key),
        sem::ValueExprKind::UnitLit
        | sem::ValueExprKind::IntLit(_)
        | sem::ValueExprKind::BoolLit(_)
        | sem::ValueExprKind::CharLit(_)
        | sem::ValueExprKind::StringLit { .. }
        | sem::ValueExprKind::StringFmt { .. }
        | sem::ValueExprKind::Move { .. }
        | sem::ValueExprKind::ImplicitMove { .. }
        | sem::ValueExprKind::AddrOf { .. }
        | sem::ValueExprKind::Load { .. }
        | sem::ValueExprKind::Len { .. }
        | sem::ValueExprKind::ClosureRef { .. } => false,
    }
}

fn block_item_has_for(item: &sem::BlockItem) -> bool {
    match item {
        sem::BlockItem::Stmt(stmt) => stmt_has_for(stmt),
        sem::BlockItem::Expr(expr) => value_has_for(expr),
    }
}

fn stmt_has_for(stmt: &sem::StmtExpr) -> bool {
    match &stmt.kind {
        sem::StmtExprKind::For { .. } => true,
        sem::StmtExprKind::LetBind { value, .. } | sem::StmtExprKind::VarBind { value, .. } => {
            value_has_for(value)
        }
        sem::StmtExprKind::Assign { value, .. } => value_has_for(value),
        sem::StmtExprKind::While { cond, body } => value_has_for(cond) || value_has_for(body),
        sem::StmtExprKind::Return { value } => {
            value.as_ref().is_some_and(|value| value_has_for(value))
        }
        sem::StmtExprKind::VarDecl { .. }
        | sem::StmtExprKind::Break
        | sem::StmtExprKind::Continue => false,
    }
}

fn call_arg_has_for(arg: &sem::CallArg) -> bool {
    match arg {
        sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => value_has_for(expr),
        sem::CallArg::InOut { .. } | sem::CallArg::Out { .. } => false,
    }
}
