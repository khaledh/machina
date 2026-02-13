use std::collections::{BTreeSet, HashSet};

use crate::core::api::{
    FrontendPolicy, ResolveInputs, parse_module_with_id_gen, resolve_stage_with_policy,
    resolve_typecheck_pipeline_with_policy, semcheck_stage_with_policy,
    typecheck_stage_with_policy,
};
use crate::core::context::{
    ResolveStageInput, SemCheckStageOutput, TypecheckStageInput, TypecheckStageOutput,
};
use crate::core::diag::Span;
use crate::core::tree::NodeIdGen;
use crate::core::typecheck::type_map::CallSigMap;

fn parsed_context(source: &str) -> ResolveStageInput {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) =
        parse_module_with_id_gen(source, id_gen).expect("parse should succeed for parity fixture");
    ResolveStageInput::new(module, id_gen)
}

fn span_key(span: Span) -> String {
    format!(
        "{}:{}-{}:{}",
        span.start.line, span.start.column, span.end.line, span.end.column
    )
}

fn assert_diag_subset(
    stage: &str,
    strict_keys: &BTreeSet<String>,
    partial_keys: &BTreeSet<String>,
    strict_details: &[String],
    partial_details: &[String],
) {
    let missing: Vec<_> = strict_keys.difference(partial_keys).cloned().collect();
    assert!(
        missing.is_empty(),
        "{stage}: partial diagnostics are missing strict diagnostics\n\
         missing={missing:#?}\n\
         strict={strict_details:#?}\n\
         partial={partial_details:#?}"
    );
}

fn canonical_call_sigs(call_sigs: &CallSigMap) -> Vec<String> {
    let mut out = call_sigs
        .iter()
        .map(|(node_id, sig)| {
            let receiver = sig
                .receiver
                .as_ref()
                .map(|param| format!("{:?}:{:?}", param.mode, param.ty))
                .unwrap_or_else(|| "-".to_string());
            let params = sig
                .params
                .iter()
                .map(|param| format!("{:?}:{:?}", param.mode, param.ty))
                .collect::<Vec<_>>()
                .join(",");
            format!(
                "{}|def={:?}|recv={receiver}|params={params}",
                node_id.0, sig.def_id
            )
        })
        .collect::<Vec<_>>();
    out.sort();
    out
}

fn canonical_sem_output(ctx: &SemCheckStageOutput) -> Vec<String> {
    let mut out = Vec::new();

    let mut implicit_moves = ctx
        .implicit_moves
        .iter()
        .map(|id| id.0.to_string())
        .collect::<Vec<_>>();
    implicit_moves.sort();
    out.push(format!("implicit_moves={}", implicit_moves.join(",")));

    let mut init_assigns = ctx
        .init_assigns
        .iter()
        .map(|id| id.0.to_string())
        .collect::<Vec<_>>();
    init_assigns.sort();
    out.push(format!("init_assigns={}", init_assigns.join(",")));

    let mut full_init_assigns = ctx
        .full_init_assigns
        .iter()
        .map(|id| id.0.to_string())
        .collect::<Vec<_>>();
    full_init_assigns.sort();
    out.push(format!("full_init_assigns={}", full_init_assigns.join(",")));

    let mut closure_caps = ctx
        .closure_captures
        .iter()
        .map(|(def_id, caps)| format!("{}:{caps:?}", def_id.0))
        .collect::<Vec<_>>();
    closure_caps.sort();
    out.push(format!("closure_captures={}", closure_caps.join("|")));

    out
}

fn resolve_parity_keys(source: &str, policy: FrontendPolicy) -> (BTreeSet<String>, Vec<String>) {
    let out = resolve_stage_with_policy(parsed_context(source), ResolveInputs::default(), policy);
    let details = out
        .errors
        .iter()
        .map(|err| format!("{} @ {}", err, span_key(err.span())))
        .collect::<Vec<_>>();
    let keys = out
        .errors
        .iter()
        .map(|err| format!("{:?}@{}", std::mem::discriminant(err), span_key(err.span())))
        .collect::<BTreeSet<_>>();
    (keys, details)
}

fn typecheck_parity_keys(source: &str, policy: FrontendPolicy) -> (BTreeSet<String>, Vec<String>) {
    let resolved = resolve_stage_with_policy(
        parsed_context(source),
        ResolveInputs::default(),
        FrontendPolicy::Partial,
    );
    let resolved_ctx: TypecheckStageInput = resolved
        .context
        .expect("typecheck parity fixture should resolve");
    let out = typecheck_stage_with_policy(resolved_ctx, resolved.imported_facts, policy);
    let details = out
        .errors
        .iter()
        .map(|err| format!("{} @ {}", err, span_key(err.span())))
        .collect::<Vec<_>>();
    let keys = out
        .errors
        .iter()
        .map(|err| {
            format!(
                "{:?}@{}",
                std::mem::discriminant(err.kind()),
                span_key(err.span())
            )
        })
        .collect::<BTreeSet<_>>();
    (keys, details)
}

fn semcheck_parity_keys(source: &str, policy: FrontendPolicy) -> (BTreeSet<String>, Vec<String>) {
    let resolved = resolve_stage_with_policy(
        parsed_context(source),
        ResolveInputs::default(),
        FrontendPolicy::Partial,
    );
    let resolved_ctx: TypecheckStageInput = resolved
        .context
        .expect("semcheck parity fixture should resolve");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Partial,
    );
    let typed_ctx = typed
        .context
        .expect("semcheck parity fixture should typecheck");
    let out = semcheck_stage_with_policy(typed_ctx, policy, &HashSet::new());
    let details = out
        .errors
        .iter()
        .map(|err| format!("{} @ {}", err, span_key(err.span())))
        .collect::<Vec<_>>();
    let keys = out
        .errors
        .iter()
        .map(|err| format!("{:?}@{}", std::mem::discriminant(err), span_key(err.span())))
        .collect::<BTreeSet<_>>();
    (keys, details)
}

#[test]
fn resolve_partial_is_superset_of_strict() {
    let source = r#"
fn bad_region() -> u64 {
    missing
}

fn main() -> u64 {
    0
}
"#;

    let (strict_keys, strict_details) = resolve_parity_keys(source, FrontendPolicy::Strict);
    let (partial_keys, partial_details) = resolve_parity_keys(source, FrontendPolicy::Partial);
    assert_diag_subset(
        "resolve",
        &strict_keys,
        &partial_keys,
        &strict_details,
        &partial_details,
    );
}

#[test]
fn typecheck_partial_is_superset_of_strict() {
    let source = r#"
fn bad_region() -> u64 {
    let x: u64 = true;
    x
}

fn main() -> u64 {
    0
}
"#;

    let (strict_keys, strict_details) = typecheck_parity_keys(source, FrontendPolicy::Strict);
    let (partial_keys, partial_details) = typecheck_parity_keys(source, FrontendPolicy::Partial);
    assert_diag_subset(
        "typecheck",
        &strict_keys,
        &partial_keys,
        &strict_details,
        &partial_details,
    );
}

#[test]
fn semcheck_partial_is_superset_of_strict() {
    let source = r#"
fn bad(x: u64[4]) -> u64[] {
    x[1..3]
}

fn main() -> u64 {
    0
}
"#;

    let (strict_keys, strict_details) = semcheck_parity_keys(source, FrontendPolicy::Strict);
    let (partial_keys, partial_details) = semcheck_parity_keys(source, FrontendPolicy::Partial);
    assert_diag_subset(
        "semcheck",
        &strict_keys,
        &partial_keys,
        &strict_details,
        &partial_details,
    );
}

#[test]
fn mixed_region_partial_reports_downstream_diagnostics() {
    let source = r#"
fn bad_resolve() -> u64 {
    missing
}

fn bad_type() -> u64 {
    let x: u64 = true;
    x
}

fn main() -> u64 {
    0
}
"#;

    let strict = resolve_typecheck_pipeline_with_policy(
        parsed_context(source),
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    let partial = resolve_typecheck_pipeline_with_policy(
        parsed_context(source),
        ResolveInputs::default(),
        None,
        FrontendPolicy::Partial,
    );

    let strict_resolve_keys = strict
        .resolve_errors
        .iter()
        .map(|err| format!("{:?}@{}", std::mem::discriminant(err), span_key(err.span())))
        .collect::<BTreeSet<_>>();
    let partial_resolve_keys = partial
        .resolve_errors
        .iter()
        .map(|err| format!("{:?}@{}", std::mem::discriminant(err), span_key(err.span())))
        .collect::<BTreeSet<_>>();
    let strict_resolve_details = strict
        .resolve_errors
        .iter()
        .map(|err| format!("{} @ {}", err, span_key(err.span())))
        .collect::<Vec<_>>();
    let partial_resolve_details = partial
        .resolve_errors
        .iter()
        .map(|err| format!("{} @ {}", err, span_key(err.span())))
        .collect::<Vec<_>>();
    assert_diag_subset(
        "pipeline.resolve",
        &strict_resolve_keys,
        &partial_resolve_keys,
        &strict_resolve_details,
        &partial_resolve_details,
    );

    assert!(
        strict.type_errors.is_empty(),
        "strict should stop before downstream type diagnostics when resolve fails"
    );
    assert!(
        !partial.type_errors.is_empty(),
        "partial should preserve downstream type diagnostics for mixed-region fixtures"
    );
}

#[test]
fn strict_and_partial_success_paths_are_semantically_equivalent() {
    let source = r#"
type Pair = {
    left: u64,
    right: u64,
}

fn add_pair(p: Pair) -> u64 {
    p.left + p.right
}

fn main() -> u64 {
    let p = Pair { left: 1, right: 2 };
    add_pair(p)
}
"#;

    let strict_resolve = resolve_stage_with_policy(
        parsed_context(source),
        ResolveInputs::default(),
        FrontendPolicy::Strict,
    );
    let partial_resolve = resolve_stage_with_policy(
        parsed_context(source),
        ResolveInputs::default(),
        FrontendPolicy::Partial,
    );
    assert!(
        strict_resolve.errors.is_empty() && partial_resolve.errors.is_empty(),
        "fixture should stay clean for resolve parity"
    );

    let strict_typed = typecheck_stage_with_policy(
        strict_resolve
            .context
            .expect("strict resolve should yield context"),
        strict_resolve.imported_facts,
        FrontendPolicy::Strict,
    );
    let partial_typed = typecheck_stage_with_policy(
        partial_resolve
            .context
            .expect("partial resolve should yield context"),
        partial_resolve.imported_facts,
        FrontendPolicy::Partial,
    );
    assert!(
        strict_typed.errors.is_empty() && partial_typed.errors.is_empty(),
        "fixture should stay clean for typecheck parity"
    );

    let strict_typed_ctx: TypecheckStageOutput = strict_typed
        .context
        .expect("strict typecheck should yield context");
    let partial_typed_ctx: TypecheckStageOutput = partial_typed
        .context
        .expect("partial typecheck should yield context");

    assert_eq!(
        format!("{}", strict_typed_ctx.type_map),
        format!("{}", partial_typed_ctx.type_map),
        "type maps diverged between strict and partial policies"
    );
    assert_eq!(
        canonical_call_sigs(&strict_typed_ctx.call_sigs),
        canonical_call_sigs(&partial_typed_ctx.call_sigs),
        "call signatures diverged between strict and partial policies"
    );

    let strict_sem =
        semcheck_stage_with_policy(strict_typed_ctx, FrontendPolicy::Strict, &HashSet::new());
    let partial_sem =
        semcheck_stage_with_policy(partial_typed_ctx, FrontendPolicy::Partial, &HashSet::new());
    assert!(
        strict_sem.errors.is_empty() && partial_sem.errors.is_empty(),
        "fixture should stay clean for semcheck parity"
    );

    let strict_sem_ctx = strict_sem
        .context
        .expect("strict semcheck should yield context");
    let partial_sem_ctx = partial_sem
        .context
        .expect("partial semcheck should yield context");
    assert_eq!(
        canonical_sem_output(&strict_sem_ctx),
        canonical_sem_output(&partial_sem_ctx),
        "semantic fact outputs diverged between strict and partial policies"
    );
}
