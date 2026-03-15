use crate::core::api::{FrontendPolicy, ResolveInputs, resolve_stage_with_policy};
use crate::core::ast::NodeIdGen;
use crate::core::capsule::ModuleId;
use crate::core::context::ParsedContext;
use crate::core::context::ResolvedContext;
use crate::core::lexer::{LexError, Lexer};
use crate::core::parse::Parser;
use crate::core::resolve::ResolveError;
use crate::core::typecheck::type_check;
use crate::services::analysis::results::{
    ResolvedModuleResult, SymbolLookup, TypeLookup, TypedModuleResult,
};

fn resolve_source(source: &str) -> Result<ResolvedContext, Vec<ResolveError>> {
    let id_gen = NodeIdGen::new();
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<_>, LexError>>()
        .expect("lexing should succeed");
    let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
    let module = parser.parse().expect("parsing should succeed");
    let parsed = ParsedContext::new(module, parser.into_id_gen());
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    if out.errors.is_empty() {
        Ok(out
            .context
            .expect("strict resolve should produce context on success"))
    } else {
        Err(out.errors)
    }
}
#[test]
fn symbol_lookup_matches_between_context_and_result() {
    let source = r#"
fn id(x: u64) -> u64 { x }
"#;

    let resolved = resolve_source(source).expect("resolve should succeed");
    let id_func = resolved
        .module
        .func_defs()
        .into_iter()
        .find(|f| f.sig.name == "id")
        .expect("id function exists");

    let context_def_id = resolved
        .lookup_def_id_by_node(id_func.id)
        .expect("function node should map to def");
    let context_def = resolved
        .lookup_def_by_node(id_func.id)
        .expect("def should exist");
    assert_eq!(context_def.name, "id");

    let result = ResolvedModuleResult::from_context(ModuleId(42), resolved.clone());
    assert_eq!(result.module_id, ModuleId(42));
    assert_eq!(
        result.lookup_def_id_by_node(id_func.id),
        Some(context_def_id)
    );
    assert_eq!(
        result
            .lookup_def_by_node(id_func.id)
            .map(|d| d.name.as_str()),
        Some("id")
    );

    let roundtrip = result.into_context();
    assert_eq!(
        roundtrip.lookup_def_id_by_node(id_func.id),
        Some(context_def_id)
    );
}

#[test]
fn type_lookup_matches_between_context_and_result() {
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;

    let resolved = resolve_source(source).expect("resolve should succeed");
    let typed = type_check(resolved).expect("typecheck should succeed");

    let id_func = typed
        .module
        .func_defs()
        .into_iter()
        .find(|f| f.sig.name == "id")
        .expect("id function exists");
    let call_node = *typed
        .call_sigs
        .keys()
        .next()
        .expect("expected at least one call signature");

    let id_def_id = typed
        .lookup_def_id_by_node(id_func.id)
        .expect("id node maps to def");
    let context_node_ty = typed.lookup_node_type(call_node);
    let context_call_sig_param_count = typed
        .lookup_call_sig(call_node)
        .map(|sig| sig.params.len())
        .expect("call node should have signature");
    let context_def_ty = typed.lookup_def_type(id_def_id);

    let result = TypedModuleResult::from_context(ModuleId(7), typed.clone());
    assert_eq!(result.module_id, ModuleId(7));
    assert_eq!(result.lookup_node_type(call_node), context_node_ty);
    assert_eq!(
        result
            .lookup_call_sig(call_node)
            .map(|sig| sig.params.len()),
        Some(context_call_sig_param_count)
    );
    assert_eq!(result.lookup_def_type(id_def_id), context_def_ty);

    let roundtrip = result.into_context();
    assert_eq!(roundtrip.lookup_node_type(call_node), context_node_ty);
}

#[test]
fn linear_result_roundtrip_succeeds() {
    let resolved = resolve_source(
        r#"
        @linear
        type Door = {
            states { Closed, Open }
            actions { open: Closed -> Open }
        }

        Door :: {
            fn open(self) -> Open {
                Open {}
            }
        }
        "#,
    )
    .expect("resolve should succeed for linear program");

    let result = ResolvedModuleResult::from_context(ModuleId(77), resolved.clone());
    let _roundtrip = result.into_context();
}

#[test]
fn linear_index_roundtrip_matches_between_context_and_result() {
    let source = r#"
@linear
type PullRequest = {
    id: u64,

    states {
        Draft,
        Review,
    }

    actions {
        submit: Draft -> Review,
    }

    roles {
        Author { submit }
    }
}

PullRequest :: {
    fn submit(self) -> Review {
        Review {}
    }
}

machine PRService hosts PullRequest(key: id) {
    fn new() -> Self {
        Self {}
    }
}
"#;

    let resolved = resolve_source(source).expect("resolve should succeed");
    let type_count = resolved.linear_index.types.len();
    let host_count = resolved.linear_index.machine_hosts.len();
    let derived_interaction_count = resolved.linear_index.derived_interactions.len();

    let result = ResolvedModuleResult::from_context(ModuleId(10), resolved.clone());
    assert_eq!(result.linear_index.types.len(), type_count);
    assert_eq!(result.linear_index.machine_hosts.len(), host_count);
    assert_eq!(
        result.linear_index.derived_interactions.len(),
        derived_interaction_count
    );

    let roundtrip = result.into_context();
    assert_eq!(roundtrip.linear_index.types.len(), type_count);
    assert_eq!(roundtrip.linear_index.machine_hosts.len(), host_count);
    assert_eq!(
        roundtrip.linear_index.derived_interactions.len(),
        derived_interaction_count
    );
}

#[test]
fn derived_interaction_facts_roundtrip_between_context_and_result() {
    let source = r#"
type AuthCheck = {}
type AuthApproved = { order_id: u64 }
type AuthDenied = { order_id: u64 }

@linear
type Order = {
    id: u64,

    states {
        Draft,
        PendingAuth,
        Confirmed,
        Rejected,
    }

    actions {
        submit: Draft -> PendingAuth,
    }

    triggers {
        AuthApproved: PendingAuth -> Confirmed,
        AuthDenied: PendingAuth -> Rejected,
    }

    roles {
        Buyer { submit }
    }
}

Order :: {
    fn submit(self) -> PendingAuth {
        PendingAuth {}
    }
}

@linear
type AuthWorker = {
    id: u64,

    states { Idle }
    roles { System {} }
}

machine AuthService hosts AuthWorker(key: id) {
    fn new() -> Self {
        Self {}
    }

    on AuthCheck(_check) {}
}

machine OrderService hosts Order(key: id) {
    fields {
        auth_service: Machine<AuthService>,
    }

    fn new(auth_service: Machine<AuthService>) -> Self {
        Self { auth_service: auth_service }
    }

    action submit(draft) -> PendingAuth {
        send(self.auth_service, AuthCheck {});
        draft.submit()
    }

    trigger AuthApproved(pending) {
        Confirmed {}
    }

    trigger AuthDenied(pending) {
        Rejected {}
    }
}
"#;

    let resolved = resolve_source(source).expect("resolve should succeed");
    let expected = resolved
        .linear_index
        .derived_interactions
        .get(&("OrderService".to_string(), "submit".to_string()))
        .expect("expected derived interaction facts")
        .clone();

    let result = ResolvedModuleResult::from_context(ModuleId(11), resolved.clone());
    let from_result = result
        .linear_index
        .derived_interactions
        .get(&("OrderService".to_string(), "submit".to_string()))
        .expect("expected derived interaction facts in result");
    assert_eq!(from_result.request_type_name, expected.request_type_name);
    assert_eq!(from_result.waiting_state, expected.waiting_state);
    assert_eq!(from_result.reply_types, expected.reply_types);

    let roundtrip = result.into_context();
    let from_roundtrip = roundtrip
        .linear_index
        .derived_interactions
        .get(&("OrderService".to_string(), "submit".to_string()))
        .expect("expected derived interaction facts after roundtrip");
    assert_eq!(from_roundtrip.request_type_name, expected.request_type_name);
    assert_eq!(from_roundtrip.waiting_state, expected.waiting_state);
    assert_eq!(from_roundtrip.reply_types, expected.reply_types);
}
