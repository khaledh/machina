use crate::core::api::{FrontendPolicy, ResolveInputs, resolve_stage_with_policy};
use crate::core::capsule::ModuleId;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer};
use crate::core::parse::{Parser, ParserOptions};
use crate::core::resolve::{ResolveError, resolve};
use crate::core::tree::NodeIdGen;
use crate::core::typecheck::type_check;
use crate::services::analysis::results::{
    ResolvedModuleResult, SymbolLookup, TypeLookup, TypedModuleResult,
};

fn resolve_source(
    source: &str,
) -> Result<crate::core::context::ResolvedContext, Vec<ResolveError>> {
    let id_gen = NodeIdGen::new();
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<_>, LexError>>()
        .expect("lexing should succeed");
    let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
    let module = parser.parse().expect("parsing should succeed");
    let parsed = ParsedContext::new(module, parser.into_id_gen());
    resolve(parsed)
}

fn resolve_source_with_typestate(
    source: &str,
) -> Result<crate::core::context::ResolvedContext, Vec<ResolveError>> {
    let id_gen = NodeIdGen::new();
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<_>, LexError>>()
        .expect("lexing should succeed");
    let mut parser = Parser::new_with_id_gen_and_options(
        &tokens,
        id_gen,
        ParserOptions {
            experimental_typestate: true,
        },
    );
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
fn protocol_index_roundtrip_matches_between_context_and_result() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    role Client;
    role Server;
    req Client -> Server: AuthReq => AuthOk;
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Ready {
        Ready { server: server }
    }

    state Ready {}
}
"#;

    let resolved = resolve_source_with_typestate(source).expect("resolve should succeed");
    let protocol_count = resolved.protocol_index.protocols.len();
    let gateway_bindings = resolved
        .protocol_index
        .typestate_bindings
        .get("Gateway")
        .map(|v| v.len())
        .unwrap_or(0);

    let result = ResolvedModuleResult::from_context(ModuleId(9), resolved.clone());
    assert_eq!(result.protocol_index.protocols.len(), protocol_count);
    assert_eq!(
        result
            .protocol_index
            .typestate_bindings
            .get("Gateway")
            .map(|v| v.len())
            .unwrap_or(0),
        gateway_bindings
    );

    let roundtrip = result.into_context();
    assert_eq!(roundtrip.protocol_index.protocols.len(), protocol_count);
    assert_eq!(
        roundtrip
            .protocol_index
            .typestate_bindings
            .get("Gateway")
            .map(|v| v.len())
            .unwrap_or(0),
        gateway_bindings
    );
}
