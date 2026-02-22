//! Canonical protocol facts index.
//!
//! `ProtocolIndex` is built once from resolved protocol defs + typestate role
//! bindings and threaded through stage contexts.

use std::collections::{HashMap, HashSet};

use crate::core::context::TypestateRoleImplBinding;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::{Module, ProtocolDef};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::Type;

#[derive(Debug, Clone, Default)]
pub struct ProtocolIndex {
    pub protocols: HashMap<String, ProtocolFact>,
    pub typestate_bindings: HashMap<String, Vec<TypestateProtocolBindingFact>>,
}

impl ProtocolIndex {
    pub fn role_shape(&self, protocol_name: &str, role_name: &str) -> Option<&ProtocolRoleShape> {
        self.protocols
            .get(protocol_name)?
            .roles
            .get(role_name)
            .map(|role| &role.shape)
    }
}

#[derive(Debug, Clone)]
pub struct ProtocolFact {
    pub name: String,
    pub roles: HashMap<String, ProtocolRoleFact>,
    pub request_contracts: Vec<ProtocolRequestContractFact>,
}

#[derive(Debug, Clone)]
pub struct ProtocolRoleFact {
    pub name: String,
    pub states: HashMap<String, ProtocolStateFact>,
    pub shape: ProtocolRoleShape,
}

#[derive(Debug, Clone, Default)]
pub struct ProtocolRoleShape {
    pub required_incoming: HashSet<Type>,
    pub allowed_outgoing: HashSet<Type>,
}

#[derive(Debug, Clone)]
pub struct ProtocolStateFact {
    pub name: String,
    pub shape: ProtocolStateShape,
    pub transitions: Vec<ProtocolTransitionFact>,
}

#[derive(Debug, Clone, Default)]
pub struct ProtocolStateShape {
    pub required_incoming: HashSet<Type>,
    pub allowed_outgoing: HashSet<Type>,
}

#[derive(Debug, Clone)]
pub struct ProtocolTransitionFact {
    pub trigger_payload_ty: Option<Type>,
    pub trigger_from_role: Option<String>,
    pub next_state: String,
    pub effects: Vec<ProtocolEffectFact>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ProtocolEffectFact {
    pub payload_ty: Option<Type>,
    pub to_role: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ProtocolRequestContractFact {
    pub from_role: String,
    pub to_role: String,
    pub request_ty: Option<Type>,
    pub response_tys: Vec<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypestateProtocolBindingFact {
    pub node_id: crate::core::tree::NodeId,
    pub typestate_name: String,
    pub protocol_name: String,
    pub role_name: String,
    pub role_label: String,
    pub role_def_id: Option<DefId>,
    pub peer_role_bindings: Vec<TypestateProtocolPeerBindingFact>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypestateProtocolPeerBindingFact {
    pub node_id: crate::core::tree::NodeId,
    pub field_name: String,
    pub role_name: String,
    pub role_def_id: Option<DefId>,
    pub span: Span,
}

pub fn build_protocol_index(
    module: &Module,
    def_table: &DefTable,
    role_impls: &[TypestateRoleImplBinding],
) -> ProtocolIndex {
    let protocols = module
        .protocol_defs()
        .into_iter()
        .map(|protocol| build_protocol_fact(module, def_table, protocol))
        .map(|fact| (fact.name.clone(), fact))
        .collect();

    let mut typestate_bindings = HashMap::<String, Vec<TypestateProtocolBindingFact>>::new();
    for binding in role_impls {
        if binding.path.len() < 2 {
            continue;
        }
        let protocol_name = binding.path[0].clone();
        let role_name = binding.path[1].clone();
        typestate_bindings
            .entry(binding.typestate_name.clone())
            .or_default()
            .push(TypestateProtocolBindingFact {
                node_id: binding.node_id,
                typestate_name: binding.typestate_name.clone(),
                protocol_name,
                role_name,
                role_label: binding.path.join("::"),
                role_def_id: binding.role_def_id,
                peer_role_bindings: binding
                    .peer_role_bindings
                    .iter()
                    .map(|peer| TypestateProtocolPeerBindingFact {
                        node_id: peer.node_id,
                        field_name: peer.field_name.clone(),
                        role_name: peer.role_name.clone(),
                        role_def_id: peer.role_def_id,
                        span: peer.span,
                    })
                    .collect(),
                span: binding.span,
            });
    }

    ProtocolIndex {
        protocols,
        typestate_bindings,
    }
}

fn build_protocol_fact(
    module: &Module,
    def_table: &DefTable,
    protocol: &ProtocolDef,
) -> ProtocolFact {
    let mut roles = HashMap::new();
    for role in &protocol.roles {
        let mut shape = ProtocolRoleShape::default();
        let mut states = HashMap::new();
        for state in &role.states {
            let mut state_shape = ProtocolStateShape::default();
            let mut transitions = Vec::new();
            for transition in &state.transitions {
                let trigger_payload_ty =
                    resolve_type_expr(def_table, module, &transition.trigger.selector_ty).ok();
                if transition.trigger.from_role.is_some()
                    && let Some(trigger_ty) = &trigger_payload_ty
                {
                    shape.required_incoming.insert(trigger_ty.clone());
                    state_shape.required_incoming.insert(trigger_ty.clone());
                }

                let mut effects = Vec::new();
                for effect in &transition.effects {
                    let payload_ty = resolve_type_expr(def_table, module, &effect.payload_ty).ok();
                    if let Some(payload_ty) = &payload_ty {
                        shape.allowed_outgoing.insert(payload_ty.clone());
                        state_shape.allowed_outgoing.insert(payload_ty.clone());
                    }
                    effects.push(ProtocolEffectFact {
                        payload_ty,
                        to_role: effect.to_role.clone(),
                        span: effect.span,
                    });
                }
                transitions.push(ProtocolTransitionFact {
                    trigger_payload_ty,
                    trigger_from_role: transition.trigger.from_role.clone(),
                    next_state: transition.next_state.clone(),
                    effects,
                    span: transition.span,
                });
            }
            states.insert(
                state.name.clone(),
                ProtocolStateFact {
                    name: state.name.clone(),
                    shape: state_shape,
                    transitions,
                },
            );
        }
        roles.insert(
            role.name.clone(),
            ProtocolRoleFact {
                name: role.name.clone(),
                states,
                shape,
            },
        );
    }

    let mut request_contracts = Vec::new();
    for contract in &protocol.request_contracts {
        let request_ty = resolve_type_expr(def_table, module, &contract.request_ty).ok();
        let mut response_tys = Vec::new();
        for ty in &contract.response_tys {
            if let Ok(ty) = resolve_type_expr(def_table, module, ty) {
                response_tys.push(ty);
            }
        }

        if let Some(request_ty) = &request_ty {
            if let Some(from_role) = roles.get_mut(&contract.from_role) {
                from_role.shape.allowed_outgoing.insert(request_ty.clone());
            }
            if let Some(to_role) = roles.get_mut(&contract.to_role) {
                to_role.shape.required_incoming.insert(request_ty.clone());
            }
        }

        request_contracts.push(ProtocolRequestContractFact {
            from_role: contract.from_role.clone(),
            to_role: contract.to_role.clone(),
            request_ty,
            response_tys,
            span: contract.span,
        });
    }

    ProtocolFact {
        name: protocol.name.clone(),
        roles,
        request_contracts,
    }
}

#[cfg(test)]
mod tests {
    use crate::core::api::{FrontendPolicy, ResolveInputs, resolve_stage_with_policy};
    use crate::core::context::ParsedContext;
    use crate::core::lexer::{LexError, Lexer, Token};
    use crate::core::parse::{Parser, ParserOptions};

    fn resolve_source(source: &str) -> crate::core::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("failed to tokenize");
        let mut parser = Parser::new_with_id_gen_and_options(
            &tokens,
            crate::core::tree::NodeIdGen::new(),
            ParserOptions {
                experimental_typestate: true,
            },
        );
        let module = parser.parse().expect("failed to parse");
        let parsed = ParsedContext::new(module, parser.into_id_gen());
        let out =
            resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
        assert!(
            out.errors.is_empty(),
            "expected clean resolve, got {:?}",
            out.errors
        );
        out.context.expect("strict resolve should produce context")
    }

    #[test]
    fn protocol_index_collects_role_shape_and_transitions() {
        let source = r#"
            type Start = {}
            type AuthReq = {}
            type AuthOk = {}

            protocol Auth {
                msg AuthReq;
                msg AuthOk;
                req Client -> Server: AuthReq => AuthOk;

                role Client {
                    state Idle {
                        on Start -> Awaiting {
                            effects: [ AuthReq ~> Server ]
                        }
                    }
                    state Awaiting {
                        on AuthOk@Server -> Idle;
                    }
                }

                role Server {
                    state Ready {
                        on AuthReq@Client -> Ready {
                            effects: [ AuthOk ~> Client ]
                        }
                    }
                }
            }
        "#;

        let resolved = resolve_source(source);
        let index = &resolved.protocol_index;
        let protocol = index
            .protocols
            .get("Auth")
            .expect("expected protocol fact for Auth");
        let client = protocol
            .roles
            .get("Client")
            .expect("expected role fact for Client");

        assert_eq!(client.states.len(), 2);
        assert!(client.states.contains_key("Idle"));
        assert!(client.states.contains_key("Awaiting"));
        assert_eq!(protocol.request_contracts.len(), 1);
        assert!(
            client
                .shape
                .allowed_outgoing
                .iter()
                .any(|ty| ty.to_string().contains("AuthReq")),
            "expected outgoing set to include AuthReq, got {:?}",
            client.shape.allowed_outgoing
        );
        assert!(
            client
                .shape
                .required_incoming
                .iter()
                .any(|ty| ty.to_string().contains("AuthOk")),
            "expected incoming set to include AuthOk, got {:?}",
            client.shape.required_incoming
        );
    }

    #[test]
    fn protocol_index_collects_typestate_role_bindings() {
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

        let resolved = resolve_source(source);
        let bindings = resolved
            .protocol_index
            .typestate_bindings
            .get("Gateway")
            .expect("expected typestate protocol binding");
        assert_eq!(bindings.len(), 1);
        let binding = &bindings[0];
        assert_eq!(binding.protocol_name, "Auth");
        assert_eq!(binding.role_name, "Client");
        assert_eq!(binding.peer_role_bindings.len(), 1);
        assert_eq!(binding.peer_role_bindings[0].field_name, "server");
        assert_eq!(binding.peer_role_bindings[0].role_name, "Server");
    }
}
