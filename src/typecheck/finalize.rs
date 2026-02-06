use std::collections::HashSet;

use crate::context::TypeCheckedContext;
use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::tree::map::TreeMapper;
use crate::tree::resolved as res;
use crate::tree::typed::build_module;
use crate::typecheck::constraints::{CallCallee, TyTerm};
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;
use crate::typeck::type_map::{
    CallParam, CallSig, CallSigMap, GenericInstMap, TypeMap, TypeMapBuilder,
};
use crate::types::Type;

#[derive(Debug, Clone)]
pub(crate) struct FinalizeOutput {
    pub(crate) type_map: TypeMap,
    pub(crate) call_sigs: CallSigMap,
    pub(crate) generic_insts: GenericInstMap,
}

/// Pass 5: finalize side tables.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    if !engine.state().diags.is_empty() {
        return Err(engine.state().diags.clone());
    }

    let finalized = build_outputs(engine);
    engine.state_mut().finalize = Some(finalized);
    Ok(())
}

pub(crate) fn materialize(
    engine: TypecheckEngine,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    if let Some(finalized) = engine.state().finalize.clone() {
        let typed_module = build_module(&finalized.type_map, &engine.context().module);
        return Ok(engine.context().clone().with_type_map(
            finalized.type_map,
            finalized.call_sigs,
            finalized.generic_insts,
            typed_module,
        ));
    }

    // Migration fallback: should disappear once finalize is always populated.
    crate::typeck::type_check(engine.context().clone())
}

fn build_outputs(engine: &TypecheckEngine) -> FinalizeOutput {
    let mut builder = TypeMapBuilder::new();

    let all_payload_nodes = collect_payload_nodes(&engine.context().module);
    for node_id in all_payload_nodes {
        let ty = engine
            .state()
            .solve
            .resolved_node_types
            .get(&node_id)
            .cloned()
            .unwrap_or(Type::Unknown);
        builder.record_node_type(node_id, ty);
    }

    for def in engine.context().def_table.clone() {
        let ty = engine
            .state()
            .solve
            .resolved_def_types
            .get(&def.id)
            .cloned()
            .unwrap_or(Type::Unknown);
        builder.record_def_type(def, ty);
    }

    for obligation in &engine.state().constrain.call_obligations {
        let def_id = match &obligation.callee {
            CallCallee::NamedFunction { def_id, .. } => Some(*def_id),
            CallCallee::Method { .. } | CallCallee::Dynamic { .. } => None,
        };
        let params = obligation
            .arg_terms
            .iter()
            .map(|term| CallParam {
                mode: crate::tree::ParamMode::In,
                ty: resolve_term(term, engine),
            })
            .collect::<Vec<_>>();
        builder.record_call_sig(
            obligation.call_node,
            CallSig {
                def_id,
                receiver: None,
                params,
            },
        );
    }

    let (type_map, call_sigs, generic_insts) = builder.finish();
    FinalizeOutput {
        type_map,
        call_sigs,
        generic_insts,
    }
}

fn resolve_term(term: &TyTerm, engine: &TypecheckEngine) -> Type {
    match term {
        TyTerm::Concrete(ty) => ty.clone(),
        TyTerm::Var(var) => engine.type_vars().apply(&Type::Var(*var)),
    }
}

struct PayloadNodeCollector {
    nodes: HashSet<NodeId>,
}

impl PayloadNodeCollector {
    fn collect(module: &res::Module) -> HashSet<NodeId> {
        let mut collector = Self {
            nodes: HashSet::new(),
        };
        let mut ctx = ();
        let _mapped = collector.map_module(module, &mut ctx);
        collector.nodes
    }
}

impl TreeMapper for PayloadNodeCollector {
    type Context = ();
    type InD = DefId;
    type InT = ();
    type OutD = DefId;
    type OutT = ();

    fn map_def_id(
        &mut self,
        _node_id: NodeId,
        def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        *def_id
    }

    fn map_type_payload(
        &mut self,
        node_id: NodeId,
        _payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
        self.nodes.insert(node_id);
    }
}

fn collect_payload_nodes(module: &res::Module) -> HashSet<NodeId> {
    PayloadNodeCollector::collect(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::{collect, constraints, solve, validate};

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");
        let id_gen = parser.into_id_gen();
        let ast_context = ParsedContext::new(module, id_gen);
        resolve(ast_context).expect("Failed to resolve")
    }

    #[test]
    fn test_finalize_materializes_typechecked_context() {
        let source = r#"
            fn test() -> u64 {
                let x = 1;
                x
            }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        constraints::run(&mut engine).expect("constrain pass failed");
        solve::run(&mut engine).expect("solve pass failed");
        validate::run(&mut engine).expect("validate pass failed");
        run(&mut engine).expect("finalize pass failed");

        let checked = materialize(engine).expect("materialize failed");
        let func_body_id = checked.module.func_defs()[0].body.id;
        assert!(checked.type_map.lookup_node_type(func_body_id).is_some());
    }
}
