use std::collections::HashSet;

use crate::context::TypeCheckedContext;
use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::tree::map::TreeMapper;
use crate::tree::resolved as res;
use crate::tree::typed::build_module;
use crate::typecheck::constraints::{CallCallee, TyTerm};
use crate::typecheck::engine::CollectedCallableSig;
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;
use crate::typeck::Unifier;
use crate::typeck::type_map::{
    CallParam, CallSig, CallSigMap, GenericInst, GenericInstMap, TypeMap, TypeMapBuilder,
};
use crate::types::{FnParam, FnParamMode, TyVarId, Type};

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
    crate::typeck::type_check_legacy(engine.context().clone())
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

    let callable_types = collect_callable_def_types(engine);

    for def in engine.context().def_table.clone() {
        let mut ty = engine
            .state()
            .solve
            .resolved_def_types
            .get(&def.id)
            .cloned()
            .unwrap_or(Type::Unknown);
        if ty == Type::Unknown
            && let Some(callable_ty) = callable_types.get(&def.id)
        {
            ty = callable_ty.clone();
        }
        builder.record_def_type(def, ty);
    }

    for obligation in &engine.state().constrain.call_obligations {
        let arg_types = obligation
            .arg_terms
            .iter()
            .map(|term| resolve_term(term, engine))
            .collect::<Vec<_>>();
        let resolved = match &obligation.callee {
            CallCallee::NamedFunction { name, .. } => resolve_named_call(
                engine,
                name,
                &arg_types,
                obligation.span,
                &resolve_term(&obligation.ret_ty, engine),
            ),
            CallCallee::Method { name } => resolve_method_call(
                engine,
                obligation.receiver.as_ref(),
                name,
                &arg_types,
                obligation.span,
                &resolve_term(&obligation.ret_ty, engine),
            ),
            CallCallee::Dynamic { .. } => None,
        };
        let (def_id, receiver, params, generic_inst) = if let Some(resolved) = resolved {
            (
                Some(resolved.def_id),
                resolved.receiver,
                resolved.params,
                resolved.inst,
            )
        } else {
            let params = arg_types
                .into_iter()
                .map(|ty| CallParam {
                    mode: crate::tree::ParamMode::In,
                    ty,
                })
                .collect::<Vec<_>>();
            (None, None, params, None)
        };
        builder.record_call_sig(
            obligation.call_node,
            CallSig {
                def_id,
                receiver,
                params,
            },
        );
        if let Some(inst) = generic_inst {
            builder.record_generic_inst(obligation.call_node, inst);
        }
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

fn collect_callable_def_types(engine: &TypecheckEngine) -> std::collections::HashMap<DefId, Type> {
    let mut out = std::collections::HashMap::new();
    for overloads in engine.env().func_sigs.values() {
        for sig in overloads {
            out.insert(sig.def_id, callable_sig_to_fn_type(sig));
        }
    }
    for by_name in engine.env().method_sigs.values() {
        for overloads in by_name.values() {
            for sig in overloads {
                out.insert(sig.def_id, callable_sig_to_fn_type(sig));
            }
        }
    }
    out
}

fn callable_sig_to_fn_type(sig: &CollectedCallableSig) -> Type {
    let params = sig
        .params
        .iter()
        .map(|param| FnParam {
            mode: fn_param_mode(param.mode.clone()),
            ty: param.ty.clone(),
        })
        .collect::<Vec<_>>();
    Type::Fn {
        params,
        ret_ty: Box::new(sig.ret_ty.clone()),
    }
}

fn fn_param_mode(mode: crate::tree::ParamMode) -> FnParamMode {
    match mode {
        crate::tree::ParamMode::In => FnParamMode::In,
        crate::tree::ParamMode::InOut => FnParamMode::InOut,
        crate::tree::ParamMode::Out => FnParamMode::Out,
        crate::tree::ParamMode::Sink => FnParamMode::Sink,
    }
}

struct ResolvedCall {
    def_id: DefId,
    receiver: Option<CallParam>,
    params: Vec<CallParam>,
    inst: Option<GenericInst>,
}

fn resolve_named_call(
    engine: &TypecheckEngine,
    name: &str,
    arg_types: &[Type],
    span: crate::diag::Span,
    expected_ret: &Type,
) -> Option<ResolvedCall> {
    let overloads = engine.env().func_sigs.get(name)?;
    let sig = pick_overload(overloads, arg_types.len())?;
    Some(instantiate_call_sig(
        sig,
        arg_types,
        None,
        span,
        expected_ret,
    ))
}

fn resolve_method_call(
    engine: &TypecheckEngine,
    receiver: Option<&TyTerm>,
    method_name: &str,
    arg_types: &[Type],
    span: crate::diag::Span,
    expected_ret: &Type,
) -> Option<ResolvedCall> {
    let receiver_ty = receiver.map(|term| resolve_term(term, engine))?;
    let owner = match &receiver_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::String => "string".to_string(),
        _ => return None,
    };
    let by_name = engine.env().method_sigs.get(&owner)?;
    let overloads = by_name.get(method_name)?;
    let sig = pick_overload(overloads, arg_types.len())?;
    let receiver = Some(CallParam {
        mode: sig.self_mode.clone().unwrap_or(crate::tree::ParamMode::In),
        ty: receiver_ty,
    });
    Some(instantiate_call_sig(
        sig,
        arg_types,
        receiver,
        span,
        expected_ret,
    ))
}

fn pick_overload<'a>(
    overloads: &'a [CollectedCallableSig],
    arity: usize,
) -> Option<&'a CollectedCallableSig> {
    let mut matches = overloads.iter().filter(|sig| sig.params.len() == arity);
    let first = matches.next()?;
    if matches.next().is_some() {
        // Ambiguous in the rewrite path for now; caller will fall back.
        return None;
    }
    Some(first)
}

fn instantiate_call_sig(
    sig: &CollectedCallableSig,
    arg_types: &[Type],
    receiver: Option<CallParam>,
    span: crate::diag::Span,
    expected_ret: &Type,
) -> ResolvedCall {
    if sig.type_param_count == 0 {
        let params = sig
            .params
            .iter()
            .map(|param| CallParam {
                mode: param.mode.clone(),
                ty: param.ty.clone(),
            })
            .collect::<Vec<_>>();
        return ResolvedCall {
            def_id: sig.def_id,
            receiver,
            params,
            inst: None,
        };
    }

    let mut unifier = Unifier::new();
    for (param, arg_ty) in sig.params.iter().zip(arg_types.iter()) {
        let _ = unifier.unify(&param.ty, arg_ty);
    }
    let _ = unifier.unify(&sig.ret_ty, expected_ret);

    let type_args = (0..sig.type_param_count)
        .map(|i| unifier.apply(&Type::Var(TyVarId::new(i as u32))))
        .collect::<Vec<_>>();
    let inst = if type_args.iter().any(|ty| matches!(ty, Type::Var(_))) {
        None
    } else {
        Some(GenericInst {
            def_id: sig.def_id,
            type_args: type_args.clone(),
            call_span: span,
        })
    };

    let params = sig
        .params
        .iter()
        .map(|param| CallParam {
            mode: param.mode.clone(),
            ty: unifier.apply(&param.ty),
        })
        .collect::<Vec<_>>();

    ResolvedCall {
        def_id: sig.def_id,
        receiver,
        params,
        inst,
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
