//! Build deterministic metadata plans for hosted linear machines.
//!
//! These plans sit beside the older typestate machine plans. They are not
//! consumed by lowering yet; the goal of this pass is to give later runtime
//! bridging work a stable, deterministic view of hosted machine structure.

use std::collections::{BTreeMap, HashMap};

use crate::core::analysis::facts::{DefTableOverlay, TypeMapOverlay};
use crate::core::ast::{MethodItem, Module};
use crate::core::linear::{
    LinearIndex, direct_action_method_name, machine_create_fn_name, machine_resume_fn_name,
    machine_spawn_fn_name, machine_trigger_handler_fn_name,
};
use crate::core::plans::{
    LinearMachineActionPlan, LinearMachinePlan, LinearMachinePlanMap, LinearMachineStateTagPlan,
    LinearMachineTriggerPlan,
};
use crate::core::resolve::DefId;
use crate::core::typecheck::type_map::resolve_type_expr;

pub fn build_linear_machine_plans(
    module: &Module,
    def_table: &DefTableOverlay,
    _type_map: &TypeMapOverlay,
    linear_index: &LinearIndex,
) -> LinearMachinePlanMap {
    let func_def_ids = function_def_ids(module, def_table);
    let type_def_ids = type_def_ids(module, def_table);
    let method_def_ids = method_def_ids(module, def_table);
    let mut machines = BTreeMap::new();

    let mut machine_names = linear_index
        .machine_hosts
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    machine_names.sort();

    for machine_name in machine_names {
        let host_info = linear_index
            .machine_hosts
            .get(&machine_name)
            .expect("machine name gathered from host index");
        let Some(type_info) = linear_index.types.get(&host_info.hosted_type_name) else {
            continue;
        };

        let state_tags = state_tag_plans(&type_info.state_names);
        let state_tag_lookup = state_tags
            .iter()
            .map(|state| (state.state_name.clone(), state.tag))
            .collect::<HashMap<_, _>>();
        let initial_state_tag = state_tags
            .first()
            .map(|state| state.tag)
            .expect("hosted linear machine must have at least one state");

        let key_ty = resolve_type_expr(def_table, module, &host_info.key_ty)
            .expect("hosted machine key type should resolve during elaborate");

        let handle_type_def_id = *type_def_ids
            .get(&host_info.handle_type_name)
            .expect("generated machine handle type must exist");
        let spawn_fn_def_id = *func_def_ids
            .get(&machine_spawn_fn_name(&machine_name))
            .expect("generated machine spawn helper must exist");

        let create_fn_def_ids = type_info
            .roles
            .keys()
            .map(|role_name| {
                let helper_name =
                    machine_create_fn_name(&machine_name, &host_info.hosted_type_name, role_name);
                let def_id = *func_def_ids
                    .get(&helper_name)
                    .expect("generated machine create helper must exist");
                (role_name.clone(), def_id)
            })
            .collect::<BTreeMap<_, _>>();

        let resume_fn_def_ids = type_info
            .roles
            .keys()
            .map(|role_name| {
                let helper_name =
                    machine_resume_fn_name(&machine_name, &host_info.hosted_type_name, role_name);
                let def_id = *func_def_ids
                    .get(&helper_name)
                    .expect("generated machine resume helper must exist");
                (role_name.clone(), def_id)
            })
            .collect::<BTreeMap<_, _>>();

        let deliver_fn_def_ids = host_info
            .deliver_helpers
            .iter()
            .map(|(event_type_name, helper_name)| {
                let def_id = *func_def_ids
                    .get(helper_name)
                    .expect("generated machine deliver helper must exist");
                (event_type_name.clone(), def_id)
            })
            .collect::<BTreeMap<_, _>>();

        let wait_fn_def_ids = host_info
            .wait_helpers
            .iter()
            .map(|(source_state, helper_name)| {
                let def_id = *func_def_ids
                    .get(helper_name)
                    .expect("generated machine wait helper must exist");
                (source_state.clone(), def_id)
            })
            .collect::<BTreeMap<_, _>>();

        let mut action_keys = type_info.actions.keys().cloned().collect::<Vec<_>>();
        action_keys.sort();
        let actions = action_keys
            .into_iter()
            .map(|(source_state_name, action_name)| {
                let action = type_info
                    .actions
                    .get(&(source_state_name.clone(), action_name.clone()))
                    .expect("action key gathered from map");
                let source_state_tag = *state_tag_lookup
                    .get(&source_state_name)
                    .expect("action source state must have a tag");
                let target_state_tag = *state_tag_lookup
                    .get(&action.target_state)
                    .expect("action target state must have a tag");
                let handler_def_id = host_info
                    .action_overrides
                    .get(&action_name)
                    .and_then(|helper_name| func_def_ids.get(helper_name).copied())
                    .unwrap_or_else(|| {
                        let method_name =
                            direct_action_method_name(&source_state_name, &action_name);
                        *method_def_ids
                            .get(&(host_info.hosted_type_name.clone(), method_name))
                            .expect("hosted linear action method must exist")
                    });
                let param_tys = action
                    .params
                    .iter()
                    .map(|param_ty| {
                        resolve_type_expr(def_table, module, param_ty)
                            .expect("linear action param type should resolve during elaborate")
                    })
                    .collect::<Vec<_>>();
                LinearMachineActionPlan {
                    action_name,
                    source_state_name,
                    source_state_tag,
                    target_state_name: action.target_state.clone(),
                    target_state_tag,
                    handler_def_id,
                    param_tys,
                }
            })
            .collect::<Vec<_>>();

        let mut trigger_names = type_info.triggers.keys().cloned().collect::<Vec<_>>();
        trigger_names.sort();
        let triggers = trigger_names
            .into_iter()
            .map(|event_type_name| {
                let trigger = type_info
                    .triggers
                    .get(&event_type_name)
                    .expect("trigger name gathered from map");
                let source_state_tag = *state_tag_lookup
                    .get(&trigger.source_state)
                    .expect("trigger source state must have a tag");
                let target_state_tag = *state_tag_lookup
                    .get(&trigger.target_state)
                    .expect("trigger target state must have a tag");
                let helper_name = machine_trigger_handler_fn_name(&machine_name, &event_type_name);
                let handler_def_id = *func_def_ids
                    .get(&helper_name)
                    .expect("generated machine trigger helper must exist");
                LinearMachineTriggerPlan {
                    event_type_name,
                    source_state_name: trigger.source_state.clone(),
                    source_state_tag,
                    target_state_name: trigger.target_state.clone(),
                    target_state_tag,
                    handler_def_id,
                }
            })
            .collect::<Vec<_>>();

        machines.insert(
            machine_name.clone(),
            LinearMachinePlan {
                machine_name,
                hosted_type_name: host_info.hosted_type_name.clone(),
                key_field_name: host_info.key_field.clone(),
                key_ty,
                state_tags,
                initial_state_tag,
                actions,
                triggers,
                handle_type_def_id,
                spawn_fn_def_id,
                create_fn_def_ids,
                resume_fn_def_ids,
                deliver_fn_def_ids,
                wait_fn_def_ids,
            },
        );
    }

    LinearMachinePlanMap { machines }
}

fn function_def_ids(module: &Module, def_table: &DefTableOverlay) -> HashMap<String, DefId> {
    module
        .func_defs()
        .into_iter()
        .map(|func_def| (func_def.sig.name.clone(), def_table.def_id(func_def.id)))
        .collect()
}

fn type_def_ids(module: &Module, def_table: &DefTableOverlay) -> HashMap<String, DefId> {
    module
        .type_defs()
        .into_iter()
        .map(|type_def| (type_def.name.clone(), def_table.def_id(type_def.id)))
        .collect()
}

fn method_def_ids(
    module: &Module,
    def_table: &DefTableOverlay,
) -> HashMap<(String, String), DefId> {
    let mut ids = HashMap::new();
    for block in module.method_blocks() {
        if block.trait_name.is_some() {
            continue;
        }
        for item in &block.method_items {
            let MethodItem::Def(method) = item else {
                continue;
            };
            ids.insert(
                (block.type_name.clone(), method.sig.name.clone()),
                def_table.def_id(method.id),
            );
        }
    }
    ids
}

fn state_tag_plans(state_names: &[String]) -> Vec<LinearMachineStateTagPlan> {
    state_names
        .iter()
        .enumerate()
        .map(|(index, state_name)| LinearMachineStateTagPlan {
            state_name: state_name.clone(),
            tag: index as u64 + 1,
        })
        .collect()
}
