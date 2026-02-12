use std::collections::HashMap;

use crate::backend::lower::globals::GlobalArena;
use crate::backend::lower::lowerer::FuncLowerer;
use crate::backend::lower::{LowerToIrError, LoweredFunction};
use crate::ir::Terminator;
use crate::resolve::{DefId, DefTable};
use crate::tree::resolved as res;
use crate::tree::semantic as sem;
use crate::typecheck::nominal::NominalKey;
use crate::typecheck::nominal::TypeView;
use crate::typecheck::type_map::{TypeMap, resolve_type_expr};
use crate::typecheck::type_view::TypeViewResolver;
use crate::types::Type;
use crate::types::{EnumVariant, StructField};

/// Tracks drop-glue functions needed for recursive heap elements.
///
/// Recursive type definitions lower to shallow types for recursive occurrences.
/// When dropping a heap element whose type is shallow, we route the drop through
/// a generated drop-glue function to preserve recursive drop behavior without
/// inlining infinitely.
pub(super) struct DropGlueRegistry {
    next_def: u32,
    defs: HashMap<NominalKey, DefId>,
    tys: HashMap<NominalKey, Type>,
    full_tys: HashMap<NominalKey, Type>,
}

impl DropGlueRegistry {
    pub(super) fn new(def_table: &DefTable) -> Self {
        Self {
            next_def: def_table.next_def_id().0,
            defs: HashMap::new(),
            tys: HashMap::new(),
            full_tys: HashMap::new(),
        }
    }

    pub(super) fn from_module(def_table: &DefTable, module: &sem::Module) -> Self {
        let mut full_tys = HashMap::new();
        let mut view_resolver = TypeViewResolver::new(def_table, module);
        for type_def in module.type_defs() {
            if !type_def.type_params.is_empty() {
                // Generic type defs are instantiated on demand.
                continue;
            }
            let key = NominalKey::new(type_def.def_id, Vec::new());
            let ty = view_resolver
                .view_of_key(&key)
                .map(type_from_view)
                .or_else(|| {
                    // Keep fallback behavior for non-nominal aliases.
                    let type_expr = res::TypeExpr {
                        id: type_def.id,
                        kind: res::TypeExprKind::Named {
                            ident: type_def.name.clone(),
                            def_id: type_def.def_id,
                            type_args: Vec::new(),
                        },
                        span: type_def.span,
                    };
                    resolve_type_expr(def_table, module, &type_expr).ok()
                })
                .unwrap_or_else(|| panic!("backend drop glue type def {}", type_def.name));
            full_tys.insert(key, ty);
        }
        Self {
            next_def: def_table.next_def_id().0,
            defs: HashMap::new(),
            tys: HashMap::new(),
            full_tys,
        }
    }

    pub(super) fn def_id_for(&mut self, ty: &Type, type_map: &TypeMap) -> DefId {
        let nominal_key = nominal_key_for_type(ty, type_map);

        if let Some(def_id) = self.defs.get(&nominal_key).copied() {
            return def_id;
        }

        let def_id = DefId(self.next_def);
        self.next_def += 1;
        self.defs.insert(nominal_key.clone(), def_id);

        let full_ty = self
            .full_tys
            .get(&nominal_key)
            .cloned()
            .unwrap_or_else(|| ty.clone());
        self.tys.insert(nominal_key, full_ty);

        def_id
    }

    pub(super) fn take_glue_functions(
        &mut self,
        def_table: &DefTable,
        type_map: &TypeMap,
        globals: &mut GlobalArena,
        trace_drops: bool,
    ) -> Result<Vec<LoweredFunction>, LowerToIrError> {
        let mut funcs = Vec::new();
        let empty_plans = sem::LoweringPlanMap::default();
        let mut pending: Vec<(NominalKey, Type)> = self.tys.drain().collect();
        while let Some((nominal_key, ty)) = pending.pop() {
            let def_id =
                self.defs.get(&nominal_key).copied().unwrap_or_else(|| {
                    panic!("backend drop glue missing def for {:?}", nominal_key)
                });
            let type_def = def_table.lookup_def(nominal_key.def_id).unwrap_or_else(|| {
                panic!(
                    "backend drop glue missing type def {:?}",
                    nominal_key.def_id
                )
            });
            let func_name = if nominal_key.type_args.is_empty() {
                format!("__drop_{}", type_def.name)
            } else {
                format!("__drop_{}${}", type_def.name, def_id.0)
            };
            let mut lowerer = FuncLowerer::new_drop_glue(
                def_id,
                func_name,
                ty.clone(),
                def_table,
                type_map,
                &empty_plans,
                self,
                globals,
                trace_drops,
            );

            let entry = lowerer.builder.current_block();
            let param_ty = lowerer.param_tys[0];
            let arg = lowerer.builder.add_block_param(entry, param_ty);
            lowerer.drop_value_at_addr(arg, &ty)?;
            lowerer
                .builder
                .terminate(Terminator::Return { value: None });

            let (func, types) = lowerer.finish();
            funcs.push(LoweredFunction {
                func,
                types,
                globals: Vec::new(),
            });

            if !self.tys.is_empty() {
                pending.extend(self.tys.drain());
            }
        }

        Ok(funcs)
    }
}

fn type_from_view(view: TypeView) -> Type {
    match view {
        TypeView::Struct(struct_view) => Type::Struct {
            name: struct_view.name,
            fields: struct_view
                .fields
                .into_iter()
                .map(|field| StructField {
                    name: field.name,
                    ty: field.ty,
                })
                .collect(),
        },
        TypeView::Enum(enum_view) => Type::Enum {
            name: enum_view.name,
            variants: enum_view
                .variants
                .into_iter()
                .map(|variant| EnumVariant {
                    name: variant.name,
                    payload: variant.payload,
                })
                .collect(),
        },
    }
}

fn nominal_key_for_type(ty: &Type, type_map: &TypeMap) -> NominalKey {
    let type_id = type_map
        .type_table()
        .lookup_id(ty)
        .unwrap_or_else(|| panic!("backend drop glue missing type id for {:?}", ty));
    type_map
        .lookup_nominal_key_for_type_id(type_id)
        .cloned()
        .unwrap_or_else(|| panic!("backend drop glue missing nominal key for {:?}", ty))
}
