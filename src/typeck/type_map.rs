use crate::ast::{NodeId, TypeExpr, TypeExprKind};
use crate::resolve::def_map::{Def, DefId, DefKind, DefMap};
use crate::typeck::errors::TypeCheckError;
use crate::types::{EnumVariant, StructField, Type};
use std::collections::HashMap;
use std::fmt;

pub(crate) fn resolve_type_expr(
    def_map: &DefMap,
    type_expr: &TypeExpr,
) -> Result<Type, TypeCheckError> {
    match &type_expr.kind {
        TypeExprKind::Named(name) => {
            let def = def_map
                .lookup_def(type_expr.id)
                .ok_or(TypeCheckError::UnknownType(type_expr.span))?;

            // Map built-in type names to Type values
            match name.as_str() {
                "()" => Ok(Type::Unit),
                "u8" => Ok(Type::uint(8)),
                "u16" => Ok(Type::uint(16)),
                "u32" => Ok(Type::uint(32)),
                "u64" => Ok(Type::uint(64)),
                "i8" => Ok(Type::sint(8)),
                "i16" => Ok(Type::sint(16)),
                "i32" => Ok(Type::sint(32)),
                "i64" => Ok(Type::sint(64)),
                "bool" => Ok(Type::Bool),
                "char" => Ok(Type::Char),
                "string" => Ok(Type::String),
                _ => match &def.kind {
                    DefKind::TypeAlias { ty_expr } => resolve_type_expr(def_map, ty_expr),
                    DefKind::StructDef { fields } => {
                        // Convert AST struct fields to Type struct fields
                        let struct_fields = fields
                            .iter()
                            .map(|f| {
                                let field_ty = resolve_type_expr(def_map, &f.ty)?;
                                Ok(StructField {
                                    name: f.name.clone(),
                                    ty: field_ty,
                                })
                            })
                            .collect::<Result<Vec<StructField>, _>>()?;
                        Ok(Type::Struct {
                            name: def.name.clone(),
                            fields: struct_fields,
                        })
                    }
                    DefKind::EnumDef { variants } => {
                        let mut enum_variants = Vec::new();
                        for variant in variants {
                            let payload = variant
                                .payload
                                .iter()
                                .map(|p| resolve_type_expr(def_map, p))
                                .collect::<Result<Vec<Type>, _>>()?;
                            enum_variants.push(EnumVariant {
                                name: variant.name.clone(),
                                payload,
                            });
                        }

                        Ok(Type::Enum {
                            name: def.name.clone(),
                            variants: enum_variants,
                        })
                    }
                    _ => Err(TypeCheckError::UnknownType(type_expr.span)),
                },
            }
        }
        TypeExprKind::Array { elem_ty, dims } => {
            let elem_ty = resolve_type_expr(def_map, elem_ty)?;
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.clone(),
            })
        }
        TypeExprKind::Tuple { fields } => {
            let field_types = fields
                .iter()
                .map(|f| resolve_type_expr(def_map, f))
                .collect::<Result<Vec<Type>, _>>()?;
            Ok(Type::Tuple {
                fields: field_types,
            })
        }
        TypeExprKind::Range { min, max } => Ok(Type::Range {
            min: *min,
            max: *max,
        }),
        TypeExprKind::Slice { elem_ty } => {
            let elem_ty = resolve_type_expr(def_map, elem_ty)?;
            Ok(Type::Slice {
                elem_ty: Box::new(elem_ty),
            })
        }
    }
}

pub struct TypeMapBuilder {
    node_type: HashMap<NodeId, Type>, // maps node to its type
    def_type: HashMap<Def, Type>,     // maps def to its type
    call_def: HashMap<NodeId, DefId>, // maps call expr node id to func def id (overload-resolved)
}

impl TypeMapBuilder {
    pub fn new() -> Self {
        Self {
            node_type: HashMap::new(),
            def_type: HashMap::new(),
            call_def: HashMap::new(),
        }
    }

    pub fn record_def_type(&mut self, def: Def, typ: Type) {
        self.def_type.insert(def, typ);
    }

    pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
        self.node_type.insert(node_id, typ);
    }

    pub fn record_call_def(&mut self, node_id: NodeId, def_id: DefId) {
        self.call_def.insert(node_id, def_id);
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type.get(def).cloned()
    }

    pub fn finish(self) -> TypeMap {
        TypeMap {
            def_type: self.def_type,
            node_type: self.node_type,
            call_def: self.call_def,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeMap {
    def_type: HashMap<Def, Type>,
    node_type: HashMap<NodeId, Type>,
    call_def: HashMap<NodeId, DefId>,
}

impl TypeMap {
    pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
        self.node_type.get(&node).cloned()
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type.get(def).cloned()
    }

    pub fn lookup_call_def(&self, node: NodeId) -> Option<DefId> {
        self.call_def.get(&node).cloned()
    }
}

impl<'a> IntoIterator for &'a TypeMap {
    type Item = (&'a Def, &'a Type);
    type IntoIter = std::vec::IntoIter<(&'a Def, &'a Type)>;

    fn into_iter(self) -> Self::IntoIter {
        let mut items: Vec<_> = self.def_type.iter().collect();
        items.sort_by_key(|(def, _)| def.id);
        items.into_iter()
    }
}

impl fmt::Display for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // sort by node id
        let mut node_type = self.node_type.iter().collect::<Vec<(&NodeId, &Type)>>();
        node_type.sort_by_key(|(node, _)| node.0);
        for (node, typ) in node_type {
            writeln!(f, "Node [{}] -> Type [{}]", node, typ)?;
        }
        Ok(())
    }
}
