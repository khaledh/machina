use crate::ast::{NodeId, TypeExpr, TypeExprKind};
use crate::resolve::def_map::{Def, DefKind, DefMap};
use crate::typeck::errors::TypeCheckError;
use crate::types::{StructField, Type};
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
                "u64" => Ok(Type::UInt64),
                "bool" => Ok(Type::Bool),
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
    }
}

pub struct TypeMapBuilder {
    node_type: HashMap<NodeId, Type>,
    def_type: HashMap<Def, Type>,
}

impl TypeMapBuilder {
    pub fn new() -> Self {
        Self {
            node_type: HashMap::new(),
            def_type: HashMap::new(),
        }
    }

    pub fn record_def_type(&mut self, def: Def, typ: Type) {
        self.def_type.insert(def, typ);
    }

    pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
        self.node_type.insert(node_id, typ);
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type.get(def).cloned()
    }

    pub fn finish(self) -> TypeMap {
        TypeMap {
            def_type: self.def_type,
            node_type: self.node_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeMap {
    def_type: HashMap<Def, Type>,
    node_type: HashMap<NodeId, Type>,
}

impl TypeMap {
    pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
        self.node_type.get(&node).cloned()
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
