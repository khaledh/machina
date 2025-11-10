use crate::analysis::{DefMap, TypeMap};
use crate::ast::Module;

#[derive(Clone)]
pub struct Context {
    pub module: Module,
}

impl Context {
    pub fn new(module: Module) -> Self {
        Self { module }
    }

    pub fn with_def_map(self, def_map: DefMap) -> ResolvedContext {
        ResolvedContext {
            module: self.module,
            def_map: def_map,
        }
    }
}

#[derive(Clone)]
pub struct ResolvedContext {
    pub module: Module,
    pub def_map: DefMap,
}

impl ResolvedContext {
    pub fn with_type_map(self, type_map: TypeMap) -> TypeCheckedContext {
        TypeCheckedContext {
            module: self.module,
            def_map: self.def_map,
            type_map: type_map,
        }
    }
}

#[allow(unused)]
#[derive(Clone)]
pub struct TypeCheckedContext {
    pub module: Module,
    pub def_map: DefMap,
    pub type_map: TypeMap,
}
