use crate::analysis::{DefMap, TypeMap};
use crate::ast::Module;
use crate::ir::IrFunction;

#[derive(Clone)]
pub struct AstContext {
    pub module: Module,
}

impl AstContext {
    pub fn new(module: Module) -> Self {
        Self { module }
    }

    pub fn with_def_map(self, def_map: DefMap) -> ResolvedContext {
        ResolvedContext {
            module: self.module,
            def_map,
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
            type_map,
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

impl TypeCheckedContext {
    pub fn with_ir_funcs(self, ir_funcs: Vec<IrFunction>) -> LoweredContext {
        LoweredContext {
            module: self.module,
            def_map: self.def_map,
            type_map: self.type_map,
            ir_funcs,
        }
    }
}

#[derive(Clone)]
pub struct LoweredContext {
    pub module: Module,
    pub def_map: DefMap,
    pub type_map: TypeMap,
    pub ir_funcs: Vec<IrFunction>,
}
