use crate::analysis::DefMap;
use crate::ast::Module;

pub struct Context<'a> {
    module: &'a Module,
    def_map: Option<&'a DefMap>,
}

impl<'a> Context<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            def_map: None,
        }
    }

    pub fn with_def_map(mut self, def_map: &'a DefMap) -> Self {
        self.def_map = Some(def_map);
        self
    }
}
