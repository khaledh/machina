use crate::analysis::ResolutionMap;
use crate::ast::Module;

pub struct Context<'a> {
    module: &'a Module,
    resolution: Option<&'a ResolutionMap>,
}

impl<'a> Context<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            resolution: None,
        }
    }

    pub fn with_resolution(mut self, resolution: &'a ResolutionMap) -> Self {
        self.resolution = Some(resolution);
        self
    }
}
