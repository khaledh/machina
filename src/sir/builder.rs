use crate::sir::model as sir;
use crate::tir::model as tir;

pub trait ToSir {
    type Output;

    fn to_sir(&self) -> Self::Output;
}

impl ToSir for tir::Module {
    type Output = sir::Module;

    fn to_sir(&self) -> Self::Output {
        self.clone()
    }
}

/// Build a SIR module from TIR.
///
/// This is a 1:1 conversion. Semantic lifts are handled by the normalize stage.
pub fn build_module(module: &tir::Module) -> sir::Module {
    module.to_sir()
}
