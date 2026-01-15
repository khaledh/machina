use crate::tree::normalized as nir;
use crate::tree::typed::model as tir;

pub trait ToNormalized {
    type Output;

    fn to_normalized(self) -> Self::Output;
}

impl ToNormalized for tir::Module {
    type Output = nir::Module;

    fn to_normalized(self) -> Self::Output {
        self
    }
}
