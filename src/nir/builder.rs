use crate::nir::model as nir;
use crate::tir::model as tir;

pub trait ToNir {
    type Output;

    fn to_nir(self) -> Self::Output;
}

impl ToNir for tir::Module {
    type Output = nir::Module;

    fn to_nir(self) -> Self::Output {
        self
    }
}
