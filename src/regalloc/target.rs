#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysReg(pub u8);

pub trait TargetSpec {
    fn allocatable_regs(&self) -> &[PhysReg];
    fn caller_saved(&self) -> &[PhysReg];
    fn callee_saved(&self) -> &[PhysReg];
    fn param_reg(&self, index: u32) -> Option<PhysReg>;
    fn result_reg(&self) -> PhysReg;
    fn indirect_result_reg(&self) -> Option<PhysReg>;
    fn scratch_regs(&self) -> &[PhysReg];
    fn reg_name(&self, reg: PhysReg) -> &'static str;
}
