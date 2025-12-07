use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrConst, IrOperand, IrTempId, IrTerminator, IrType};

// Helper to build a minimal function with given name / signature
#[allow(dead_code)]
fn mk_builder() -> IrFunctionBuilder {
    IrFunctionBuilder::new(
        "test".to_string(),
        IrType::Int {
            bits: 1,
            signed: false,
        },
    )
}

// Helpers for common types and operands

#[allow(dead_code)]
fn u64_ty() -> IrType {
    IrType::Int {
        bits: 64,
        signed: false,
    }
}

#[allow(dead_code)]
fn bool_ty() -> IrType {
    IrType::Bool
}

#[allow(dead_code)]
fn unit_ty() -> IrType {
    IrType::Unit
}


#[allow(dead_code)]
fn const_u64(value: i64) -> IrOperand {
    IrOperand::Const(IrConst::Int {
        value,
        bits: 64,
        signed: false,
    })
}

#[allow(dead_code)]
fn temp(id: u32) -> IrOperand {
    IrOperand::Temp(IrTempId(id))
}

#[allow(dead_code)]
fn temp_operand(temp: IrTempId) -> IrOperand {
    IrOperand::Temp(temp)
}
