use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrConst, IrOperand, IrTempId, IrTerminator, IrType};

// Helper to build a minimal function with given name / signature
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

fn u32_ty() -> IrType {
    IrType::Int {
        bits: 32,
        signed: false,
    }
}

fn bool_ty() -> IrType {
    IrType::Bool
}

fn unit_ty() -> IrType {
    IrType::Unit
}


fn const_u32(value: i64) -> IrOperand {
    IrOperand::Const(IrConst::Int {
        value,
        bits: 32,
        signed: false,
    })
}

fn temp(id: u32) -> IrOperand {
    IrOperand::Temp(IrTempId(id))
}

fn temp_operand(temp: IrTempId) -> IrOperand {
    IrOperand::Temp(temp)
}
