use crate::regalloc::constraints::{analyze_call, analyze_fn_params};
use crate::regalloc::regs::Arm64Reg;

include!("ir_test_utils.rs");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analyze_fn_params() {
        let mut fb = IrFunctionBuilder::new("test".to_string(), u64_ty());
        let p0 = fb.new_param(0, "a".to_string(), u64_ty());
        let p1 = fb.new_param(1, "b".to_string(), u64_ty());
        fb.terminate(IrTerminator::Ret { value: None });
        let func = fb.finish();

        let constraints = analyze_fn_params(&func);

        assert_eq!(constraints.len(), 2);
        assert_eq!(constraints[0].temp, p0);
        assert_eq!(constraints[0].reg, Arm64Reg::X0);
        assert_eq!(constraints[1].temp, p1);
        assert_eq!(constraints[1].reg, Arm64Reg::X1);
    }
}

#[test]
fn test_analyze_call_constraints() {
    // Build a function that calls another function
    let mut fb = IrFunctionBuilder::new("caller".to_string(), u64_ty());

    // Create two temps for arguments
    let arg1 = fb.new_temp(u64_ty());
    let arg2 = fb.new_temp(u64_ty());

    // Make a call: result = callee(arg1, arg2)
    let result = fb.new_temp(u64_ty());
    fb.call(
        Some(result),
        "callee".to_string(),
        vec![temp_operand(arg1), temp_operand(arg2)],
        u64_ty(),
    );

    fb.terminate(IrTerminator::Ret { value: None });
    let func = fb.finish();

    let constraints = analyze_call(&func);

    assert_eq!(constraints.len(), 1);

    let call = &constraints[0];
    assert_eq!(call.args.len(), 2);
    assert_eq!(call.args[0].operand, temp_operand(arg1));
    assert_eq!(call.args[0].reg, Arm64Reg::X0);
    assert_eq!(call.args[1].operand, temp_operand(arg2));
    assert_eq!(call.args[1].reg, Arm64Reg::X1);

    assert!(call.result.is_some());
    let res = call.result.as_ref().unwrap();
    assert_eq!(res.temp, result);
    assert_eq!(res.reg, Arm64Reg::X0);
}
